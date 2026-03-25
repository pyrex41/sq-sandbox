(** Materializer: write files from an Irmin tree to a filesystem directory.

    For restore operations, materializes the snapshot's tree into upper/data.
    Performs incremental restore when possible: only writes changed files,
    removes deleted files, skips unchanged files. *)

(** Recursively list all files in a directory with their stats.
    Returns (relative_path, stat) pairs. *)
let scan_dir root =
  Walker.walk_dir root ""

(** Remove a file or directory recursively. *)
let rec remove_path path =
  let stats = Unix.lstat path in
  match stats.Unix.st_kind with
  | Unix.S_DIR ->
    let entries = Sys.readdir path |> Array.to_list in
    List.iter (fun name -> remove_path (Filename.concat path name)) entries;
    Unix.rmdir path
  | _ -> Unix.unlink path

(** Ensure parent directory exists. *)
let ensure_parent path =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if Sys.file_exists d then ()
    else begin
      mkdir_p (Filename.dirname d);
      (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    end
  in
  mkdir_p dir

(** Collect all entries from an Irmin tree as (path_components, file_contents). *)
let rec collect_tree tree prefix =
  let open Lwt.Syntax in
  let* entries = Store.Store.Tree.list tree prefix in
  Lwt_list.concat_map_s (fun (step, subtree) ->
    let key = prefix @ [step] in
    let* contents = Store.Store.Tree.find subtree [] in
    match contents with
    | Some file ->
      Lwt.return [(key, file)]
    | None ->
      (* It's a directory node, recurse *)
      collect_tree subtree []
      |> Lwt.map (List.map (fun (k, v) -> (key @ k, v)))
  ) entries

(** Materialize an Irmin tree into a target directory.
    Returns (files_written, files_deleted). *)
let materialize store sandbox_id label target_dir =
  let open Lwt.Syntax in
  (* Get the branch and find the commit for this label *)
  let* branch = Store.branch store sandbox_id in
  let* head = Store.Store.Head.find branch in

  let commit = match head with
    | None -> failwith (Printf.sprintf "no snapshots found for %s" sandbox_id)
    | Some c -> c
  in

  (* Walk the commit tree to get all files *)
  let* tree = Store.Store.Commit.tree commit |> Lwt.return in
  let* tree_entries = collect_tree tree [] in

  (* Build a set of paths that should exist *)
  let target_paths = Hashtbl.create 256 in
  List.iter (fun (key, _) ->
    let rel = String.concat "/" key in
    Hashtbl.replace target_paths rel true
  ) tree_entries;

  (* Scan current directory to find files to delete *)
  let current_files =
    if Sys.file_exists target_dir then scan_dir target_dir
    else []
  in

  let files_deleted = ref 0 in
  let files_written = ref 0 in

  (* Delete files not in target tree *)
  List.iter (fun (rel, _stats) ->
    if not (Hashtbl.mem target_paths rel) then begin
      let full = Filename.concat target_dir rel in
      (try remove_path full; incr files_deleted
       with Unix.Unix_error _ -> ())
    end
  ) current_files;

  (* Write files from tree *)
  List.iter (fun (key, (file : Store.File_contents.t)) ->
    let rel = String.concat "/" key in
    let full = Filename.concat target_dir rel in
    (* Check if file already exists with same content *)
    let needs_write =
      if Sys.file_exists full then begin
        try
          let stats = Unix.lstat full in
          (* Quick check: if size and mtime match, skip *)
          not (stats.Unix.st_size = file.size &&
               abs_float (stats.Unix.st_mtime -. file.mtime) < 0.001)
        with Unix.Unix_error _ -> true
      end else true
    in
    if needs_write then begin
      ensure_parent full;
      let oc = open_out_bin full in
      output_string oc file.content;
      close_out oc;
      Unix.chmod full file.mode;
      Unix.utimes full file.mtime file.mtime;
      incr files_written
    end
  ) tree_entries;

  Lwt.return (!files_written, !files_deleted)
