(** Filesystem walker: recursively walk a directory tree and store each file
    in the Irmin store. Returns snapshot statistics. *)

(** Walk a directory recursively, returning all file paths relative to root. *)
let rec walk_dir root prefix =
  let full = Filename.concat root prefix in
  let entries = Sys.readdir full |> Array.to_list in
  List.concat_map (fun name ->
    let rel = if prefix = "" then name else Filename.concat prefix name in
    let path = Filename.concat root rel in
    let stats = Unix.lstat path in
    match stats.Unix.st_kind with
    | Unix.S_REG -> [ (rel, stats) ]
    | Unix.S_DIR -> walk_dir root rel
    | Unix.S_LNK -> [ (rel, stats) ]  (* store symlinks as their target *)
    | _ -> []  (* skip special files *)
  ) entries

(** Read file contents. For symlinks, store the link target. *)
let read_file path stats =
  match stats.Unix.st_kind with
  | Unix.S_LNK -> Unix.readlink path
  | _ ->
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    close_in ic;
    Bytes.unsafe_to_string buf

(** Split a relative path into Irmin key components. *)
let path_to_key rel_path =
  String.split_on_char '/' rel_path

(** Snapshot a directory into the Irmin store.
    Returns (commit_hash, stats). *)
let snapshot store sandbox_id label upper_data =
  let open Lwt.Syntax in
  let files = walk_dir upper_data "" in
  let branch = Store.branch store sandbox_id in
  let* t = branch in

  let blobs_new = ref 0 in
  let blobs_reused = ref 0 in
  let total_size = ref 0 in

  (* Build the tree atomically *)
  let* () = Store.with_tree_exn t []
    ~info:(fun () -> Store.info ~author:"sq-store" label)
    ~strategy:`Set
    (fun _old_tree ->
      let tree = Store.Tree.empty () in
      let* tree = Lwt_list.fold_left_s (fun tree (rel_path, stats) ->
        let full_path = Filename.concat upper_data rel_path in
        let content = read_file full_path stats in
        let file = Store.File_contents.{
          mode = stats.Unix.st_perm;
          size = stats.Unix.st_size;
          mtime = stats.Unix.st_mtime;
          content;
        } in
        total_size := !total_size + stats.Unix.st_size;
        (* Check if this exact content already exists at this path *)
        let key = path_to_key rel_path in
        let* existing = Store.Tree.find tree key in
        (match existing with
         | Some old when old.content = content ->
           incr blobs_reused
         | _ ->
           incr blobs_new);
        Store.Tree.add tree key file
      ) tree files in
      Lwt.return_some tree)
  in

  (* Get the commit hash *)
  let* head = Store.Head.find t in
  let commit_hash = match head with
    | Some c -> Irmin.Type.to_string Store.Hash.t (Store.Commit.hash c)
    | None -> "unknown"
  in

  let stats = Protocol.{
    files = List.length files;
    blobs_new = !blobs_new;
    blobs_reused = !blobs_reused;
  } in
  Lwt.return (commit_hash, !total_size, stats)
