(** S3 blob sync for the Irmin CAS store.

    Pushes content-addressed blobs to S3 using the sq-s3 CLI tool
    (the same tool used by sq-sync for squashfs snapshots).
    Only new blobs are pushed — existing ones are skipped.

    Key scheme: cas/{sha256[0:2]}/{sha256}.zst
    Commit metadata: cas/meta/{sandbox_id}/{label}.json *)

(** Check if S3 is configured (SQUASH_S3_BUCKET is set). *)
let s3_enabled config =
  config.Config.s3_bucket <> None

(** Push a single blob to S3 if it doesn't already exist.
    Uses sq-s3 CLI for transport (same as sq-sync).
    Returns true if the blob was pushed, false if it already existed. *)
let push_blob config ~hash ~data =
  match config.Config.s3_bucket with
  | None -> Lwt.return false
  | Some _bucket ->
    let prefix = String.sub hash 0 2 in
    let s3_key = Printf.sprintf "cas/%s/%s.zst" prefix hash in

    (* Write blob to temp file *)
    let tmp_path = Filename.temp_file "sq-store-blob-" ".zst" in
    let oc = open_out_bin tmp_path in
    output_string oc data;
    close_out oc;

    (* Check if blob exists on S3 *)
    let exists_cmd = Printf.sprintf "sq-s3 exists %s 2>/dev/null" s3_key in
    let exists_code = Sys.command exists_cmd in
    if exists_code = 0 then begin
      Unix.unlink tmp_path;
      Lwt.return false
    end else begin
      (* Push blob *)
      let push_cmd = Printf.sprintf "sq-s3 push %s %s 2>/dev/null" tmp_path s3_key in
      let push_code = Sys.command push_cmd in
      Unix.unlink tmp_path;
      Lwt.return (push_code = 0)
    end

(** Pull a blob from S3 by hash. Returns the blob data or None. *)
let pull_blob config ~hash =
  match config.Config.s3_bucket with
  | None -> Lwt.return_none
  | Some _bucket ->
    let prefix = String.sub hash 0 2 in
    let s3_key = Printf.sprintf "cas/%s/%s.zst" prefix hash in
    let tmp_path = Filename.temp_file "sq-store-pull-" ".zst" in
    let pull_cmd = Printf.sprintf "sq-s3 pull %s %s 2>/dev/null" s3_key tmp_path in
    let code = Sys.command pull_cmd in
    if code = 0 && Sys.file_exists tmp_path then begin
      let ic = open_in_bin tmp_path in
      let len = in_channel_length ic in
      let data = Bytes.create len in
      really_input ic data 0 len;
      close_in ic;
      Unix.unlink tmp_path;
      Lwt.return_some (Bytes.unsafe_to_string data)
    end else begin
      (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
      Lwt.return_none
    end

(** Push snapshot commit metadata to S3.
    Stores a small JSON file with tree info for remote restore. *)
let push_commit_metadata config ~sandbox_id ~label ~commit_hash ~tree_size ~file_count =
  match config.Config.s3_bucket with
  | None -> Lwt.return_unit
  | Some _bucket ->
    let s3_key = Printf.sprintf "cas/meta/%s/%s.json" sandbox_id label in
    let json = Printf.sprintf
      "{\"sandbox_id\":\"%s\",\"label\":\"%s\",\"commit\":\"%s\",\"size\":%d,\"files\":%d,\"created\":\"%s\"}"
      sandbox_id label commit_hash tree_size file_count
      (let t = Unix.gettimeofday () in
       let tm = Unix.gmtime t in
       Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
         (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
         tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
    in
    let tmp_path = Filename.temp_file "sq-store-meta-" ".json" in
    let oc = open_out tmp_path in
    output_string oc json;
    close_out oc;
    let push_cmd = Printf.sprintf "sq-s3 push %s %s 2>/dev/null" tmp_path s3_key in
    let _code = Sys.command push_cmd in
    (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
    Lwt.return_unit

(** Sync all new blobs from a snapshot to S3.
    Walks the Irmin tree for the given commit and pushes any blobs
    whose hash isn't already on S3.

    This is called after snapshot creation when S3 is configured.
    Returns the number of blobs pushed. *)
let sync_snapshot_blobs config store ~sandbox_id ~label =
  let open Lwt.Syntax in
  if not (s3_enabled config) then
    Lwt.return 0
  else begin
    let* branch = Store.branch store sandbox_id in
    let* head = Store.Store.Head.find branch in
    match head with
    | None -> Lwt.return 0
    | Some commit ->
      let commit_hash =
        Irmin.Type.to_string Store.Hash.t (Store.Store.Commit.hash commit)
      in
      let tree = Store.Store.Commit.tree commit in
      let* entries = Materializer.collect_tree tree [] in
      let pushed = ref 0 in
      let* () = Lwt_list.iter_s (fun (_key, (file : Store.File_contents.t)) ->
        (* Hash the content to get the CAS key *)
        let hash = Digestif.SHA256.(to_hex (digest_string file.content)) in
        let* was_pushed = push_blob config ~hash ~data:file.content in
        if was_pushed then incr pushed;
        Lwt.return_unit
      ) entries in
      (* Push commit metadata *)
      let total_size = List.fold_left (fun acc (_, (f : Store.File_contents.t)) ->
        acc + f.size
      ) 0 entries in
      let* () = push_commit_metadata config ~sandbox_id ~label
        ~commit_hash ~tree_size:total_size ~file_count:(List.length entries) in
      Lwt.return !pushed
  end
