(** High-level snapshot operations: snapshot, restore, fork, diff, history.

    These are the operations exposed via the bus protocol. Each takes a store
    handle and returns a protocol response. *)

(** Create a snapshot of a sandbox's upper/data directory. *)
let create store ~sandbox_id ~label ~upper_data =
  let open Lwt.Syntax in
  try%lwt
    if not (Sys.file_exists upper_data) then
      Lwt.return (Protocol.Error
        (Printf.sprintf "upper_data directory not found: %s" upper_data))
    else begin
      let* (commit_hash, total_size, stats) =
        Walker.snapshot store sandbox_id label upper_data
      in
      (* Sync new blobs to S3 if configured *)
      let* _pushed = S3_sync.sync_snapshot_blobs store.Store.config store
        ~sandbox_id ~label in
      Lwt.return (Protocol.Ok_snapshot {
        label;
        size = total_size;
        commit = commit_hash;
        stats;
      })
    end
  with exn ->
    Lwt.return (Protocol.Error
      (Printf.sprintf "snapshot failed: %s" (Printexc.to_string exn)))

(** Restore a snapshot by materializing files into the target directory. *)
let restore store ~sandbox_id ~label ~upper_data =
  let open Lwt.Syntax in
  try%lwt
    let _ = label in  (* label is used to find the commit; for now we use HEAD *)
    let* (files_written, files_deleted) =
      Materializer.materialize store sandbox_id label upper_data
    in
    Lwt.return (Protocol.Ok_restore { label; files_written; files_deleted })
  with exn ->
    Lwt.return (Protocol.Error
      (Printf.sprintf "restore failed: %s" (Printexc.to_string exn)))

(** Fork a sandbox's snapshot history to a new sandbox ID. O(1) branch clone. *)
let fork store ~source_id ~source_label:_ ~target_id =
  try%lwt
    let open Lwt.Syntax in
    let* () = Store.fork store ~source_id ~target_id in
    Lwt.return Protocol.Ok_fork
  with exn ->
    Lwt.return (Protocol.Error
      (Printf.sprintf "fork failed: %s" (Printexc.to_string exn)))

(** Diff two snapshots, returning lists of added/modified/deleted paths. *)
let diff store ~sandbox_id ~from_label ~to_label =
  let open Lwt.Syntax in
  try%lwt
    let _ = from_label and _ = to_label in  (* TODO: look up commits by label *)
    let* branch = Store.branch store sandbox_id in
    let* head = Store.Store.Head.find branch in
    match head with
    | None ->
      Lwt.return (Protocol.Error "no snapshots found")
    | Some commit ->
      let parents = Store.Store.Commit.parents commit in
      (match parents with
       | [] ->
         (* No parent, everything is "added" *)
         let tree = Store.Store.Commit.tree commit in
         let* entries = Materializer.collect_tree tree [] in
         let added = List.map (fun (key, _) -> String.concat "/" key) entries in
         Lwt.return (Protocol.Ok_diff { added; modified = []; deleted = [] })
       | parent_hash :: _ ->
         let* parent = Store.Store.Commit.of_hash store.Store.repo parent_hash in
         (match parent with
          | None ->
            Lwt.return (Protocol.Error "parent commit not found")
          | Some parent_commit ->
            let current_tree = Store.Store.Commit.tree commit in
            let parent_tree = Store.Store.Commit.tree parent_commit in
            let* diffs = Store.Store.Tree.diff parent_tree current_tree in
            let added = ref [] in
            let modified = ref [] in
            let deleted = ref [] in
            List.iter (fun (key, change) ->
              let path = String.concat "/" key in
              match change with
              | `Added _ -> added := path :: !added
              | `Removed _ -> deleted := path :: !deleted
              | `Updated _ -> modified := path :: !modified
            ) diffs;
            Lwt.return (Protocol.Ok_diff {
              added = List.rev !added;
              modified = List.rev !modified;
              deleted = List.rev !deleted;
            })))
  with exn ->
    Lwt.return (Protocol.Error
      (Printf.sprintf "diff failed: %s" (Printexc.to_string exn)))

(** Get snapshot history for a sandbox. *)
let history store ~sandbox_id =
  try%lwt
    let open Lwt.Syntax in
    let* entries = Store.history store sandbox_id in
    Lwt.return (Protocol.Ok_history { snapshots = entries })
  with exn ->
    Lwt.return (Protocol.Error
      (Printf.sprintf "history failed: %s" (Printexc.to_string exn)))
