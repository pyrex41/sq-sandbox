(** Irmin store initialization and branch management.

    Uses irmin-pack with string contents (file bytes stored directly).
    Each sandbox maps to an Irmin branch. Snapshots are commits on that branch
    with the label stored in commit info. *)

module Hash = Irmin.Hash.SHA256

(** File metadata stored alongside content in the tree.
    We encode metadata as a prefix in the stored value:
    first line is JSON metadata, rest is file content. *)
module File_contents = struct
  type t = {
    mode : int;
    size : int;
    mtime : float;
    content : string;
  }

  let t =
    let open Irmin.Type in
    record "file_contents" (fun mode size mtime content ->
      { mode; size; mtime; content })
    |+ field "mode" int (fun t -> t.mode)
    |+ field "size" int (fun t -> t.size)
    |+ field "mtime" float (fun t -> t.mtime)
    |+ field "content" string (fun t -> t.content)
    |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end

(** Irmin-pack store configuration *)
module Store_conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end

module Store_maker = Irmin_pack_unix.KV (Store_conf)
module Store = Store_maker.Make (File_contents)

type t = {
  repo : Store.Repo.t;
  config : Config.t;
}

let info ~author msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  Store.Info.v ~author ~message:msg date

(** Initialize the store, creating the data directory if needed. *)
let init config =
  let open Lwt.Syntax in
  let () =
    if not (Sys.file_exists config.Config.store_dir) then
      Unix.mkdir config.store_dir 0o755
  in
  let irmin_config =
    Irmin_pack.config ~fresh:false
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
      config.store_dir
  in
  let+ repo = Store.Repo.v irmin_config in
  { repo; config }

(** Get a branch handle for a sandbox. Creates the branch if it doesn't exist. *)
let branch t sandbox_id =
  Store.of_branch t.repo sandbox_id

(** Clone a branch (O(1) fork). *)
let fork t ~source_id ~target_id =
  let open Lwt.Syntax in
  let* src = Store.of_branch t.repo source_id in
  let+ _dst = Store.clone ~src ~dst:target_id in
  ()

(** List all branches (sandbox IDs with snapshots). *)
let list_branches t =
  Store.Repo.branches t.repo

(** Get the commit history for a branch. *)
let history t sandbox_id =
  let open Lwt.Syntax in
  let* branch = Store.of_branch t.repo sandbox_id in
  let* head = Store.Head.find branch in
  let rec walk acc = function
    | None -> Lwt.return (List.rev acc)
    | Some commit ->
      let hash = Irmin.Type.to_string Store.Hash.t (Store.Commit.hash commit) in
      let info = Store.Commit.info commit in
      let message = Store.Info.message info in
      let date = Store.Info.date info in
      let created = string_of_float (Int64.to_float date) in
      let parents = Store.Commit.parents commit in
      let* parent_commit = match parents with
        | [] -> Lwt.return None
        | p :: _ -> Store.Commit.of_hash t.repo p
      in
      let parent_hash = match parents with
        | [] -> None
        | p :: _ -> Some (Irmin.Type.to_string Store.Hash.t p)
      in
      let entry = (message, created, parent_hash, 0) in
      walk (entry :: acc) parent_commit
  in
  walk [] head
