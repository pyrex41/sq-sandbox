(** JSON protocol for the sq-store bus.

    Request-response over Unix domain socket:
    - Client sends one JSON line
    - Server reads it, processes, writes one JSON line back
    - Client reads response, connection closes *)

type request =
  | Snapshot of { sandbox_id : string; label : string; upper_data : string }
  | Restore of { sandbox_id : string; label : string; upper_data : string }
  | Fork of { source_id : string; source_label : string; target_id : string }
  | Diff of { sandbox_id : string; from_label : string; to_label : string }
  | History of { sandbox_id : string }
  | Shutdown

type snapshot_stats = {
  files : int;
  blobs_new : int;
  blobs_reused : int;
}

type response =
  | Ok_snapshot of {
      label : string;
      size : int;
      commit : string;
      stats : snapshot_stats;
    }
  | Ok_restore of {
      label : string;
      files_written : int;
      files_deleted : int;
    }
  | Ok_fork
  | Ok_diff of {
      added : string list;
      modified : string list;
      deleted : string list;
    }
  | Ok_history of {
      snapshots : (string * string * string option * int) list;
      (* label, created, parent, size *)
    }
  | Ok_shutdown
  | Error of string

let parse_request json_str =
  try
    let j = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let op = j |> member "op" |> to_string in
    match op with
    | "snapshot" ->
      let sandbox_id = j |> member "sandbox_id" |> to_string in
      let label = j |> member "label" |> to_string in
      let upper_data = j |> member "upper_data" |> to_string in
      Some (Snapshot { sandbox_id; label; upper_data })
    | "restore" ->
      let sandbox_id = j |> member "sandbox_id" |> to_string in
      let label = j |> member "label" |> to_string in
      let upper_data = j |> member "upper_data" |> to_string in
      Some (Restore { sandbox_id; label; upper_data })
    | "fork" ->
      let source_id = j |> member "source_id" |> to_string in
      let source_label = j |> member "source_label" |> to_string in
      let target_id = j |> member "target_id" |> to_string in
      Some (Fork { source_id; source_label; target_id })
    | "diff" ->
      let sandbox_id = j |> member "sandbox_id" |> to_string in
      let from_label = j |> member "from" |> to_string in
      let to_label = j |> member "to" |> to_string in
      Some (Diff { sandbox_id; from_label; to_label })
    | "history" ->
      let sandbox_id = j |> member "sandbox_id" |> to_string in
      Some (History { sandbox_id })
    | "shutdown" -> Some Shutdown
    | _ -> None
  with _ -> None

let response_to_json = function
  | Ok_snapshot { label; size; commit; stats } ->
    `Assoc [
      ("ok", `Bool true);
      ("label", `String label);
      ("size", `Int size);
      ("commit", `String commit);
      ("stats", `Assoc [
        ("files", `Int stats.files);
        ("blobs_new", `Int stats.blobs_new);
        ("blobs_reused", `Int stats.blobs_reused);
      ]);
    ]
  | Ok_restore { label; files_written; files_deleted } ->
    `Assoc [
      ("ok", `Bool true);
      ("label", `String label);
      ("files_written", `Int files_written);
      ("files_deleted", `Int files_deleted);
    ]
  | Ok_fork ->
    `Assoc [ ("ok", `Bool true) ]
  | Ok_diff { added; modified; deleted } ->
    `Assoc [
      ("ok", `Bool true);
      ("added", `List (List.map (fun s -> `String s) added));
      ("modified", `List (List.map (fun s -> `String s) modified));
      ("deleted", `List (List.map (fun s -> `String s) deleted));
    ]
  | Ok_history { snapshots } ->
    let snap_json (label, created, parent, size) =
      let base = [
        ("label", `String label);
        ("created", `String created);
        ("size", `Int size);
      ] in
      let base = match parent with
        | Some p -> ("parent", `String p) :: base
        | None -> ("parent", `Null) :: base
      in
      `Assoc base
    in
    `Assoc [
      ("ok", `Bool true);
      ("snapshots", `List (List.map snap_json snapshots));
    ]
  | Ok_shutdown ->
    `Assoc [ ("ok", `Bool true) ]
  | Error msg ->
    `Assoc [ ("ok", `Bool false); ("error", `String msg) ]

let response_to_string resp =
  Yojson.Safe.to_string (response_to_json resp)
