(** sq-store — content-addressed snapshot sidecar for sq-sandbox.

    Listens on a Unix domain socket for snapshot/restore/fork/diff/history
    requests. Uses Irmin-pack as the backing content-addressed store.

    Protocol: newline-delimited JSON, request-response.
    Client sends one JSON line, server processes and returns one JSON line. *)

let src = Logs.Src.create "sq-store" ~doc:"sq-store sidecar"
module Log = (val Logs.src_log src : Logs.LOG)

(** Handle a single client connection. *)
let handle_client store ic oc =
  let open Lwt.Syntax in
  try%lwt
    let* line = Lwt_io.read_line_opt ic in
    match line with
    | None -> Lwt.return_unit
    | Some line ->
      Log.info (fun m -> m "request: %s" line);
      let request = Sq_store.Protocol.parse_request line in
      let* response = match request with
        | None ->
          Lwt.return (Sq_store.Protocol.Error
            (Printf.sprintf "unknown request: %s" line))
        | Some (Sq_store.Protocol.Snapshot { sandbox_id; label; upper_data }) ->
          Sq_store.Snapshot.create store ~sandbox_id ~label ~upper_data
        | Some (Sq_store.Protocol.Restore { sandbox_id; label; upper_data }) ->
          Sq_store.Snapshot.restore store ~sandbox_id ~label ~upper_data
        | Some (Sq_store.Protocol.Fork { source_id; source_label; target_id }) ->
          Sq_store.Snapshot.fork store ~source_id ~source_label ~target_id
        | Some (Sq_store.Protocol.Diff { sandbox_id; from_label; to_label }) ->
          Sq_store.Snapshot.diff store ~sandbox_id ~from_label ~to_label
        | Some (Sq_store.Protocol.History { sandbox_id }) ->
          Sq_store.Snapshot.history store ~sandbox_id
        | Some Sq_store.Protocol.Shutdown ->
          Lwt.return Sq_store.Protocol.Ok_shutdown
      in
      let json = Sq_store.Protocol.response_to_string response in
      Log.info (fun m -> m "response: %s" json);
      let* () = Lwt_io.write_line oc json in
      let* () = Lwt_io.flush oc in
      (* If shutdown was requested, exit after responding *)
      (match request with
       | Some Sq_store.Protocol.Shutdown ->
         Log.info (fun m -> m "shutdown requested, exiting");
         exit 0
       | _ -> Lwt.return_unit)
  with
  | End_of_file -> Lwt.return_unit
  | exn ->
    Log.err (fun m -> m "client error: %s" (Printexc.to_string exn));
    (try%lwt
       let json = Sq_store.Protocol.response_to_string
         (Sq_store.Protocol.Error (Printexc.to_string exn)) in
       Lwt_io.write_line oc json
     with _ -> Lwt.return_unit)

(** Start the Unix socket server. *)
let run_server config store =
  let open Lwt.Syntax in
  let sock_path = config.Sq_store.Config.sock_path in

  (* Clean up stale socket *)
  (try Unix.unlink sock_path with Unix.Unix_error _ -> ());

  let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX sock_path in
  let* () = Lwt_unix.bind socket addr in
  Lwt_unix.listen socket 16;
  Unix.chmod sock_path 0o660;

  Log.info (fun m -> m "listening on %s" sock_path);
  Log.info (fun m -> m "store dir: %s" config.store_dir);

  (* Accept loop *)
  let rec accept_loop () =
    let* (client_fd, _addr) = Lwt_unix.accept socket in
    (* Handle each client in its own lightweight thread *)
    Lwt.async (fun () ->
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input client_fd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output client_fd in
      Lwt.finalize
        (fun () -> handle_client store ic oc)
        (fun () ->
          (try%lwt Lwt_io.close ic with _ -> Lwt.return_unit) >>= fun () ->
          (try%lwt Lwt_io.close oc with _ -> Lwt.return_unit))
    );
    accept_loop ()
  in
  accept_loop ()

let () =
  (* Set up logging *)
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);

  (* Load configuration *)
  let config = Sq_store.Config.from_env () in

  Lwt_main.run begin
    let open Lwt.Syntax in
    (* Initialize the Irmin store *)
    let* store = Sq_store.Store.init config in
    Log.info (fun m -> m "sq-store initialized");

    (* Run the server *)
    run_server config store
  end
