open When3meet
open When3meet_server_library

(** The port this application starts on. *)
let port = 5000

(** Testing session for now. *)
let main_sess = Session.create_session "main"

(** [session_table] stores sessions with their code as key. *)
let session_table = Hashtbl.create 50

let p_main =
  let%lwt () =
    Websocket_lwt_unix.establish_standard_server
      ~mode:Conduit_lwt_unix.(`TCP (`Port port))
      (Server_logic.handle_client session_table)
  in
  let%lwt _ = Lwt_io.printl "hi" in
  let promise, _ = Lwt.wait () in
  promise

let () =
  Hashtbl.add session_table "main" main_sess;
  Lwt_main.run p_main
