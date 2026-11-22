open When3meet

let public_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000)
let main_sess = Session.create_session "main"

let handle_client client =
  let rec loop () =
    let%lwt frame = Websocket_lwt_unix.Connected_client.recv client in
    match frame.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Text ->
        (* Parse frame.content as JSON, handle the request, then send a text
           response. *)
        let request =
          Server_requests.join_request_of_yojson
            (Yojson.Safe.from_string frame.content)
        in
        begin match request with
        | Error _ -> loop ()
        | Ok request -> begin
            let () =
              Session.add_user main_sess (Session.create_user request.user_name)
            in
            let%lwt () =
              Lwt_io.printlf "Added user %s to main session" request.user_name
            in
            loop ()
          end
        end
    | Websocket.Frame.Opcode.Close -> Lwt.return_unit
    | _ -> loop ()
  in
  loop ()

let p_main =
  let%lwt () =
    Websocket_lwt_unix.establish_standard_server
      ~mode:Conduit_lwt_unix.(`TCP (`Port 5000))
      handle_client
  in
  let%lwt _ = Lwt_io.printl "hi" in
  let promise, _ = Lwt.wait () in
  promise

let () = Lwt_main.run p_main
