open When3meet

let public_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000)
let main_sess = Session.create_session "main"

(** [session_table] stores sessions with their code as key. *)
let session_table = Hashtbl.create 50

let create_status_frame error =
  Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Text
    ~content:
      (error |> Server_requests.session_codes_to_yojson
     |> Yojson.Safe.pretty_to_string)
    ()

let handle_client client =
  let rec loop user session =
    let%lwt frame = Websocket_lwt_unix.Connected_client.recv client in
    match frame.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Text ->
        let request =
          Server_requests.requests_of_yojson
            (Yojson.Safe.from_string frame.content)
        in
        begin match request with
        | Error _ ->
            let error_frame =
              create_status_frame
                (Server_requests.Malformed_message "Malformed JSON response")
            in
            let%lwt _ =
              Websocket_lwt_unix.Connected_client.send client error_frame
            in
            Lwt.return_unit
        | Ok request ->
            begin match (request, !user, !session) with
            | Server_requests.Authenticate request, None, None -> begin
                user := Some (Session.create_user request.user_name);
                let success_frame =
                  create_status_frame Server_requests.Success
                in
                let%lwt () =
                  Websocket_lwt_unix.Connected_client.send client success_frame
                in
                loop user session
              end
            | Server_requests.Authenticate _, Some _, _ -> begin
                let error_frame =
                  create_status_frame Server_requests.Already_authenticated
                in
                let%lwt () =
                  Websocket_lwt_unix.Connected_client.send client error_frame
                in
                loop user session
              end
            | Server_requests.Join request, Some curr_user, _ ->
                begin try
                  let found_session =
                    Hashtbl.find session_table request.session_code
                  in
                  Session.add_user found_session curr_user;
                  session := Some found_session;
                  let success_frame =
                    create_status_frame Server_requests.Success
                  in
                  let%lwt () =
                    Websocket_lwt_unix.Connected_client.send client
                      success_frame
                  in
                  loop user session
                with Not_found ->
                  let error_frame =
                    create_status_frame Server_requests.Nonexistent_session
                  in
                  let%lwt () =
                    Websocket_lwt_unix.Connected_client.send client error_frame
                  in
                  loop user session
                end
            | Server_requests.Create request, Some curr_user, _ ->
                begin try
                  let _ = Hashtbl.find session_table request.session_name in
                  let error_frame =
                    create_status_frame Server_requests.Session_already_exists
                  in
                  let%lwt () =
                    Websocket_lwt_unix.Connected_client.send client error_frame
                  in
                  loop user session
                with Not_found ->
                  let created_session =
                    Session.create_session request.session_name
                  in
                  Session.add_user created_session curr_user;
                  Hashtbl.add session_table request.session_name created_session;
                  session := Some created_session;
                  let success_frame =
                    create_status_frame Server_requests.Success
                  in
                  let%lwt () =
                    Websocket_lwt_unix.Connected_client.send client
                      success_frame
                  in
                  loop user session
                end
            | Server_requests.Join _, None, _
            | Server_requests.Create _, None, _ -> begin
                let error_frame =
                  create_status_frame Server_requests.Not_authenticated
                in
                let%lwt () =
                  Websocket_lwt_unix.Connected_client.send client error_frame
                in
                loop user session
              end
            | _ -> Lwt.return_unit
            end
        end
    | Websocket.Frame.Opcode.Close -> Lwt.return_unit
    | _ -> loop user session
  in
  let user = ref None in
  let session = ref None in
  loop user session

let p_main =
  let%lwt () =
    Websocket_lwt_unix.establish_standard_server
      ~mode:Conduit_lwt_unix.(`TCP (`Port 5000))
      handle_client
  in
  let%lwt _ = Lwt_io.printl "hi" in
  let promise, _ = Lwt.wait () in
  promise

let () =
  Hashtbl.add session_table "main" main_sess;
  Lwt_main.run p_main
