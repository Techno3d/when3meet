open When3meet

(** [send_status_frame client status] sends a status message to the given
    client. *)
let send_status_frame client status =
  let frame =
    Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Text
      ~content:
        (status |> Server_requests.session_codes_to_yojson
       |> Yojson.Safe.pretty_to_string)
      ()
  in
  Websocket_lwt_unix.Connected_client.send client frame

(** [join_session_handler client request user session] joins a session if the
    session code exists, and sends a status code. *)
let join_session_handler client (request : Server_requests.join_request)
    session_table user session =
  try
    let found_session = Hashtbl.find session_table request.session_code in
    Session.add_user found_session user;
    session := Some found_session;
    send_status_frame client Server_requests.Success
  with Not_found ->
    send_status_frame client Server_requests.Nonexistent_session

(** [create_session_handler client request user session] creates a session if
    one with the same name doesn't already exists, and sends a status code. *)
let create_session_handler client (request : Server_requests.create_request)
    session_table user session =
  try
    let _ = Hashtbl.find session_table request.session_name in
    send_status_frame client Server_requests.Session_already_exists
  with Not_found ->
    let created_session = Session.create_session request.session_name in
    Session.add_user created_session user;
    Hashtbl.add session_table request.session_name created_session;
    session := Some created_session;
    send_status_frame client Server_requests.Success

(** [list_session_users_handler client user session] lists the users of the
    current session. *)
let list_session_users_handler client (session : Session.t) =
  let user_frame =
    Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Text
      ~content:
        (session.users |> List.map Session.user_name
       |> Server_requests.user_list_to_yojson |> Yojson.Safe.pretty_to_string)
      ()
  in
  Websocket_lwt_unix.Connected_client.send client user_frame

(** [handle_client client] handles an incoming websocket connection. The
    connection is kept until we recieve a close code from the client. *)
let handle_client session_table client =
  let rec loop user session =
    let%lwt frame = Websocket_lwt_unix.Connected_client.recv client in
    match frame.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Text ->
        let request =
          Server_requests.requests_of_yojson
            (Yojson.Safe.from_string frame.content)
        in
        let%lwt () =
          match request with
          | Error _ ->
              send_status_frame client
                (Server_requests.Malformed_message "Malformed JSON response")
          | Ok request ->
              begin match (request, !user, !session) with
              | Server_requests.Authenticate request, None, _ -> begin
                  user := Some (Session.create_user request.user_name);
                  send_status_frame client Server_requests.Success
                end
              | Server_requests.Authenticate _, Some _, _ -> begin
                  send_status_frame client Server_requests.Already_authenticated
                end
              | Server_requests.Join request, Some curr_user, _ ->
                  join_session_handler client request session_table curr_user
                    session
              | Server_requests.Create request, Some curr_user, _ ->
                  create_session_handler client request session_table curr_user
                    session
              | Server_requests.Join _, None, _
              | Server_requests.Create _, None, _ -> begin
                  send_status_frame client Server_requests.Not_authenticated
                end
              | Server_requests.List_session_users, Some _, Some curr_session ->
              begin list_session_users_handler client curr_session
                end
              | Server_requests.List_session_users, _, _ -> begin
                  send_status_frame client Server_requests.Nonexistent_session
                end
              end
        in
        loop user session
    | Websocket.Frame.Opcode.Close -> Lwt.return_unit
    | _ -> loop user session
  in
  let user = ref None in
  let session = ref None in
  loop user session
