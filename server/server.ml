let public_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000)

let handle_client client_address (_client_in, _client_out) =
  Lwt_io.printlf "Connection from %s." ""

let p_main =
  let%lwt server =
    Lwt_io.establish_server_with_client_address public_address handle_client
  in
  let%lwt () = Lwt_io.printl "Listening. Press Enter to stop." in
  let%lwt _line = Lwt_io.(read_line stdin) in
  Lwt_io.shutdown_server server

let () = Lwt_main.run p_main
