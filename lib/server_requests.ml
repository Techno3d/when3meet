open Yojson.Basic.Util

type authenticate = { user_name : string } [@@deriving yojson]

type join_request = {
  session_code : string;
  session_password : string option;
}
[@@deriving yojson]

type create_request = {
  session_name : string;
  session_password : string option;
}
[@@deriving yojson]

type requests =
  | Authenticate of authenticate
  | Join of join_request
  | Create of create_request
[@@deriving yojson]

type session_codes =
  | Malformed_message of string
  | Password_incorrect
  | Nonexistent_session
  | Session_already_exists
  | Already_authenticated
  | Not_authenticated
  | Success
[@@deriving yojson]
