(** [Server_requests] represents the types of messages the server expects. *)

open Yojson.Basic.Util

type authenticate = { user_name : string }
(** [authenticate] is a request used to get the user's name for session. *)

type join_request = {
  session_code : string;
  session_password : string option;
}
(** [join_request] is used for a client to request access to a session, adding
    them to the session's list of users. If the session being requested has a
    password, the correct password must be provided. *)

type create_request = {
  session_name : string;
  session_password : string option;
}
(** [create_session_request] is used for a client to request a session, creating
    the session and adding them to the session's list of users. If the password
    is set, then the session is password authenticated. *)

(** [requests] is the base type of all possible requests that the server will
    accept. *)
type requests =
  | Authenticate of authenticate
  | Join of join_request
  | Create of create_request
  | List_session_users
[@@deriving yojson]

type user_list = string list [@@deriving yojson]
(** [user_list] is a list of users by their name. *)

(** [session_join_codes] represents the response a server can give. *)
type session_codes =
  | Malformed_message of string
  | Password_incorrect
  | Nonexistent_session
  | Session_already_exists
  | Already_authenticated
  | Not_authenticated
  | Success
[@@deriving yojson]
