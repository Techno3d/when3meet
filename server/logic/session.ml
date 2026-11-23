type t = {
  name : string;
  pass_phrase : string option;
  mutable users : string list;
}

(* Start user*)

type user = string
(** [user] is the name of a user. *)

(** [create_user name] returns a user with the given name. *)
let create_user name = name

(** [add_user session user] modifies session to add the user to the list, if the
    user hasn't already joined before. *)
let add_user session user =
  if List.for_all (fun u -> u <> user) session.users then
    session.users <- user :: session.users
  else ()

(** [user_name user] returns the name of the user. *)
let user_name user = user

(* End User *)

(** [session_name session] returns the name of the session. *)
let session_name (session : t) = session.name

(** [session_users session] returns the users in the session. *)
let session_users session = session.users

(** [create_session name] creates a session with the given name. *)
let create_session name = { name; pass_phrase = None; users = [] }
