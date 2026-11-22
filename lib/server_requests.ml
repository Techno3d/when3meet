open Yojson.Basic.Util

type join_request = {
  user_name : string;
  session_code : string;
  session_password : string option;
}
[@@deriving yojson]
