open Notty

(* A calendar-local date in YYYY-MM-DD format and seconds since midnight. The
   importer supplies all endpoints in the same calendar time zone. *)
type local_time = {
  date : string;
  seconds : int;
}

type interval = {
  starts : local_time;
  ends : local_time;
}

let compare_time a b =
  let by_date = String.compare a.date b.date in
  if by_date = 0 then Int.compare a.seconds b.seconds else by_date

let overlaps event slot_start slot_end =
  compare_time event.starts slot_end < 0
  && compare_time slot_start event.ends < 0

(** [render_week ~dates ~start_hour ~end_hour events] draws a week. [dates]
    contains seven consecutive YYYY-MM-DD dates, Monday first. *)
let render_week ~dates ~start_hour ~end_hour events =
  if List.length dates <> 7 then
    invalid_arg "render_week: expected seven dates, Monday first";
  if start_hour < 0 || end_hour > 24 || start_hour >= end_hour then
    invalid_arg "render_week: invalid hour range";

  let slot_minutes = 30 in
  let days = [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" ] in
  let busy_style = A.(fg white ++ bg blue) in
  let header_style = A.(st bold) in

  (* Every cell is six characters wide, plus a one-character gutter. *)
  let cell style label =
    I.hcat
      [ I.string style (Printf.sprintf "%-6s" label); I.string A.empty " " ]
  in
  let header = I.hcat (List.map (cell header_style) ("Time" :: days)) in

  let render_row i =
    let minute = (start_hour * 60) + (i * slot_minutes) in
    let clock = Printf.sprintf "%02d:%02d" (minute / 60) (minute mod 60) in
    let render_cell date =
      let slot_start = { date; seconds = minute * 60 } in
      let slot_end = { date; seconds = (minute + slot_minutes) * 60 } in
      if List.exists (fun event -> overlaps event slot_start slot_end) events
      then cell busy_style "Busy"
      else cell A.empty ""
    in
    I.hcat (cell A.empty clock :: List.map render_cell dates)
  in

  let row_count = (end_hour - start_hour) * 60 / slot_minutes in
  I.vcat (header :: List.init row_count render_row)
