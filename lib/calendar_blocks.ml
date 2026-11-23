type event = {
  title : string;
  start_min : int;
  end_min : int;
}
(*Make it as an ICal JSON*)

(** One timed event on the day being displayed. Times are minutes after
    midnight. The end time is exclusive. *)

let time_string minutes =
  Printf.sprintf "%02d:%02d" (minutes / 60) (minutes mod 60)

(** [render_day ~day ~start_hour ~end_hour events] draws one row per half hour.
    The caller supplies events for [day] in display order. Events that overlap a
    row are shown side by side. *)
let render_day ~day ~start_hour ~end_hour events =
  if start_hour < 0 || end_hour > 24 || start_hour >= end_hour then
    invalid_arg "render_day: expected 0 <= start_hour < end_hour <= 24";

  let first_min = start_hour * 60 in
  let row_count = (end_hour - start_hour) * 2 in

  let render_row row =
    let slot_start = first_min + (row * 30) in
    let slot_end = slot_start + 30 in

    let block_for_event event =
      if event.start_min >= slot_end || event.end_min <= slot_start then None
      else if row = 0 || event.start_min >= slot_start then
        Some
          (Printf.sprintf "[%s %s-%s]" event.title
             (time_string event.start_min)
             (time_string event.end_min))
      else Some (Printf.sprintf "[| %s]" event.title)
    in

    let blocks = List.filter_map block_for_event events in
    Printf.sprintf "%s | %s" (time_string slot_start)
      (String.concat "  " blocks)
  in

  String.concat "\n" (day :: List.init row_count render_row)

(** [render_week ~start_hour ~end_hour schedule] renders Monday through Sunday
    with one row per half hour. Use the day names shown in [days]. *)
let render_week ~start_hour ~end_hour schedule =
  if start_hour < 0 || end_hour > 24 || start_hour >= end_hour then
    invalid_arg "render_week: expected 0 <= start_hour < end_hour <= 24";

  let days = [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" ] in

  (* Look up each day's events once, in calendar order. *)
  let events_by_day =
    List.map
      (fun day ->
        List.filter_map
          (fun (name, events) -> if name = day then Some events else None)
          schedule
        |> List.concat)
      days
  in

  (* Build the text for one day at one half-hour slot. *)
  let render_cell slot_start events =
    let slot_end = slot_start + 30 in
    let label event =
      if event.start_min < slot_end && event.end_min > slot_start then
        Some event.title
      else None
    in
    List.filter_map label events |> String.concat " / "
  in

  let rows =
    List.init
      ((end_hour - start_hour) * 2)
      (fun i ->
        let slot_start = (start_hour * 60) + (i * 30) in
        time_string slot_start
        :: List.map (render_cell slot_start) events_by_day)
  in

  (* Pad each column to its widest entry so the days stay aligned. *)
  let table = ("Time" :: days) :: rows in
  let widths = Array.make 8 0 in
  List.iter
    (List.iteri (fun i cell ->
         widths.(i) <- max widths.(i) (String.length cell)))
    table;
  let format_row cells =
    List.mapi (fun i cell -> Printf.sprintf "%-*s" widths.(i) cell) cells
    |> String.concat " | "
  in
  List.map format_row table |> String.concat "\n"
