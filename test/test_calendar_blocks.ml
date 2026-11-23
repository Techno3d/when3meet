open OUnit2
open When3meet.Calendar_blocks

let check_equal name expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s\nExpected: %S\nActual:   %S" name expected actual)

(* Ignore padding while preserving column order and empty cells. *)
let normalize_row row =
  String.split_on_char '|' row |> List.map String.trim |> String.concat "|"

let check_week name schedule expected =
  let actual =
    render_week ~start_hour:9 ~end_hour:11 schedule
    |> String.split_on_char '\n' |> List.map normalize_row |> String.concat "\n"
  in
  check_equal name (String.concat "\n" expected) actual

let cs = { title = "CS 3110"; start_min = 540; end_min = 600 }
let meeting = { title = "Meeting"; start_min = 585; end_min = 615 }
let header = "Time|Mon|Tue|Wed|Thu|Fri|Sat|Sun"

let test_empty_week _ =
  check_week "Empty week has seven days and four half-hour rows" []
    [ header; "09:00|||||||"; "09:30|||||||"; "10:00|||||||"; "10:30|||||||" ]

let test_week_with_events _ =
  (* Supply Sunday first to check that display order stays Monday–Sunday. *)
  let schedule = [ ("Sun", [ cs ]); ("Mon", [ cs; meeting ]) ] in
  check_week "Events appear on the right days and in every occupied slot"
    schedule
    [
      header;
      "09:00|CS 3110||||||CS 3110";
      "09:30|CS 3110 / Meeting||||||CS 3110";
      "10:00|Meeting||||||";
      "10:30|||||||";
    ]

let test_display_boundaries _ =
  let early = { title = "Early"; start_min = 510; end_min = 540 } in
  let ongoing = { title = "Ongoing"; start_min = 525; end_min = 555 } in
  let late = { title = "Late"; start_min = 660; end_min = 690 } in

  check_week "Only events overlapping the displayed hours appear"
    [ ("Mon", [ early; ongoing; late ]) ]
    [
      header;
      "09:00|Ongoing||||||";
      "09:30|||||||";
      "10:00|||||||";
      "10:30|||||||";
    ]

let test_day _ =
  let actual =
    render_day ~day:"Monday" ~start_hour:9 ~end_hour:11 [ cs ]
    |> String.split_on_char '\n' |> List.map String.trim |> String.concat "\n"
  in

  let expected =
    String.concat "\n"
      [
        "Monday";
        "09:00 | [CS 3110 09:00-10:00]";
        "09:30 | [| CS 3110]";
        "10:00 |";
        "10:30 |";
      ]
  in

  check_equal "Day view shows the time range and continuation" expected actual

let suite =
  "calendar_blocks"
  >::: [
         "empty_week" >:: test_empty_week;
         "week_with_events" >:: test_week_with_events;
         "display_boundaries" >:: test_display_boundaries;
         "day_view" >:: test_day;
       ]

let () = run_test_tt_main suite
