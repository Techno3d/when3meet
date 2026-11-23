open When3meet.Block_calendar
open OUnit2

let dates =
  [
    "2026-11-23";
    "2026-11-24";
    "2026-11-25";
    "2026-11-26";
    "2026-11-27";
    "2026-11-28";
    "2026-11-29";
  ]

let at date hour minute = { date; seconds = ((hour * 60) + minute) * 60 }

let initial_tests _ =
  let lab = { starts = at "2026-11-24" 17 0; ends = at "2026-11-24" 18 30 } in
  let thanksgiving =
    { starts = at "2026-11-26" 0 0; ends = at "2026-11-28" 0 0 }
  in
  let events = [ lab; thanksgiving ] in

  let tuesday = "2026-11-24" in

  (* Check the exact slots occupied by the Tuesday lab. *)
  let lab_slots =
    [ (16, 30); (17, 0); (17, 30); (18, 0); (18, 30) ]
    |> List.filter (fun (hour, minute) ->
        overlaps lab (at tuesday hour minute) (at tuesday hour (minute + 30)))
  in
  assert (lab_slots = [ (17, 0); (17, 30); (18, 0) ]);

  (* The all-day break covers Thursday and Friday. Saturday is free. *)
  let break_days =
    List.filter
      (fun date -> overlaps thanksgiving (at date 12 0) (at date 12 30))
      dates
  in
  assert (break_days = [ "2026-11-26"; "2026-11-27" ]);

  let image = render_week ~dates ~start_hour:15 ~end_hour:19 events in
  assert (Notty.I.height image = 9);

  (* One header + eight half-hour rows. *)
  if Array.mem "--preview" Sys.argv then Notty_unix.output_image image

let compare_time_tests =
  "compare_time"
  >::: [
         ( "earlier_same_day" >:: fun _ ->
           assert_bool ""
             (compare_time (at "2026-11-24" 9 0) (at "2026-11-24" 10 0) < 0) );
         ( "later_same_day" >:: fun _ ->
           assert_bool ""
             (compare_time (at "2026-11-24" 11 0) (at "2026-11-24" 10 0) > 0) );
         ( "different_days" >:: fun _ ->
           assert_bool ""
             (compare_time (at "2026-11-25" 0 0) (at "2026-11-24" 23 59) > 0) );
         ( "equal_times" >:: fun _ ->
           assert_equal 0
             (compare_time (at "2026-11-24" 10 30) (at "2026-11-24" 10 30)) );
       ]

let overlap_tests =
  let event = { starts = at "2026-11-24" 10 0; ends = at "2026-11-24" 11 0 } in
  "overlaps"
  >::: [
         ( "starts_at_event_end" >:: fun _ ->
           assert_bool "should not overlap"
             (not
                (overlaps event (at "2026-11-24" 11 0) (at "2026-11-24" 11 30)))
         );
         ( "ends_at_event_start" >:: fun _ ->
           assert_bool "should not overlap"
             (not
                (overlaps event (at "2026-11-24" 9 30) (at "2026-11-24" 10 0)))
         );
         ( "contained_inside" >:: fun _ ->
           assert_bool "should overlap"
             (overlaps event (at "2026-11-24" 10 15) (at "2026-11-24" 10 45)) );
         ( "contains_event" >:: fun _ ->
           assert_bool "should overlap"
             (overlaps event (at "2026-11-24" 9 0) (at "2026-11-24" 12 0)) );
         ( "cross_midnight" >:: fun _ ->
           let overnight =
             { starts = at "2026-11-24" 23 0; ends = at "2026-11-25" 1 0 }
           in
           assert_bool "should overlap"
             (overlaps overnight (at "2026-11-25" 0 30) (at "2026-11-25" 1 0))
         );
       ]

let render_week_tests =
  "render_week"
  >::: [
         ( "bad_date_count" >:: fun _ ->
           assert_raises
             (Invalid_argument "render_week: expected seven dates, Monday first")
             (fun () ->
               render_week ~dates:(List.tl dates) ~start_hour:9 ~end_hour:17 [])
         );
         ( "invalid_hour_range_equal" >:: fun _ ->
           assert_raises (Invalid_argument "render_week: invalid hour range")
             (fun () -> render_week ~dates ~start_hour:10 ~end_hour:10 []) );
         ( "invalid_hour_range_negative" >:: fun _ ->
           assert_raises (Invalid_argument "render_week: invalid hour range")
             (fun () -> render_week ~dates ~start_hour:(-1) ~end_hour:10 []) );
         ( "empty_calendar_dimensions" >:: fun _ ->
           let img = render_week ~dates ~start_hour:9 ~end_hour:10 [] in
           assert_equal 3 (Notty.I.height img) );
         ( "full_day_has_expected_rows" >:: fun _ ->
           let img = render_week ~dates ~start_hour:0 ~end_hour:24 [] in
           assert_equal 49 (Notty.I.height img) );
       ]

let suite =
  "block_calendar"
  >::: [
         "intial_tests" >:: initial_tests;
         compare_time_tests;
         overlap_tests;
         render_week_tests;
       ]

let () = run_test_tt_main suite
