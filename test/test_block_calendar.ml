open When3meet.Block_calendar

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
let lab = { starts = at "2026-11-24" 17 0; ends = at "2026-11-24" 18 30 }
let thanksgiving = { starts = at "2026-11-26" 0 0; ends = at "2026-11-28" 0 0 }
let events = [ lab; thanksgiving ]

let () =
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
