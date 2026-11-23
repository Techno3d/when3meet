# iCalendar integration

The scheduling library can import a calendar export, convert its events to JSON, check whether any two events overlap, and calculate who is free during a meeting window. The backend supplies a distinct name and event list for each person. The result covers the entire window and groups consecutive times with the same free people into one run.

## Progress on [issue #4](https://github.com/Techno3d/when3meet/issues/4)

The event type and import interface are in [calendar_tools.mli](../lib/calendar_tools.mli). The implementation reads the contents of a `.ics` export with `Icalendar.parse` and returns a `calendar_event list`. To open an export from disk, pass its contents to the importer. This example returns `Error` for both file and parse failures.

```ocaml
let read_ics path =
  try
    In_channel.with_open_bin path (fun channel ->
      When3meet.Calendar_tools.ical_to_events (In_channel.input_all channel))
  with Sys_error message -> Error message
```

## Run the tests

From the repository root, install the dependencies into your active opam switch and run the suite.

```sh
opam install . --deps-only
opam exec -- dune runtest test --force
```

The calendar suite has 1,163 cases covering iCalendar imports, JSON round trips, time zones, overlapping events, and availability. The project uses `icalendar.0.1.13` for imports, Ptime for dates, and Yojson for JSON.

## Try a calendar

Arjun has a lab from 09:00 to 10:00 UTC on November 23. Maya has an empty calendar. To find who is free between 09:00 and 11:00, start the OCaml toplevel from the project root.

```sh
opam install utop
opam exec -- dune utop lib
```

Paste this example into the toplevel.

```ocaml
open When3meet.Calendar_tools;;

let lab = {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//When3meet//Study Group//EN
BEGIN:VEVENT
UID:lab-1@example.edu
DTSTAMP:20261122T120000Z
DTSTART:20261123T090000Z
DTEND:20261123T100000Z
SUMMARY:Lab
END:VEVENT
END:VCALENDAR|};;

let ( let* ) = Result.bind;;
let availability =
  let* events = ical_to_events lab in
  let calendars = [{ name = "Arjun"; events }; { name = "Maya"; events = [] }] in
  let window : interval = { start_time = 1795424400L; end_time = 1795431600L } in
  let* runs = create_calendar_rle ~window calendars in
  rle_to_json runs;;
Result.iter print_endline availability;;
```

Maya is free for the first hour. At 10:00, the lab ends and both people are free. Output times are Unix seconds and each end is exclusive.

```json
[
  {"start":1795424400,"end":1795428000,"names_free":["Maya"]},
  {"start":1795428000,"end":1795431600,"names_free":["Arjun","Maya"]}
]
```

## Import and scheduling rules

- Pass a complete `VCALENDAR` export to `ical_to_events`. Use `ical_to_json` for a JSON array of the imported events, and `json_to_events` to read that array back. A failed import returns `Error message`.
- Imports preserve date-only, UTC, floating, and named-zone values. For scheduling, supply `~default_timezone` for date-only or floating values and a `~resolve` function for named zones beyond the built-in UTC aliases.
- Cancelled and transparent events leave a person free. Overlapping events on one calendar keep that person busy until the last one ends. Events that touch at a boundary can be scheduled back to back.
- Recurring events and events using `DURATION` return an import error. Supply individual occurrences with explicit end times for those events.

The [public interface](../lib/calendar_tools.mli) documents the event fields, resolver signature, and return types.
