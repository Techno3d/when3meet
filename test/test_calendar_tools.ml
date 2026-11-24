(** Calendar_tools regression suite. This single file intentionally depends only
    on the standard library and [when3meet], so the existing Dune test stanza is
    sufficient. Run [opam exec -- dune runtest test --force] from the repository
    root, or [opam exec -- dune runtest --force] from this directory.

    Tests cover the public contract rather than private helpers. Exhaustive
    small interval grids and a per-second availability oracle independently
    check the sweep-line calculations. Every generated case has a distinct name
    and concrete expected result; a failed case is printed before exit. *)

open When3meet.Calendar_tools

let large_batch_size = 1000
let long_text_length = 4 * 1024 * 1024
let tests = ref []
let add name body = tests := (name, body) :: !tests
let fail message = raise (Failure message)
let check condition message = if not condition then fail message

let equal expected actual =
  if expected <> actual then fail "actual value differs from expected value"

let ok = function
  | Ok value -> value
  | Error message -> fail message

let error = function
  | Ok _ -> fail "expected Error, received Ok"
  | Error _ -> ()

let has text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let date year month day : date = { year; month; day }
let time hour minute second : time = { hour; minute; second }
let epoch = date 1970 1 1
let clock second = time (second / 3600) (second / 60 mod 60) (second mod 60)

let at ?(timezone = Utc) second =
  let date, second =
    if second < 0 then (date 1969 12 31, 86400 + second)
    else if second >= 86400 then (date 1970 1 2, second - 86400)
    else (epoch, second)
  in
  Date_time { date; time = clock second; timezone }

let event ?(uid = "event") ?(status = Confirmed) ?(transparency = Opaque) start
    stop : calendar_event =
  {
    uid;
    dtstart = at start;
    dtend = Some (at stop);
    summary = None;
    description = None;
    location = None;
    status;
    transparency;
  }

let basic = event 0 10

let day_event ?(uid = "day") start stop : calendar_event =
  {
    basic with
    uid;
    dtstart = Date start;
    dtend = Option.map (fun d -> Date d) stop;
  }

let interval start stop : interval =
  { start_time = Int64.of_int start; end_time = Int64.of_int stop }

let run start stop names_free : calendar_run =
  { start_time = Int64.of_int start; end_time = Int64.of_int stop; names_free }

let calendar name events : named_calendar = { name; events }
let query calendars = ok (create_calendar_rle ~window:(interval 0 10) calendars)

(* Fixture construction is deliberately separate from the production codecs. *)
let quote text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | c when Char.code c < 32 ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buffer c)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let object_ fields =
  "{"
  ^ String.concat "," (List.map (fun (k, v) -> quote k ^ ":" ^ v) fields)
  ^ "}"

let temporal value =
  object_
    [ ("kind", quote "date_time"); ("value", quote value); ("tzid", "null") ]

let json_date value = object_ [ ("kind", quote "date"); ("value", quote value) ]

let fields =
  [
    ("uid", quote "event");
    ("dtstart", temporal "19700101T000000Z");
    ("dtend", temporal "19700101T000010Z");
    ("summary", "null");
    ("description", "null");
    ("location", "null");
    ("status", quote "confirmed");
    ("transparency", quote "opaque");
  ]

let json = object_ fields

let replace name value values =
  List.map (fun (key, old) -> (key, if key = name then value else old)) values

let with_json name value = object_ (replace name value fields)

let vevent properties =
  "BEGIN:VEVENT\n" ^ String.concat "\n" properties ^ "\nEND:VEVENT\n"

let minimal =
  [ "UID:event"; "DTSTART:19700101T000000Z"; "DTEND:19700101T000010Z" ]

let ical properties =
  "BEGIN:VCALENDAR\nVERSION:2.0\n" ^ vevent properties ^ "END:VCALENDAR\n"

let ical_with events =
  "BEGIN:VCALENDAR\nVERSION:2.0\n" ^ String.concat "" events ^ "END:VCALENDAR\n"

let replace_property name value properties =
  List.map
    (fun p ->
      if String.starts_with ~prefix:(name ^ ":") p then name ^ ":" ^ value
      else p)
    properties

let () =
  add "typed/basic event" (fun () -> equal () (ok (validate_event basic)));
  add "typed/status does not change event validity" (fun () ->
      List.iter
        (fun status -> equal () (ok (validate_event { basic with status })))
        [ Confirmed; Tentative; Cancelled ]);
  List.iter
    (fun year ->
      List.iteri
        (fun month days ->
          let month = month + 1 in
          add (Printf.sprintf "typed/month boundary %04d-%02d" year month)
            (fun () ->
              let e =
                {
                  basic with
                  dtstart =
                    Date_time
                      {
                        date = date year month days;
                        time = time 0 0 0;
                        timezone = Utc;
                      };
                  dtend = None;
                }
              in
              equal () (ok (validate_event e));
              error
                (validate_event (day_event (date year month (days + 1)) None))))
        [
          31;
          (if year mod 400 = 0 || (year mod 4 = 0 && year mod 100 <> 0) then 29
           else 28);
          31;
          30;
          31;
          30;
          31;
          31;
          30;
          31;
          30;
          31;
        ])
    [ 1; 1600; 1900; 1970; 2000; 2024; 2100; 9999 ];
  List.iter
    (fun (year, month, day) ->
      add (Printf.sprintf "typed/invalid date %d/%d/%d" year month day)
        (fun () ->
          error (validate_event (day_event (date year month day) None))))
    [
      (0, 1, 1);
      (-1, 1, 1);
      (10000, 1, 1);
      (2026, 0, 1);
      (2026, 13, 1);
      (2026, 1, 0);
      (2026, 1, -1);
      (2026, 1, 32);
      (2026, 2, 29);
      (1900, 2, 29);
      (2100, 2, 29);
      (2024, 2, 30);
    ];
  List.iter
    (fun (hour, minute, second) ->
      add (Printf.sprintf "typed/invalid clock %d:%d:%d" hour minute second)
        (fun () ->
          error
            (validate_event
               {
                 basic with
                 dtstart =
                   Date_time
                     {
                       date = epoch;
                       time = time hour minute second;
                       timezone = Utc;
                     };
                 dtend = None;
               })))
    [
      (-1, 0, 0);
      (24, 0, 0);
      (25, 0, 0);
      (0, -1, 0);
      (0, 60, 0);
      (0, 61, 0);
      (0, 0, -1);
      (0, 0, 60);
      (0, 0, 61);
    ];
  for hour = 0 to 23 do
    add (Printf.sprintf "typed/clock hour %02d" hour) (fun () ->
        equal ()
          (ok
             (validate_event
                {
                  basic with
                  dtstart =
                    Date_time
                      { date = epoch; time = time hour 59 59; timezone = Utc };
                  dtend = None;
                })))
  done;
  List.iteri
    (fun i value ->
      add (Printf.sprintf "typed/metadata text %d" i) (fun () ->
          let e =
            {
              basic with
              summary = Some value;
              description = Some value;
              location = Some value;
            }
          in
          equal e (ok (json_to_event (ok (event_to_json e))))))
    [
      "";
      "plain";
      "line one\nline two";
      "tab\ttext";
      "Café";
      "図書館";
      "🚀 ☕";
      "quote \" and slash \\";
      "literal,semi;colon";
      "combining e\204\129";
    ];
  for code = 0 to 31 do
    add (Printf.sprintf "typed/UID JSON escape roundtrip %02x" code) (fun () ->
        let value = { basic with uid = "u" ^ String.make 1 (Char.chr code) } in
        equal value (ok (json_to_event (ok (event_to_json value)))))
  done;
  List.iteri
    (fun i uid ->
      add (Printf.sprintf "typed/UID text validity %d" i) (fun () ->
          if uid = "u\127" then
            equal { basic with uid }
              (ok (json_to_event (ok (event_to_json { basic with uid }))))
          else error (validate_event { basic with uid })))
    [ ""; " "; "\t"; "u\127"; "\255"; "\192\128"; "\237\160\128" ];
  List.iteri
    (fun i zone ->
      add (Printf.sprintf "typed/TZID text validity %d" i) (fun () ->
          let value =
            {
              basic with
              dtstart = at ~timezone:(Tzid zone) 0;
              dtend = Some (at ~timezone:(Tzid zone) 10);
            }
          in
          if zone = "zone\n" || zone = "zone\000" then
            equal value (ok (json_to_event (ok (event_to_json value))))
          else error (validate_event value)))
    [ ""; " "; "zone\n"; "zone\000"; "\255" ];
  List.iteri
    (fun i (start, stop) ->
      add (Printf.sprintf "typed/mismatched or reversed endpoints %d" i)
        (fun () ->
          error
            (validate_event { basic with dtstart = start; dtend = Some stop })))
    [
      (at 10, at 10);
      (at 10, at 0);
      (Date epoch, Date epoch);
      (Date (date 1970 1 2), Date epoch);
      (Date epoch, at 10);
      (at 0, Date epoch);
      (at 0, at ~timezone:Floating 10);
      (at ~timezone:(Tzid "A") 0, at ~timezone:(Tzid "B") 10);
    ];
  List.iteri
    (fun i timezone ->
      add (Printf.sprintf "typed/preserve timezone variant %d" i) (fun () ->
          let e =
            {
              basic with
              dtstart = at ~timezone 0;
              dtend = Some (at ~timezone 10);
            }
          in
          equal e (ok (json_to_event (ok (event_to_json e))))))
    [ Utc; Floating; Tzid "America/New_York"; Tzid "Custom/Zone" ];
  add "typed/absent end remains absent" (fun () ->
      let e = { basic with dtend = None } in
      equal e (ok (json_to_event (ok (event_to_json e)))));
  add "typed/date fields remain dates" (fun () ->
      let e = day_event epoch (Some (date 1970 1 3)) in
      equal e (ok (json_to_event (ok (event_to_json e)))));
  add "typed/implicit all-day end cannot exceed supported years" (fun () ->
      error (validate_event (day_event (date 9999 12 31) None)));
  add "typed/explicit all-day upper boundary remains valid" (fun () ->
      equal ()
        (ok
           (validate_event
              (day_event (date 9999 12 30) (Some (date 9999 12 31))))));
  add "typed/long UID roundtrip" (fun () ->
      List.iter
        (fun length ->
          let value = { basic with uid = String.make length 'u' } in
          equal value (ok (json_to_event (ok (event_to_json value)))))
        [ 1024; 1025 ]);
  add "typed/long TZID roundtrip" (fun () ->
      let with_zone n =
        {
          basic with
          dtstart = at ~timezone:(Tzid (String.make n 'z')) 0;
          dtend = Some (at ~timezone:(Tzid (String.make n 'z')) 10);
        }
      in
      List.iter
        (fun length ->
          let value = with_zone length in
          equal value (ok (json_to_event (ok (event_to_json value)))))
        [ 128; 129 ]);
  List.iteri
    (fun i invalid ->
      add (Printf.sprintf "typed/metadata JSON escape roundtrip %d" i)
        (fun () ->
          let value =
            {
              basic with
              summary = Some invalid;
              description = Some invalid;
              location = Some invalid;
            }
          in
          equal value (ok (json_to_event (ok (event_to_json value))))))
    [ "\000"; "\001"; "\011"; "\127" ];

  add "json/known fixture has expected typed value" (fun () ->
      equal basic (ok (json_to_event json)));
  add "json/minimal object uses defaults" (fun () ->
      equal
        { basic with dtend = None }
        (ok
           (json_to_event
              (object_
                 [
                   ("uid", quote "event");
                   ("dtstart", temporal "19700101T000000Z");
                 ]))));
  add "json/empty array" (fun () ->
      equal [] (ok (json_to_events "[]"));
      equal "[]" (ok (events_to_json [])));
  add "json/validator accepts one object" (fun () ->
      equal () (ok (validate_json json)));
  add "json/validator accepts array" (fun () ->
      equal () (ok (validate_json ("[" ^ json ^ "]"))));
  add "json/single decoder rejects array" (fun () ->
      error (json_to_event ("[" ^ json ^ "]")));
  add "json/batch decoder rejects object" (fun () ->
      error (json_to_events json));
  List.iter
    (fun name ->
      add ("json/missing " ^ name) (fun () ->
          error (json_to_event (object_ (List.remove_assoc name fields)))))
    [ "uid"; "dtstart" ];
  List.iter
    (fun (name, _) ->
      add ("json/first duplicate field wins: " ^ name) (fun () ->
          equal basic
            (ok
               (json_to_event
                  (object_ (fields @ [ (name, quote "ignored duplicate") ]))))))
    fields;
  List.iter
    (fun name ->
      add ("json/ignores extra field: " ^ name) (fun () ->
          equal basic
            (ok (json_to_event (object_ (fields @ [ (name, "null") ]))))))
    [ "UID"; "start"; "busy"; "source"; "access_token"; "rrule" ];
  List.iter
    (fun name ->
      List.iteri
        (fun i raw ->
          add (Printf.sprintf "json/wrong field type %s/%d" name i) (fun () ->
              error (json_to_event (with_json name raw))))
        [ "true"; "0"; "1.25"; "[]"; "{}" ])
    [
      "uid";
      "dtstart";
      "dtend";
      "summary";
      "description";
      "location";
      "status";
      "transparency";
    ];
  List.iter
    (fun name ->
      add ("json/optional omission " ^ name) (fun () ->
          let actual =
            ok (json_to_event (object_ (List.remove_assoc name fields)))
          in
          equal
            (if name = "dtend" then { basic with dtend = None } else basic)
            actual))
    [ "dtend"; "summary"; "description"; "location"; "status"; "transparency" ];
  List.iteri
    (fun i raw ->
      add (Printf.sprintf "json/non-event document %d" i) (fun () ->
          error (validate_json raw)))
    [
      "null";
      "false";
      "true";
      "42";
      "1.2";
      "\"text\"";
      "{}";
      "[null]";
      "[true]";
      "[[]]";
    ];
  List.iteri
    (fun i raw ->
      add (Printf.sprintf "json/invalid event document %d" i) (fun () ->
          error (validate_json raw)))
    [
      "";
      " ";
      "{";
      "[";
      "}";
      "[}";
      "{,}";
      "[,]";
      "[1,]";
      "{\"x\":}";
      "{x:1}";
      "{'x':1}";
      "[] []";
      "[NaN]";
      "[Infinity]";
      "[+1]";
      "[01]";
      "[.1]";
      "[1.]";
      "[1e]";
      "[0x10]";
      "[undefined]";
      "[\"bad\\x20\"]";
      "[\"bad\\u12\"]";
      "[\"bad\ntext\"]";
      "[\"bad\000text\"]";
      "\239\187\191[]";
      "[\255]";
    ];
  List.iteri
    (fun i source ->
      add (Printf.sprintf "json/Yojson comment syntax %d" i) (fun () ->
          equal [] (ok (json_to_events source))))
    [ "/* comment */[]"; "// comment\n[]" ];
  List.iter
    (fun (raw, expected) ->
      add ("json/Unicode escape " ^ raw) (fun () ->
          equal (Some expected)
            (ok (json_to_event (with_json "summary" raw))).summary))
    [
      ("\"\\u0041\"", "A");
      ("\"\\u00e9\"", "é");
      ("\"\\u65e5\"", "日");
      ("\"\\ud83d\\ude80\"", "🚀");
      ("\"\\u0022\\u005c\"", "\"\\");
    ];
  List.iteri
    (fun i raw ->
      add (Printf.sprintf "json/invalid Unicode escape %d" i) (fun () ->
          error (json_to_event (with_json "summary" raw))))
    [
      "\"\\ud800\"";
      "\"\\udc00\"";
      "\"\\ud800x\"";
      "\"\\ud800\\u0041\"";
      "\"\\udc00\\ud800\"";
      "\"\\ud800\\ud800\"";
      "\"\\uZZZZ\"";
      "\"\240\128\128\128\"";
      "\"\244\144\128\128\"";
    ];
  List.iter
    (fun (name, values) ->
      List.iter
        (fun value ->
          add
            ("json/invalid enum " ^ name ^ "/" ^ value)
            (fun () -> error (json_to_event (with_json name (quote value)))))
        values)
    [
      ("status", [ "CONFIRMED"; "unknown"; "busy"; "" ]);
      ("transparency", [ "OPAQUE"; "free"; "busy"; "" ]);
    ];
  List.iteri
    (fun i raw ->
      add (Printf.sprintf "json/temporal fields %d" i) (fun () ->
          if i = 6 || i = 7 then
            equal basic (ok (json_to_event (with_json "dtstart" raw)))
          else error (json_to_event (with_json "dtstart" raw))))
    [
      object_ [ ("kind", quote "instant"); ("value", quote "19700101T000000Z") ];
      object_ [ ("kind", quote "date_time") ];
      object_ [ ("value", quote "19700101T000000Z") ];
      object_
        [
          ("kind", quote "date");
          ("value", quote "19700101");
          ("tzid", quote "UTC");
        ];
      object_
        [
          ("kind", quote "date_time");
          ("value", quote "19700101T000000Z");
          ("tzid", quote "UTC");
        ];
      object_
        [
          ("kind", quote "date_time");
          ("value", quote "19700101T000000");
          ("tzid", "1");
        ];
      object_
        [
          ("kind", quote "date_time");
          ("kind", quote "date_time");
          ("value", quote "19700101T000000Z");
        ];
      object_
        [
          ("kind", quote "date_time");
          ("value", quote "19700101T000000Z");
          ("extra", "null");
        ];
    ];
  List.iteri
    (fun i value ->
      add (Printf.sprintf "json/invalid datetime value %d" i) (fun () ->
          error (json_to_event (with_json "dtstart" (temporal value)))))
    [
      "1970-01-01T00:00:00Z";
      "19700101";
      "19700101T000000+0000";
      "19700101T000000.5Z";
      "19700101T000060Z";
      "19700101T240000Z";
      "19700230T000000Z";
      "19701301T000000Z";
      "00000101T000000Z";
      "19700101t000000z";
    ];
  add "json/preserve batch order" (fun () ->
      let values =
        [
          { basic with uid = "z" };
          { basic with uid = "a" };
          { basic with uid = "m" };
        ]
      in
      equal values (ok (json_to_events (ok (events_to_json values)))));
  add "json/preserves repeated event IDs" (fun () ->
      equal [ basic; basic ]
        (ok (json_to_events (ok (events_to_json [ basic; basic ])))));
  add "json/bad later event has no partial result" (fun () ->
      error (json_to_events ("[" ^ json ^ ",{}]")));
  add "json/large batch roundtrip" (fun () ->
      let values =
        List.init large_batch_size (fun i ->
            { basic with uid = string_of_int i })
      in
      equal large_batch_size
        (List.length (ok (json_to_events (ok (events_to_json values))))));
  add "json/batches have no fixed event limit" (fun () ->
      let values =
        List.init (large_batch_size + 1) (fun i ->
            { basic with uid = string_of_int i })
      in
      equal values (ok (json_to_events (ok (events_to_json values)))));
  add "json/long whitespace is still missing a document" (fun () ->
      error (validate_json (String.make (long_text_length + 1) ' ')));
  add "json/long description roundtrip" (fun () ->
      let value =
        { basic with description = Some (String.make long_text_length 'x') }
      in
      equal value (ok (json_to_event (ok (event_to_json value)))));
  add "json/nested arrays are not event objects" (fun () ->
      error (validate_json (String.make 65 '[' ^ "null" ^ String.make 65 ']')));
  add "json/long trailing whitespace is accepted" (fun () ->
      equal ()
        (ok (validate_json ("[]" ^ String.make (long_text_length - 2) ' '))));
  add "json/escaped first duplicate key wins" (fun () ->
      equal
        { basic with uid = "first" }
        (ok
           (json_to_event
              ("{\"\\u0075id\":\"first\","
              ^ String.sub json 1 (String.length json - 1)))));

  add "ical/known calendar fixture" (fun () ->
      equal basic (ok (ical_to_event (ical minimal))));
  add "ical/requires VCALENDAR wrapper" (fun () ->
      error (ical_to_events (vevent minimal)));
  add "ical/requires at least one component" (fun () ->
      error (ical_to_events (ical_with []));
      error (ical_to_event (ical_with [])));
  add "ical/multiple event cardinality" (fun () ->
      let source =
        ical_with
          [ vevent minimal; vevent (replace_property "UID" "second" minimal) ]
      in
      equal 2 (List.length (ok (ical_to_events source)));
      error (ical_to_event source));
  add "ical/preserves repeated event UIDs" (fun () ->
      equal [ basic; basic ]
        (ok (ical_to_events (ical_with [ vevent minimal; vevent minimal ]))));
  add "ical/to JSON always emits array" (fun () ->
      let output = ok (ical_to_json (ical minimal)) in
      check (output.[0] = '[') "expected array";
      equal [ basic ] (ok (json_to_events output)));
  add "ical/validator uses conversion rules" (fun () ->
      equal () (ok (validate_ical (ical minimal)));
      error (validate_ical (ical [ "UID:event" ])));
  add "ical/default instant keeps absent DTEND" (fun () ->
      equal
        { basic with dtend = None }
        (ok (ical_to_event (ical [ "UID:event"; "DTSTART:19700101T000000Z" ]))));
  add "ical/default all-day end stays absent" (fun () ->
      equal
        (day_event ~uid:"event" epoch None)
        (ok
           (ical_to_event (ical [ "UID:event"; "DTSTART;VALUE=DATE:19700101" ]))));
  List.iteri
    (fun i (lines, expected) ->
      add (Printf.sprintf "ical/timezone preservation %d" i) (fun () ->
          equal expected (ok (ical_to_event (ical lines))).dtstart))
    [
      ([ "UID:event"; "DTSTART:19700101T000000" ], at ~timezone:Floating 0);
      ( [ "UID:event"; "DTSTART;TZID=America/New_York:19700101T000000" ],
        at ~timezone:(Tzid "America/New_York") 0 );
      ( [ "UID:event"; "DTSTART;TZID=\"Custom/Zone\":19700101T000000" ],
        at ~timezone:(Tzid "Custom/Zone") 0 );
    ];
  List.iter
    (fun (property, value, expected) ->
      add
        ("ical/text escape " ^ property ^ "/" ^ value)
        (fun () ->
          let e =
            ok (ical_to_event (ical (minimal @ [ property ^ ":" ^ value ])))
          in
          equal (Some expected)
            (if property = "SUMMARY" then e.summary
             else if property = "LOCATION" then e.location
             else e.description)))
    [
      ("SUMMARY", "hello\\, world", "hello, world");
      ("LOCATION", "room\\; floor", "room; floor");
      ("DESCRIPTION", "one\\ntwo", "one\ntwo");
      ("DESCRIPTION", "one\\Ntwo", "one\ntwo");
      ("SUMMARY", "a\\\\b", "a\\b");
      ("SUMMARY", "Café 🚀", "Café 🚀");
    ];
  List.iteri
    (fun i suffix ->
      add (Printf.sprintf "ical/folding %d" i) (fun () ->
          equal (Some "Hello world")
            (ok (ical_to_event (ical (minimal @ [ suffix ])))).summary))
    [
      "SUMMARY:Hello\n  world";
      "SUMMARY:Hello\n\t world";
      "SUMMARY:Hello\r\n  world";
    ];
  List.iteri
    (fun i newline ->
      add (Printf.sprintf "ical/line ending %d" i) (fun () ->
          let source =
            String.concat newline (String.split_on_char '\n' (ical minimal))
          in
          equal basic (ok (ical_to_event source))))
    [ "\n"; "\r\n" ];
  List.iter
    (fun property ->
      add ("ical/repeated property " ^ property) (fun () ->
          let values =
            match property with
            | "UID" -> minimal @ [ "UID:again" ]
            | "DTSTART" -> minimal @ [ "DTSTART:19700101T000001Z" ]
            | "DTEND" -> minimal @ [ "DTEND:19700101T000011Z" ]
            | "STATUS" -> minimal @ [ "STATUS:CONFIRMED"; "STATUS:TENTATIVE" ]
            | "TRANSP" -> minimal @ [ "TRANSP:OPAQUE"; "TRANSP:TRANSPARENT" ]
            | _ -> minimal @ [ property ^ ":one"; property ^ ":two" ]
          in
          match property with
          | "UID" | "DTSTART" | "DTEND" -> error (ical_to_events (ical values))
          | _ ->
              let expected =
                match property with
                | "SUMMARY" -> { basic with summary = Some "two" }
                | "DESCRIPTION" -> { basic with description = Some "two" }
                | "LOCATION" -> { basic with location = Some "two" }
                | "STATUS" -> { basic with status = Tentative }
                | _ -> { basic with transparency = Transparent }
              in
              equal expected (ok (ical_to_event (ical values)))))
    [
      "UID";
      "DTSTART";
      "DTEND";
      "SUMMARY";
      "DESCRIPTION";
      "LOCATION";
      "STATUS";
      "TRANSP";
    ];
  List.iter
    (fun property ->
      add ("ical/unsupported property handling: " ^ property) (fun () ->
          let source = ical (minimal @ [ property ^ ":value" ]) in
          if property = "UNKNOWN" then equal basic (ok (ical_to_event source))
          else error (ical_to_events source)))
    [
      "RRULE";
      "RDATE";
      "EXDATE";
      "EXRULE";
      "RECURRENCE-ID";
      "DURATION";
      "UNKNOWN";
    ];
  List.iter
    (fun property ->
      add ("ical/ignore harmless metadata " ^ property) (fun () ->
          equal basic
            (ok (ical_to_event (ical (minimal @ [ property ^ ":value" ]))))))
    [
      "DTSTAMP";
      "CREATED";
      "LAST-MODIFIED";
      "SEQUENCE";
      "URL";
      "CLASS";
      "CATEGORIES";
      "COMMENT";
      "ORGANIZER";
      "ATTENDEE";
      "X-CUSTOM";
      "X-PROVIDER-TAG";
    ];
  List.iteri
    (fun i source ->
      add (Printf.sprintf "ical/component and metadata parsing %d" i) (fun () ->
          match i with
          | 12 -> equal [] (ok (ical_to_events source))
          | 20 ->
              equal
                { basic with summary = Some "invalid" }
                (ok (ical_to_event source))
          | 21 | 22 -> equal basic (ok (ical_to_event source))
          | _ -> error (ical_to_events source)))
    [
      "";
      "garbage";
      "BEGIN:VCALENDAR\nEND:VCALENDAR\n";
      "BEGIN:VCALENDAR\nVERSION:1.0\nEND:VCALENDAR\n";
      "BEGIN:VCALENDAR\nVERSION:2.0\nVERSION:2.0\nEND:VCALENDAR\n";
      "BEGIN:VCALENDAR\nVERSION:2.0\nMETHOD:CANCEL\nEND:VCALENDAR\n";
      "BEGIN:VCALENDAR\n\
       VERSION:2.0\n\
       METHOD:PUBLISH\n\
       METHOD:PUBLISH\n\
       END:VCALENDAR\n";
      "BEGIN:VCALENDAR\nVERSION:2.0\n";
      "END:VCALENDAR\n";
      "BEGIN:VEVENT\nUID:event\n";
      vevent minimal ^ vevent minimal;
      " stray folding\n" ^ ical minimal;
      "BEGIN:VCALENDAR\n\
       VERSION:2.0\n\
       BEGIN:VTIMEZONE\n\
       END:VTIMEZONE\n\
       END:VCALENDAR\n";
      ical (minimal @ [ "BEGIN:VALARM"; "END:VALARM" ]);
      ical minimal ^ "SUMMARY:outside\n";
      ical (minimal @ [ "SUMMARY:no\000nul" ]);
      ical (minimal @ [ "SUMMARY:\255" ]);
      ical (minimal @ [ "SUMMARY:bare\rcarriage" ]);
      ical (minimal @ [ "SUMMARY;BAD NAME=x:invalid" ]);
      ical (minimal @ [ "SUMMARY;LANGUAGE=\"en\"suffix:invalid" ]);
      ical (minimal @ [ "SUMMARY;LANGUAGE=en;LANGUAGE=fr:invalid" ]);
      ical (minimal @ [ "SUMMARY:bad\\q" ]);
      ical (minimal @ [ "SUMMARY:bad\\" ]);
      ical [ "DTSTART:19700101T000000Z" ];
      ical [ "UID:event" ];
    ];
  List.iteri
    (fun i property ->
      add (Printf.sprintf "ical/DTSTART interpretation %d" i) (fun () ->
          let source = ical [ "UID:event"; property ] in
          match i with
          | 1 ->
              equal
                { basic with dtstart = at 60; dtend = None }
                (ok (ical_to_event source))
          | 6 ->
              equal
                (day_event ~uid:"event" epoch None)
                (ok (ical_to_event source))
          | 7 -> equal { basic with dtend = None } (ok (ical_to_event source))
          | _ -> error (ical_to_event source)))
    [
      "DTSTART:1970-01-01T00:00:00Z";
      "DTSTART:19700101T000060Z";
      "DTSTART:19700101T240000Z";
      "DTSTART:19700230T000000Z";
      "DTSTART;VALUE=DATE:19700132";
      "DTSTART;VALUE=DATE:00000101";
      "DTSTART;VALUE=DATE;TZID=UTC:19700101";
      "DTSTART;TZID=UTC:19700101T000000Z";
      "DTSTART;VALUE=TIME:000000";
      "DTSTART;VALUE=DATE-TIME:19700101";
    ];
  add "ical/fail entire calendar for bad second event" (fun () ->
      error
        (ical_to_events (ical_with [ vevent minimal; vevent [ "UID:second" ] ])));
  add "ical/folded Unicode octets are validated after unfolding" (fun () ->
      equal (Some "Café")
        (ok (ical_to_event (ical (minimal @ [ "SUMMARY:Caf\195\r\n \169" ]))))
          .summary);
  add "ical/Gregorian calendar scale" (fun () ->
      let source =
        "BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:GREGORIAN\n" ^ vevent minimal
        ^ "END:VCALENDAR\n"
      in
      equal [ basic ] (ok (ical_to_events source)));
  add "ical/other calendar metadata is ignored" (fun () ->
      let source =
        "BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:OTHER\n" ^ vevent minimal
        ^ "END:VCALENDAR\n"
      in
      equal [ basic ] (ok (ical_to_events source)));
  add "ical/duplicate calendar scale is metadata" (fun () ->
      let source =
        "BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:GREGORIAN\nCALSCALE:GREGORIAN\n"
        ^ vevent minimal ^ "END:VCALENDAR\n"
      in
      equal [ basic ] (ok (ical_to_events source)));
  add "ical/long non-calendar text fails" (fun () ->
      error (validate_ical (String.make (long_text_length + 1) 'x')));
  add "ical/large calendars have no fixed event limit" (fun () ->
      let make i = vevent (replace_property "UID" (string_of_int i) minimal) in
      equal large_batch_size
        (List.length
           (ok (ical_to_events (ical_with (List.init large_batch_size make)))));
      equal (large_batch_size + 1)
        (List.length
           (ok
              (ical_to_events
                 (ical_with (List.init (large_batch_size + 1) make))))));

  (* These fixtures exercise the real iCalendar package and the small adapter,
     rather than requiring a second custom iCalendar grammar in
     Calendar_tools. *)
  add "ical/standard exported calendar" (fun () ->
      let source =
        String.concat "\r\n"
          [
            "BEGIN:VCALENDAR";
            "VERSION:2.0";
            "PRODID:-//When3meet//Tests//EN";
            "BEGIN:VEVENT";
            "UID:project-meeting";
            "DTSTAMP:20261101T120000Z";
            "DTSTART:20261123T150000Z";
            "DTEND:20261123T160000Z";
            "SUMMARY:Project meeting";
            "DESCRIPTION:Plan\\nReview\\, then test";
            "LOCATION:Library";
            "STATUS:TENTATIVE";
            "TRANSP:OPAQUE";
            "ORGANIZER:mailto:arjun@example.com";
            "ATTENDEE:mailto:maya@example.com";
            "END:VEVENT";
            "END:VCALENDAR";
            "";
          ]
      in
      let expected =
        {
          basic with
          uid = "project-meeting";
          dtstart =
            Date_time
              { date = date 2026 11 23; time = time 15 0 0; timezone = Utc };
          dtend =
            Some
              (Date_time
                 { date = date 2026 11 23; time = time 16 0 0; timezone = Utc });
          summary = Some "Project meeting";
          description = Some "Plan\nReview, then test";
          location = Some "Library";
          status = Tentative;
        }
      in
      equal expected (ok (ical_to_event source));
      equal [ expected ] (ok (json_to_events (ok (ical_to_json source)))));
  List.iter
    (fun property ->
      add ("ical/rejects unexpanded recurrence: " ^ property) (fun () ->
          error (ical_to_events (ical (minimal @ [ property ])))))
    [
      "RRULE:FREQ=DAILY;COUNT=2";
      "RDATE:19700102T000000Z";
      "EXDATE:19700102T000000Z";
      "RECURRENCE-ID:19700102T000000Z";
    ];
  add "ical/valid DURATION requires explicit DTEND instead" (fun () ->
      error
        (ical_to_event
           (ical [ "UID:event"; "DTSTART:19700101T000000Z"; "DURATION:PT10S" ])));
  List.iter
    (fun (name, value) ->
      add ("ical/malformed scheduling property is not ignored: " ^ name)
        (fun () ->
          let properties =
            if name = "DTEND" then replace_property name value minimal
            else minimal @ [ name ^ ":" ^ value ]
          in
          error (ical_to_event (ical properties))))
    [
      ("DTEND", "not-a-date");
      ("STATUS", "NOT-A-STATUS");
      ("TRANSP", "NOT-A-TRANSPARENCY");
    ];
  add "ical/global TZID marker is preserved" (fun () ->
      let source =
        ical [ "UID:event"; "DTSTART;TZID=/example.org/Office:19700101T000000" ]
      in
      equal
        (at ~timezone:(Tzid "/example.org/Office") 0)
        (ok (ical_to_event source)).dtstart);
  add "ical/quoted TZID endpoints reach the caller resolver" (fun () ->
      let source =
        ical
          [
            "UID:event";
            "DTSTART;TZID=\"Custom/Zone\":19700101T000000";
            "DTEND;TZID=\"Custom/Zone\":19700101T000010";
          ]
      in
      let imported = ok (ical_to_event source) in
      let expected =
        {
          basic with
          dtstart = at ~timezone:(Tzid "Custom/Zone") 0;
          dtend = Some (at ~timezone:(Tzid "Custom/Zone") 10);
        }
      in
      equal expected imported;
      let resolve ~tzid ~date:actual_date ~time:actual_time =
        equal "Custom/Zone" tzid;
        equal epoch actual_date;
        Ok (Int64.of_int (3600 + actual_time.second))
      in
      equal (Some (interval 3600 3610)) (ok (event_interval ~resolve imported)));

  add "interval/epoch seconds" (fun () ->
      equal (Some (interval 0 10)) (ok (event_interval basic)));
  add "interval/pre-epoch seconds" (fun () ->
      equal (Some (interval (-10) 10)) (ok (event_interval (event (-10) 10))));
  add "interval/instant does not occupy time" (fun () ->
      equal None (ok (event_interval { basic with dtend = None })));
  add "interval/nonblocking still has temporal span" (fun () ->
      equal
        (Some (interval 0 10))
        (ok (event_interval { basic with status = Cancelled })));
  add "interval/all-day needs a timezone" (fun () ->
      error (event_interval (day_event epoch None)));
  add "interval/floating needs a timezone" (fun () ->
      error
        (event_interval
           {
             basic with
             dtstart = at ~timezone:Floating 0;
             dtend = Some (at ~timezone:Floating 10);
           }));
  add "interval/unknown TZID needs a resolver" (fun () ->
      error
        (event_interval
           {
             basic with
             dtstart = at ~timezone:(Tzid "Some/Zone") 0;
             dtend = Some (at ~timezone:(Tzid "Some/Zone") 10);
           }));
  List.iter
    (fun zone ->
      add ("interval/UTC alias " ^ zone) (fun () ->
          let resolve ~tzid:_ ~date:_ ~time:_ =
            fail "resolver must not be called"
          in
          let e =
            {
              basic with
              dtstart = at ~timezone:(Tzid zone) 0;
              dtend = Some (at ~timezone:(Tzid zone) 10);
            }
          in
          equal (Some (interval 0 10)) (ok (event_interval ~resolve e));
          equal
            (Some (interval 0 86400))
            (ok
               (event_interval ~resolve ~default_timezone:zone
                  (day_event epoch None)))))
    [ "UTC"; "Etc/UTC"; "GMT" ];
  List.iter
    (fun length ->
      add (Printf.sprintf "interval/all-day resolves both midnights %d" length)
        (fun () ->
          let calls = ref [] in
          let resolve ~tzid ~date ~time =
            calls := (tzid, date, time) :: !calls;
            Ok (if date.day = 1 then 0L else Int64.of_int length)
          in
          equal
            (Some (interval 0 length))
            (ok
               (event_interval ~resolve ~default_timezone:"Test/DST"
                  (day_event epoch None)));
          equal 2 (List.length !calls);
          List.iter (fun (_, _, t) -> equal (time 0 0 0) t) !calls))
    [ 82800; 86400; 90000 ];
  List.iter
    (fun offset ->
      add (Printf.sprintf "interval/named resolver offset %d" offset) (fun () ->
          let resolve ~tzid:_ ~date:_ ~time =
            Ok
              (Int64.of_int
                 ((time.hour * 3600) + (time.minute * 60) + time.second - offset))
          in
          let e =
            {
              basic with
              dtstart = at ~timezone:(Tzid "Test/Offset") 0;
              dtend = Some (at ~timezone:(Tzid "Test/Offset") 10);
            }
          in
          equal
            (Some (interval (-offset) (10 - offset)))
            (ok (event_interval ~resolve e))))
    [ -43200; -18000; 0; 19800; 20700; 45900; 50400 ];
  add "interval/resolver error propagates" (fun () ->
      let resolve ~tzid:_ ~date:_ ~time:_ = Error "resolver unavailable" in
      match
        event_interval ~resolve ~default_timezone:"Test/Zone"
          (day_event epoch None)
      with
      | Error message ->
          check (has message "resolver unavailable") "resolver error lost"
      | Ok _ -> fail "expected resolution error");
  add "interval/resolver exception is redacted" (fun () ->
      let resolve ~tzid:_ ~date:_ ~time:_ = fail "secret-provider-detail" in
      match
        event_interval ~resolve ~default_timezone:"Test/Zone"
          (day_event epoch None)
      with
      | Error message ->
          check
            (not (has message "secret-provider-detail"))
            "exception text leaked"
      | Ok _ -> fail "expected resolution error");
  List.iter
    (fun end_ ->
      add (Printf.sprintf "interval/reject resolved end %d" end_) (fun () ->
          let resolve ~tzid:_ ~date ~time:_ =
            Ok (if date.day = 1 then 100L else Int64.of_int end_)
          in
          error
            (event_interval ~resolve ~default_timezone:"Test/Zone"
               (day_event epoch None))))
    [ 0; 99; 100 ];

  add "overlap/empty" (fun () -> equal false (ok (check_overlap [])));
  add "overlap/single" (fun () -> equal false (ok (check_overlap [ basic ])));
  add "overlap/any pair not common intersection" (fun () ->
      equal true (ok (check_overlap [ event 0 2; event 1 3; event 3 5 ])));
  add "overlap/three touching events" (fun () ->
      equal false (ok (check_overlap [ event 0 2; event 2 4; event 4 6 ])));
  add "overlap/invalid excluded event still fails" (fun () ->
      error
        (check_overlap
           [ { basic with status = Cancelled; dtend = Some (at 0) } ]));
  add "overlap/unresolved nonblocking event needs no resolver" (fun () ->
      equal false
        (ok
           (check_overlap
              [ { (day_event epoch None) with status = Cancelled } ])));
  add "overlap/more events than serialization limit" (fun () ->
      equal false
        (ok
           (check_overlap
              (List.init 1500 (fun i -> event (2 * i) ((2 * i) + 1))))));
  let spans =
    List.concat
      (List.init 5 (fun start ->
           List.init (5 - start) (fun width -> (start, start + width + 1))))
  in
  List.iter
    (fun (a, b) ->
      List.iter
        (fun (c, d) ->
          add (Printf.sprintf "overlap/grid [%d,%d) [%d,%d)" a b c d) (fun () ->
              let expected = max a c < min b d in
              equal expected (ok (check_overlap [ event a b; event c d ]));
              equal expected (ok (check_overlap [ event c d; event a b ]))))
        spans)
    spans;
  List.iter
    (fun (left_status, left_transparency) ->
      List.iter
        (fun (right_status, right_transparency) ->
          let label status transparency =
            (match status with
              | Confirmed -> "confirmed"
              | Tentative -> "tentative"
              | Cancelled -> "cancelled")
            ^ "/"
            ^
            match transparency with
            | Opaque -> "opaque"
            | Transparent -> "transparent"
          in
          add
            ("overlap/blocking "
            ^ label left_status left_transparency
            ^ " + "
            ^ label right_status right_transparency)
            (fun () ->
              let a =
                event ~status:left_status ~transparency:left_transparency 0 3
              and b =
                event ~status:right_status ~transparency:right_transparency 1 2
              in
              let active status transparency =
                status <> Cancelled && transparency = Opaque
              in
              equal
                (active left_status left_transparency
                && active right_status right_transparency)
                (ok (check_overlap [ a; b ]));
              equal true (ok (check_overlap ~blocking_only:false [ a; b ]))))
        [
          (Confirmed, Opaque);
          (Confirmed, Transparent);
          (Tentative, Opaque);
          (Tentative, Transparent);
          (Cancelled, Opaque);
          (Cancelled, Transparent);
        ])
    [
      (Confirmed, Opaque);
      (Confirmed, Transparent);
      (Tentative, Opaque);
      (Tentative, Transparent);
      (Cancelled, Opaque);
      (Cancelled, Transparent);
    ];

  add "rle/no calendars" (fun () -> equal [ run 0 10 [] ] (query []));
  add "rle/empty calendar is free" (fun () ->
      equal [ run 0 10 [ "Ada" ] ] (query [ calendar "Ada" [] ]));
  add "rle/caller name order" (fun () ->
      equal
        [ run 0 10 [ "Zoe"; "Ada" ] ]
        (query [ calendar "Zoe" []; calendar "Ada" [] ]));
  add "rle/nested events for one person remain busy" (fun () ->
      equal
        [
          run 0 1 [ "A"; "B" ];
          run 1 4 [ "B" ];
          run 4 5 [];
          run 5 8 [ "B" ];
          run 8 10 [ "A"; "B" ];
        ]
        (query
           [ calendar "A" [ event 1 6; event 3 8 ]; calendar "B" [ event 4 5 ] ]));
  add "rle/touching events merge" (fun () ->
      equal
        [ run 0 1 [ "A" ]; run 1 5 []; run 5 10 [ "A" ] ]
        (query [ calendar "A" [ event 1 3; event 3 5 ] ]));
  add "rle/simultaneous boundary changes names" (fun () ->
      equal
        [ run 0 5 [ "B" ]; run 5 10 [ "A" ] ]
        (query [ calendar "A" [ event 0 5 ]; calendar "B" [ event 5 10 ] ]));
  add "rle/clip both boundaries" (fun () ->
      equal [ run 0 10 [] ] (query [ calendar "A" [ event (-2) 12 ] ]));
  add "rle/outside events introduce no boundaries" (fun () ->
      equal
        [ run 0 10 [ "A" ] ]
        (query [ calendar "A" [ event (-2) 0; event 10 12 ] ]));
  add "rle/same UID across calendars is allowed" (fun () ->
      equal
        [ run 0 10 [] ]
        (query [ calendar "A" [ basic ]; calendar "B" [ basic ] ]));
  add "rle/instantaneous event does not block" (fun () ->
      equal
        [ run 0 10 [ "A" ] ]
        (query [ calendar "A" [ { basic with dtend = None } ] ]));
  add "rle/nonblocking unresolved events do not need resolver" (fun () ->
      equal
        [ run 0 10 [ "A" ] ]
        (query
           [
             calendar "A"
               [ { (day_event epoch None) with transparency = Transparent } ];
           ]));
  add "rle/all-day input requires default timezone" (fun () ->
      error
        (create_calendar_rle ~window:(interval 0 10)
           [ calendar "A" [ day_event epoch None ] ]));
  add "rle/all-day spans explicit query window" (fun () ->
      equal
        [ run 0 10 [] ]
        (ok
           (create_calendar_rle ~default_timezone:"UTC" ~window:(interval 0 10)
              [ calendar "A" [ day_event epoch None ] ])));
  add "rle/invalid excluded event fails" (fun () ->
      error
        (create_calendar_rle ~window:(interval 0 10)
           [ calendar "A" [ { basic with status = Cancelled; uid = "" } ] ]));
  List.iteri
    (fun i name ->
      add (Printf.sprintf "rle/caller name validity %d" i) (fun () ->
          if i = 3 || i = 4 || i = 6 then
            equal [ run 0 10 [ name ] ] (query [ calendar name [] ])
          else
            error
              (create_calendar_rle ~window:(interval 0 10) [ calendar name [] ])))
    [ ""; " "; "\t"; "name\n"; "name\127"; "\255"; String.make 257 'a' ];
  add "rle/long caller name" (fun () ->
      let name = String.make 256 'a' in
      equal [ run 0 10 [ name ] ] (query [ calendar name [] ]));
  add "rle/duplicate caller names" (fun () ->
      error
        (create_calendar_rle ~window:(interval 0 10)
           [ calendar "A" []; calendar "A" [] ]));
  add "rle/caller names are case sensitive" (fun () ->
      equal
        [ run 0 10 [ "Ada"; "ada" ] ]
        (query [ calendar "Ada" []; calendar "ada" [] ]));
  add "rle/long Unicode caller names are preserved" (fun () ->
      let name = String.concat "" (List.init 128 (fun _ -> "é")) in
      equal [ run 0 10 [ name ] ] (query [ calendar name [] ]);
      equal [ run 0 10 [ name ^ "é" ] ] (query [ calendar (name ^ "é") [] ]));
  add "rle/repeated UID within one person still counts event occupancy"
    (fun () ->
      equal
        [ run 0 1 [ "A" ]; run 1 8 []; run 8 10 [ "A" ] ]
        (query [ calendar "A" [ event 1 5; event 3 8 ] ]));
  add "rle/resolver failure does not produce a partial calendar" (fun () ->
      let resolve ~tzid:_ ~date:_ ~time:_ = Error "unresolved" in
      error
        (create_calendar_rle ~resolve ~default_timezone:"Test/Zone"
           ~window:(interval 0 10)
           [
             calendar "known" [ basic ];
             calendar "unknown" [ day_event epoch None ];
           ]));
  add "rle/equal window boundaries" (fun () ->
      error (create_calendar_rle ~window:(interval 1 1) []));
  add "rle/reversed window boundaries" (fun () ->
      error (create_calendar_rle ~window:(interval 10 0) []));
  add "rle/extreme window does not overflow" (fun () ->
      let window : interval =
        { start_time = Int64.min_int; end_time = Int64.max_int }
      in
      equal
        [
          {
            start_time = Int64.min_int;
            end_time = Int64.max_int;
            names_free = [];
          };
        ]
        (ok (create_calendar_rle ~window [])));
  add "rle/many overlapping events for one person" (fun () ->
      let events = List.init 1500 (fun i -> event ~uid:(string_of_int i) 1 9) in
      equal
        [ run 0 1 [ "A" ]; run 1 9 []; run 9 10 [ "A" ] ]
        (query [ calendar "A" events ]));

  (* Independent oracle: inspect every integer cell, then compress its exact
     free-name list. It does not sort endpoints or maintain busy counters. *)
  let expected_runs calendars =
    let names_at second =
      List.filter_map
        (fun (name, spans) ->
          if
            List.exists
              (fun (start, stop, blocks) ->
                blocks && start <= second && second < stop)
              spans
          then None
          else Some name)
        calendars
    in
    let rec build current_start current_names second acc =
      if second = 10 then List.rev (run current_start 10 current_names :: acc)
      else
        let names = names_at second in
        if names = current_names then
          build current_start current_names (second + 1) acc
        else
          build second names (second + 1)
            (run current_start second current_names :: acc)
    in
    build 0 (names_at 0) 1 []
  in
  let to_calendars raw =
    List.mapi
      (fun person (name, spans) ->
        calendar name
          (List.mapi
             (fun index (start, stop, blocks) ->
               event
                 ~uid:(Printf.sprintf "%d-%d" person index)
                 ~transparency:(if blocks then Opaque else Transparent)
                 start stop)
             spans))
      raw
  in
  List.iter
    (fun (a, b) ->
      List.iter
        (fun (c, d) ->
          add (Printf.sprintf "rle/two-person grid [%d,%d) [%d,%d)" a b c d)
            (fun () ->
              let raw =
                [ ("Zoe", [ (a, b, true) ]); ("Ada", [ (c, d, true) ]) ]
              in
              equal (expected_runs raw) (query (to_calendars raw))))
        spans)
    spans;
  let random = Random.State.make [| 3110; 2026; 23 |] in
  for scenario = 0 to 119 do
    let raw =
      List.init
        (1 + Random.State.int random 5)
        (fun person ->
          let spans =
            List.init (Random.State.int random 6) (fun _ ->
                let start = Random.State.int random 14 - 2 in
                let stop = start + 1 + Random.State.int random 7 in
                (start, stop, Random.State.int random 4 <> 0))
          in
          (Printf.sprintf "Person %d" person, spans))
    in
    add (Printf.sprintf "rle/deterministic multi-calendar oracle %03d" scenario)
      (fun () ->
        let actual = query (to_calendars raw) in
        equal (expected_runs raw) actual;
        ignore (ok (rle_to_json actual)))
  done;

  add "rle JSON/empty" (fun () -> equal "[]" (ok (rle_to_json [])));
  add "rle JSON/known output" (fun () ->
      equal "[{\"start\":0,\"end\":10,\"names_free\":[\"Ada\",\"Zoe\"]}]"
        (ok (rle_to_json [ run 0 10 [ "Ada"; "Zoe" ] ])));
  add "rle JSON/int64 beyond double precision" (fun () ->
      let output =
        ok
          (rle_to_json
             [
               {
                 start_time = 9007199254740993L;
                 end_time = 9007199254740995L;
                 names_free = [];
               };
             ])
      in
      check (has output "9007199254740993") "start precision lost";
      check (has output "9007199254740995") "end precision lost");
  List.iteri
    (fun i runs ->
      add (Printf.sprintf "rle JSON/serializes supplied runs %d" i) (fun () ->
          let expected_run (run : calendar_run) =
            object_
              [
                ("start", Int64.to_string run.start_time);
                ("end", Int64.to_string run.end_time);
                ( "names_free",
                  "[" ^ String.concat "," (List.map quote run.names_free) ^ "]"
                );
              ]
          in
          equal
            ("[" ^ String.concat "," (List.map expected_run runs) ^ "]")
            (ok (rle_to_json runs))))
    [
      [ run 0 0 [] ];
      [ run 2 1 [] ];
      [ run 0 2 []; run 3 5 [ "A" ] ];
      [ run 0 3 []; run 2 5 [ "A" ] ];
      [ run 2 5 []; run 0 2 [ "A" ] ];
      [ run 0 2 [ "A" ]; run 2 5 [ "A" ] ];
      [ run 0 2 [ "A"; "A" ] ];
      [ run 0 2 [ "" ] ];
      [ run 0 2 [ "\255" ] ];
    ];
  add "rle JSON/preserve Unicode names" (fun () ->
      let output = ok (rle_to_json [ run 0 10 [ "Zoë"; "図書館" ] ]) in
      check (has output "Zoë") "name lost";
      check (has output "図書館") "name lost")

let () =
  let cases = List.rev !tests in
  let names = Hashtbl.create (List.length cases) in
  List.iter
    (fun (name, _) ->
      if Hashtbl.mem names name then fail ("duplicate test name: " ^ name);
      Hashtbl.add names name ())
    cases;
  if List.length cases < 300 then
    fail "expected at least 300 distinct calendar tests";
  let failures = ref [] in
  List.iter
    (fun (name, body) ->
      try body ()
      with exn -> failures := (name, Printexc.to_string exn) :: !failures)
    cases;
  List.iter
    (fun (name, message) -> Printf.eprintf "FAIL %s\n  %s\n" name message)
    (List.rev !failures);
  Printf.printf "Calendar_tools: %d tests, %d passed, %d failed.\n%!"
    (List.length cases)
    (List.length cases - List.length !failures)
    (List.length !failures);
  if !failures <> [] then exit 1
