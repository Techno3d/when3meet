(* Calendar_tools keeps parsing, civil dates and availability independent of
   machine-local timezone state and provider-specific calendar formats. The
   public contract and examples live beside each declaration in the .mli. *)

type date = {
  year : int;
  month : int;
  day : int;
}

type time = {
  hour : int;
  minute : int;
  second : int;
}

type timezone =
  | Utc
  | Floating
  | Tzid of string

type date_time = {
  date : date;
  time : time;
  timezone : timezone;
}

type temporal =
  | Date of date
  | Date_time of date_time

type status =
  | Confirmed
  | Tentative
  | Cancelled

type transparency =
  | Opaque
  | Transparent

type calendar_event = {
  uid : string;
  dtstart : temporal;
  dtend : temporal option;
  summary : string option;
  description : string option;
  location : string option;
  status : status;
  transparency : transparency;
}

type interval = {
  start_time : int64;
  end_time : int64;
}

type named_calendar = {
  name : string;
  events : calendar_event list;
}

type calendar_run = {
  start_time : int64;
  end_time : int64;
  names_free : string list;
}

type calendar_rle = calendar_run list

type timezone_resolver =
  tzid:string -> date:date -> time:time -> (int64, string) result

exception Invalid of string

let fail message = raise (Invalid message)

let result_exn = function
  | Ok value -> value
  | Error message -> fail message

let protect f =
  try Ok (f ()) with
  | Invalid message
  | Yojson.Json_error message
  | Yojson.Safe.Util.Type_error (message, _)
  | Scanf.Scan_failure message -> Error message
  | End_of_file -> Error "incomplete date or time"

let check_text context value =
  if not (String.is_valid_utf_8 value) then fail (context ^ ": invalid UTF-8")

let nonblank context value =
  check_text context value;
  if String.trim value = "" then fail (context ^ ": must not be blank")

let midnight = { hour = 0; minute = 0; second = 0 }

(* Ptime owns Gregorian date validation and timestamp arithmetic. *)
let stamp (date : date) (time : time) =
  if date.year < 1 || time.second > 59 then fail "invalid date or time";
  match
    Ptime.of_date_time
      ( (date.year, date.month, date.day),
        ((time.hour, time.minute, time.second), 0) )
  with
  | Some value -> value
  | None -> fail "invalid date or time"

let date_of_ptime value =
  let year, month, day = Ptime.to_date value in
  { year; month; day }

let seconds_of_local date time =
  (* Whole seconds in years 1--9999 are represented exactly by a float. *)
  Int64.of_float (Ptime.to_float_s (stamp date time))

let next_date date =
  match Ptime.add_span (stamp date midnight) (Ptime.Span.of_int_s 86400) with
  | Some value -> date_of_ptime value
  | None -> fail "next day is outside the supported date range"

let date_of_ical text =
  Scanf.sscanf text "%4d%2d%2d%!" (fun year month day ->
      let date = { year; month; day } in
      ignore (stamp date midnight);
      date)

let datetime_of_ical ~tzid text =
  let utc = String.ends_with ~suffix:"Z" text in
  let local =
    if utc then String.sub text 0 (String.length text - 1) else text
  in
  Scanf.sscanf local "%4d%2d%2dT%2d%2d%2d%!"
    (fun year month day hour minute second ->
      let date = { year; month; day } and time = { hour; minute; second } in
      ignore (stamp date time);
      let timezone =
        match (utc, tzid) with
        | true, None -> Utc
        | true, Some _ -> fail "UTC DATE-TIME must not have a TZID"
        | false, None -> Floating
        | false, Some name ->
            nonblank "TZID" name;
            Tzid name
      in
      Date_time { date; time; timezone })

let format_date (d : date) = Printf.sprintf "%04d%02d%02d" d.year d.month d.day

let format_temporal = function
  | Date date -> (format_date date, None)
  | Date_time { date; time; timezone } -> (
      let value =
        Printf.sprintf "%sT%02d%02d%02d" (format_date date) time.hour
          time.minute time.second
      in
      match timezone with
      | Utc -> (value ^ "Z", None)
      | Floating -> (value, None)
      | Tzid name -> (value, Some name))

let string_of_status = function
  | Confirmed -> "confirmed"
  | Tentative -> "tentative"
  | Cancelled -> "cancelled"

let status_of_string = function
  | "confirmed" -> Confirmed
  | "tentative" -> Tentative
  | "cancelled" -> Cancelled
  | _ -> fail "status: expected confirmed, tentative or cancelled"

let string_of_transparency = function
  | Opaque -> "opaque"
  | Transparent -> "transparent"

let transparency_of_string = function
  | "opaque" -> Opaque
  | "transparent" -> Transparent
  | _ -> fail "transparency: expected opaque or transparent"

let civil_time = function
  | Date date -> stamp date midnight
  | Date_time { date; time; timezone } ->
      (match timezone with
      | Tzid name -> nonblank "TZID" name
      | _ -> ());
      stamp date time

let validate_event_exn event =
  nonblank "uid" event.uid;
  List.iter
    (Option.iter (check_text "event text"))
    [ event.summary; event.description; event.location ];
  let start = civil_time event.dtstart in
  match event.dtend with
  | None -> (
      match event.dtstart with
      | Date date -> ignore (next_date date)
      | _ -> ())
  | Some finish ->
      let stop = civil_time finish in
      (match (event.dtstart, finish) with
      | Date _, Date _ -> ()
      | Date_time a, Date_time b when a.timezone = b.timezone -> ()
      | _ -> fail "dtend must match dtstart's kind and timezone");
      if Ptime.compare start stop >= 0 then fail "dtend must follow dtstart"

let validate_event event = protect (fun () -> validate_event_exn event)
let validate_events_exn events = List.iter validate_event_exn events
let is_blocking event = event.status <> Cancelled && event.transparency = Opaque

let resolve_named ?resolve ~tzid ~date ~time () =
  nonblank "timezone" tzid;
  if List.mem tzid [ "UTC"; "Etc/UTC"; "GMT" ] then seconds_of_local date time
  else
    match resolve with
    | None -> fail "named timezone requires a caller-supplied resolver"
    | Some resolver ->
        result_exn
          (try resolver ~tzid ~date ~time
           with _ -> Error "timezone resolver raised an exception")

let resolve_temporal ?resolve ?default_timezone temporal =
  let default () =
    match default_timezone with
    | Some name -> name
    | None -> fail "floating and date-only events require default_timezone"
  in
  let named tzid date time = resolve_named ?resolve ~tzid ~date ~time () in
  match temporal with
  | Date date -> named (default ()) date midnight
  | Date_time { date; time; timezone = Utc } -> seconds_of_local date time
  | Date_time { date; time; timezone = Floating } ->
      named (default ()) date time
  | Date_time { date; time; timezone = Tzid tzid } -> named tzid date time

let event_interval_exn ?resolve ?default_timezone event =
  validate_event_exn event;
  let finish =
    match (event.dtstart, event.dtend) with
    | Date date, None -> Some (Date (next_date date))
    | _, finish -> finish
  in
  Option.map
    (fun finish ->
      let start_time =
        resolve_temporal ?resolve ?default_timezone event.dtstart
      in
      let end_time = resolve_temporal ?resolve ?default_timezone finish in
      if start_time >= end_time then
        fail "resolved end must follow resolved start";
      ({ start_time; end_time } : interval))
    finish

let event_interval ?resolve ?default_timezone event =
  protect (fun () -> event_interval_exn ?resolve ?default_timezone event)

let intervals_exn ?resolve ?default_timezone ~blocking_only events =
  validate_events_exn events;
  List.filter_map
    (fun event ->
      if blocking_only && not (is_blocking event) then None
      else event_interval_exn ?resolve ?default_timezone event)
    events

let check_overlap ?resolve ?default_timezone ?(blocking_only = true) events =
  protect (fun () ->
      let intervals =
        intervals_exn ?resolve ?default_timezone ~blocking_only events
        |> List.sort (fun (a : interval) b ->
            Int64.compare a.start_time b.start_time)
      in
      let rec overlap = function
        | (a : interval) :: ((b : interval) :: _ as rest) ->
            a.end_time > b.start_time || overlap rest
        | _ -> false
      in
      overlap intervals)

let create_calendar_rle ?resolve ?default_timezone ~(window : interval)
    calendars =
  protect (fun () ->
      if window.start_time >= window.end_time then
        fail "window start must precede end";
      let names =
        List.map
          (fun calendar ->
            nonblank "calendar name" calendar.name;
            calendar.name)
          calendars
      in
      if List.length (List.sort_uniq String.compare names) <> List.length names
      then fail "duplicate calendar name";
      let clip (span : interval) =
        let start_time = max window.start_time span.start_time in
        let end_time = min window.end_time span.end_time in
        if start_time < end_time then Some ({ start_time; end_time } : interval)
        else None
      in
      let people =
        List.map
          (fun calendar ->
            let spans =
              intervals_exn ?resolve ?default_timezone ~blocking_only:true
                calendar.events
            in
            (calendar.name, List.filter_map clip spans))
          calendars
      in
      (* Nothing changes between event boundaries, so check each stretch
         once. *)
      let boundaries =
        List.concat_map
          (fun (_, spans) ->
            List.concat_map
              (fun (span : interval) -> [ span.start_time; span.end_time ])
              spans)
          people
      in
      let boundaries =
        List.sort_uniq Int64.compare
          (window.start_time :: window.end_time :: boundaries)
      in
      let rec runs acc = function
        | start_time :: (end_time :: _ as rest) ->
            let names_free =
              List.filter_map
                (fun (name, spans) ->
                  if
                    List.exists
                      (fun (span : interval) ->
                        span.start_time <= start_time
                        && start_time < span.end_time)
                      spans
                  then None
                  else Some name)
                people
            in
            let acc =
              match acc with
              | previous :: tail when previous.names_free = names_free ->
                  { previous with end_time } :: tail
              | _ -> { start_time; end_time; names_free } :: acc
            in
            runs acc rest
        | _ -> List.rev acc
      in
      runs [] boundaries)

(* Yojson handles JSON syntax and field access. Extra fields are harmless;
   required fields and the resulting event's dates are still checked. *)
module Json = Yojson.Safe.Util

let json_optional decode = function
  | `Null -> None
  | value -> Some (decode value)

let json_option encode = function
  | None -> `Null
  | Some value -> encode value

let json_string text = `String text

let json_temporal json =
  let value = Json.member "value" json |> Json.to_string in
  match Json.member "kind" json |> Json.to_string with
  | "date" -> Date (date_of_ical value)
  | "date_time" ->
      datetime_of_ical
        ~tzid:(Json.member "tzid" json |> json_optional Json.to_string)
        value
  | _ -> fail "temporal kind must be date or date_time"

let event_of_json json =
  let optional name decode = Json.member name json |> json_optional decode in
  let enum name decode default = Option.value (optional name decode) ~default in
  let event =
    {
      uid = Json.member "uid" json |> Json.to_string;
      dtstart = Json.member "dtstart" json |> json_temporal;
      dtend = optional "dtend" json_temporal;
      summary = optional "summary" Json.to_string;
      description = optional "description" Json.to_string;
      location = optional "location" Json.to_string;
      status =
        enum "status"
          (fun value -> status_of_string (Json.to_string value))
          Confirmed;
      transparency =
        enum "transparency"
          (fun value -> transparency_of_string (Json.to_string value))
          Opaque;
    }
  in
  validate_event_exn event;
  event

let events_of_json json = Json.to_list json |> List.map event_of_json

let temporal_as_json temporal =
  let value, tzid = format_temporal temporal in
  let kind, extra =
    match temporal with
    | Date _ -> ("date", [])
    | Date_time _ -> ("date_time", [ ("tzid", json_option json_string tzid) ])
  in
  `Assoc ([ ("kind", `String kind); ("value", `String value) ] @ extra)

let event_as_json event =
  `Assoc
    [
      ("uid", `String event.uid);
      ("dtstart", temporal_as_json event.dtstart);
      ("dtend", json_option temporal_as_json event.dtend);
      ("summary", json_option json_string event.summary);
      ("description", json_option json_string event.description);
      ("location", json_option json_string event.location);
      ("status", `String (string_of_status event.status));
      ("transparency", `String (string_of_transparency event.transparency));
    ]

let event_to_json event =
  protect (fun () ->
      validate_event_exn event;
      Yojson.Safe.to_string (event_as_json event))

let events_to_json events =
  protect (fun () ->
      validate_events_exn events;
      Yojson.Safe.to_string (`List (List.map event_as_json events)))

let json_to_event text =
  protect (fun () -> event_of_json (Yojson.Safe.from_string text))

let json_to_events text =
  protect (fun () -> events_of_json (Yojson.Safe.from_string text))

let validate_json text =
  protect (fun () ->
      match Yojson.Safe.from_string text with
      | `List _ as json -> ignore (events_of_json json)
      | json -> ignore (event_of_json json))

let rle_to_json runs =
  protect (fun () ->
      let encode (run : calendar_run) =
        `Assoc
          [
            ("start", `Intlit (Int64.to_string run.start_time));
            ("end", `Intlit (Int64.to_string run.end_time));
            ("names_free", `List (List.map json_string run.names_free));
          ]
      in
      Yojson.Safe.to_string (`List (List.map encode runs)))

(* Icalendar handles the file grammar, folding, escaping and property parsing.
   This adapter only translates its values into our scheduling model. *)
module Ical = struct
  let date (year, month, day) = { year; month; day }

  let timestamp timezone stamp =
    let civil_date, ((hour, minute, second), _) = Ptime.to_date_time stamp in
    Date_time
      { date = date civil_date; time = { hour; minute; second }; timezone }

  let temporal (parameters, value) =
    match value with
    | `Date value -> Date (date value)
    | `Datetime (`Utc value) -> timestamp Utc value
    | `Datetime (`Local value) ->
        (* The library keeps quoted TZIDs in its generic parameter map. *)
        let timezone =
          match
            Icalendar.Params.find (Icalendar.Iana_param "TZID") parameters
          with
          | None -> Floating
          | Some [ (`Quoted name | `String name) ] -> Tzid name
          | Some _ -> fail "expected one TZID"
        in
        timestamp timezone value
    | `Datetime (`With_tzid (value, (global, name))) ->
        timestamp (Tzid ((if global then "/" else "") ^ name)) value

  let event (source : Icalendar.event) =
    if source.rrule <> None then fail "recurring events are not supported";
    let dtend =
      match source.dtend_or_duration with
      | None -> None
      | Some (`Dtend value) -> Some (temporal value)
      | Some (`Duration _) -> fail "use DTEND instead of DURATION"
    in
    let initial =
      {
        uid = snd source.uid;
        dtstart = temporal source.dtstart;
        dtend;
        summary = None;
        description = None;
        location = None;
        status = Confirmed;
        transparency = Opaque;
      }
    in
    List.fold_left
      (fun event -> function
        | `Summary (_, value) -> { event with summary = Some value }
        | `Description (_, value) -> { event with description = Some value }
        | `Location (_, value) -> { event with location = Some value }
        | `Status (_, `Confirmed) -> { event with status = Confirmed }
        | `Status (_, `Tentative) -> { event with status = Tentative }
        | `Status (_, `Cancelled) -> { event with status = Cancelled }
        | `Status _ -> fail "unsupported event status"
        | `Transparency (_, `Opaque) -> { event with transparency = Opaque }
        | `Transparency (_, `Transparent) ->
            { event with transparency = Transparent }
        | `Rdate _ | `Exdate _ | `Recur_id _ ->
            fail "recurring events are not supported"
        | `Iana_prop (name, _, _)
          when List.mem
                 (String.uppercase_ascii name)
                 [
                   "DTEND";
                   "DURATION";
                   "RRULE";
                   "RDATE";
                   "EXDATE";
                   "EXRULE";
                   "RECURRENCE-ID";
                   "STATUS";
                   "TRANSP";
                 ] -> fail ("unsupported scheduling property: " ^ name)
        | _ -> event)
      initial source.props

  let parse input =
    let _, components = result_exn (Icalendar.parse input) in
    let events =
      List.filter_map
        (function
          | `Event value -> Some (event value)
          | _ -> None)
        components
    in
    validate_events_exn events;
    events
end

let ical_to_events input = protect (fun () -> Ical.parse input)

let ical_to_event input =
  match ical_to_events input with
  | Error message -> Error message
  | Ok [ event ] -> Ok event
  | Ok _ -> Error "iCalendar: expected exactly one VEVENT"

let validate_ical input = Result.map (fun _ -> ()) (ical_to_events input)

(* A calendar import always produces an array, even for one VEVENT. *)
let ical_to_json text =
  match ical_to_events text with
  | Error message -> Error message
  | Ok events -> events_to_json events
