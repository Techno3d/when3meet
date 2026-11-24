(** Pure calendar validation, conversion, and availability calculations.

    Uses Yojson for JSON, Ptime for dates, and the [icalendar] package for
    importing calendars. Install dependencies with
    [opam install yojson ptime icalendar.0.1.13]. Parsing follows those
    libraries' behavior; this module adds the event and availability rules
    below. It performs no I/O or recurrence expansion. Fallible operations
    return [Error message]. Input-size limits belong to the caller.

    The OCaml representation follows iCalendar concepts rather than an API
    provider's event format. In particular, dates, floating local times, UTC
    times, and named local times remain distinct through JSON roundtrips.
    Supported iCalendar behavior is documented under {!ical_to_events}.

    Intervals are half-open: the start is included and the end is excluded.
    Touching events do not overlap. Instants use whole POSIX seconds, not
    milliseconds. Directly constructed times use seconds 0--59; the iCalendar
    parser may normalize imported leap seconds. *)

type date = {
  year : int;
  month : int;
  day : int;
}
(** Gregorian civil date, with year 1--9999. *)

type time = {
  hour : int;
  minute : int;
  second : int;
}
(** Clock time: hour 0--23, minute and second 0--59. *)

(** [Floating] has no timezone until the caller supplies one. [Tzid name]
    preserves the original identifier; it does not imply that the module
    contains an IANA timezone database. *)
type timezone =
  | Utc
  | Floating
  | Tzid of string

type date_time = {
  date : date;
  time : time;
  timezone : timezone;
}

(** A [Date] is an all-day boundary, not UTC midnight. *)
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
(** An iCalendar-style VEVENT. [dtend] is exclusive when present. An all-day
    event without [dtend] occupies one civil day; a timed event without [dtend]
    is instantaneous and occupies no interval. An implicit all-day end must
    remain within the supported date range. Defaults when importing omitted
    properties are [Confirmed] and [Opaque].

    Endpoints must have the same temporal kind, and timed endpoints must use the
    same timezone constructor/identifier. An explicit end must follow its start
    in civil time. UIDs and TZIDs must be nonblank UTF-8 strings. Metadata is
    optional UTF-8 text. Duplicate UIDs are allowed; no event is deduplicated.
*)

type interval = {
  start_time : int64;
  end_time : int64;
}
(** A positive-length interval of POSIX seconds. *)

type named_calendar = {
  name : string;
  events : calendar_event list;
}
(** A caller-owned identity and its events. Names must be distinct, nonblank,
    valid UTF-8 strings. The module never invents names or takes them from event
    summaries. *)

type calendar_run = {
  start_time : int64;
  end_time : int64;
  names_free : string list;
}

type calendar_rle = calendar_run list
(** Consecutive runs of identical [names_free], in chronological order. Within
    each run, names follow the caller's calendar order. An empty list means none
    of the supplied calendars is free in that interval. *)

type timezone_resolver =
  tzid:string -> date:date -> time:time -> (int64, string) result
(** Resolves a named wall-clock time to POSIX seconds. Callers should implement
    iCalendar's first-occurrence rule for repeated times and pre-transition
    offset rule for nonexistent times. Resolver errors propagate. Resolver
    exceptions are converted to a generic error without disclosing their text.
    UTC, Etc/UTC, and GMT are handled without calling the resolver. *)

val validate_event : calendar_event -> (unit, string) result
(** Validate a directly constructed event's dates, timezone consistency, UID,
    metadata and endpoint order. This performs no timezone resolution. *)

val validate_ical : string -> (unit, string) result
(** Parse and validate using the same import path as {!ical_to_events}. *)

val validate_json : string -> (unit, string) result
(** Validate either one event object or an array of event objects in this
    module's JSON schema. This is schema validation, not just JSON syntax
    checking. A syntactically valid number, boolean, or unrelated object fails.
*)

val ical_to_event : string -> (calendar_event, string) result
(** Import exactly one event from a VCALENDAR; zero or multiple events fail. *)

val ical_to_events : string -> (calendar_event list, string) result
(** Import events using [Icalendar.parse], then validate the translated records.
    The library handles file structure, folding, escaping and properties. A
    VCALENDAR wrapper with at least one component is required; standalone
    VEVENTs and completely empty calendars fail.

    Preserve UID, DTSTART, DTEND, SUMMARY, DESCRIPTION, LOCATION, STATUS and
    TRANSP, including date-only, floating, UTC and named-zone times. Other
    metadata, alarms and non-event components are ignored. Named zones still
    require the caller's resolver for availability calculations.

    Recurrence and DURATION are rejected because this event model does not
    expand them. Unsupported or malformed scheduling properties fail rather than
    silently changing occupied time. Other parsing behavior, including duplicate
    properties and calendar headers, follows the library. *)

val event_to_json : calendar_event -> (string, string) result
(** Validate and encode a single event as compact JSON. The event fields are
    [uid], [dtstart], [dtend], [summary], [description], [location], [status],
    and [transparency]; optional values encode as [null]. Enums are lowercase.

    A temporal value is either
    {[
     {"kind":"date","value":"20261123"}
    ]}
    or
    {[
     {"kind":"date_time","value":"20261123T100000","tzid":"America/New_York"}
    ]}
    UTC DATE-TIME values end in [Z] with [tzid:null]; floating values have
    neither [Z] nor a TZID. Named values omit [Z] and preserve their TZID. This
    schema deliberately models iCalendar properties; it is not the experimental
    provider/envelope schema in [experiments/]. *)

val events_to_json : calendar_event list -> (string, string) result
(** Validate and encode an array, preserving input order and duplicate UIDs. An
    empty list encodes as an empty JSON array. *)

val ical_to_json : string -> (string, string) result
(** Equivalent to [ical_to_events] followed by [events_to_json]. Always emits an
    array, including for one event, so calendar cardinality is unambiguous. *)

val json_to_event : string -> (calendar_event, string) result
(** Decode one object with Yojson. Required fields are [uid] and [dtstart].
    Missing or [null] metadata/end becomes [None]; missing or [null] enums use
    the import defaults. Extra fields are ignored. Duplicate keys and syntax
    extensions follow Yojson (the first matching key is used). Invalid field
    types, dates, enum values and non-UTF-8 event text fail. *)

val json_to_events : string -> (calendar_event list, string) result
(** Decode an array of events, preserving order. An object is not an array. *)

val is_blocking : calendar_event -> bool
(** True for opaque, non-cancelled events, including tentative events. This
    predicate alone does not validate the event. *)

val event_interval :
  ?resolve:timezone_resolver ->
  ?default_timezone:string ->
  calendar_event ->
  (interval option, string) result
(** Resolve an event's occupied time, independent of status/transparency.
    Returns [None] for instantaneous timed events. Date-only and floating values
    require [default_timezone]; named times require a resolver unless they name
    one of the supported UTC aliases. All-day endpoints are resolved separately,
    so a daylight-saving day need not be 86,400 elapsed seconds. Rejects
    unresolved zones and reversed/equal resolved boundaries. *)

val check_overlap :
  ?resolve:timezone_resolver ->
  ?default_timezone:string ->
  ?blocking_only:bool ->
  calendar_event list ->
  (bool, string) result
(** True if any two events overlap for positive duration, not only if every
    event shares a common intersection. Empty and singleton lists return false
    after validation. [blocking_only] defaults to true: cancelled/transparent
    events do not block. Set it to false to compare their temporal spans too.
    Invalid events fail even when nonblocking; irrelevant nonblocking events do
    not need timezone resolution. Runtime O(n log n), space O(n). *)

val create_calendar_rle :
  ?resolve:timezone_resolver ->
  ?default_timezone:string ->
  window:interval ->
  named_calendar list ->
  (calendar_rle, string) result
(** Compute caller-named availability over the entire required [window].
    Validate every event/name, resolve blocking intervals, clip them to the
    window and check availability between consecutive endpoints. Overlapping
    events belonging to one person keep that person busy until the last
    overlapping event ends. Zero-duration, cancelled and transparent events do
    not block. Availability does not use UID-based deduplication, so repeated
    events in one calendar do not make a person any busier. UIDs may also repeat
    across calendars; calendar names may not repeat. No event is inferred from a
    name.

    Output is sorted, positive-length, contiguous, covers exactly [window], and
    merges adjacent runs whenever [names_free] is unchanged. Empty calendars are
    free throughout the window. No calendars yields one run with no names.
    "Free" means no supplied blocking event; it is not an assertion that the
    caller has imported every real-world commitment.

    The implementation sorts event boundaries, checks each person's events in
    each stretch, then merges equal neighboring runs. Runtime is O(e log e +
    b*(e+p)), where e is event count, b distinct endpoints and p calendars. This
    straightforward approach suits small group calendars and does not round
    times to a fixed slot grid.

    Example integration, where the backend supplies identities and event lists:
    {[
    let availability ~window arjun_events maya_events =
      create_calendar_rle ~default_timezone:"UTC" ~window
        [
          { name = "Arjun"; events = arjun_events };
          { name = "Maya"; events = maya_events };
        ]
    ]}
    For a window from second 0 to 30, suppose Arjun is busy from 0 to 10 and
    Maya is busy from 10 to 20. Only Maya is free from 0 to 10; only Arjun is
    free from 10 to 20; both are free from 20 to 30. All ends are exclusive. The
    final run orders the names as Arjun then Maya. *)

val rle_to_json : calendar_rle -> (string, string) result
(** Encode objects of the following shape as a JSON array:
    {[
      {"start":0,"end":10,"names_free":["Maya"] }
    ]}

    Bounds are integer POSIX seconds, serialized without precision loss. This is
    a serializer, not a validator: use {!create_calendar_rle} to build canonical
    runs. An empty list encodes as an empty JSON array. *)
