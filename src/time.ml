type tz_offset_s = int

let tz_offset_s_utc = 0

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type date_time = {
  year : int;
  month : month;
  day : int;
  hour : int;
  minute : int;
  second : int;
  tz_offset_s : int;
}

type weekday_range = weekday Range.range

type month_day_range = int Range.range

type day_range =
  | Weekday_range of weekday_range
  | Month_day_range of month_day_range

let first_mday = 1

let tm_year_offset = 1900

let minute_to_second_multiplier = 60L

let hour_to_second_multiplier = Int64.mul 60L minute_to_second_multiplier

let day_to_second_multiplier = Int64.mul 24L hour_to_second_multiplier

module Check = struct
  let check_unix_time (x : int64) = x >= 0L

  let check_second ~(second : int) : bool = 0 <= second && second < 60

  let check_minute_second ~(minute : int) ~(second : int) : bool =
    0 <= minute && minute < 60 && check_second ~second

  let check_hour_minute_second ~(hour : int) ~(minute : int) ~(second : int) :
    bool =
    (0 <= hour && hour < 24) && check_minute_second ~minute ~second

  let check_date_time (x : date_time) : bool =
    1 <= x.day
    && x.day <= 31
    && check_hour_minute_second ~hour:x.hour ~minute:x.minute ~second:x.second
end

let next_hour_minute ~(hour : int) ~(minute : int) : (int * int, unit) result =
  if Check.check_hour_minute_second ~hour ~minute ~second:0 then
    if minute < 59 then Ok (hour, succ minute) else Ok (succ hour mod 24, 0)
  else Error ()

let next_weekday (wday : weekday) : weekday =
  match wday with
  | `Sun -> `Mon
  | `Mon -> `Tue
  | `Tue -> `Wed
  | `Wed -> `Thu
  | `Thu -> `Fri
  | `Fri -> `Sat
  | `Sat -> `Sun

let tm_int_of_weekday (wday : weekday) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let weekday_of_tm_int (x : int) : weekday =
  match x with
  | 0 -> `Sun
  | 1 -> `Mon
  | 2 -> `Tue
  | 3 -> `Wed
  | 4 -> `Thu
  | 5 -> `Fri
  | 6 -> `Sat
  | _ -> failwith "Invalid wday int"

let tm_int_of_month (month : month) : int =
  match month with
  | `Jan -> 0
  | `Feb -> 1
  | `Mar -> 2
  | `Apr -> 3
  | `May -> 4
  | `Jun -> 5
  | `Jul -> 6
  | `Aug -> 7
  | `Sep -> 8
  | `Oct -> 9
  | `Nov -> 10
  | `Dec -> 11

let month_of_tm_int (x : int) : (month, unit) result =
  match x with
  | 0 -> Ok `Jan
  | 1 -> Ok `Feb
  | 2 -> Ok `Mar
  | 3 -> Ok `Apr
  | 4 -> Ok `May
  | 5 -> Ok `Jun
  | 6 -> Ok `Jul
  | 7 -> Ok `Aug
  | 8 -> Ok `Sep
  | 9 -> Ok `Oct
  | 10 -> Ok `Nov
  | 11 -> Ok `Dec
  | _ -> Error ()

let human_int_of_month (month : month) : int = tm_int_of_month month + 1

let month_of_human_int (x : int) : (month, unit) result = month_of_tm_int (x - 1)

let compare_month (m1 : month) (m2 : month) : int =
  compare (tm_int_of_month m1) (tm_int_of_month m2)

let month_lt m1 m2 = tm_int_of_month m1 < tm_int_of_month m2

let month_le m1 m2 = tm_int_of_month m1 <= tm_int_of_month m2

let month_gt m1 m2 = tm_int_of_month m1 > tm_int_of_month m2

let month_ge m1 m2 = tm_int_of_month m1 >= tm_int_of_month m2

let compare_weekday (d1 : weekday) (d2 : weekday) : int =
  compare (tm_int_of_weekday d1) (tm_int_of_weekday d2)

let weekday_lt d1 d2 = tm_int_of_weekday d1 < tm_int_of_weekday d2

let weekday_le d1 d2 = tm_int_of_weekday d1 <= tm_int_of_weekday d2

let weekday_gt d1 d2 = tm_int_of_weekday d1 > tm_int_of_weekday d2

let weekday_ge d1 d2 = tm_int_of_weekday d1 >= tm_int_of_weekday d2

let zero_tm_sec tm = Unix.{ tm with tm_sec = 0 }

let ptime_date_time_of_date_time (x : date_time) : Ptime.date * Ptime.time =
  ( (x.year, human_int_of_month x.month, x.day),
    ((x.hour, x.minute, x.second), x.tz_offset_s) )

let date_time_of_ptime_date_time
    (((year, month, day), ((hour, minute, second), tz_offset_s)) :
       Ptime.date * Ptime.time) : (date_time, unit) result =
  match month_of_human_int month with
  | Ok month -> Ok { year; month; day; hour; minute; second; tz_offset_s }
  | Error () -> Error ()

let unix_time_of_date_time (x : date_time) : (int64, unit) result =
  match Ptime.of_date_time (ptime_date_time_of_date_time x) with
  | None -> Error ()
  | Some x -> x |> Ptime.to_float_s |> Int64.of_float |> Result.ok

let date_time_of_unix_time ~(tz_offset_s_of_date_time : tz_offset_s) (x : int64)
  : (date_time, unit) result =
  match Ptime.of_float_s (Int64.to_float x) with
  | None -> Error ()
  | Some x ->
    x
    |> Ptime.to_date_time ~tz_offset_s:tz_offset_s_of_date_time
    |> date_time_of_ptime_date_time

(* let tm_of_date_time (x : date_time) : Unix.tm =
   {
    tm_sec = x.second;
    tm_min = x.minute;
    tm_hour = x.hour;
    tm_mday = x.day;
    tm_mon = tm_int_of_month x.month;
    tm_year = x.year;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false;
   } *)

(* let tm_of_unix_time ~(time_zone_of_tm : time_zone) (time : int64) : (Unix.tm, unit) result =
   let time = Int64.to_float time in
   match time_zone_of_tm with
   | `Local -> Ok (Unix.localtime time)
   | `UTC -> Ok (Unix.gmtime time)
   | `UTC_plus_sec tz_offset_s ->
      match Ptime.of_float_s time with
      | None -> Error ()
      | Ok x ->
          x
      |> Ptime.to_date_time ~tz_offset_s
      |> date_time_of_ptime_date_time

    let date_time = Ptime.of_float_s time in
    CalendarLib.Calendar.convert date_time CalendarLib.Time_Zone.UTC
      CalendarLib.Time_Zone.(UTC_Plus x)
    |> CalendarLib.Calendar.to_unixtm

   let unix_time_of_tm ~(time_zone_of_tm : time_zone) (tm : Unix.tm) : int64 =
   tm
   |> (fun x ->
      match time_zone_of_tm with
      | `Local ->
        let time, _ = Unix.mktime tm in
        time
      | `UTC ->
        x
        |> CalendarLib.Calendar.from_unixtm
        |> CalendarLib.Calendar.from_gmt
        |> CalendarLib.Calendar.to_unixfloat
      | `UTC_plus _ ->
        let date_time = CalendarLib.Calendar.from_unixtm tm in
        let tz = cal_time_zone_of_time_zone time_zone_of_tm in
        CalendarLib.Calendar.convert date_time tz CalendarLib.Time_Zone.UTC
        |> CalendarLib.Calendar.to_unixfloat)
   |> fun time -> time |> Int64.of_float *)

(* let normalize_tm tm =
   tm
   |> zero_tm_sec
   |> CalendarLib.Calendar.from_unixtm
   |> CalendarLib.Calendar.to_unixtm

   let tm_change_time_zone ~(from_time_zone : time_zone)
    ~(to_time_zone : time_zone) (tm : Unix.tm) : Unix.tm =
   if from_time_zone = to_time_zone then tm
   else
    let time = unix_time_of_tm ~time_zone_of_tm:from_time_zone tm in
    tm_of_unix_time ~time_zone_of_tm:to_time_zone time *)

let is_leap_year ~year =
  assert (year > 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  (divisible_by_4 && divisible_by_100 && divisible_by_400)
  || (divisible_by_4 && not divisible_by_100)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_count_of_month ~year ~(month : month) =
  match month with
  | `Jan -> 31
  | `Feb -> if is_leap_year ~year then 29 else 28
  | `Mar -> 31
  | `Apr -> 30
  | `May -> 31
  | `Jun -> 30
  | `Jul -> 31
  | `Aug -> 31
  | `Sep -> 30
  | `Oct -> 31
  | `Nov -> 30
  | `Dec -> 31

let weekday_of_month_day ~(year : int) ~(month : month) ~(mday : int) :
  (weekday, unit) result =
  match Ptime.(of_date (year, human_int_of_month month, mday)) with
  | None -> Error ()
  | Some wday -> Ok (Ptime.weekday wday)

(* let local_tm_to_utc_tm (tm : Unix.tm) : Unix.tm =
   let timestamp, _ = Unix.mktime tm in
   Unix.gmtime timestamp *)

module Second_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Minute_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Hour_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Weekday_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = Some 7

    let to_int x = x

    let of_int x = x
  end)

module Weekday_ranges = Ranges_small.Make (struct
    type t = weekday

    let modulo = Some 7

    let to_int = tm_int_of_weekday

    let of_int = weekday_of_tm_int
  end)

module Month_day_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_tm_int_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Month_ranges = Ranges_small.Make (struct
    type t = month

    let modulo = None

    let to_int = human_int_of_month

    let of_int x = x |> month_of_human_int |> Result.get_ok
  end)

module Year_ranges = Ranges_small.Make (struct
    type t = int

    let modulo = None

    let to_int x = x

    let of_int x = x
  end)

module Current = struct
  let cur_unix_time () : int64 = Unix.time () |> Int64.of_float

  let cur_date_time ~tz_offset_s_of_date_time : (date_time, unit) result =
    cur_unix_time ()
  |> date_time_of_unix_time ~tz_offset_s_of_date_time

  let cur_tm_local () : Unix.tm = Unix.time () |> Unix.localtime

  let cur_tm_utc () : Unix.tm = Unix.time () |> Unix.gmtime
end

module Interpret_string = struct
  let weekdays : (string * weekday) list =
    [
      ("sunday", `Sun);
      ("monday", `Mon);
      ("tuesday", `Tue);
      ("wednesday", `Wed);
      ("thursday", `Thu);
      ("friday", `Fri);
      ("saturday", `Sat);
    ]

  let months : (string * month) list =
    [
      ("january", `Jan);
      ("february", `Feb);
      ("march", `Mar);
      ("april", `Apr);
      ("may", `May);
      ("june", `Jun);
      ("july", `Jul);
      ("august", `Aug);
      ("september", `Sep);
      ("october", `Oct);
      ("november", `Nov);
      ("december", `Dec);
    ]

  let weekday_of_string (s : string) : (weekday, unit) result =
    match Misc_utils.prefix_string_match weekdays s with
    | [ (_, x) ] -> Ok x
    | _ -> Error ()

  let month_of_string (s : string) : (month, unit) result =
    match Misc_utils.prefix_string_match months s with
    | [ (_, x) ] -> Ok x
    | _ -> Error ()
end

let compare_date_time (x : date_time) (y : date_time) : int =
  match compare x.year y.year with
  | 0 -> (
      match
        compare (human_int_of_month x.month) (human_int_of_month y.month)
      with
      | 0 -> (
          match compare x.day y.day with
          | 0 -> (
              match compare x.hour y.hour with
              | 0 -> (
                  match compare x.minute y.minute with
                  | 0 -> compare x.second y.second
                  | n -> n )
              | n -> n )
          | n -> n )
      | n -> n )
  | n -> n

(* module Add = struct
   let add_days_unix_time ~(days : int) (x : int64) : int64 =
    tm_of_unix_time ~time_zone_of_tm:`Local x
    |> (fun tm -> { tm with tm_mday = tm.tm_mday + days })
    |> unix_time_of_tm ~time_zone_of_tm:`Local
   end *)

module Serialize = struct
  let pack_weekday (x : weekday) : Time_t.weekday = x

  let pack_month (x : month) : Time_t.month = x
end

module Deserialize = struct
  let unpack_weekday (x : Time_t.weekday) : weekday = x

  let unpack_month (x : Time_t.month) : month = x
end

module To_string = struct
  let string_of_weekday (wday : weekday) : string =
    match wday with
    | `Sun -> "Sun"
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"

  let string_of_month (month : month) : string =
    match month with
    | `Jan -> "Jan"
    | `Feb -> "Feb"
    | `Mar -> "Mar"
    | `Apr -> "Apr"
    | `May -> "May"
    | `Jun -> "Jun"
    | `Jul -> "Jul"
    | `Aug -> "Aug"
    | `Sep -> "Sep"
    | `Oct -> "Oct"
    | `Nov -> "Nov"
    | `Dec -> "Dec"

  let yyyymondd_hhmmss_string_of_tm (tm : Unix.tm) : (string, unit) result =
    match month_of_tm_int tm.tm_mon with
    | Ok mon ->
      let mon = string_of_month mon in
      Ok
        (Printf.sprintf "%04d %s %02d %02d:%02d:%02d"
           (tm.tm_year + tm_year_offset)
           mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec)
    | Error () -> Error ()

  let yyyymondd_hhmmss_string_of_date_time (x : date_time) : string =
    let mon = string_of_month x.month in
    Printf.sprintf "%04d %s %02d %02d:%02d:%02d" x.year mon x.day x.hour
      x.minute x.second

  let yyyymondd_hhmmss_string_of_unix_time
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) :
    (string, unit) result =
    date_time_of_unix_time ~tz_offset_s_of_date_time:display_using_tz_offset_s
      time
    |> Result.map yyyymondd_hhmmss_string_of_date_time

  (* let yyyymmdd_hhmmss_string_of_tm (tm : Unix.tm) : (string, unit) result =
     match month_of_tm_int tm.tm_mon with
     | Ok mon ->
       let mon = human_int_of_month mon in
       Ok
         (Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
            (tm.tm_year + tm_year_offset)
            mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec)
     | Error () -> Error () *)

  let yyyymmdd_hhmmss_string_of_date_time (x : date_time) : string =
    let mon = human_int_of_month x.month in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" x.year mon x.day x.hour
      x.minute x.second

  let yyyymmdd_hhmmss_string_of_unix_time
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) :
    (string, unit) result =
    date_time_of_unix_time ~tz_offset_s_of_date_time:display_using_tz_offset_s
      time
    |> Result.map yyyymmdd_hhmmss_string_of_date_time

  (*let yyyymondd_hhmm_string_of_tm (tm : Unix.tm) : (string, unit) result =
    match month_of_tm_int tm.tm_mon with
    | Ok mon ->
      let mon = string_of_month mon in
      Ok
        (Printf.sprintf "%04d %s %02d %02d:%02d"
           (tm.tm_year + tm_year_offset)
           mon tm.tm_mday tm.tm_hour tm.tm_min)
    | Error () -> Error ()
  *)

  let yyyymondd_hhmm_string_of_date_time (x : date_time) : string =
    let mon = string_of_month x.month in
    Printf.sprintf "%04d %s %02d %02d:%02d" x.year mon x.day x.hour x.minute

  let yyyymondd_hhmm_string_of_unix_time
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) :
    (string, unit) result =
    date_time_of_unix_time ~tz_offset_s_of_date_time:display_using_tz_offset_s
      time
    |> Result.map yyyymondd_hhmm_string_of_date_time

  (* let yyyymmdd_hhmm_string_of_tm (tm : Unix.tm) : (string, unit) result =
     match month_of_tm_int tm.tm_mon with
     | Ok mon ->
       let mon = human_int_of_month mon in
       Ok
         (Printf.sprintf "%04d-%02d-%02d %02d:%02d"
            (tm.tm_year + tm_year_offset)
            mon tm.tm_mday tm.tm_hour tm.tm_min)
     | Error () -> Error () *)

  let yyyymmdd_hhmm_string_of_date_time (x : date_time) : string =
    let mon = human_int_of_month x.month in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d" x.year mon x.day x.hour x.minute

  let yyyymmdd_hhmm_string_of_unix_time
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) :
    (string, unit) result =
    date_time_of_unix_time ~tz_offset_s_of_date_time:display_using_tz_offset_s
      time
    |> Result.map yyyymmdd_hhmm_string_of_date_time

  let debug_string_of_time ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) : string =
    ( match
        yyyymondd_hhmmss_string_of_unix_time ~display_using_tz_offset_s time
      with
      | Error () -> Debug_print.bprintf ~indent_level buffer "Invalid time\n"
      | Ok s -> Debug_print.bprintf ~indent_level buffer "%s\n" s );
    Buffer.contents buffer
end

module Print = struct
  let debug_print_time ?(indent_level = 0)
      ~(display_using_tz_offset_s : tz_offset_s) (time : int64) : unit =
    print_string
      (To_string.debug_string_of_time ~indent_level ~display_using_tz_offset_s
         time)
end

module Date_time_set = Set.Make (struct
    type t = date_time

    let compare = compare_date_time
  end)
