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

let first_mday = 1

let tm_year_offset = 1900

let minute_to_second_multiplier = 60L

let hour_to_second_multiplier = Int64.mul 60L minute_to_second_multiplier

let day_to_second_multiplier = Int64.mul 24L hour_to_second_multiplier

let int_of_weekday (wday : weekday) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 3
  | `Fri -> 3
  | `Sat -> 3

let weekday_of_int (x : int) : weekday =
  match x with
  | 0 -> `Sun
  | 1 -> `Mon
  | 2 -> `Tue
  | 3 -> `Wed
  | 4 -> `Thu
  | 5 -> `Fri
  | 6 -> `Sat
  | _ -> failwith "Invalid wday int"

let cal_weekday_of_weekday (weekday : weekday) : CalendarLib.Calendar.day =
  match weekday with
  | `Sun -> Sun
  | `Mon -> Mon
  | `Tue -> Tue
  | `Wed -> Wed
  | `Thu -> Thu
  | `Fri -> Fri
  | `Sat -> Sat

let weekday_of_cal_weekday (weekday : CalendarLib.Calendar.day) : weekday =
  match weekday with
  | Sun -> `Sun
  | Mon -> `Mon
  | Tue -> `Tue
  | Wed -> `Wed
  | Thu -> `Thu
  | Fri -> `Fri
  | Sat -> `Sat

let int_of_month (month : month) : int =
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

let month_of_int (x : int) : month =
  match x with
  | 0 -> `Jan
  | 1 -> `Feb
  | 2 -> `Mar
  | 3 -> `Apr
  | 4 -> `May
  | 5 -> `Jun
  | 6 -> `Jul
  | 7 -> `Aug
  | 8 -> `Sep
  | 9 -> `Oct
  | 10 -> `Nov
  | 11 -> `Dec
  | _ -> failwith "Invalid month int"

let cal_month_of_month (month : month) : CalendarLib.Calendar.month =
  match month with
  | `Jan -> Jan
  | `Feb -> Feb
  | `Mar -> Mar
  | `Apr -> Apr
  | `May -> May
  | `Jun -> Jun
  | `Jul -> Jul
  | `Aug -> Aug
  | `Sep -> Sep
  | `Oct -> Oct
  | `Nov -> Nov
  | `Dec -> Dec

let month_of_cal_month (month : CalendarLib.Calendar.month) : month =
  match month with
  | Jan -> `Jan
  | Feb -> `Feb
  | Mar -> `Mar
  | Apr -> `Apr
  | May -> `May
  | Jun -> `Jun
  | Jul -> `Jul
  | Aug -> `Aug
  | Sep -> `Sep
  | Oct -> `Oct
  | Nov -> `Nov
  | Dec -> `Dec

let month_compare (m1 : month) (m2 : month) : int =
  compare (int_of_month m1) (int_of_month m2)

let month_lt m1 m2 = int_of_month m1 < int_of_month m2

let month_le m1 m2 = int_of_month m1 <= int_of_month m2

let month_gt m1 m2 = int_of_month m1 > int_of_month m2

let month_ge m1 m2 = int_of_month m1 >= int_of_month m2

let weekday_compare (d1 : weekday) (d2 : weekday) : int =
  compare (int_of_weekday d1) (int_of_weekday d2)

let weekday_lt d1 d2 = int_of_weekday d1 < int_of_weekday d2

let weekday_le d1 d2 = int_of_weekday d1 <= int_of_weekday d2

let weekday_gt d1 d2 = int_of_weekday d1 > int_of_weekday d2

let weekday_ge d1 d2 = int_of_weekday d1 >= int_of_weekday d2

type time_zone =
  [ `Local
  | `UTC
  ]

let zero_tm_sec tm = Unix.{ tm with tm_sec = 0 }

let tm_of_unix_time ~(time_zone_of_tm : time_zone) (time : int64) : Unix.tm =
  time
  |> Int64.to_float
  |> fun x ->
  match time_zone_of_tm with
  | `Local -> Unix.localtime x
  | `UTC -> Unix.gmtime x

let unix_time_of_tm ~(time_zone_of_tm : time_zone) (tm : Unix.tm) : int64 =
  tm
  |> zero_tm_sec
  |> (fun x ->
      match time_zone_of_tm with
      | `Local ->
        let time, _ = Unix.mktime tm in
        time
      | `UTC ->
        x
        |> CalendarLib.Calendar.from_unixtm
        |> CalendarLib.Calendar.from_gmt
        |> CalendarLib.Calendar.to_unixfloat)
  |> fun time -> time |> Int64.of_float

let normalize_tm tm =
  tm
  |> zero_tm_sec
  |> CalendarLib.Calendar.from_unixtm
  |> CalendarLib.Calendar.to_unixtm

let tm_change_time_zone ~(from_time_zone : time_zone)
    ~(to_time_zone : time_zone) (tm : Unix.tm) : Unix.tm =
  if from_time_zone = to_time_zone then tm
  else
    let time = unix_time_of_tm ~time_zone_of_tm:from_time_zone tm in
    tm_of_unix_time ~time_zone_of_tm:to_time_zone time

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

let weekday_of_month_day ~(year : int) ~(month : month) ~(mday : int) : weekday
  =
  CalendarLib.Date.day_of_week
    (CalendarLib.Date.make year (int_of_month month) mday)
  |> weekday_of_cal_weekday

let local_tm_to_utc_tm (tm : Unix.tm) : Unix.tm =
  let timestamp, _ = Unix.mktime tm in
  Unix.gmtime timestamp

module Current = struct
  let cur_unix_time () : int64 = Unix.time () |> Int64.of_float

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

module Add = struct
  let add_days_unix_time ~(days : int) (x : int64) : int64 =
    tm_of_unix_time ~time_zone_of_tm:`Local x
    |> (fun tm -> { tm with tm_mday = tm.tm_mday + days })
    |> unix_time_of_tm ~time_zone_of_tm:`Local
end

module Check = struct
  let check_unix_time (x : int64) = x >= 0L
end

module Serialize = struct
  let pack_weekday (x : weekday) : Time_t.weekday = x

  let pack_month (x : month) : Time_t.month = x
end

module Deserialize = struct
  let unpack_weekday (x : Time_t.weekday) : weekday = x

  let unpack_month (x : Time_t.month) : month = x
end

module Print = struct
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

  let tm_to_date_string (tm : Unix.tm) : string =
    Printf.sprintf "%d-%02d-%02d_%02d:%02d"
      (tm.tm_year + tm_year_offset)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

  let date_string_of_time ~(display_in_time_zone : time_zone) (time : int64) :
    string =
    let tm = tm_of_unix_time ~time_zone_of_tm:display_in_time_zone time in
    tm_to_date_string tm

  let debug_string_of_time ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      ~(display_in_time_zone : time_zone) (time : int64) : string =
    Debug_print.bprintf ~indent_level buffer "%s\n"
      (date_string_of_time ~display_in_time_zone time);
    Buffer.contents buffer

  let debug_print_time ?(indent_level = 0) ~(display_in_time_zone : time_zone)
      (time : int64) : unit =
    print_string (debug_string_of_time ~indent_level ~display_in_time_zone time)
end
