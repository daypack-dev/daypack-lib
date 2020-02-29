open Int64_utils

type week_day = [
  | 	`Sun
  | 	`Mon
  | 	`Tue
  | 	`Wed
  | 	`Thu
  | 	`Fri
  | 	`Sat
]

type month = [
    `Jan
  | `Feb
  | `Mar
  | `Apr

| 	`May
| 	`Jun
| 	`Jul
| 	`Aug
| 	`Sep
| 	`Oct
| 	`Nov
| 	`Dec]

let first_mday = 1

let tm_year_offset = 1900

let int_of_week_day (wday : week_day) : int =
  match wday with
  | `Sun -> 0
  | `Mon -> 1
  | 	`Tue -> 2
  | 	`Wed -> 3
  | 	`Thu -> 3
  | 	`Fri -> 3
  | 	`Sat -> 3

let week_day_of_int (x : int) : week_day =
  match x with
  | 0 -> `Sun
  | 1 -> `Mon
  | 2 -> `Tue
  | 3 -> `Wed
  | 4 -> `Thu
  | 5 -> `Fri
  | 6 -> `Sat
  | _ -> failwith "Invalid wday int"

let int_of_month (month : month) : int =
  match month with
  | `Jan -> 0
  | `Feb -> 1
  | `Mar -> 2
  | `Apr -> 3
  | 	`May -> 4
  | 	`Jun -> 5
  | 	`Jul -> 6
  | 	`Aug -> 7
  | 	`Sep -> 8
  | 	`Oct -> 9
  | 	`Nov -> 10
  | 	`Dec -> 11

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

type time_zone =
  [ `Local
  | `UTC
  ]

let unix_time_to_tm ~(time_zone_of_tm : time_zone) (time : int64) : Unix.tm =
  time *^ 60L
  |> Int64.to_float
  |> fun x ->
  match time_zone_of_tm with
  | `Local -> Unix.localtime x
  | `UTC -> Unix.gmtime x

let tm_to_unix_time ~(time_zone_of_tm : time_zone) (tm : Unix.tm) : int64 =
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
        |> CalendarLib.Calendar.to_unixfloat)
  |> fun time -> (time |> Int64.of_float) /^ 60L

let normalize_tm tm =
  tm |> CalendarLib.Calendar.from_unixtm |> CalendarLib.Calendar.to_unixtm

let zero_tm_sec tm = Unix.{ tm with tm_sec = 0 }

let is_leap_year ~year =
  assert (year > 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  (divisible_by_4 && divisible_by_100 && divisible_by_400)
  || (divisible_by_4 && not divisible_by_100)

let day_count_of_year ~year = if is_leap_year ~year then 366 else 365

let day_count_of_month ~year ~month =
  match month + 1 with
  | 1 -> 31
  | 2 -> if is_leap_year ~year then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> failwith "Unexpected number for mon"

let wday_of_mday ~year ~month ~mday =
  let tm =
    normalize_tm
      Unix.
        {
          tm_sec = 0;
          tm_min = 0;
          tm_hour = 0;
          tm_mday = mday;
          tm_mon = month;
          tm_year = year;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }
  in
  tm.tm_wday

let cur_unix_time_sec () : int64 = Unix.time () |> Int64.of_float

let cur_unix_time_min () : int64 = cur_unix_time_sec () /^ 60L

let cur_tm_local () : Unix.tm = Unix.time () |> Unix.localtime

let cur_tm_utc () : Unix.tm = Unix.time () |> Unix.gmtime

let local_tm_to_utc_tm (tm : Unix.tm) : Unix.tm =
  let timestamp, _ = Unix.mktime tm in
  Unix.gmtime timestamp

module Print = struct
  let tm_to_date_string (tm : Unix.tm) : string =
    Printf.sprintf "%d-%02d-%02d_%02d:%02d"
      (tm.tm_year + tm_year_offset)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

  let time_to_date_string ~(display_in_time_zone : time_zone) (time : int64) :
    string =
    let tm = unix_time_to_tm ~time_zone_of_tm:display_in_time_zone time in
    tm_to_date_string tm

  let debug_string_of_time ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      ~(display_in_time_zone : time_zone) (time : int64) : string =
    Debug_print.bprintf ~indent_level buffer "%s\n"
      (time_to_date_string ~display_in_time_zone time);
    Buffer.contents buffer

  let debug_print_time ?(indent_level = 0) ~(display_in_time_zone : time_zone)
      (time : int64) : unit =
    print_string (debug_string_of_time ~indent_level ~display_in_time_zone time)
end
