open Int64_utils

type mode = [
    `Local
  | `UTC
]

let time_to_tm (mode : mode) (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |>
  (fun x ->
     match mode with
     | `Local -> Unix.localtime x
     | `UTC -> Unix.gmtime x)

let tm_to_time (mode : mode) (tm : Unix.tm) : int64 =
  tm
  |> (fun x ->
      match mode with
      | `Local ->
        let time, _ = Unix.mktime tm in
        time
      | `UTC ->
        x
        |> CalendarLib.Calendar.from_unixtm
        |> CalendarLib.Calendar.from_gmt
        |> CalendarLib.Calendar.to_unixfloat
    )
  |> (fun time -> (time |> Int64.of_float) /^ 60L)

let normalize_tm tm =
  tm
  |> CalendarLib.Calendar.from_unixtm
  |> CalendarLib.Calendar.to_unixtm

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

let cur_time_utc_sec () : int64 = Unix.time () |> Int64.of_float

let cur_time_utc_min () : int64 = cur_time_utc_sec () /^ 60L

let cur_tm_utc () : Unix.tm =
  Unix.time ()
|> Unix.localtime

let cur_tm_local () : Unix.tm =
  Unix.time ()
|>
  Unix.gmtime

let local_tm_to_utc_tm (tm : Unix.tm) : Unix.tm =
  let timestamp, _ = Unix.mktime tm in
  Unix.gmtime timestamp

module Print = struct
  let tm_to_date_string (tm : Unix.tm) : string =
    Printf.sprintf
      "%d-%d-%d_%d:%d"
      tm.tm_year
      tm.tm_mon
      tm.tm_mday
      tm.tm_hour
      tm.tm_min

  let time_to_date_string (mode : mode) (time : int64) : string =
    let tm = time_to_tm mode time in
    tm_to_date_string tm

  let debug_string_of_time ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (mode : mode) (time : int64) : string =
    Debug_print.bprintf ~indent_level buffer "%s\n" (time_to_date_string mode time);
    Buffer.contents buffer

  let debug_print_time ?(indent_level = 0) (mode : mode) (time : int64) : unit =
    print_string (debug_string_of_time ~indent_level mode time)
end
