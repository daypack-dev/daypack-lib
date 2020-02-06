open Int64_utils

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |> Unix.localtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L

let normalize_tm tm =
  let _, tm = Unix.mktime tm in
  tm

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
