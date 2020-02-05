open Int64_utils

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |> Unix.localtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L

let is_leap_year ~year =
  assert (year > 0);
  let divisible_by_4 = year mod 4 = 0 in
  let divisible_by_100 = year mod 100 = 0 in
  let divisible_by_400 = year mod 400 = 0 in
  (divisible_by_4 && divisible_by_100 && divisible_by_400)
|| (divisible_by_4 && not divisible_by_100)

let day_count_of_year ~year =
  if is_leap_year ~year then
    366
  else
    365
