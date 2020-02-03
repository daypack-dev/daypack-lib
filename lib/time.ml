open Int64_utils

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |> Unix.gmtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L
