open Int64_utils

type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let of_seconds (x : int64) : t =
  assert (x >= 0L);
  let seconds = Int64.rem x 60L in
  let minutes = Int64.div x 60L in
  let hours = Int64.div minutes 60L in
  let days = Int64.div hours 24L in
  let hours = Int64.rem hours 24L in
  let minutes = Int64.rem minutes 60L in
  { days = Int64.to_int days;
    hours = Int64.to_int hours;
    minutes = Int64.to_int minutes;
    seconds = Int64.to_int seconds;
  }

let to_seconds (t : t) : int64 =
  let days = Int64.of_int t.days in
  let hours = Int64.of_int t.hours in
  let minutes = Int64.of_int t.minutes in
  let seconds = Int64.of_int t.seconds in
  (days *^ Time.day_to_second_multiplier)
  +^ (hours *^ Time.hour_to_second_multiplier)
  +^ (minutes *^ Time.minute_to_second_multiplier)
  +^ seconds

let normalize (t : t) : t =
  t
  |> to_seconds
  |> of_seconds

module Interpret_string = struct
  type duration = t

  open Angstrom
  open Parser_components

  let seconds_string : unit t =
    alpha_string
    >>= fun x ->
    match
      Misc_utils.prefix_string_match [ ("seconds", ()); ("secs", ()) ] x
    with
    | [] -> fail "String doesn't match keyword representing seconds"
    | _ -> return ()

  let minutes_string : unit t =
    alpha_string
    >>= fun x ->
    match
      Misc_utils.prefix_string_match [ ("minutes", ()); ("mins", ()) ] x
    with
    | [] -> fail "String doesn't match keyword representing minutes"
    | _ -> return ()

  let hours_string : unit t =
    alpha_string
    >>= fun x ->
    match Misc_utils.prefix_string_match [ ("hours", ()); ("hrs", ()) ] x with
    | [] -> fail "String doesn't match keyword representing hours"
    | _ -> return ()

  let days_string : unit t =
    alpha_string
    >>= fun x ->
    match Misc_utils.prefix_string_match [ ("days", ()) ] x with
    | [] -> fail "String doesn't match keyword representing days"
    | _ -> return ()

  let duration_expr : duration t =
    option 0 (nat_zero <* space <* days_string)
    >>= fun days ->
    space *> option 0 (nat_zero <* space <* hours_string)
    >>= fun hours ->
    space *> option 0 (nat_zero <* space <* minutes_string)
    >>= fun minutes ->
    space *> option 0 (nat_zero <* space <* seconds_string)
    >>= fun seconds ->
    return (normalize { days; hours; minutes; seconds })

  let of_string (s : string) : (duration, string) result =
    parse_string (duration_expr <* end_of_input) s
end

module To_string = struct
  let human_readable_string_of_duration ({ days; hours; minutes; seconds } : t) : string =
    if days > 0 then
      Printf.sprintf "%d days %d hours %d mins %d secs" days hours minutes seconds
    else if hours > 0 then
      Printf.sprintf "%d hours %d mins %d secs" hours minutes seconds
    else if minutes > 0 then Printf.sprintf "%d mins %d secs" minutes seconds
    else Printf.sprintf "%d secs" seconds
end
