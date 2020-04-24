open Int64_utils

module Interpret_string = struct
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

  let duration_expr : Duration_expr_ast.t t =
    option 0 (integer <* space <* days_string)
    >>= fun days ->
    space *> option 0 (integer <* space <* hours_string)
    >>= fun hours ->
    space *> option 0 (integer <* space <* minutes_string)
    >>= fun minutes ->
    space *> option 0 (integer <* space <* seconds_string)
    >>= fun seconds ->
    return Duration_expr_ast.{ days; hours; minutes; seconds }

  let of_string (s : string) : (int64, string) result =
    match parse_string (duration_expr <* end_of_input) s with
    | Ok { days; hours; minutes; seconds } ->
      if days < 0 then Error "Day count is negative"
      else if hours < 0 then Error "Hour count is negative"
      else if minutes < 0 then Error "Minute count is negative"
      else if seconds < 0 then Error "Second count is negative"
      else
        let days = Int64.of_int days in
        let hours = Int64.of_int hours in
        let minutes = Int64.of_int minutes in
        let seconds = Int64.of_int seconds in
        Ok
          ( (days *^ Time.day_to_second_multiplier)
            +^ (hours *^ Time.hour_to_second_multiplier)
            +^ (minutes *^ Time.minute_to_second_multiplier)
            +^ seconds )
    | Error msg -> Error msg
end

module To_string = struct
  let human_readable_string_of_duration (duration : int64) : string =
    let seconds = Int64.rem duration 60L in
    let minutes = Int64.div duration 60L in
    let hours = Int64.div minutes 60L in
    let days = Int64.div hours 24L in
    if days > 0L then
      Printf.sprintf "%Ld days %Ld hours %Ld mins %Ld secs" days
        (Int64.rem hours 24L) (Int64.rem minutes 60L) seconds
    else if hours > 0L then
      Printf.sprintf "%Ld hours %Ld mins %Ld secs" hours (Int64.rem minutes 60L)
        seconds
    else if minutes > 0L then Printf.sprintf "%Ld mins %Ld secs" minutes seconds
    else Printf.sprintf "%Ld secs" seconds
end
