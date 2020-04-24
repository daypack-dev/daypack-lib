open Int64_utils

module Interpret_string = struct
  open Angstrom
  open Parser_components

  let minutes_string =
    alpha_string
    >>| fun x ->
    Misc_utils.prefix_string_match [ ("minutes", ()); ("mins", ()) ] x

  let hours_string =
    alpha_string
    >>| fun x -> Misc_utils.prefix_string_match [ ("hours", ()); ("hrs", ()) ] x

  let days_string =
    alpha_string >>| fun x -> Misc_utils.prefix_string_match [ ("days", ()) ] x

  let duration_expr : Duration_expr_ast.t t =
    choice
      [
        ( integer
          >>= fun minutes ->
          space
          *> minutes_string
          *> space
          *> return Duration_expr_ast.{ days = 0; hours = 0; minutes } );
        ( integer
          >>= fun hours ->
          space *> hours_string *> space *> integer
          >>= fun minutes ->
          space
          *> minutes_string
          *> space
          *> return Duration_expr_ast.{ days = 0; hours; minutes } );
        ( integer
          >>= fun days ->
          space *> days_string *> space *> integer
          >>= fun hours ->
          space *> hours_string *> space *> integer
          >>= fun minutes ->
          space
          *> minutes_string
          *> space
          *> return Duration_expr_ast.{ days; hours; minutes } );
      ]

  let of_string (s : string) : (int64, string) result =
    match parse_string duration_expr s with
    | Ok { days; hours; minutes } ->
      if days < 0 then Error "Day count is negative"
      else if hours < 0 then Error "Hour count is negative"
      else if minutes < 0 then Error "Minute count is negative"
      else
        let days = Int64.of_int days in
        let hours = Int64.of_int hours in
        let minutes = Int64.of_int minutes in
        Ok
          ( (days *^ Time.day_to_second_multiplier)
            +^ (hours *^ Time.hour_to_second_multiplier)
            +^ (minutes *^ Time.minute_to_second_multiplier) )
    | Error msg -> Error msg
end

module To_string = struct
  let human_readable_string_of_duration (duration : int64) : string =
    let minutes = Int64.div duration 60L in
    let hours = Int64.div minutes 60L in
    let days = Int64.div hours 24L in
    if days > 0L then
      Printf.sprintf "%Ld days %Ld hours %Ld mins" days (Int64.rem hours 24L)
        (Int64.rem minutes 60L)
    else if hours > 0L then
      Printf.sprintf "%Ld hours %Ld mins" hours (Int64.rem minutes 60L)
    else Printf.sprintf "%Ld mins" minutes
end
