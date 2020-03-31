module Interpret_string = struct
  let minute_to_second_multiplier = 60L

  let hour_to_second_multiplier = Int64.mul 60L minute_to_second_multiplier

  let day_to_second_multiplier = Int64.mul 24L hour_to_second_multiplier

  let minute_unit_strings = [
    "min"; "mins"; "minute"; "minutes"
  ]

  let hour_unit_strings = [
    "hr"; "hrs"; "hour"; "hours"
  ]

  let day_unit_strings = [
    "day"; "days"
  ]

  let seconds_of_minutes_string (s : string) : (int64, unit) result =
    try
      Scanf.sscanf s "%Ld-%s" (fun min unit ->
          if List.mem unit minute_unit_strings then (
            Ok (Int64.mul minute_to_second_multiplier min)
          )
          else
            Error ()
        )
    with
    _ -> Error ()

  let seconds_of_hours_string (s : string) : (int64, unit) result =
    try
      Scanf.sscanf s "%Ld-%s" (fun min unit ->
          if List.mem unit hour_unit_strings then
            Ok (Int64.mul hour_to_second_multiplier min)
          else
            Error ()
        )
    with
    _ -> Error ()

  let seconds_of_day_string (s : string) : (int64, unit) result =
    try
      Scanf.sscanf s "%Ld-%s" (fun min unit ->
          if List.mem unit day_unit_strings then
            Ok (Int64.mul day_to_second_multiplier min)
          else
            Error ()
        )
    with
    _ -> Error ()

  let seconds_of_hour_minute_string (s : string) : (int64, unit) result =
    try
      Scanf.sscanf s "%[^,],%[^,]" (fun hour min ->
          match seconds_of_hours_string hour with
          | Error () -> Error ()
          | Ok hour_seconds ->
            match seconds_of_minutes_string min with
            | Error () -> Error ()
            | Ok min_seconds ->
              Ok (Int64.add hour_seconds min_seconds)
        )
    with
      _ -> Error ()

  let seconds_of_day_hour_minute_string (s : string) : (int64, unit) result =
    try
      Scanf.sscanf s "%[^,],%[^,],%[^,]" (fun day hour min ->
          match seconds_of_day_string day with
          | Error () -> Error ()
          | Ok day_seconds ->
            match seconds_of_hours_string hour with
            | Error () -> Error ()
            | Ok hour_seconds ->
            match seconds_of_minutes_string min with
            | Error () -> Error ()
            | Ok min_seconds ->
              Ok (Int64.add (Int64.add day_seconds hour_seconds) min_seconds)
        )
    with
      _ -> Error ()

  let of_string (s : string) : (int64, string) result =
    match seconds_of_day_hour_minute_string s with
    | Ok x -> Ok x
    | Error () ->
      match seconds_of_hour_minute_string s with
      | Ok x -> Ok x
      | Error () ->
        match seconds_of_minutes_string s with
        | Ok x -> Ok x
        | Error () -> Error "Failed to interpret string as duration"
end

module Print = struct
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
