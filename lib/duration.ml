
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
