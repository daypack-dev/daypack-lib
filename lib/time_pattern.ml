open Int64_utils

type day =
  | Weekday of int
  | Month_day of int

type t = {
  year : int option;
  mon : int option;
  day : day option;
  hour : int option;
  min : int option;
}

type normalize_dir = [
  | `Start
  | `End
]

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |> Unix.gmtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L

let normalize_pattern (dir : normalize_dir) t =
  let map_none upper x default_val =
    match x with
    | Some x -> Some x
    | None -> ( match upper with Some _ -> Some default_val | None -> None )
  in
  t
  |> (fun t -> { t with mon = map_none t.year t.mon (match dir with `Start -> 0 | `End -> 11) })
  |> (fun t -> match dir with `Start -> { t with day = map_none t.mon t.day (Month_day 0) }
                            | `End -> { t with mon = Option.map pred t.mon; day = map_none t.mon t.day (Month_day (-1)) }
    )
  |> (fun t -> { t with hour = map_none t.day t.hour (match dir with `Start -> 0 | `End -> 23) })
  |> fun t -> { t with min = map_none t.hour t.min (match dir with `Start  -> 0 | `End -> 59) }

let normalize_tm tm =
  let _, tm = Unix.mktime tm in
  tm

let next_match_tm ~normalize_dir (t : t) (tm : Unix.tm) : Unix.tm option =
  let bump cur pat ub_exc =
    match pat with
    | Some x -> if cur < x then (false, x) else (true, x)
    | None ->
      let next = succ cur in
      if next < ub_exc then (false, next) else (true, 0)
  in
  let next_is_in_past =
    match t.year with Some x -> (x - 1900) < tm.tm_year | None -> false
  in
  if next_is_in_past then None
  else
    let t = normalize_pattern normalize_dir t in
    let tm_sec = 0 in
    let bump_hour, tm_min = bump tm.tm_min t.min 60 in
    let bump_mday, tm_hour =
      if bump_hour then bump tm.tm_hour t.hour 24 else (false, tm.tm_hour)
    in
    let definitely_bump_mon, tm_mday =
      if bump_mday then
        match t.day with
        | Some x -> (
            match x with
            | Weekday x ->
              ( false,
                if tm.tm_wday < x then tm.tm_mday + (tm.tm_wday - x)
                else tm.tm_mday + 7 + (x - tm.tm_wday) )
            | Month_day x -> if tm.tm_mday < x then (false, x) else (true, x) )
        | None -> (false, succ tm.tm_mday)
      else (false, tm.tm_mday)
    in
    (* normalize calculated item thus far *)
    let tm = normalize_tm { tm with tm_sec; tm_min; tm_hour; tm_mday } in
    (* if certain to bump month, then do so,
       otherwise check if tm_mon in normalized tm already matches pattern *)
    let bump_year, tm_mon =
      if definitely_bump_mon then bump tm.tm_mon t.mon 12
      else
        match t.mon with
        | Some x -> if tm.tm_mon < x then (false, x) else (true, x)
        | None -> (false, tm.tm_mon)
    in
    let tm_year = if bump_year then succ tm.tm_year else tm.tm_year in
    { tm with tm_mon; tm_year }
    |> normalize_tm
    |> Option.some

let next_match_int64 ~normalize_dir (t : t) (time : int64) : int64 option =
  time_to_tm time |> next_match_tm ~normalize_dir t |> Option.map tm_to_time

(* let matching_time_slots  (t : t) (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
 *   (\* assume 1 time unit in time slot = 1 minute *\)
 *   let rec aux t time_slots =
 *     match time_slots () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons ((start, end_exc), rest) ->
 * 
 *   in
 *   aux t time_slots *)
