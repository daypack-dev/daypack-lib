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

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |> Unix.gmtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L

let normalize_tm tm =
  let _, tm = Unix.mktime tm in
  tm

(* let next_match_tm (t : t) (tm : Unix.tm) : Unix.tm =
 *   let bump cur pat ub_exc =
 *     match pat with
 *     | Some x ->
 *       if cur < x then
 *         x
 *       else
 *         x + ub_exc
 *     | None ->
 *       succ cur
 *   in
 *   let tm_sec = 0 in
 *   { tm with tm_sec }
 *   |> (fun tm ->
 *       let tm_min =
 *         bump tm.tm_min t.min 60
 *       in
 *       { tm with tm_min }
 *     )
 *   |> normalize_tm
 *   |> (fun tm ->
 *       let tm_hour =
 *         bump tm.tm_hour t.hour 24
 *       in
 *       { tm with tm_hour }
 *     ) *)

let next_match_tm (t : t) (tm : Unix.tm) : Unix.tm option =
  let bump cur pat ub_exc =
    match pat with
    | Some x ->
      if cur < x then
        (false, x)
      else
        (true, x)
    | None ->
      let next = succ cur in
      if next < ub_exc then
        (false, next)
      else
        (true, 0)
  in
  let next_is_in_past =
     match t.year with
     | Some x -> x < tm.tm_year
     | None -> false
  in
  if next_is_in_past then None
  else
    let tm_sec = 0 in
    let (bump_hour, tm_min) =
      bump tm.tm_min t.min 60
    in
    let (bump_mday, tm_hour) =
      if bump_hour then
        bump tm.tm_hour t.hour 24
      else
        (false, tm.tm_hour)
    in
    let (definitely_bump_mon, tm_mday) =
      if bump_mday then (
        match t.day with
        | Some x -> (
            match x with
            | Weekday x ->
              (false,
               if tm.tm_wday < x then tm.tm_mday + (tm.tm_wday - x)
               else tm.tm_mday + 7 + (x - tm.tm_wday))
            | Month_day x ->
              if tm.tm_mday < x then
                (false, x)
              else
                (true, x)
          )
        | None -> (false, succ tm.tm_mday)
      )
      else
        (false, tm.tm_mday)
    in
    (* normalize calculated item thus far *)
    let old_tm = tm in
    let tm =
      normalize_tm { tm with tm_sec; tm_min; tm_hour; tm_mday }
    in
    (* if certain to bump month, then do so,
       otherwise check if tm_mon in normalized tm already matches pattern *)
    let (bump_year, tm_mon) =
      if definitely_bump_mon then
        bump tm.tm_mon t.mon 12
      else
        (* if old_tm.tm_mon < tm.tm_mon || old_tm.tm_year < tm.tm_year then
         *   (\* month or year already bumped in above *\)
         *   match t.mon with
         *   | Some x ->
         *     if tm.tm_mon < x then (false, x) else (true, x)
         *   | None -> (false, tm.tm_mon)
         * else
         *   match t.mon with
         *   | Some x -> *)
        match t.mon with
        | Some x ->
         if tm.tm_mon < x then (false, x) else (true, x)
        | None -> (false, tm.tm_mon)
    in
    let tm_year =
      if bump_year then
        succ tm.tm_year
      else
        tm.tm_year
    in
    { tm with tm_mon; tm_year }
    (* tm *)
    |> normalize_tm
    |> Option.some

let next_match_int64 (t : t) (time : int64) : int64 option =
  time_to_tm time |> next_match_tm t |> Option.map tm_to_time

(* let time_patches_pattern (t : t) (time : int64) : bool =
 *   let tm =   in *)

(* let matching_time_slots  (t : t) (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
 *   (\* assume 1 time unit in time slot = 1 minute *\)
 *   let rec aux t time_slots =
 *     match time_slots () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons ((start, end_exc), rest) ->
 * 
 *   in
 *   aux t time_slots *)
