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

let jump_to_next_match_tm (t : t) (tm : Unix.tm) : Unix.tm =
  let tm_sec = 0 in
  let tm_min = match t.min with
    | Some x -> x
    | None -> succ tm.tm_min
  in
  let tm_hour = match t.hour with
    | Some x -> x
    | None -> succ tm.tm_hour
  in
  let tm_mday = match t.day with
    | Some x -> (match x with
        | Weekday x ->
          if tm.tm_wday < x then
            tm.tm_mday + (tm.tm_wday - x)
          else
            tm.tm_mday + 7 + (x - tm.tm_wday)
        | Month_day x ->
          x
      )
    | None -> succ tm.tm_mday
  in
  let tm_mon = match t.mon with
    | Some x -> x
    | None -> succ tm.tm_mon in
  let tm_year = match t.year with
    | Some x -> x
    | None -> succ tm.tm_year
  in
  {
    tm with
    tm_sec;
    tm_min;
    tm_hour;
    tm_mday;
    tm_mon;
    tm_year;
  }

let jump_to_next_match_int64 (t : t) (time : int64) : int64 =
  time_to_tm time
  |> jump_to_next_match_tm t
  |> tm_to_time

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
