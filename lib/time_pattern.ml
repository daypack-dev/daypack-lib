open Int64_utils

type day =
  | Weekday of int
  | Month_day of int

type t = {
  year : int option;
  month : int option;
  day : day option;
  hour : int option;
  min : int option;
}

let time_to_tm (time : int64) : Unix.tm =
  time *^ 60L |> Int64.to_float |>Unix.gmtime

let tm_to_time (tm : Unix.tm) : int64 =
  let time, _ = Unix.mktime tm in
  (time |> Int64.of_float) /^ 60L

(* let jump_to_next_match (t : t) (time : int64) : bool =
 *   let tm = time_to_tm time in
 *   match t.min with
 *   | Some x ->
 *     
 *   | None 
 * 
 * let time_patches_pattern (t : t) (time : int64) : bool =
 *   let tm =   in
 * 
 * let matching_time_slots  (t : t) (time_slots : Time_slot.t Seq.t) : Time_slot.t Seq.t =
 *   (\* assume 1 time unit in time slot = 1 minute *\)
 *   let rec aux t time_slots =
 *     match time_slots () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons ((start, end_exc), rest) ->
 * 
 *   in
 *   aux t time_slots *)
