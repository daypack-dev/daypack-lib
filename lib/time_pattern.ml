open Int64_utils

type days =
  [ `Weekdays of int list
  | `Month_days of int list
  ]

type t = {
  years : int list;
  months : int list;
  days : days;
  hours : int list;
  minutes : int list;
}

(* type normalize_dir =
 *   [ `Start
 *   | `End
 *   ] *)

let first_mday = 1

let tm_year_offset = 1900

(* let normalize_pattern (dir : normalize_dir) t =
 *   let map_none upper x default_val =
 *     match x with
 *     | Some x -> Some x
 *     | None -> ( match upper with Some _ -> Some default_val | None -> None )
 *   in
 *   t
 *   |> (fun t ->
 *       {
 *         t with
 *         mon = map_none t.year t.mon (match dir with `Start -> 0 | `End -> 11);
 *       })
 *   |> (fun t ->
 *       match dir with
 *       | `Start -> { t with day = map_none t.mon t.day (`Month_day first_mday) }
 *       | `End ->
 *         {
 *           t with
 *           mon = Option.map succ t.mon;
 *           day = map_none t.mon t.day (`Month_day 0);
 *         })
 *   |> (fun t ->
 *       {
 *         t with
 *         hour = map_none t.day t.hour (match dir with `Start -> 0 | `End -> 23);
 *       })
 *   |> fun t ->
 *   {
 *     t with
 *     min = map_none t.hour t.min (match dir with `Start -> 0 | `End -> 59);
 *   } *)

let matching_minutes (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
      && acc.tm_hour = start.tm_hour
    then start.tm_min
    else 0
  in
  match t.minutes with
  | [] -> Seq.map (fun tm_min -> { acc with tm_min }) OSeq.(start --^ 60)
  | pat_min_list ->
    pat_min_list
    |> List.to_seq
    |> Seq.filter (fun pat_min -> start <= pat_min)
    |> Seq.map (fun pat_min -> { acc with tm_min = pat_min })

let matching_hours (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
    then start.tm_hour
    else 0
  in
  match t.hours with
  | [] -> Seq.map (fun tm_hour -> { acc with tm_hour }) OSeq.(start --^ 24)
  | pat_hour_list ->
    pat_hour_list
    |> List.to_seq
    |> Seq.filter (fun pat_hour -> start <= pat_hour)
    |> Seq.map (fun pat_hour -> { acc with tm_hour = pat_hour })

let matching_days (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let year = acc.tm_year + tm_year_offset in
  let month = acc.tm_mon in
  let day_count = Time.day_count_of_month ~year ~month in
  let start =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      start.tm_mday
    else 0
  in
  match t.days with
  | `Month_days [] | `Weekdays [] ->
    Seq.map (fun tm_mday -> { acc with tm_mday }) OSeq.(start --^ day_count)
  | `Month_days pat_mday_list ->
    pat_mday_list
    |> List.to_seq
    |> Seq.filter (fun pat_mday -> start <= pat_mday)
    |> Seq.map (fun pat_mday -> { acc with tm_mday = pat_mday })
  | `Weekdays pat_wday_list ->
    Seq.filter_map
      (fun mday ->
         let wday = Time.wday_of_mday ~year ~month ~mday in
         if List.mem wday pat_wday_list then Some { acc with tm_mday = mday }
         else None)
      OSeq.(start --^ day_count)

let matching_months (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start = if acc.tm_year = start.tm_year then start.tm_mon else 0 in
  match t.months with
  | [] -> Seq.map (fun tm_mon -> { acc with tm_mon }) OSeq.(start --^ 12)
  | pat_mon_list ->
    pat_mon_list
    |> List.to_seq
    |> Seq.filter (fun pat_mon -> start <= pat_mon)
    |> Seq.map (fun pat_mon -> { acc with tm_mon = pat_mon })

let matching_years ~search_years_ahead (t : t) (start : Unix.tm) (acc : Unix.tm)
  : Unix.tm Seq.t =
  match t.years with
  | [] ->
    Seq.map
      (fun tm_year -> { acc with tm_year })
      OSeq.(start.tm_year --^ (start.tm_year + search_years_ahead))
  | pat_year_list ->
    pat_year_list
    |> List.to_seq
    |> Seq.filter (fun pat_year -> start.tm_year <= pat_year)
    |> Seq.map (fun pat_year ->
        { acc with tm_year = pat_year - tm_year_offset })

let matching_tm_seq ~search_years_ahead (t : t) (start : Unix.tm) :
  Unix.tm Seq.t =
  matching_years ~search_years_ahead t start start
  |> Seq.flat_map (fun acc -> matching_months t start acc)
  |> Seq.flat_map (fun acc -> matching_days t start acc)
  |> Seq.flat_map (fun acc -> matching_hours t start acc)
  |> Seq.flat_map (fun acc -> matching_minutes t start acc)

(* let matching_time_seq ~search_years_ahead (t : t) (start : int64) : int64 Seq.t =
 *   let start_tm = Time.time_to_tm start in
 *   matching_tm_seq ~search_years_ahead *)

(* let next_match_tm ~(normalize_dir : normalize_dir) ~search_years_ahead (t : t)
 *     (tm : Unix.tm) : Unix.tm option =
 *   let s = matching_tm_seq ~search_years_ahead t tm in
 *   match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x *)

let matching_time_slots (t : t) (time_slots : Time_slot_ds.t list) :
  Time_slot_ds.t Seq.t =
  match Time_slot_ds.min_start_and_max_end_exc_list time_slots with
  | None -> Seq.empty
  | Some (start, end_exc) ->
    let start_tm = Time.time_to_tm `Local start in
    let end_exc_tm = Time.time_to_tm `Local end_exc in
    let search_years_ahead = end_exc_tm.tm_year - start_tm.tm_year + 1 in
    matching_tm_seq ~search_years_ahead t start_tm
    |> Seq.map (Time.tm_to_time `UTC)
    |> Seq.map (fun time -> (time, time +^ 1L))
    |> Time_slot_ds.intersect (List.to_seq time_slots)
    |> Time_slot_ds.normalize ~skip_filter:false ~skip_sort:false

(* let next_match_int64 ?(time_slots : Time_slot_ds.t list = []) ~normalize_dir
 *     ~search_years_ahead (t : t) (time : int64) : int64 option =
 *   Time.time_to_tm time
 *   |> next_match_tm ~normalize_dir ~search_years_ahead t
 *   |> Option.map Time.tm_to_time
 *   |> fun time ->
 *   match time with
 *   | None -> None
 *   | Some time -> (
 *       match time_slots with
 *       | [] -> Some time
 *       | l -> (
 *           let s =
 *             l
 *             |> List.to_seq
 *             |> fun s ->
 *             match normalize_dir with
 *             | `Start -> Time_slot_ds.slice ~start:time s
 *             | `End -> Time_slot_ds.slice ~end_exc:time s
 *           in
 *           match s () with
 *           | Seq.Nil -> None
 *           | Seq.Cons ((start, end_exc), _) -> (
 *               match normalize_dir with
 *               | `Start -> Some start
 *               | `End -> Some end_exc ) ) ) *)

module Serialize = struct
  let pack_days (x : days) : Time_pattern_t.days = x

  let pack_pattern (t : t) : Time_pattern_t.t =
    {
      years = t.years;
      months = t.months;
      days = pack_days t.days;
      hours = t.hours;
      minutes = t.minutes;
    }
end

module Deserialize = struct
  let unpack_days (x : Time_pattern_t.days) : days = x

  let pack_pattern (t : Time_pattern_t.t) : t =
    {
      years = t.years;
      months = t.months;
      days = unpack_days t.days;
      hours = t.hours;
      minutes = t.minutes;
    }
end

module Print = struct
  let debug_string_of_pattern ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (t : t) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "year : [%s]\n"
      (aux t.years);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "mon : [%s]\n"
      (aux t.months);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "day : %s\n"
      ( match t.days with
        | `Month_days xs -> Printf.sprintf "month day [%s]" (aux xs)
        | `Weekdays xs -> Printf.sprintf "weekday [%s]" (aux xs) );
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hour : [%s]\n"
      (aux t.hours);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "min : [%s]\n"
      (aux t.minutes);
    Buffer.contents buffer

  let debug_print_pattern ?(indent_level = 0) t =
    print_string (debug_string_of_pattern ~indent_level t)
end
