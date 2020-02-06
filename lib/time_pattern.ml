open Int64_utils

type day =
  [ `Weekday of int
  | `Month_day of int
  ]

type t = {
  year : int option;
  mon : int option;
  day : day option;
  hour : int option;
  min : int option;
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
  match t.min with
  | None -> Seq.map (fun tm_min -> { acc with tm_min }) OSeq.(start --^ 60)
  | Some pat_min ->
    if pat_min < start then Seq.empty
    else Seq.return { acc with tm_min = pat_min }

let matching_hours (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
    then start.tm_hour
    else 0
  in
  match t.hour with
  | None -> Seq.map (fun tm_hour -> { acc with tm_hour }) OSeq.(start --^ 24)
  | Some pat_hour ->
    if pat_hour < start then Seq.empty
    else Seq.return { acc with tm_hour = pat_hour }

let matching_days (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let year = acc.tm_year + tm_year_offset in
  let month = acc.tm_mon in
  let day_count = Time.day_count_of_month ~year ~month in
  let start =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      start.tm_mday
    else 0
  in
  match t.day with
  | None ->
    Seq.map (fun tm_mday -> { acc with tm_mday }) OSeq.(start --^ day_count)
  | Some (`Month_day pat_mday) ->
    if pat_mday < start then Seq.empty
    else Seq.return { acc with tm_mday = pat_mday }
  | Some (`Weekday pat_wday) ->
    Seq.filter_map
      (fun mday ->
         let wday = Time.wday_of_mday ~year ~month ~mday in
         if wday = pat_wday then Some { acc with tm_mday = mday } else None)
      OSeq.(start --^ day_count)

let matching_months (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start = if acc.tm_year = start.tm_year then start.tm_mon else 0 in
  match t.mon with
  | None -> Seq.map (fun tm_mon -> { acc with tm_mon }) OSeq.(start --^ 12)
  | Some pat_mon ->
    if pat_mon < start then Seq.empty
    else Seq.return { acc with tm_mon = pat_mon }

let matching_years ~search_years_ahead (t : t) (start : Unix.tm) (acc : Unix.tm)
  : Unix.tm Seq.t =
  match t.year with
  | None ->
    Seq.map
      (fun tm_year -> { acc with tm_year })
      OSeq.(start.tm_year --^ (start.tm_year + search_years_ahead))
  | Some pat_year ->
    if pat_year < start.tm_year then Seq.empty
    else Seq.return { acc with tm_year = pat_year - tm_year_offset }

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

let matching_time_slots (t : t) (time_slots : Time_slot.t list) :
  Time_slot.t Seq.t =
  match Time_slot.min_start_and_max_end_exc_list time_slots with
  | None -> Seq.empty
  | Some (start, end_exc) ->
    let start_tm = Time.time_to_tm start in
    let end_exc_tm = Time.time_to_tm end_exc in
    let search_years_ahead = end_exc_tm.tm_year - start_tm.tm_year + 1 in
    matching_tm_seq ~search_years_ahead t start_tm
    |> Seq.map Time.tm_to_time
    |> Seq.map (fun time -> (time, time +^ 1L))
    |> Time_slot.intersect (List.to_seq time_slots)
    |> Time_slot.normalize ~skip_filter:false ~skip_sort:false

(* let next_match_int64 ?(time_slots : Time_slot.t list = []) ~normalize_dir
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
 *             | `Start -> Time_slot.slice ~start:time s
 *             | `End -> Time_slot.slice ~end_exc:time s
 *           in
 *           match s () with
 *           | Seq.Nil -> None
 *           | Seq.Cons ((start, end_exc), _) -> (
 *               match normalize_dir with
 *               | `Start -> Some start
 *               | `End -> Some end_exc ) ) ) *)

module Print = struct
  let debug_string_of_pattern ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (t : t) : string =
    let aux = Option.fold ~some:string_of_int ~none:"None" in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "year : %s\n"
      (aux t.year);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "mon : %s\n"
      (aux t.mon);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "day : %s\n"
      ( match t.day with
        | Some (`Month_day x) -> Printf.sprintf "month day %d" x
        | Some (`Weekday x) -> Printf.sprintf "weekday %d" x
        | None -> "None" );
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hour : %s\n"
      (aux t.hour);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "min : %s\n"
      (aux t.min);
    Buffer.contents buffer

  let debug_print_pattern ?(indent_level = 0) t =
    print_string (debug_string_of_pattern ~indent_level t)
end
