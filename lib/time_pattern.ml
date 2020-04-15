open Int64_utils

type search_param =
  | Time_slots of {
      search_in_time_zone : Time.time_zone;
      time_slots : Time_slot_ds.t list;
    }
  | Years_ahead_start_unix_time of {
      search_in_time_zone : Time.time_zone;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_tm of {
      search_in_time_zone : Time.time_zone;
      time_zone_of_tm : Time.time_zone;
      start : Unix.tm;
      search_years_ahead : int;
    }

type t = {
  years : int list;
  months : Time.month list;
  month_days : int list;
  weekdays : Time.weekday list;
  hours : int list;
  minutes : int list;
  seconds : int list;
}

type time_range_pattern = t Range.t

type single_or_ranges =
  | Single_time_pattern of t
  | Time_range_patterns of time_range_pattern list

let empty =
  {
    years = [];
    months = [];
    weekdays = [];
    month_days = [];
    hours = [];
    minutes = [];
    seconds = [];
  }

let get_search_in_time_zone (param : search_param) : Time.time_zone =
  match param with
  | Time_slots { search_in_time_zone; _ } -> search_in_time_zone
  | Years_ahead_start_unix_time { search_in_time_zone; _ } ->
    search_in_time_zone
  | Years_ahead_start_tm { search_in_time_zone; _ } -> search_in_time_zone

let push_search_param_to_later_start ~(start : int64)
    (search_param : search_param) : search_param =
  match search_param with
  | Time_slots { search_in_time_zone; time_slots } -> (
      match Time_slot_ds.min_start_and_max_end_exc_list time_slots with
      | None -> search_param
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slot_ds.intersect (Seq.return (start, end_exc'))
          |> List.of_seq
        in
        Time_slots { search_in_time_zone; time_slots } )
  | Years_ahead_start_unix_time
      { search_in_time_zone; start = start'; search_years_ahead } ->
    let start = max start' start in
    Years_ahead_start_unix_time
      { search_in_time_zone; start; search_years_ahead }
  | Years_ahead_start_tm
      {
        search_in_time_zone;
        time_zone_of_tm;
        start = start';
        search_years_ahead;
      } ->
    let start = max (Time.unix_time_of_tm ~time_zone_of_tm start') start in
    Years_ahead_start_tm
      {
        search_in_time_zone;
        time_zone_of_tm;
        start = Time.tm_of_unix_time ~time_zone_of_tm start;
        search_years_ahead;
      }

let matching_seconds (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
      && acc.tm_hour = start.tm_hour
      && acc.tm_min = start.tm_min
    then start.tm_min
    else 0
  in
  match t.seconds with
  | [] -> Seq.map (fun tm_sec -> { acc with tm_sec }) OSeq.(start --^ 60)
  | pat_sec_list ->
    pat_sec_list
    |> List.to_seq
    |> Seq.filter (fun pat_sec -> start <= pat_sec)
    |> Seq.map (fun pat_sec -> { acc with tm_min = pat_sec })

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

let matching_weekdays (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t
  =
  let year = acc.tm_year + Time.tm_year_offset in
  let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
  let day_count = Time.day_count_of_month ~year ~month in
  let start =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      start.tm_mday
    else 1
  in
  match t.weekdays with
  | [] ->
    Seq.map (fun tm_mday -> { acc with tm_mday }) OSeq.(start -- day_count)
  | l ->
    Seq.filter_map
      (fun mday ->
         let wday = Time.weekday_of_month_day ~year ~month ~mday in
         if List.mem wday l then Some { acc with tm_mday = mday } else None)
      OSeq.(start --^ day_count)

let matching_month_days (t : t) (start : Unix.tm) (acc : Unix.tm) :
  Unix.tm Seq.t =
  let year = acc.tm_year + Time.tm_year_offset in
  let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
  let day_count = Time.day_count_of_month ~year ~month in
  let start =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      start.tm_mday
    else 1
  in
  match t.month_days with
  | [] ->
    Seq.map (fun tm_mday -> { acc with tm_mday }) OSeq.(start -- day_count)
  | l ->
    l
    |> List.to_seq
    |> Seq.filter (fun pat_mday -> start <= pat_mday)
    |> Seq.map (fun pat_mday -> { acc with tm_mday = pat_mday })

let matching_months (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
  let start =
    if acc.tm_year = start.tm_year then
      Time.month_of_tm_int start.tm_mon |> Result.get_ok
    else `Jan
  in
  match t.months with
  | [] ->
    Seq.map
      (fun tm_mon -> { acc with tm_mon })
      OSeq.(Time.tm_int_of_month start --^ 12)
  | pat_mon_list ->
    pat_mon_list
    |> List.to_seq
    |> Seq.filter (fun pat_mon -> Time.month_le start pat_mon)
    |> Seq.map (fun pat_mon ->
        { acc with tm_mon = Time.tm_int_of_month pat_mon })

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
        { acc with tm_year = pat_year - Time.tm_year_offset })

let start_tm_and_search_years_ahead_of_search_param
    (search_param : search_param) : (Unix.tm * int) option =
  match search_param with
  | Time_slots { search_in_time_zone; time_slots } -> (
      match Time_slot_ds.min_start_and_max_end_exc_list time_slots with
      | None -> None
      | Some (start, end_exc) ->
        let start_tm =
          Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone start
        in
        let end_exc_tm =
          Time.tm_of_unix_time ~time_zone_of_tm:`Local end_exc
        in
        let search_years_ahead = end_exc_tm.tm_year - start_tm.tm_year + 1 in
        Some (start_tm, search_years_ahead) )
  | Years_ahead_start_unix_time
      { search_in_time_zone; start; search_years_ahead } ->
    let start_tm =
      Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone start
    in
    Some (start_tm, search_years_ahead)
  | Years_ahead_start_tm
      { search_in_time_zone; time_zone_of_tm; start; search_years_ahead } ->
    let start =
      start
      |> Time.unix_time_of_tm ~time_zone_of_tm
      |> Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone
    in
    Some (start, search_years_ahead)

module Single_pattern = struct
  let matching_tm_seq (search_param : search_param) (t : t) : Unix.tm Seq.t =
    match start_tm_and_search_years_ahead_of_search_param search_param with
    | None -> Seq.empty
    | Some (start, search_years_ahead) ->
      (* let start = Time.zero_tm_sec start in *)
      matching_years ~search_years_ahead t start start
      |> Seq.flat_map (fun acc -> matching_months t start acc)
      |> Seq.flat_map (fun acc -> matching_month_days t start acc)
      |> Seq.flat_map (fun acc -> matching_weekdays t start acc)
      |> Seq.flat_map (fun acc -> matching_hours t start acc)
      |> Seq.flat_map (fun acc -> matching_minutes t start acc)
      |> Seq.flat_map (fun acc -> matching_seconds t start acc)

  let matching_time_slots (search_param : search_param) (t : t) :
    Time_slot_ds.t Seq.t =
    let time_slots =
      match search_param with
      | Time_slots { time_slots; _ } -> Some time_slots
      | _ -> None
    in
    let search_in_time_zone = get_search_in_time_zone search_param in
    matching_tm_seq search_param t
    |> Seq.map (Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone)
    |> Seq.map (fun time -> (time, time +^ 1L))
    |> (fun l ->
        match time_slots with
        | None -> l
        | Some time_slots -> Time_slot_ds.intersect (List.to_seq time_slots) l)
    |> Time_slot_ds.normalize ~skip_filter:false ~skip_sort:true

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : t list) : Time_slot_ds.t list Seq.t =
    l
    |> List.map (matching_time_slots search_param)
    |> Time_slot_ds.collect_round_robin_non_decreasing
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : t list) : Time_slot_ds.t Seq.t =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Seq.flat_map List.to_seq

  let next_match_tm (search_param : search_param) (t : t) : Unix.tm option =
    match (matching_tm_seq search_param t) () with
    | Seq.Nil -> None
    | Seq.Cons (x, _) -> Some x

  let next_match_unix_time (search_param : search_param) (t : t) : int64 option
    =
    let search_in_time_zone = get_search_in_time_zone search_param in
    next_match_tm search_param t
    |> Option.map (Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone)

  let next_match_time_slot (search_param : search_param) (t : t) :
    (int64 * int64) option =
    match matching_time_slots search_param t () with
    | Seq.Nil -> None
    | Seq.Cons (x, _) -> Some x
end

module Range_pattern = struct
  let matching_time_slots (search_param : search_param)
      (range : time_range_pattern) : Time_slot_ds.t Seq.t =
    let search_and_get_start (search_param : search_param) (t : t)
        ((start, _) : Time_slot_ds.t) : Time_slot_ds.t option =
      let search_param = push_search_param_to_later_start ~start search_param in
      match Single_pattern.next_match_time_slot search_param t with
      | None -> None
      | Some (start', _) -> Some (start, start')
    in
    let search_and_get_end_exc (search_param : search_param) (t : t)
        ((start, _) : Time_slot_ds.t) : Time_slot_ds.t option =
      let search_param = push_search_param_to_later_start ~start search_param in
      match Single_pattern.next_match_time_slot search_param t with
      | None -> None
      | Some (_, end_exc') -> Some (start, end_exc')
    in
    let start_seq =
      match range with `Range_inc (t1, _) | `Range_exc (t1, _) -> t1
    in
    Single_pattern.matching_time_slots search_param start_seq
    |>
    match range with
    | `Range_inc (_, t2) ->
      Seq.filter_map (search_and_get_end_exc search_param t2)
    | `Range_exc (_, t2) ->
      Seq.filter_map (search_and_get_start search_param t2)

  let next_match_time_slot (search_param : search_param)
      (range : time_range_pattern) : (int64 * int64) option =
    match matching_time_slots search_param range () with
    | Seq.Nil -> None
    | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

  let matching_time_slots_multi (search_param : search_param)
      (l : time_range_pattern list) : Time_slot_ds.t Seq.t =
    l
    |> List.to_seq
    |> Seq.map (matching_time_slots search_param)
    |> Time_slot_ds.merge_multi_seq

  let next_match_time_slot_multi (search_param : search_param)
      (l : time_range_pattern list) : (int64 * int64) option =
    match matching_time_slots_multi search_param l () with
    | Seq.Nil -> None
    | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : time_range_pattern list) :
    Time_slot_ds.t list Seq.t =
    l
    |> List.map (matching_time_slots search_param)
    |> Time_slot_ds.collect_round_robin_non_decreasing
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : time_range_pattern list) :
    Time_slot_ds.t Seq.t =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Seq.flat_map List.to_seq
end

module Single_or_ranges = struct
  let matching_time_slots (search_param : search_param) (x : single_or_ranges) :
    Time_slot_ds.t Seq.t =
    match x with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots search_param pat
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_multi search_param l

  let next_match_time_slot (search_param : search_param) (x : single_or_ranges)
    : Time_slot_ds.t option =
    match matching_time_slots search_param x () with
    | Seq.Nil -> None
    | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (t : single_or_ranges) :
    Time_slot_ds.t list Seq.t =
    match t with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots_round_robin_non_decreasing
        search_param [ pat ]
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_round_robin_non_decreasing
        search_param l

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (t : single_or_ranges) :
    Time_slot_ds.t Seq.t =
    matching_time_slots_round_robin_non_decreasing search_param t
    |> Seq.flat_map List.to_seq
end

module Serialize = struct
  let pack_pattern (t : t) : Time_pattern_t.t =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
    }
end

module Deserialize = struct
  let unpack_pattern (t : Time_pattern_t.t) : t =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
    }
end

module Equal = struct
  let equal (pat1 : t) (pat2 : t) : bool =
    List.sort compare pat1.years = List.sort compare pat2.years
    && List.sort compare pat1.months = List.sort compare pat2.months
    && List.sort compare pat1.weekdays = List.sort compare pat2.weekdays
    && List.sort compare pat1.month_days = List.sort compare pat2.month_days
    && List.sort compare pat1.hours = List.sort compare pat2.hours
    && List.sort compare pat1.minutes = List.sort compare pat2.minutes
end

module To_string = struct
  let debug_string_of_weekdays (days : Time.weekday list) : string =
    let aux l =
      String.concat "," (List.map Time.To_string.string_of_weekday l)
    in
    Printf.sprintf "weekday [%s]" (aux days)

  let debug_string_of_month_days (days : int list) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    Printf.sprintf "month day [%s]" (aux days)

  let debug_string_of_time_pattern ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : t) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    let aux_months l =
      String.concat "," (List.map Time.To_string.string_of_month l)
    in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "years : [%s]\n"
      (aux t.years);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "months : [%s]\n" (aux_months t.months);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "month days : %s\n"
      (debug_string_of_month_days t.month_days);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "weekdays : %s\n"
      (debug_string_of_weekdays t.weekdays);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hours : [%s]\n"
      (aux t.hours);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "minutes : [%s]\n" (aux t.minutes);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "seconds : [%s]\n" (aux t.seconds);
    Buffer.contents buffer

  let debug_string_of_time_range_pattern ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : time_range_pattern) : string =
    ( match t with
      | `Range_inc (t1, t2) ->
        Debug_print.bprintf ~indent_level buffer
          "time range pattern inclusive:\n";
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t1
        |> ignore;
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t2
        |> ignore
      | `Range_exc (t1, t2) ->
        Debug_print.bprintf ~indent_level buffer
          "time range pattern exclusive:\n";
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t1
        |> ignore;
        debug_string_of_time_pattern ~indent_level:(indent_level + 1) ~buffer t2
        |> ignore );
    Buffer.contents buffer

  let debug_string_of_single_or_ranges ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : single_or_ranges) : string =
    match t with
    | Single_time_pattern t ->
      debug_string_of_time_pattern ~indent_level ~buffer t
    | Time_range_patterns l ->
      List.iter
        (fun t ->
           debug_string_of_time_range_pattern ~indent_level ~buffer t |> ignore)
        l;
      Buffer.contents buffer
end

module Print = struct
  let debug_print_time_pattern ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_time_pattern ~indent_level t)

  let debug_print_time_range_pattern ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_time_range_pattern ~indent_level t)

  let debug_print_single_or_ranges ?(indent_level = 0) t =
    print_string (To_string.debug_string_of_single_or_ranges ~indent_level t)
end
