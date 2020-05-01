type search_param =
  | Time_slots of {
      search_in_time_zone : Time.time_zone;
      time_slots : Time_slot.t list;
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
  unix_times : int64 list;
}

type time_range_pattern = t Range.range

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
    unix_times = [];
  }

let of_unix_time ~(time_zone_of_time_pattern : Time.time_zone) (x : int64) : t =
  let tm = Time.tm_of_unix_time ~time_zone_of_tm:time_zone_of_time_pattern x in
  {
    years = [ tm.tm_year ];
    months = [ Time.month_of_tm_int tm.tm_mon |> Result.get_ok ];
    weekdays = [];
    month_days = [ tm.tm_mday ];
    hours = [ tm.tm_hour ];
    minutes = [ tm.tm_min ];
    seconds = [ tm.tm_sec ];
    unix_times = [];
  }

let search_in_time_zone_of_search_param (param : search_param) : Time.time_zone
  =
  match param with
  | Time_slots { search_in_time_zone; _ } -> search_in_time_zone
  | Years_ahead_start_unix_time { search_in_time_zone; _ } ->
    search_in_time_zone
  | Years_ahead_start_tm { search_in_time_zone; _ } -> search_in_time_zone

let push_search_param_to_later_start ~(start : int64)
    (search_param : search_param) : search_param =
  match search_param with
  | Time_slots { search_in_time_zone; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> search_param
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slots.intersect (Seq.return (start, end_exc'))
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

module Matching_seconds = struct
  let get_start ~(start : Unix.tm) ~(acc : Unix.tm) : int =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
      && acc.tm_hour = start.tm_hour
      && acc.tm_min = start.tm_min
    then start.tm_min
    else 0

  let matching_seconds (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t
    =
    let start_sec = get_start ~start ~acc in
    match t.seconds with
    | [] -> Seq.map (fun tm_sec -> { acc with tm_sec }) OSeq.(start_sec --^ 60)
    | pat_sec_list ->
      pat_sec_list
      |> List.to_seq
      |> Seq.filter (fun pat_sec -> start_sec <= pat_sec)
      |> Seq.map (fun pat_sec -> { acc with tm_min = pat_sec })

  let matching_second_ranges (t : t) (start : Unix.tm) (acc : Unix.tm) :
    Unix.tm Range.range Seq.t =
    let start_sec = get_start ~start ~acc in
    match t.seconds with
    | [] ->
      Seq.return
        (`Range_exc
           ({ acc with tm_sec = start_sec }, { acc with tm_sec = 60 }))
    | l ->
      List.sort_uniq compare l
      |> Ranges_small.Of_list.range_seq_of_list
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map
        (Range.map
           ~f_inc:(fun (x, y) ->
               ({ acc with tm_sec = x }, { acc with tm_sec = y }))
           ~f_exc:(fun (x, y) ->
               ({ acc with tm_sec = x }, { acc with tm_sec = y })))
end

module Matching_minutes = struct
  let get_start_min_sec ~(start : Unix.tm) ~(acc : Unix.tm) : int * int =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
      && acc.tm_hour = start.tm_hour
    then (start.tm_min, start.tm_sec)
    else (0, 0)

  let matching_minutes (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t
    =
    let start_min, _start_sec = get_start_min_sec ~start ~acc in
    match t.minutes with
    | [] -> Seq.map (fun tm_min -> { acc with tm_min }) OSeq.(start_min --^ 60)
    | pat_min_list ->
      pat_min_list
      |> List.to_seq
      |> Seq.filter (fun pat_min -> start_min <= pat_min)
      |> Seq.map (fun pat_min -> { acc with tm_min = pat_min })

  let matching_minute_ranges (t : t) (start : Unix.tm) (acc : Unix.tm) :
    Unix.tm Range.range Seq.t =
    let start_min, start_sec = get_start_min_sec ~start ~acc in
    match t.minutes with
    | [] ->
      Seq.return
        (`Range_exc
           ( { acc with tm_min = start_min; tm_sec = start_sec },
             { acc with tm_min = 60; tm_sec = 0 } ))
    | l ->
      let f_inc (x, y) =
        if x = start_min then
          ( { acc with tm_min = x; tm_sec = start_sec },
            { acc with tm_min = y; tm_sec = 59 } )
        else
          ( { acc with tm_min = x; tm_sec = 0 },
            { acc with tm_min = y; tm_sec = 59 } )
      in
      let f_exc (x, y) =
        if x = start_min then
          ( { acc with tm_min = x; tm_sec = start_sec },
            { acc with tm_min = y; tm_sec = 0 } )
        else
          ( { acc with tm_min = x; tm_sec = 0 },
            { acc with tm_min = y; tm_sec = 0 } )
      in
      List.filter (fun pat_min -> start_min <= pat_min) l
      |> List.sort_uniq compare
      |> Ranges_small.Of_list.range_seq_of_list
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_hours = struct
  let get_start_hour_min_sec ~(start : Unix.tm) ~(acc : Unix.tm) :
    int * int * int =
    if
      acc.tm_year = start.tm_year
      && acc.tm_mon = start.tm_mon
      && acc.tm_mday = start.tm_mday
    then (start.tm_hour, start.tm_min, start.tm_sec)
    else (0, 0, 0)

  let matching_hours (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
    let start_hour, _start_min, _start_sec =
      get_start_hour_min_sec ~start ~acc
    in
    match t.hours with
    | [] ->
      Seq.map (fun tm_hour -> { acc with tm_hour }) OSeq.(start_hour --^ 24)
    | pat_hour_list ->
      pat_hour_list
      |> List.to_seq
      |> Seq.filter (fun pat_hour -> start_hour <= pat_hour)
      |> Seq.map (fun pat_hour -> { acc with tm_hour = pat_hour })

  let matching_hour_ranges (t : t) (start : Unix.tm) (acc : Unix.tm) :
    Unix.tm Range.range Seq.t =
    let start_hour, start_min, start_sec = get_start_hour_min_sec ~start ~acc in
    let start_tm =
      { acc with tm_hour = start_hour; tm_min = start_min; tm_sec = start_sec }
    in
    match t.hours with
    | [] ->
      Seq.return
        (`Range_exc
           (start_tm, { acc with tm_hour = 23; tm_min = 0; tm_sec = 0 }))
    | l ->
      let f_inc (x, y) =
        if x = start_hour then
          (start_tm, { acc with tm_hour = y; tm_min = 59; tm_sec = 59 })
        else
          ( { acc with tm_hour = x; tm_min = 0; tm_sec = 0 },
            { acc with tm_hour = y; tm_min = 59; tm_sec = 59 } )
      in
      let f_exc (x, y) =
        if x = start_hour then
          (start_tm, { acc with tm_hour = y; tm_min = 0; tm_sec = 0 })
        else
          ( { acc with tm_hour = x; tm_min = 0; tm_sec = 0 },
            { acc with tm_hour = y; tm_min = 0; tm_sec = 0 } )
      in
      List.filter (fun hour -> start_hour <= hour) l
      |> List.sort_uniq compare
      |> Ranges_small.Of_list.range_seq_of_list
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_days = struct
  let get_start_mday_hour_min_sec ~(start : Unix.tm) ~(acc : Unix.tm) :
    int * int * int * int =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      (start.tm_mday, start.tm_hour, start.tm_min, start.tm_sec)
    else (1, 0, 0, 0)

  let matching_weekdays (t : t) (start : Unix.tm) (acc : Unix.tm) : int Seq.t =
    let year = acc.tm_year + Time.tm_year_offset in
    let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
    let day_count = Time.day_count_of_month ~year ~month in
    let start_mday, _start_hour, _start_min, _start_sec =
      get_start_mday_hour_min_sec ~start ~acc
    in
    match t.weekdays with
    | [] -> OSeq.(start_mday -- day_count)
    | l ->
      OSeq.(start_mday --^ day_count)
      |> Seq.filter (fun mday ->
          let wday = Time.weekday_of_month_day ~year ~month ~mday in
          List.mem wday l)

  let matching_month_days (t : t) (start : Unix.tm) (acc : Unix.tm) : int Seq.t
    =
    let year = acc.tm_year + Time.tm_year_offset in
    let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
    let day_count = Time.day_count_of_month ~year ~month in
    let start_mday, _start_hour, _start_min, _start_sec =
      get_start_mday_hour_min_sec ~start ~acc
    in
    match t.month_days with
    | [] -> OSeq.(start_mday -- day_count)
    | l ->
      List.filter (fun pat_mday -> start_mday <= pat_mday) l
      |> List.sort_uniq compare
      |> List.to_seq

  let matching_int_days (t : t) (start : Unix.tm) (acc : Unix.tm) : int Seq.t =
    let matching_weekdays = matching_weekdays t start acc |> List.of_seq in
    let matching_month_days = matching_month_days t start acc |> List.of_seq in
    OSeq.(1 -- 31)
    |> Seq.filter (fun mday ->
        List.mem mday matching_weekdays && List.mem mday matching_month_days)

  let matching_days (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t =
    matching_int_days t start acc
    |> Seq.map (fun mday -> { acc with tm_mday = mday })

  let matching_day_ranges (t : t) (start : Unix.tm) (acc : Unix.tm) :
    Unix.tm Range.range Seq.t =
    let start_mday, start_hour, start_min, start_sec =
      get_start_mday_hour_min_sec ~start ~acc
    in
    let year = acc.tm_year + Time.tm_year_offset in
    let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
    let day_count = Time.day_count_of_month ~year ~month in
    let start_tm =
      {
        acc with
        tm_mday = start_mday;
        tm_hour = start_hour;
        tm_min = start_min;
        tm_sec = start_sec;
      }
    in
    let f_inc (x, y) =
      let end_tm =
        { acc with tm_mday = y; tm_hour = 23; tm_min = 59; tm_sec = 59 }
      in
      if x = start_mday then (start_tm, end_tm)
      else
        ({ acc with tm_mday = x; tm_hour = 0; tm_min = 0; tm_sec = 0 }, end_tm)
    in
    let f_exc (x, y) =
      let end_tm =
        { acc with tm_mday = y; tm_hour = 0; tm_min = 0; tm_sec = 0 }
      in
      if x = start_mday then (start_tm, end_tm)
      else
        ({ acc with tm_mday = x; tm_hour = 0; tm_min = 0; tm_sec = 0 }, end_tm)
    in
    match (t.month_days, t.weekdays) with
    | [], [] ->
      Seq.return
        (`Range_inc
           ( start_tm,
             {
               acc with
               tm_mday = day_count;
               tm_hour = 0;
               tm_min = 0;
               tm_sec = 0;
             } ))
    | [], _weekdays ->
      matching_weekdays t start acc
      |> Ranges_small.Of_seq.range_seq_of_seq
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _month_days, [] ->
      matching_month_days t start acc
      |> Ranges_small.Of_seq.range_seq_of_seq
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _, _ ->
      matching_int_days t start acc
      |> Ranges_small.Of_seq.range_seq_of_seq
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_months = struct
  let get_start_mon_mday_hour_min_sec ~(start : Unix.tm) ~(acc : Unix.tm) :
    int * int * int * int * int =
    if acc.tm_year = start.tm_year then
      (start.tm_mon, start.tm_mday, start.tm_hour, start.tm_min, start.tm_sec)
    else (Time.tm_int_of_month `Jan, 0, 0, 0, 0)

  let matching_months (t : t) (start : Unix.tm) (acc : Unix.tm) : Unix.tm Seq.t
    =
    let start_mon, _start_mday, _start_hour, _start_min, _start_sec =
      get_start_mon_mday_hour_min_sec ~start ~acc
    in
    match t.months with
    | [] -> Seq.map (fun tm_mon -> { acc with tm_mon }) OSeq.(start_mon --^ 12)
    | pat_mon_list ->
      pat_mon_list
      |> List.to_seq
      |> Seq.filter (fun pat_mon -> start_mon <= Time.tm_int_of_month pat_mon)
      |> Seq.map (fun pat_mon ->
          { acc with tm_mon = Time.tm_int_of_month pat_mon })

  let matching_month_ranges (t : t) (start : Unix.tm) (acc : Unix.tm) :
    Unix.tm Range.range Seq.t =
    let start_mon, start_mday, start_hour, start_min, start_sec =
      get_start_mon_mday_hour_min_sec ~start ~acc
    in
    let start_tm =
      {
        acc with
        tm_mon = start_mon;
        tm_mday = start_mday;
        tm_hour = start_hour;
        tm_min = start_min;
        tm_sec = start_sec;
      }
    in
    match t.months with
    | [] ->
      Seq.return
        (`Range_inc
           ( start_tm,
             {
               acc with
               tm_mon = Time.tm_int_of_month `Dec;
               tm_mday = 31;
               tm_hour = 23;
               tm_min = 59;
               tm_sec = 59;
             } ))
    | l ->
      let f_inc (x, y) =
        let end_tm =
          let year = acc.tm_year + Time.tm_year_offset in
          let month = Time.month_of_tm_int y |> Result.get_ok in
          let day_count = Time.day_count_of_month ~year ~month in
          {
            acc with
            tm_mon = y;
            tm_mday = day_count;
            tm_hour = 23;
            tm_min = 59;
            tm_sec = 59;
          }
        in
        if x = start_mon then (start_tm, end_tm)
        else
          ( {
            acc with
            tm_mon = x;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          },
            end_tm )
      in
      let f_exc (x, y) =
        let end_tm =
          {
            acc with
            tm_mon = y;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          }
        in
        if x = start_mon then (start_tm, end_tm)
        else
          ( {
            acc with
            tm_mon = x;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          },
            end_tm )
      in
      List.map Time.tm_int_of_month l
      |> List.sort_uniq compare
      |> Ranges_small.Of_list.range_seq_of_list
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_years = struct
  let matching_years ~search_years_ahead (t : t) (start : Unix.tm)
      (acc : Unix.tm) : Unix.tm Seq.t =
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

  let matching_year_ranges ~search_years_ahead (t : t) (start : Unix.tm)
      (acc : Unix.tm) : Unix.tm Range.range Seq.t =
    let start_tm = start in
    match t.years with
    | [] ->
      Seq.return
        (`Range_exc
           (start_tm, { acc with tm_year = start.tm_year + search_years_ahead }))
    | l ->
      let f_inc (x, y) =
        let end_tm =
          {
            acc with
            tm_year = y;
            tm_mon = Time.tm_int_of_month `Dec;
            tm_mday = 31;
            tm_hour = 23;
            tm_min = 59;
            tm_sec = 59;
          }
        in
        if x = start.tm_year then (start_tm, end_tm)
        else
          ( {
            acc with
            tm_year = y;
            tm_mon = Time.tm_int_of_month `Jan;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          },
            end_tm )
      in
      let f_exc (x, y) =
        let end_tm =
          {
            acc with
            tm_year = y;
            tm_mon = Time.tm_int_of_month `Jan;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          }
        in
        if x = start.tm_year then (start_tm, end_tm)
        else
          ( {
            acc with
            tm_year = y;
            tm_mon = Time.tm_int_of_month `Jan;
            tm_mday = 1;
            tm_hour = 0;
            tm_min = 0;
            tm_sec = 0;
          },
            end_tm )
      in
      List.sort_uniq compare l
      |> Ranges_small.Of_list.range_seq_of_list
        ~to_int:(fun x -> x)
        ~of_int:(fun x -> x)
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_unix_times = struct
  let matching_unix_times ~(search_in_time_zone : Time.time_zone) (t : t)
      (start : Unix.tm) : Unix_tm_set.t =
    let start =
      Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone start
    in
    t.unix_times
    |> List.sort_uniq compare
    |> List.to_seq
    |> OSeq.filter (fun x -> x >= start)
    |> Seq.map (Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone)
    |> Unix_tm_set.of_seq
end

let start_tm_and_search_years_ahead_of_search_param
    (search_param : search_param) : (Unix.tm * int) option =
  match search_param with
  | Time_slots { search_in_time_zone; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
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
  let filter_using_matching_unix_times ~search_in_time_zone (t : t) start
      (s : Unix.tm Seq.t) : Unix.tm Seq.t =
    let matching_unix_times =
      Matching_unix_times.matching_unix_times ~search_in_time_zone t start
    in
    if Unix_tm_set.is_empty matching_unix_times then s
    else
      match s () with
      | Seq.Nil -> Unix_tm_set.to_seq matching_unix_times
      | Seq.Cons _ as s ->
        Seq.filter
          (fun x -> Unix_tm_set.mem x matching_unix_times)
          (fun () -> s)

  let tm_range_seq_of_unix_times ~search_in_time_zone (s : int64 Seq.t) :
    Unix.tm Range.range Seq.t =
    let f (x, y) =
      ( Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone x,
        Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone y )
    in
    s
    |> Ranges.Of_seq.range_seq_of_seq
      ~to_int64:(fun x -> x)
      ~of_int64:(fun x -> x)
    |> Seq.map (Range.map ~f_inc:f ~f_exc:f)

  let matching_tm_seq (search_param : search_param) (t : t) : Unix.tm Seq.t =
    match start_tm_and_search_years_ahead_of_search_param search_param with
    | None -> Seq.empty
    | Some (start, search_years_ahead) ->
      let search_in_time_zone =
        search_in_time_zone_of_search_param search_param
      in
      Matching_years.matching_years ~search_years_ahead t start start
      |> Seq.flat_map (Matching_months.matching_months t start)
      |> Seq.flat_map (Matching_days.matching_days t start)
      |> Seq.flat_map (Matching_hours.matching_hours t start)
      |> Seq.flat_map (Matching_minutes.matching_minutes t start)
      |> Seq.flat_map (Matching_seconds.matching_seconds t start)
      |> filter_using_matching_unix_times ~search_in_time_zone t start

  let matching_tm_range_seq (search_param : search_param) (t : t) :
    Unix.tm Range.range Seq.t =
    match start_tm_and_search_years_ahead_of_search_param search_param with
    | None -> Seq.empty
    | Some (start, search_years_ahead) -> (
        let search_in_time_zone =
          search_in_time_zone_of_search_param search_param
        in
        match
          ( t.years,
            t.months,
            t.month_days,
            t.weekdays,
            t.hours,
            t.minutes,
            t.seconds,
            t.unix_times )
        with
        | [], [], [], [], [], [], [], unix_times ->
          unix_times
          |> List.to_seq
          |> tm_range_seq_of_unix_times ~search_in_time_zone
        | _years, [], [], [], [], [], [], [] ->
          Matching_years.matching_year_ranges ~search_years_ahead t start
            start
        | _years, _months, [], [], [], [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_month_ranges t start)
        | _years, _months, _month_days, _weekdays, [], [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_day_ranges t start)
        | _years, _months, _month_days, _weekdays, _hours, [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hour_ranges t start)
        | _years, _months, _month_days, _weekdays, _hours, _minutes, [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minute_ranges t start)
        | ( _years,
            _months,
            _month_days,
            _weekdays,
            _hours,
            _minutes,
            _seconds,
            [] ) ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minutes t start)
          |> Seq.flat_map (Matching_seconds.matching_second_ranges t start)
        | ( _years,
            _months,
            _month_days,
            _weekdays,
            _hours,
            _minutes,
            _seconds,
            _unix_times ) ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minutes t start)
          |> Seq.flat_map (Matching_seconds.matching_seconds t start)
          |> filter_using_matching_unix_times ~search_in_time_zone t start
          |> Seq.map (fun x -> `Range_inc (x, x)) )

  let matching_time_slots (search_param : search_param) (t : t) :
    Time_slot.t Seq.t =
    let time_slots =
      match search_param with
      | Time_slots { time_slots; _ } -> Some time_slots
      | _ -> None
    in
    let search_in_time_zone =
      search_in_time_zone_of_search_param search_param
    in
    let f (x, y) =
      ( Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone x,
        Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone y )
    in
    matching_tm_range_seq search_param t
    |> Seq.map (Range.map ~f_inc:f ~f_exc:f)
    |> Seq.map (fun r ->
        match r with
        | `Range_inc (x, y) -> (x, Int64.succ y)
        | `Range_exc (x, y) -> (x, y))
    |> fun l ->
    match time_slots with
    | None -> l
    | Some time_slots ->
      Time_slots.intersect (List.to_seq time_slots) l
      |> Time_slots.Normalize.normalize ~skip_sort:true

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : t list) : Time_slot.t list Seq.t =
    l
    |> List.map (matching_time_slots search_param)
    |> Time_slots.Round_robin.collect_round_robin_non_decreasing
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : t list) : Time_slot.t Seq.t =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Seq.flat_map List.to_seq

  let next_match_tm (search_param : search_param) (t : t) : Unix.tm option =
    match (matching_tm_seq search_param t) () with
    | Seq.Nil -> None
    | Seq.Cons (x, _) -> Some x

  let next_match_unix_time (search_param : search_param) (t : t) : int64 option
    =
    let search_in_time_zone =
      search_in_time_zone_of_search_param search_param
    in
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
      (range : time_range_pattern) : Time_slot.t Seq.t =
    let search_and_get_start (search_param : search_param) (t : t)
        ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param = push_search_param_to_later_start ~start search_param in
      match Single_pattern.next_match_time_slot search_param t with
      | None -> None
      | Some (start', _) -> Some (start, start')
    in
    let search_and_get_end_exc (search_param : search_param) (t : t)
        ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param = push_search_param_to_later_start ~start search_param in
      match Single_pattern.next_match_time_slot search_param t with
      | None -> None
      | Some (_, end_exc') -> Some (start, end_exc')
    in
    let start_pat =
      match range with `Range_inc (t1, _) | `Range_exc (t1, _) -> t1
    in
    Single_pattern.matching_time_slots search_param start_pat
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
      (l : time_range_pattern list) : Time_slot.t Seq.t =
    l
    |> List.to_seq
    |> Seq.map (matching_time_slots search_param)
    |> Time_slots.Merge.merge_multi_seq

  let next_match_time_slot_multi (search_param : search_param)
      (l : time_range_pattern list) : (int64 * int64) option =
    match matching_time_slots_multi search_param l () with
    | Seq.Nil -> None
    | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : time_range_pattern list) :
    Time_slot.t list Seq.t =
    l
    |> List.map (matching_time_slots search_param)
    |> Time_slots.Round_robin.collect_round_robin_non_decreasing
    |> OSeq.take_while (List.for_all Option.is_some)
    |> Seq.map (List.map Option.get)

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : time_range_pattern list) :
    Time_slot.t Seq.t =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Seq.flat_map List.to_seq
end

module Single_or_ranges = struct
  let matching_time_slots (search_param : search_param) (x : single_or_ranges) :
    Time_slot.t Seq.t =
    match x with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots search_param pat
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_multi search_param l

  let next_match_time_slot (search_param : search_param) (x : single_or_ranges)
    : Time_slot.t option =
    match matching_time_slots search_param x () with
    | Seq.Nil -> None
    | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (t : single_or_ranges) :
    Time_slot.t list Seq.t =
    match t with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots_round_robin_non_decreasing
        search_param [ pat ]
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_round_robin_non_decreasing
        search_param l

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (t : single_or_ranges) : Time_slot.t Seq.t =
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
      unix_times = List.map Misc_utils.int32_int32_of_int64 t.unix_times;
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
      unix_times = List.map Misc_utils.int64_of_int32_int32 t.unix_times;
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
