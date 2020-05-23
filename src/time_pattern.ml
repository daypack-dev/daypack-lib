type search_param =
  | Time_slots of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      time_slots : Time_slot.t list;
    }
  | Years_ahead_start_unix_second of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_date_time of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : Time.date_time;
      search_years_ahead : int;
    }

type search_param_error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead
  | Too_far_into_future

type time_pattern = {
  years : int list;
  months : Time.month list;
  month_days : int list;
  weekdays : Time.weekday list;
  hours : int list;
  minutes : int list;
  seconds : int list;
  unix_seconds : int64 list;
}

type time_pattern_error =
  | Invalid_years of int list
  | Invalid_month_days of int list
  | Invalid_hours of int list
  | Invalid_minutes of int list
  | Invalid_seconds of int list
  | Invalid_unix_seconds of int64 list

type error =
  | Invalid_search_param of search_param_error
  | Invalid_time_pattern of time_pattern_error

type time_range_pattern = time_pattern Range.range

type single_or_ranges =
  | Single_time_pattern of time_pattern
  | Time_range_patterns of time_range_pattern list

module Check = struct
  let check_search_param (x : search_param) : (unit, search_param_error) result
    =
    match x with
    | Time_slots { search_using_tz_offset_s = _; time_slots } ->
      if
        List.for_all
          (fun (x, y) ->
             Time_slot.Check.is_valid (x, y)
             && Time.date_time_of_unix_second ~tz_offset_s_of_date_time:None x
                |> Result.is_ok
             && Time.date_time_of_unix_second ~tz_offset_s_of_date_time:None y
                |> Result.is_ok)
          time_slots
      then Ok ()
      else Error Invalid_time_slots
    | Years_ahead_start_unix_second
        { search_using_tz_offset_s; start; search_years_ahead } -> (
        match
          Time.date_time_of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s start
        with
        | Error () -> Error Invalid_start
        | Ok start ->
          if search_years_ahead <= 0 then Error Invalid_search_years_ahead
          else if start.year + search_years_ahead > Time.max.year then
            Error Too_far_into_future
          else Ok () )
    | Years_ahead_start_date_time
        { search_using_tz_offset_s = _; start; search_years_ahead } ->
      if Time.Check.check_date_time start then
        if search_years_ahead <= 0 then Error Invalid_search_years_ahead
        else if start.year + search_years_ahead > Time.max.year then
          Error Too_far_into_future
        else Ok ()
      else Error Invalid_start

  let check_time_pattern (x : time_pattern) : (unit, time_pattern_error) result
    =
    let invalid_years = List.filter (fun x -> x < 0 || 9999 < x) x.years in
    let invalid_month_days =
      List.filter (fun x -> x < 1 || 31 < x) x.month_days
    in
    let invalid_hours = List.filter (fun x -> x < 0 || 23 < x) x.hours in
    let invalid_minutes = List.filter (fun x -> x < 0 || 59 < x) x.minutes in
    let invalid_seconds = List.filter (fun x -> x < 0 || 59 < x) x.seconds in
    let invalid_unix_seconds =
      List.filter
        (fun x ->
           Result.is_ok
             (Time.date_time_of_unix_second ~tz_offset_s_of_date_time:None x))
        x.unix_seconds
    in
    match invalid_years with
    | [] -> (
        match invalid_month_days with
        | [] -> (
            match invalid_hours with
            | [] -> (
                match invalid_minutes with
                | [] -> (
                    match invalid_seconds with
                    | [] -> (
                        match invalid_unix_seconds with
                        | [] -> Ok ()
                        | l -> Error (Invalid_unix_seconds l) )
                    | l -> Error (Invalid_seconds l) )
                | l -> Error (Invalid_minutes l) )
            | l -> Error (Invalid_hours l) )
        | l -> Error (Invalid_month_days l) )
    | l -> Error (Invalid_years l)

  let check_time_range_pattern (x : time_range_pattern) :
    (unit, time_pattern_error) result =
    match x with
    | `Range_inc (x, y) | `Range_exc (x, y) -> (
        match check_time_pattern x with
        | Error e -> Error e
        | Ok () -> (
            match check_time_pattern y with
            | Error e -> Error e
            | Ok () -> Ok () ) )

  let check_search_param_and_time_pattern (search_param : search_param)
      (x : time_pattern) : (unit, error) result =
    match check_search_param search_param with
    | Error e -> Error (Invalid_search_param e)
    | Ok () -> (
        match check_time_pattern x with
        | Error e -> Error (Invalid_time_pattern e)
        | Ok () -> Ok () )

  let check_search_param_and_time_range_pattern (search_param : search_param)
      (x : time_range_pattern) : (unit, error) result =
    match check_search_param search_param with
    | Error e -> Error (Invalid_search_param e)
    | Ok () -> (
        match check_time_range_pattern x with
        | Error e -> Error (Invalid_time_pattern e)
        | Ok () -> Ok () )
end

let empty =
  {
    years = [];
    months = [];
    weekdays = [];
    month_days = [];
    hours = [];
    minutes = [];
    seconds = [];
    unix_seconds = [];
  }

let of_unix_second ~(tz_offset_s_of_time_pattern : Time.tz_offset_s option)
    (x : int64) : (time_pattern, unit) result =
  Time.date_time_of_unix_second
    ~tz_offset_s_of_date_time:tz_offset_s_of_time_pattern x
  |> Result.map (fun x ->
      let open Time in
      {
        years = [ x.year ];
        months = [ x.month ];
        weekdays = [];
        month_days = [ x.day ];
        hours = [ x.hour ];
        minutes = [ x.minute ];
        seconds = [ x.second ];
        unix_seconds = [];
      })

(* let search_in_time_zone_of_search_param (param : search_param) : Time.time_zone
   =
   match param with
   | Time_slots { search_in_time_zone; _ } -> search_in_time_zone
   | Years_ahead_start_unix_second { search_in_time_zone; _ } ->
    search_in_time_zone
   | Years_ahead_start_tm { search_in_time_zone; _ } -> search_in_time_zone
*)

let search_using_tz_offset_s_of_search_param (param : search_param) :
  Time.tz_offset_s option =
  match param with
  | Time_slots { search_using_tz_offset_s; _ } -> search_using_tz_offset_s
  | Years_ahead_start_unix_second { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s
  | Years_ahead_start_date_time { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s

let push_search_param_to_later_start ~(start : int64)
    (search_param : search_param) : (search_param, unit) result =
  match search_param with
  | Time_slots { search_using_tz_offset_s; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> Ok search_param
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slots.intersect (Seq.return (start, end_exc'))
          |> List.of_seq
        in
        Ok (Time_slots { search_using_tz_offset_s; time_slots }) )
  | Years_ahead_start_unix_second
      { search_using_tz_offset_s; start = start'; search_years_ahead } ->
    let start = max start' start in
    Ok
      (Years_ahead_start_unix_second
         { search_using_tz_offset_s; start; search_years_ahead })
  | Years_ahead_start_date_time
      { search_using_tz_offset_s; start = start'; search_years_ahead } -> (
      match Time.unix_second_of_date_time start' with
      | Error () -> Error ()
      | Ok start' ->
        let start = max start' start in
        Time.date_time_of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s start
        |> Result.map (fun start ->
            Years_ahead_start_date_time
              { search_using_tz_offset_s; start; search_years_ahead }) )

module Matching_seconds = struct
  let get_start ~(start : Time.date_time) ~(acc : Time.date_time) : int =
    if
      acc.year = start.year
      && acc.month = start.month
      && acc.day = start.day
      && acc.hour = start.hour
      && acc.minute = start.minute
    then start.minute
    else 0

  let matching_seconds (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Seq.t =
    let start_sec = get_start ~start ~acc in
    match t.seconds with
    | [] -> Seq.map (fun second -> { acc with second }) OSeq.(start_sec --^ 60)
    | pat_sec_list ->
      pat_sec_list
      |> List.to_seq
      |> Seq.filter (fun pat_sec -> start_sec <= pat_sec)
      |> Seq.map (fun pat_sec -> { acc with second = pat_sec })

  let matching_second_ranges (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Range.range Seq.t =
    let start_sec = get_start ~start ~acc in
    match t.seconds with
    | [] ->
      Seq.return
        (`Range_inc
           ({ acc with second = start_sec }, { acc with second = 59 }))
    | l ->
      List.sort_uniq compare l
      |> Time.Second_ranges.Of_list.range_seq_of_list
      |> Seq.map
        (Range.map
           ~f_inc:(fun (x, y) ->
               ({ acc with second = x }, { acc with second = y }))
           ~f_exc:(fun (x, y) ->
               ({ acc with second = x }, { acc with second = y })))
end

module Matching_minutes = struct
  let get_start_min_sec ~(start : Time.date_time) ~(acc : Time.date_time) :
    int * int =
    if
      acc.year = start.year
      && acc.month = start.month
      && acc.day = start.day
      && acc.hour = start.hour
    then (start.minute, start.second)
    else (0, 0)

  let matching_minutes (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Seq.t =
    let start_min, _start_sec = get_start_min_sec ~start ~acc in
    match t.minutes with
    | [] -> Seq.map (fun minute -> { acc with minute }) OSeq.(start_min --^ 60)
    | pat_min_list ->
      pat_min_list
      |> List.to_seq
      |> Seq.filter (fun pat_min -> start_min <= pat_min)
      |> Seq.map (fun pat_min -> { acc with minute = pat_min })

  let matching_minute_ranges (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Range.range Seq.t =
    let start_min, start_sec = get_start_min_sec ~start ~acc in
    match t.minutes with
    | [] ->
      Seq.return
        (`Range_exc
           ( { acc with minute = start_min; second = start_sec },
             { acc with minute = 60; second = 0 } ))
    | l ->
      let f_inc (x, y) =
        if x = start_min then
          ( { acc with minute = x; second = start_sec },
            { acc with minute = y; second = 59 } )
        else
          ( { acc with minute = x; second = 0 },
            { acc with minute = y; second = 59 } )
      in
      let f_exc (x, y) =
        if x = start_min then
          ( { acc with minute = x; second = start_sec },
            { acc with minute = y; second = 0 } )
        else
          ( { acc with minute = x; second = 0 },
            { acc with minute = y; second = 0 } )
      in
      List.filter (fun pat_min -> start_min <= pat_min) l
      |> List.sort_uniq compare
      |> Time.Minute_ranges.Of_list.range_seq_of_list
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_hours = struct
  let get_start_hour_min_sec ~(start : Time.date_time) ~(acc : Time.date_time) :
    int * int * int =
    if acc.year = start.year && acc.month = start.month && acc.day = start.day
    then (start.hour, start.minute, start.second)
    else (0, 0, 0)

  let matching_hours (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Seq.t =
    let start_hour, _start_min, _start_sec =
      get_start_hour_min_sec ~start ~acc
    in
    match t.hours with
    | [] -> Seq.map (fun hour -> { acc with hour }) OSeq.(start_hour --^ 24)
    | pat_hour_list ->
      pat_hour_list
      |> List.to_seq
      |> Seq.filter (fun pat_hour -> start_hour <= pat_hour)
      |> Seq.map (fun pat_hour -> { acc with hour = pat_hour })

  let matching_hour_ranges (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Range.range Seq.t =
    let start_hour, start_min, start_sec = get_start_hour_min_sec ~start ~acc in
    let start_tm =
      { acc with hour = start_hour; minute = start_min; second = start_sec }
    in
    match t.hours with
    | [] ->
      Seq.return
        (`Range_exc
           (start_tm, { acc with hour = 23; minute = 0; second = 0 }))
    | l ->
      let f_inc (x, y) =
        if x = start_hour then
          (start_tm, { acc with hour = y; minute = 59; second = 59 })
        else
          ( { acc with hour = x; minute = 0; second = 0 },
            { acc with hour = y; minute = 59; second = 59 } )
      in
      let f_exc (x, y) =
        if x = start_hour then
          (start_tm, { acc with hour = y; minute = 0; second = 0 })
        else
          ( { acc with hour = x; minute = 0; second = 0 },
            { acc with hour = y; minute = 0; second = 0 } )
      in
      List.filter (fun hour -> start_hour <= hour) l
      |> List.sort_uniq compare
      |> Time.Hour_ranges.Of_list.range_seq_of_list
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_days = struct
  let get_start_mday_hour_min_sec ~(start : Time.date_time)
      ~(acc : Time.date_time) : int * int * int * int =
    if acc.year = start.year && acc.month = start.month then
      (start.day, start.hour, start.minute, start.second)
    else (1, 0, 0, 0)

  let month_days_of_matching_weekdays (t : time_pattern)
      (start : Time.date_time) (acc : Time.date_time) : int Seq.t =
    let year = acc.year in
    let month = acc.month in
    let day_count = Time.day_count_of_month ~year ~month in
    let start_mday, _start_hour, _start_min, _start_sec =
      get_start_mday_hour_min_sec ~start ~acc
    in
    match t.weekdays with
    | [] -> OSeq.(start_mday -- day_count)
    | l ->
      OSeq.(start_mday --^ day_count)
      |> Seq.filter (fun mday ->
          match Time.weekday_of_month_day ~year ~month ~mday with
          | Ok wday -> List.mem wday l
          | Error () -> false)

  let matching_month_days (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : int Seq.t =
    let year = acc.year in
    let month = acc.month in
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

  let matching_int_days (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : int Seq.t =
    let month_days_of_matching_weekdays =
      month_days_of_matching_weekdays t start acc |> List.of_seq
    in
    let matching_month_days = matching_month_days t start acc |> List.of_seq in
    OSeq.(1 -- 31)
    |> Seq.filter (fun mday ->
        List.mem mday month_days_of_matching_weekdays
        && List.mem mday matching_month_days)

  let matching_days (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Seq.t =
    matching_int_days t start acc
    |> Seq.map (fun mday -> { acc with day = mday })

  let matching_day_ranges (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Range.range Seq.t =
    let start_mday, start_hour, start_min, start_sec =
      get_start_mday_hour_min_sec ~start ~acc
    in
    let year = acc.year in
    let month = acc.month in
    let day_count = Time.day_count_of_month ~year ~month in
    let start_tm =
      {
        acc with
        day = start_mday;
        hour = start_hour;
        minute = start_min;
        second = start_sec;
      }
    in
    let f_inc (x, y) =
      let end_tm = { acc with day = y; hour = 23; minute = 59; second = 59 } in
      if x = start_mday then (start_tm, end_tm)
      else ({ acc with day = x; hour = 0; minute = 0; second = 0 }, end_tm)
    in
    let f_exc (x, y) =
      let end_tm = { acc with day = y; hour = 0; minute = 0; second = 0 } in
      if x = start_mday then (start_tm, end_tm)
      else ({ acc with day = x; hour = 0; minute = 0; second = 0 }, end_tm)
    in
    match (t.month_days, t.weekdays) with
    | [], [] ->
      Seq.return
        (`Range_inc
           ( start_tm,
             { acc with day = day_count; hour = 0; minute = 0; second = 0 } ))
    | [], _weekdays ->
      month_days_of_matching_weekdays t start acc
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _month_days, [] ->
      matching_month_days t start acc
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Range.map ~f_inc ~f_exc)
    | _, _ ->
      matching_int_days t start acc
      |> Time.Month_day_ranges.Of_seq.range_seq_of_seq
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_months = struct
  let get_start_mon_mday_hour_min_sec ~(start : Time.date_time)
      ~(acc : Time.date_time) : int * int * int * int * int =
    if acc.year = start.year then
      ( Time.human_int_of_month start.month,
        start.day,
        start.hour,
        start.minute,
        start.second )
    else (Time.human_int_of_month `Jan, 0, 0, 0, 0)

  let matching_months (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Seq.t =
    let start_mon, _start_mday, _start_hour, _start_min, _start_sec =
      get_start_mon_mday_hour_min_sec ~start ~acc
    in
    match t.months with
    | [] ->
      OSeq.(start_mon -- 12)
      |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
      |> Seq.map (fun month -> { acc with month })
    | pat_mon_list ->
      pat_mon_list
      |> List.to_seq
      |> Seq.map Time.human_int_of_month
      |> Seq.filter (fun pat_mon -> start_mon <= pat_mon)
      |> Seq.map (fun month -> Time.month_of_human_int month |> Result.get_ok)
      |> Seq.map (fun month -> { acc with month })

  let matching_month_ranges (t : time_pattern) (start : Time.date_time)
      (acc : Time.date_time) : Time.date_time Range.range Seq.t =
    let start_mon, start_mday, start_hour, start_min, start_sec =
      get_start_mon_mday_hour_min_sec ~start ~acc
    in
    let start =
      {
        acc with
        month = Time.month_of_human_int start_mon |> Result.get_ok;
        day = start_mday;
        hour = start_hour;
        minute = start_min;
        second = start_sec;
      }
    in
    match t.months with
    | [] ->
      Seq.return
        (`Range_inc
           ( start,
             {
               acc with
               month = `Dec;
               day = 31;
               hour = 23;
               minute = 59;
               second = 59;
             } ))
    | l ->
      let f_inc (x, y) =
        let end_inc =
          let year = acc.year in
          let day_count = Time.day_count_of_month ~year ~month:y in
          {
            acc with
            month = y;
            day = day_count;
            hour = 23;
            minute = 59;
            second = 59;
          }
        in
        if Time.human_int_of_month x = start_mon then (start, end_inc)
        else
          ( { acc with month = x; day = 1; hour = 0; minute = 0; second = 0 },
            end_inc )
      in
      let f_exc (x, y) =
        let end_exc =
          { acc with month = y; day = 1; hour = 0; minute = 0; second = 0 }
        in
        if Time.human_int_of_month x = start_mon then (start, end_exc)
        else
          ( { acc with month = x; day = 1; hour = 0; minute = 0; second = 0 },
            end_exc )
      in
      l
      |> List.sort_uniq Time.compare_month
      |> Time.Month_ranges.Of_list.range_seq_of_list
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_years = struct
  let matching_years ~search_years_ahead (t : time_pattern)
      (start : Time.date_time) (acc : Time.date_time) : Time.date_time Seq.t =
    match t.years with
    | [] ->
      Seq.map
        (fun year -> { acc with year })
        OSeq.(start.year --^ (start.year + search_years_ahead))
    | pat_year_list ->
      pat_year_list
      |> List.to_seq
      |> Seq.filter (fun pat_year -> start.year <= pat_year)
      |> Seq.map (fun year -> { acc with year })

  let matching_year_ranges ~search_years_ahead (t : time_pattern)
      (start : Time.date_time) (acc : Time.date_time) :
    Time.date_time Range.range Seq.t =
    match t.years with
    | [] ->
      Seq.return
        (`Range_inc
           (start, { acc with year = start.year + search_years_ahead - 1 }))
    | l ->
      let f_inc (x, y) =
        let end_inc =
          {
            acc with
            year = y;
            month = `Dec;
            day = 31;
            hour = 23;
            minute = 59;
            second = 59;
          }
        in
        if x = start.year then (start, end_inc)
        else
          ( {
            acc with
            year = y;
            month = `Jan;
            day = 1;
            hour = 0;
            minute = 0;
            second = 0;
          },
            end_inc )
      in
      let f_exc (x, y) =
        let end_exc =
          {
            acc with
            year = y;
            month = `Jan;
            day = 1;
            hour = 0;
            minute = 0;
            second = 0;
          }
        in
        if x = start.year then (start, end_exc)
        else
          ( {
            acc with
            year = y;
            month = `Jan;
            day = 1;
            hour = 0;
            minute = 0;
            second = 0;
          },
            end_exc )
      in
      List.sort_uniq compare l
      |> Time.Year_ranges.Of_list.range_seq_of_list
      |> Seq.map (Range.map ~f_inc ~f_exc)
end

module Matching_unix_seconds = struct
  let matching_unix_seconds
      ~(search_using_tz_offset_s : Time.tz_offset_s option) (t : time_pattern)
      (start : Time.date_time) : Time.Date_time_set.t =
    match Time.unix_second_of_date_time start with
    | Error () -> Time.Date_time_set.empty
    | Ok start ->
      t.unix_seconds
      |> List.sort_uniq compare
      |> List.to_seq
      |> OSeq.filter (fun x -> x >= start)
      |> Seq.filter_map (fun x ->
          match
            Time.date_time_of_unix_second
              ~tz_offset_s_of_date_time:search_using_tz_offset_s x
          with
          | Ok x -> Some x
          | Error () -> None)
      |> Time.Date_time_set.of_seq
end

let start_date_time_and_search_years_ahead_of_search_param
    (search_param : search_param) : (Time.date_time * int) option =
  match search_param with
  | Time_slots { search_using_tz_offset_s; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> None
      | Some (start, end_exc) ->
        let start =
          Time.date_time_of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s start
          |> Result.get_ok
        in
        let end_exc =
          Time.date_time_of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s end_exc
          |> Result.get_ok
        in
        let search_years_ahead = end_exc.year - start.year + 1 in
        Some (start, search_years_ahead) )
  | Years_ahead_start_unix_second
      { search_using_tz_offset_s; start; search_years_ahead } ->
    let start =
      Time.date_time_of_unix_second
        ~tz_offset_s_of_date_time:search_using_tz_offset_s start
      |> Result.get_ok
    in
    Some (start, search_years_ahead)
  | Years_ahead_start_date_time
      { search_using_tz_offset_s = _; start; search_years_ahead } ->
    Some (start, search_years_ahead)

module Single_pattern = struct
  let filter_using_matching_unix_seconds ~search_using_tz_offset_s
      (t : time_pattern) start (s : Time.date_time Seq.t) : Time.date_time Seq.t
    =
    let matching_unix_seconds =
      Matching_unix_seconds.matching_unix_seconds ~search_using_tz_offset_s t
        start
    in
    if Time.Date_time_set.is_empty matching_unix_seconds then s
    else Seq.filter (fun x -> Time.Date_time_set.mem x matching_unix_seconds) s

  let date_time_range_seq_of_unix_seconds ~search_using_tz_offset_s
      (s : int64 Seq.t) : Time.date_time Range.range Seq.t =
    let f (x, y) =
      ( Time.date_time_of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s x,
        Time.date_time_of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s y )
    in
    s
    |> Ranges.Of_seq.range_seq_of_seq ~modulo:None
      ~to_int64:(fun x -> x)
      ~of_int64:(fun x -> x)
    |> Seq.map (Range.map ~f_inc:f ~f_exc:f)
    |> Seq.filter_map Range_utils.result_range_get

  let matching_date_times (search_param : search_param) (t : time_pattern) :
    (Time.date_time Seq.t, error) result =
    Check.check_search_param_and_time_pattern search_param t
    |> Result.map (fun () ->
        match
          start_date_time_and_search_years_ahead_of_search_param search_param
        with
        | None -> Seq.empty
        | Some (start, search_years_ahead) ->
          let search_using_tz_offset_s =
            search_using_tz_offset_s_of_search_param search_param
          in
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minutes t start)
          |> Seq.flat_map (Matching_seconds.matching_seconds t start)
          |> filter_using_matching_unix_seconds ~search_using_tz_offset_s t
            start)

  let matching_unix_seconds (search_param : search_param) (t : time_pattern) :
    (int64 Seq.t, error) result =
    matching_date_times search_param t
    |> Result.map (fun s ->
        Seq.filter_map
          (fun x ->
             match Time.unix_second_of_date_time x with
             | Ok x -> Some x
             | Error () -> None)
          s)

  let matching_date_time_ranges (search_param : search_param) (t : time_pattern)
    : (Time.date_time Range.range Seq.t, error) result =
    match
      start_date_time_and_search_years_ahead_of_search_param search_param
    with
    | None -> Ok Seq.empty
    | Some (start, search_years_ahead) -> (
        let search_using_tz_offset_s =
          search_using_tz_offset_s_of_search_param search_param
        in
        match
          ( t.years,
            t.months,
            t.month_days,
            t.weekdays,
            t.hours,
            t.minutes,
            t.seconds,
            t.unix_seconds )
        with
        | [], [], [], [], [], [], [], unix_seconds ->
          unix_seconds
          |> List.to_seq
          |> date_time_range_seq_of_unix_seconds ~search_using_tz_offset_s
          |> Result.ok
        | _years, [], [], [], [], [], [], [] ->
          Matching_years.matching_year_ranges ~search_years_ahead t start
            start
          |> Result.ok
        | _years, _months, [], [], [], [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_month_ranges t start)
          |> Result.ok
        | _years, _months, _month_days, _weekdays, [], [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_day_ranges t start)
          |> Result.ok
        | _years, _months, _month_days, _weekdays, _hours, [], [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hour_ranges t start)
          |> Result.ok
        | _years, _months, _month_days, _weekdays, _hours, _minutes, [], [] ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minute_ranges t start)
          |> Result.ok
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
          |> Result.ok
        | ( _years,
            _months,
            _month_days,
            _weekdays,
            _hours,
            _minutes,
            _seconds,
            _unix_seconds ) ->
          Matching_years.matching_years ~search_years_ahead t start start
          |> Seq.flat_map (Matching_months.matching_months t start)
          |> Seq.flat_map (Matching_days.matching_days t start)
          |> Seq.flat_map (Matching_hours.matching_hours t start)
          |> Seq.flat_map (Matching_minutes.matching_minutes t start)
          |> Seq.flat_map (Matching_seconds.matching_seconds t start)
          |> filter_using_matching_unix_seconds ~search_using_tz_offset_s t
            start
          |> Seq.map (fun x -> `Range_inc (x, x))
          |> Result.ok )

  let matching_time_slots (search_param : search_param) (t : time_pattern) :
    (Time_slot.t Seq.t, error) result =
    let f (x, y) =
      (Time.unix_second_of_date_time x, Time.unix_second_of_date_time y)
    in
    matching_date_time_ranges search_param t
    |> Result.map (fun s ->
        s
        |> Seq.map (Range.map ~f_inc:f ~f_exc:f)
        |> Seq.filter_map Range_utils.result_range_get
        |> Seq.map (fun r ->
            match r with
            | `Range_inc (x, y) -> (x, Int64.succ y)
            | `Range_exc (x, y) -> (x, y))
        |> fun l ->
        let time_slots =
          match search_param with
          | Time_slots { time_slots; _ } ->
            let time_slots =
              time_slots |> Time_slots.Normalize.normalize_list_in_seq_out
            in
            Some time_slots
          | _ -> None
        in
        match time_slots with
        | None -> l
        | Some time_slots ->
          Time_slots.intersect time_slots ~skip_check:true l
          |> Time_slots.Normalize.normalize ~skip_filter_invalid:true
            ~skip_sort:true)

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : time_pattern list) :
    (Time_slot.t list Seq.t, error) result =
    let l = List.map (matching_time_slots search_param) l in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing
        ~skip_check:true
      |> OSeq.take_while (List.for_all Option.is_some)
      |> Seq.map (List.map Option.get)
      |> Result.ok

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : time_pattern list) :
    (Time_slot.t Seq.t, error) result =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Result.map (Seq.flat_map List.to_seq)

  let next_match_date_time (search_param : search_param) (t : time_pattern) :
    (Time.date_time option, error) result =
    matching_date_times search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)

  let next_match_unix_second (search_param : search_param) (t : time_pattern) :
    (int64 option, error) result =
    next_match_date_time search_param t
    |> Result.map (fun x ->
        match x with
        | None -> None
        | Some x -> (
            match Time.unix_second_of_date_time x with
            | Error () -> None
            | Ok x -> Some x ))

  let next_match_time_slot (search_param : search_param) (t : time_pattern) :
    (Time_slot.t option, error) result =
    matching_time_slots search_param t
    |> Result.map (fun s ->
        match s () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x)
end

module Range_pattern = struct
  let matching_time_slots (search_param : search_param)
      (range : time_range_pattern) : (Time_slot.t Seq.t, error) result =
    let search_and_get_start (search_param : search_param) (t : time_pattern)
        ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param =
        push_search_param_to_later_start ~start search_param |> Result.get_ok
      in
      match
        Single_pattern.next_match_time_slot search_param t |> Result.get_ok
      with
      | None -> None
      | Some (start', _) -> Some (start, start')
    in
    let search_and_get_end_exc (search_param : search_param) (t : time_pattern)
        ((start, _) : Time_slot.t) : Time_slot.t option =
      let search_param =
        push_search_param_to_later_start ~start search_param |> Result.get_ok
      in
      match
        Single_pattern.next_match_time_slot search_param t |> Result.get_ok
      with
      | None -> None
      | Some (_, end_exc') -> Some (start, end_exc')
    in
    let start_pat =
      match range with `Range_inc (t1, _) | `Range_exc (t1, _) -> t1
    in
    Check.check_search_param_and_time_range_pattern search_param range
    |> Result.map (fun () ->
        let s =
          Single_pattern.matching_time_slots search_param start_pat
          |> Result.get_ok
        in
        match range with
        | `Range_inc (_, t2) ->
          Seq.filter_map (search_and_get_end_exc search_param t2) s
        | `Range_exc (_, t2) ->
          Seq.filter_map (search_and_get_start search_param t2) s)

  let next_match_time_slot (search_param : search_param)
      (range : time_range_pattern) : ((int64 * int64) option, error) result =
    matching_time_slots search_param range
    |> Result.map (fun s ->
        match s () with
        | Seq.Nil -> None
        | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))

  let matching_time_slots_multi (search_param : search_param)
      (l : time_range_pattern list) : (Time_slot.t Seq.t, error) result =
    let l = List.map (matching_time_slots search_param) l in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Merge.merge_multi_list ~skip_check:true
      |> Result.ok

  let next_match_time_slot_multi (search_param : search_param)
      (l : time_range_pattern list) : ((int64 * int64) option, error) result =
    matching_time_slots_multi search_param l
    |> Result.map (fun s ->
        match s () with
        | Seq.Nil -> None
        | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (l : time_range_pattern list) :
    (Time_slot.t list Seq.t, error) result =
    let l = List.map (matching_time_slots search_param) l in
    match List.find_opt Result.is_error l with
    | Some e -> Error (Result.get_error e)
    | None ->
      l
      |> List.map Result.get_ok
      |> Time_slots.Round_robin.collect_round_robin_non_decreasing
        ~skip_check:true
      |> OSeq.take_while (List.for_all Option.is_some)
      |> Seq.map (List.map Option.get)
      |> Result.ok

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (l : time_range_pattern list) :
    (Time_slot.t Seq.t, error) result =
    matching_time_slots_round_robin_non_decreasing search_param l
    |> Result.map (Seq.flat_map List.to_seq)
end

module Single_or_ranges = struct
  let matching_time_slots (search_param : search_param) (x : single_or_ranges) :
    (Time_slot.t Seq.t, error) result =
    match x with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots search_param pat
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_multi search_param l

  let next_match_time_slot (search_param : search_param) (x : single_or_ranges)
    : (Time_slot.t option, error) result =
    matching_time_slots search_param x
    |> Result.map (fun s ->
        match s () with
        | Seq.Nil -> None
        | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc))

  let matching_time_slots_round_robin_non_decreasing
      (search_param : search_param) (t : single_or_ranges) :
    (Time_slot.t list Seq.t, error) result =
    match t with
    | Single_time_pattern pat ->
      Single_pattern.matching_time_slots_round_robin_non_decreasing
        search_param [ pat ]
    | Time_range_patterns l ->
      Range_pattern.matching_time_slots_round_robin_non_decreasing
        search_param l

  let matching_time_slots_round_robin_non_decreasing_flat
      (search_param : search_param) (t : single_or_ranges) :
    (Time_slot.t Seq.t, error) result =
    matching_time_slots_round_robin_non_decreasing search_param t
    |> Result.map (Seq.flat_map List.to_seq)
end

module Serialize = struct
  let pack_pattern (t : time_pattern) : Time_pattern_t.time_pattern =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      unix_seconds = List.map Misc_utils.int32_int32_of_int64 t.unix_seconds;
    }
end

module Deserialize = struct
  let unpack_pattern (t : Time_pattern_t.time_pattern) : time_pattern =
    {
      years = t.years;
      months = t.months;
      weekdays = t.weekdays;
      month_days = t.month_days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      unix_seconds = List.map Misc_utils.int64_of_int32_int32 t.unix_seconds;
    }
end

module Equal = struct
  let equal (pat1 : time_pattern) (pat2 : time_pattern) : bool =
    List.sort compare pat1.years = List.sort compare pat2.years
    && List.sort compare pat1.months = List.sort compare pat2.months
    && List.sort compare pat1.weekdays = List.sort compare pat2.weekdays
    && List.sort compare pat1.month_days = List.sort compare pat2.month_days
    && List.sort compare pat1.hours = List.sort compare pat2.hours
    && List.sort compare pat1.minutes = List.sort compare pat2.minutes
end

module Of_string = struct
  open Angstrom
  open Parser_components

  let range_inc_expr (p : 'a t) : 'a Range.range t =
    p >>= (fun x -> hyphen *> p >>| (fun y -> `Range_inc (x, y)))

  let ranges_expr (p : 'a t) : 'a Range.range list t =
    sep_by_comma1 (range_inc_expr p)

  module Second = struct
    let second_expr =
      nat_zero
      >>= fun x ->
      if x >= 60 then fail (Printf.sprintf "Invalid second: %d" x)
      else return x

    let second_ranges_expr =
      (char '*' *> return []
      )
      <|>
      (ranges_expr second_expr >>|
       Time.Second_ranges.Flatten.flatten_list
      )
  end

  module Minute = struct
    let minute_expr =
      nat_zero
      >>= fun x ->
      if x >= 60 then fail (Printf.sprintf "Invalid minute: %d" x)
      else return x

    let minute_ranges_expr =
      (char '*' *> return []
      )
      <|>
      (ranges_expr minute_expr >>|
       Time.Minute_ranges.Flatten.flatten_list
      )
  end

  module Hour = struct
    let hour_expr =
      nat_zero
      >>= fun x ->
      if x >= 24 then fail (Printf.sprintf "Invalid hour: %d" x)
      else return x

    let hour_ranges_expr =
      (char '*' *> return []
      )
      <|>
      (ranges_expr hour_expr >>|
       Time.Hour_ranges.Flatten.flatten_list
      )
  end

  module Month_day = struct
    let month_day_expr =
      nat_zero
      >>= fun x ->
      if 1 <= x && x <= 31 then return x
      else fail (Printf.sprintf "Invalid month day: %d" x)

    let month_day_ranges_expr =
      (char '*' *> return []
      )
      <|>
      (ranges_expr month_day_expr >>|
       Time.Month_day_ranges.Flatten.flatten_list
      )
  end

  module Month = struct
    let month_expr ~for_cron =
      (nat_zero
       >>= fun x ->
       match Time.month_of_human_int x with
       | Ok x -> return x
       | Error () -> fail (Printf.sprintf "Invalid month int: %d" x)
      )
      <|>
      (alpha_string
       >>= fun x ->
       if for_cron && String.length x <> 3 then fail (Printf.sprintf "Invalid length for month string: %s" x)
       else
         match Time.Of_string.month_of_string x with
         | Ok x -> return x
         | Error () -> fail (Printf.sprintf "Invalid month string: %s" x)
      )

    let month_ranges_expr ~for_cron =
      (char '*' *> return []
      )
      <|>
      (
        ranges_expr (month_expr ~for_cron) >>|
        Time.Month_ranges.Flatten.flatten_list
      )
  end

  module Year = struct
    let year_expr =
      nat_zero

    let year_ranges_expr =
      (char '*' *> return []
      )
      <|>
      (
        ranges_expr year_expr >>|
        Time.Year_ranges.Flatten.flatten_list
      )
  end

  module Weekday = struct
    let weekday_expr ~for_cron =
      (nat_zero
       >>= fun x ->
       match Time.weekday_of_tm_int x with
       | Ok x -> return x
       | Error () -> fail (Printf.sprintf "Invalid weekday int: %d" x)
      )
      <|>
      (alpha_string
       >>= fun x ->
       if for_cron && String.length x <> 3 then fail (Printf.sprintf "Invalid length for weekday string: %s" x)
       else
         match Time.Of_string.weekday_of_string x with
         | Ok x -> return x
         | Error () -> fail (Printf.sprintf "Invalid weekday string: %s" x)
      )

    let weekday_ranges_expr ~for_cron =
      (char '*' *> return []
      )
      <|>
      (
        ranges_expr (weekday_expr ~for_cron) >>|
        Time.Weekday_ranges.Flatten.flatten_list
      )
  end

  let cron_expr =
    Minute.minute_ranges_expr
    >>=
    (fun minutes ->
       Hour.hour_ranges_expr >>= (fun hours ->
           Month_day.month_day_ranges_expr >>= (fun month_days ->
               Month.month_ranges_expr ~for_cron:true >>= (fun months ->
                   Year.year_ranges_expr >>= (fun years ->
                       Weekday.weekday_ranges_expr ~for_cron:true >>| (fun weekdays ->
                           {
                             years; months; month_days; weekdays; hours; minutes; seconds = []; unix_seconds = [];
                           }
                         )
                     )
                 )
             )
         )
    )
end

module To_string = struct
  let string_of_error (e : error) : string =
    match e with
    | Invalid_search_param _ -> "Invalid search param"
    | Invalid_time_pattern _ -> "Invalid time pattern"

  let debug_string_of_weekdays (days : Time.weekday list) : string =
    let aux l =
      String.concat "," (List.map Time.To_string.string_of_weekday l)
    in
    Printf.sprintf "weekday [%s]" (aux days)

  let debug_string_of_month_days (days : int list) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    Printf.sprintf "month day [%s]" (aux days)

  let debug_string_of_time_pattern ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : time_pattern) : string =
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
