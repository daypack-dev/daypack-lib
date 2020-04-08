open Int64_utils

type search_type =
  | Time_slots of Time_slot_ds.t list
  | Years_ahead_start_unix_time of {
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_tm of {
      time_zone_of_tm : Time.time_zone;
      start : Unix.tm;
      search_years_ahead : int;
    }

type days =
  [ `Weekdays of Time.weekday list
  | `Month_days of int list
  ]

type t = {
  years : int list;
  months : Time.month list;
  days : days;
  hours : int list;
  minutes : int list;
  seconds : int list;
  max_time_slot_match_count : int option;
}

type single_or_paired =
  | Single of t
  | Paired of (t * t) list

let empty =
  {
    years = [];
    months = [];
    days = `Month_days [];
    hours = [];
    minutes = [];
    seconds = [];
    max_time_slot_match_count = None;
  }

let update_max_time_slot_match_count (n : int option) (t : t) : t =
  { t with
    max_time_slot_match_count = n
  }

let update_max_time_slot_match_count_paired_time_pattern (n : int option) ((x, y) : t * t) : t * t =
  (update_max_time_slot_match_count n x,
   update_max_time_slot_match_count n y
  )

let push_search_type_to_later_start ~(start : int64) (search_type : search_type)
  : search_type =
  match search_type with
  | Time_slots time_slots -> (
      match Time_slot_ds.min_start_and_max_end_exc_list time_slots with
      | None -> search_type
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slot_ds.intersect (Seq.return (start, end_exc'))
          |> List.of_seq
        in
        Time_slots time_slots )
  | Years_ahead_start_unix_time { start = start'; search_years_ahead } ->
    let start = max start' start in
    Years_ahead_start_unix_time { start; search_years_ahead }
  | Years_ahead_start_tm { time_zone_of_tm; start = start'; search_years_ahead }
    ->
    let start = max (Time.unix_time_of_tm ~time_zone_of_tm start') start in
    Years_ahead_start_tm
      {
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
  | pat_min_list ->
    pat_min_list
    |> List.to_seq
    |> Seq.filter (fun pat_min -> start <= pat_min)
    |> Seq.map (fun pat_min -> { acc with tm_min = pat_min })

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
  let year = acc.tm_year + Time.tm_year_offset in
  let month = Time.month_of_tm_int acc.tm_mon |> Result.get_ok in
  let day_count = Time.day_count_of_month ~year ~month in
  let start =
    if acc.tm_year = start.tm_year && acc.tm_mon = start.tm_mon then
      start.tm_mday
    else 1
  in
  match t.days with
  | `Month_days [] | `Weekdays [] ->
    Seq.map (fun tm_mday -> { acc with tm_mday }) OSeq.(start -- day_count)
  | `Month_days pat_mday_list ->
    pat_mday_list
    |> List.to_seq
    |> Seq.filter (fun pat_mday -> start <= pat_mday)
    |> Seq.map (fun pat_mday -> { acc with tm_mday = pat_mday })
  | `Weekdays pat_wday_list ->
    Seq.filter_map
      (fun mday ->
         let wday = Time.weekday_of_month_day ~year ~month ~mday in
         if List.mem wday pat_wday_list then Some { acc with tm_mday = mday }
         else None)
      OSeq.(start --^ day_count)

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

let start_tm_and_search_years_ahead_of_search_type
    ~(search_in_time_zone : Time.time_zone) (search_type : search_type) :
  (Unix.tm * int) option =
  match search_type with
  | Time_slots time_slots -> (
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
  | Years_ahead_start_unix_time { start; search_years_ahead } ->
    let start_tm =
      Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone start
    in
    Some (start_tm, search_years_ahead)
  | Years_ahead_start_tm { time_zone_of_tm; start; search_years_ahead } ->
    let start =
      start
      |> Time.unix_time_of_tm ~time_zone_of_tm
      |> Time.tm_of_unix_time ~time_zone_of_tm:search_in_time_zone
    in
    Some (start, search_years_ahead)

let matching_tm_seq ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t : t) : Unix.tm Seq.t =
  match
    start_tm_and_search_years_ahead_of_search_type ~search_in_time_zone
      search_type
  with
  | None -> Seq.empty
  | Some (start, search_years_ahead) ->
    (* let start = Time.zero_tm_sec start in *)
    matching_years ~search_years_ahead t start start
    |> Seq.flat_map (fun acc -> matching_months t start acc)
    |> Seq.flat_map (fun acc -> matching_days t start acc)
    |> Seq.flat_map (fun acc -> matching_hours t start acc)
    |> Seq.flat_map (fun acc -> matching_minutes t start acc)
    |> Seq.flat_map (fun acc -> matching_seconds t start acc)

let matching_time_slots ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t : t) : Time_slot_ds.t Seq.t =
  let time_slots =
    match search_type with
    | Time_slots time_slots -> Some time_slots
    | _ -> None
  in
  matching_tm_seq ~search_in_time_zone search_type t
  |> Seq.map (Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone)
  |> Seq.map (fun time -> (time, time +^ 1L))
  |> (fun l ->
      match time_slots with
      | None -> l
      | Some time_slots -> Time_slot_ds.intersect (List.to_seq time_slots) l)
  |> Time_slot_ds.normalize ~skip_filter:false ~skip_sort:true
  |> (match t.max_time_slot_match_count with
      | None -> fun x -> x
      | Some n ->
        OSeq.take n
    )

let next_match_tm ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t : t) : Unix.tm option =
  match (matching_tm_seq ~search_in_time_zone search_type t) () with
  | Seq.Nil -> None
  | Seq.Cons (x, _) -> Some x

let next_match_int64 ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t : t) : int64 option =
  next_match_tm ~search_in_time_zone search_type t
  |> Option.map (Time.unix_time_of_tm ~time_zone_of_tm:search_in_time_zone)

let next_match_time_slot ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t : t) : (int64 * int64) option =
  match matching_time_slots ~search_in_time_zone search_type t () with
  | Seq.Nil -> None
  | Seq.Cons (x, _) -> Some x

let matching_time_slots_paired_patterns ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t1 : t) (t2 : t) : Time_slot_ds.t Seq.t =
  matching_time_slots ~search_in_time_zone search_type t1
  |> Seq.filter_map (fun (start, _) ->
      let search_type = push_search_type_to_later_start ~start search_type in
      match matching_time_slots ~search_in_time_zone search_type t2 () with
      | Seq.Nil -> None
      | Seq.Cons ((_, end_exc), _) -> Some (start, end_exc))

let next_match_time_slot_paired_patterns ~(search_in_time_zone : Time.time_zone)
    (search_type : search_type) (t1 : t) (t2 : t) : (int64 * int64) option =
  match
    matching_time_slots_paired_patterns ~search_in_time_zone search_type t1 t2
      ()
  with
  | Seq.Nil -> None
  | Seq.Cons ((start, end_exc), _) -> Some (start, end_exc)

module Serialize = struct
  let pack_days (x : days) : Time_pattern_t.days = x

  let pack_pattern (t : t) : Time_pattern_t.t =
    {
      years = t.years;
      months = t.months;
      days = pack_days t.days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      max_time_slot_match_count = t.max_time_slot_match_count;
    }
end

module Deserialize = struct
  let unpack_days (x : Time_pattern_t.days) : days = x

  let unpack_pattern (t : Time_pattern_t.t) : t =
    {
      years = t.years;
      months = t.months;
      days = unpack_days t.days;
      hours = t.hours;
      minutes = t.minutes;
      seconds = t.seconds;
      max_time_slot_match_count = t.max_time_slot_match_count;
    }
end

module Interpret_time_expr = struct
  exception Invalid_time_expr of string

  let check_hour_minute_expr ({ hour; minute } : Time_expr_ast.hour_minute_expr)
    : unit =
    if Time.check_hour_minute ~hour ~minute then ()
    else
      raise
        (Invalid_time_expr
           (Printf.sprintf "Invalid hour minute: %d:%d" hour minute))

  let check_hour_minutes_expr (hour_minutes : Time_expr_ast.hour_minutes_expr) :
    unit =
    match hour_minutes with
    | Time_expr_ast.Range_inc (x, y) | Time_expr_ast.Range_exc (x, y) ->
      check_hour_minute_expr x;
      check_hour_minute_expr y

  let next_hour_minute_expr ({ hour; minute } : Time_expr_ast.hour_minute_expr)
    : Time_expr_ast.hour_minute_expr =
    match Time.next_hour_minute ~hour ~minute with
    | Ok (hour, minute) -> { hour; minute }
    | Error () ->
      raise
        (Invalid_time_expr
           (Printf.sprintf "Invalid hour minute: %d:%d" hour minute))

  let days_of_day_range_expr (e : Time_expr_ast.day_range_expr) :
    Time_expr_ast.day_expr list =
    match e with
    | Weekday_range (start, end_inc) ->
      Time.weekday_list_of_weekday_range ~start ~end_inc
      |> List.map (fun x -> Time_expr_ast.Weekday x)
    | Month_day_range (start, end_inc) ->
      OSeq.(start -- end_inc)
      |> Seq.map (fun x -> Time_expr_ast.Month_day x)
      |> List.of_seq

  let time_pattern_of_day_expr ?(base : t = empty) (e : Time_expr_ast.day_expr)
    : t =
    match e with
    | Weekday x -> { base with days = `Weekdays [ x ] }
    | Month_day x ->
      if 1 <= x && x <= 31 then { base with days = `Month_days [ x ] }
      else
        raise
          (Invalid_time_expr (Printf.sprintf "Invalid day of month: %d" x))

  let time_pattern_of_month_expr ?(base : t = empty)
      (e : Time_expr_ast.month_expr) : t =
    let month =
      match e with
      | Direct_pick_month x -> x
      | Human_int_month n -> (
          match Time.month_of_human_int n with
          | Ok x -> x
          | Error () ->
            raise (Invalid_time_expr (Printf.sprintf "Invalid month: %d" n)) )
    in
    { base with months = [ month ] }

  let time_pattern_of_year_expr ?(base : t = empty)
      (e : Time_expr_ast.year_expr) : t =
    { base with years = [ e ] }

  let paired_hour_minute_of_range_expr
      (e : Time_expr_ast.hour_minute_expr Time_expr_ast.range_expr) :
    Time_expr_ast.hour_minute_expr * Time_expr_ast.hour_minute_expr =
    match e with
    | Range_inc (x, y) -> (x, next_hour_minute_expr y)
    | Range_exc (x, y) -> (x, y)

  let time_pattern_of_hour_minute_expr ?(base : t = empty)
      (e : Time_expr_ast.hour_minute_expr) : t =
    { base with hours = [ e.hour ]; minutes = [ e.minute ] }

  let paired_time_pattern_of_hour_minute_range_expr ?(base : t = empty)
      (e : Time_expr_ast.hour_minute_expr Time_expr_ast.range_expr) : t * t =
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr e in
    ( time_pattern_of_hour_minute_expr ~base hm_start,
      time_pattern_of_hour_minute_expr ~base hm_end_exc )

  let time_pattern_of_time_point_expr (e : Time_expr_ast.time_point_expr) :
    (t, string) result =
    try
      Ok
        ( match e with
          | Year_month_day_hour_minute { year; month; month_day; hour_minute } ->
            time_pattern_of_year_expr year
            |> (fun base -> time_pattern_of_month_expr ~base month)
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
            |> update_max_time_slot_match_count (Some 1)
          | Month_day_hour_minute { month; month_day; hour_minute } ->
            time_pattern_of_month_expr month
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
            |> update_max_time_slot_match_count (Some 1)
          | Day_hour_minute { day; hour_minute } ->
            time_pattern_of_day_expr day
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
            |> update_max_time_slot_match_count (Some 1)
          | Hour_minute hour_minute ->
            time_pattern_of_hour_minute_expr hour_minute
            |> update_max_time_slot_match_count (Some 1)
        )
    with Invalid_time_expr msg -> Error msg

  let paired_time_patterns_of_time_slots_expr
      (e : Time_expr_ast.time_slots_expr) : ((t * t) list, string) result =
    try
      Ok
        ( match e with
          | Hour_minutes_of_day_list { hour_minutes; days } ->
            check_hour_minutes_expr hour_minutes;
            List.map time_pattern_of_day_expr days
            |> List.to_seq
            |> Seq.map (update_max_time_slot_match_count (Some 1))
            |> Seq.map (fun pat ->
                paired_time_pattern_of_hour_minute_range_expr ~base:pat
                  hour_minutes)
            |> List.of_seq
          | Hour_minutes_of_day_range { hour_minutes; days } ->
            days
            |> days_of_day_range_expr
            |> List.to_seq
            |> Seq.map time_pattern_of_day_expr
            |> Seq.map (update_max_time_slot_match_count (Some 1))
            |> Seq.map (fun pat ->
                paired_time_pattern_of_hour_minute_range_expr ~base:pat
                  hour_minutes)
            |> List.of_seq
          (* | Hour_minutes_of_next_n_days { hour_minutes; day_count } ->
           *   let tm = Time.tm_of_unix_time ~time_zone_of_tm:`Local start in
           *   let mday = tm.tm_mday in
           *   let day_pats =
           *     OSeq.(mday --^ (mday + day_count))
           *     |> Seq.map (fun mday -> Time_expr_ast.Month_day mday)
           *     |> Seq.map time_pattern_of_day_expr
           *     |> List.of_seq
           *   in
           *   day_pats
           *   |> List.to_seq
           *   |> Seq.map (fun pat ->
           *       paired_time_pattern_of_hour_minute_range_expr ~base:pat
           *         hour_minutes)
           *   |> List.of_seq *)
          | Hour_minutes_of_day_list_of_month_list { hour_minutes; days; months }
            ->
            let month_pats = List.map time_pattern_of_month_expr months in
            let day_pats =
              month_pats
              |> List.to_seq
              |> Seq.map (update_max_time_slot_match_count (Some 1))
              |> Seq.flat_map (fun base ->
                  days
                  |> List.to_seq
                  |> Seq.map (time_pattern_of_day_expr ~base))
              |> List.of_seq
            in
            day_pats
            |> List.to_seq
            |> Seq.map (fun pat ->
                paired_time_pattern_of_hour_minute_range_expr ~base:pat
                  hour_minutes)
            |> List.of_seq
          | Hour_minutes_of_every_weekday_list_of_month_list
              { hour_minutes; weekdays; months } ->
            let month_pats = List.map time_pattern_of_month_expr months in
            let day_pats =
              month_pats
              |> List.to_seq
              |> Seq.flat_map (fun base ->
                  weekdays
                  |> List.to_seq
                  |> Seq.map (fun weekday ->
                      time_pattern_of_day_expr ~base (Weekday weekday)))
              |> List.of_seq
            in
            day_pats
            |> List.to_seq
            |> Seq.map (fun pat ->
                paired_time_pattern_of_hour_minute_range_expr ~base:pat
                  hour_minutes)
            |> List.of_seq )
    with Invalid_time_expr msg -> Error msg

  let single_or_paired_time_patterns_of_time_expr (e : Time_expr_ast.t) :
    (single_or_paired, string) result =
    match e with
    | Time_expr_ast.Time_point_expr e -> (
        match time_pattern_of_time_point_expr e with
        | Ok x -> Ok (Single x)
        | Error msg -> Error msg )
    | Time_expr_ast.Time_slots_expr e -> (
        match paired_time_patterns_of_time_slots_expr e with
        | Ok x -> Ok (Paired x)
        | Error msg -> Error msg )
end

module Interpret_string = struct
  let check_hour x = assert (x < 24)

  let check_minute x = assert (x < 60)

  let single_or_paired_time_patterns_of_string (s : string) :
    (single_or_paired, string) result =
    match Time_expr.Interpret_string.of_string s with
    | Error msg -> Error msg
    | Ok time_expr -> (
        match
          Interpret_time_expr.single_or_paired_time_patterns_of_time_expr
            time_expr
        with
        | Error msg -> Error msg
        | Ok x -> Ok x )

  let time_pattern_of_string (s : string) : (t, string) result =
    match single_or_paired_time_patterns_of_string s with
    | Ok (Single x) -> Ok x
    | Ok (Paired _) -> Error "Time expression translates to time slot patterns"
    | Error msg -> Error msg

  let paired_time_patterns_of_string (s : string) :
    ((t * t) list, string) result =
    match single_or_paired_time_patterns_of_string s with
    | Ok (Single _) -> Error "Time expression translates to time point pattern"
    | Ok (Paired x) -> Ok x
    | Error msg -> Error msg

  let paired_time_pattern_of_string (s : string) : (t * t, string) result =
    match single_or_paired_time_patterns_of_string s with
    | Ok (Single _) -> Error "Time expression translates to time point pattern"
    | Ok (Paired l) -> (
        match l with
        | [] ->
          Error
            "Time expression translates to empty list of time slot patterns"
        | [ x ] -> Ok x
        | _ ->
          Error
            "Time expression translates to more than one time slot patterns" )
    | Error msg -> Error msg

  (* let of_date_time_string (s : string) : (t, unit) result =
   *   try
   *     Scanf.sscanf s "%d-%d-%d%c%d:%d" (fun year month day _sep hour minute ->
   *         check_hour hour;
   *         check_minute minute;
   *         let month = Time.month_of_human_int month |> Result.get_ok in
   *         Ok
   *           {
   *             years = [ year ];
   *             months = [ month ];
   *             days = `Month_days [ day ];
   *             hours = [ hour ];
   *             minutes = [ minute ];
   *             seconds = [];
   *           })
   *   with _ -> (
   *       try
   *         Scanf.sscanf s "%d-%d%c%d:%d" (fun month day _sep hour minute ->
   *             check_hour hour;
   *             check_minute minute;
   *             let month = Time.month_of_human_int month |> Result.get_ok in
   *             Ok
   *               {
   *                 years = [];
   *                 months = [ month ];
   *                 days = `Month_days [ day ];
   *                 hours = [ hour ];
   *                 minutes = [ minute ];
   *                 seconds = [];
   *               })
   *       with _ -> (
   *           try
   *             Scanf.sscanf s "%d%c%d:%d" (fun day _sep hour minute ->
   *                 check_hour hour;
   *                 check_minute minute;
   *                 Ok
   *                   {
   *                     years = [];
   *                     months = [];
   *                     days = `Month_days [ day ];
   *                     hours = [ hour ];
   *                     minutes = [ minute ];
   *                     seconds = [];
   *                   })
   *           with _ -> (
   *               try
   *                 Scanf.sscanf s "%d:%d" (fun hour minute ->
   *                     check_hour hour;
   *                     check_minute minute;
   *                     Ok
   *                       {
   *                         years = [];
   *                         months = [];
   *                         days = `Month_days [];
   *                         hours = [ hour ];
   *                         minutes = [ minute ];
   *                         seconds = [];
   *                       })
   *               with _ -> Error () ) ) )
   * 
   * let of_weekday_time_string (s : string) : (t, unit) result =
   *   try
   *     Scanf.sscanf s "%[a-zA-Z]%c%d:%d" (fun maybe_weekday _sep hour minute ->
   *         check_hour hour;
   *         check_minute minute;
   *         let weekday =
   *           Time.Interpret_string.weekday_of_string maybe_weekday
   *           |> Result.get_ok
   *         in
   *         Ok
   *           {
   *             years = [];
   *             months = [];
   *             days = `Weekdays [ weekday ];
   *             hours = [ hour ];
   *             minutes = [ minute ];
   *             seconds = [];
   *           })
   *   with _ -> Error ()
   * 
   * let of_weekday_string (s : string) : (t, unit) result =
   *   try
   *     let weekday =
   *       Time.Interpret_string.weekday_of_string s |> Result.get_ok
   *     in
   *     Ok
   *       {
   *         years = [];
   *         months = [];
   *         days = `Weekdays [ weekday ];
   *         hours = [];
   *         minutes = [];
   *         seconds = [];
   *       }
   *   with _ -> Error ()
   * 
   * let of_string (s : string) : (t, string) result =
   *   match of_date_time_string s with
   *   | Ok x -> Ok x
   *   | Error () -> (
   *       match of_weekday_time_string s with
   *       | Ok x -> Ok x
   *       | Error () -> (
   *           match of_weekday_string s with
   *           | Ok x -> Ok x
   *           | Error () -> Error "Failed to interpret string as a time pattern" )
   *     )
   * 
   * let paired_patterns_of_string (s : string) : (t * t, string) result =
   *   try
   *     Scanf.sscanf s "%[^, ]%[, ]%[^, ]" (fun start _sep end_exc ->
   *         match of_string start with
   *         | Error _ ->
   *           Error "Failed to interpret start string as a time pattern"
   *         | Ok start -> (
   *             match of_string end_exc with
   *             | Error _ ->
   *               Error "Failed to interpret end exc string as a time pattern"
   *             | Ok end_exc -> Ok (start, end_exc) ))
   *   with _ -> Error "Failed to interpret string as time pattern pair" *)
end

module Equal = struct
  let equal (pat1 : t) (pat2 : t) : bool =
    List.sort compare pat1.years = List.sort compare pat2.years
    && List.sort compare pat1.months = List.sort compare pat2.months
    && ( match (pat1.days, pat2.days) with
        | `Weekdays l1, `Weekdays l2 ->
          List.sort compare l1 = List.sort compare l2
        | `Month_days l1, `Month_days l2 ->
          List.sort compare l1 = List.sort compare l2
        | _ -> false )
    && List.sort compare pat1.hours = List.sort compare pat2.hours
    && List.sort compare pat1.minutes = List.sort compare pat2.minutes
end

module Print = struct
  let debug_string_of_days (days : days) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    let aux_weekdays l =
      String.concat "," (List.map Time.Print.string_of_weekday l)
    in
    match days with
    | `Month_days xs -> Printf.sprintf "month day [%s]" (aux xs)
    | `Weekdays xs -> Printf.sprintf "weekday [%s]" (aux_weekdays xs)

  let debug_string_of_pattern ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (t : t) : string =
    let aux l = String.concat "," (List.map string_of_int l) in
    let aux_months l =
      String.concat "," (List.map Time.Print.string_of_month l)
    in
    Debug_print.bprintf ~indent_level buffer "time pattern :\n";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "year : [%s]\n"
      (aux t.years);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "mon : [%s]\n"
      (aux_months t.months);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "day : %s\n"
      (debug_string_of_days t.days);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "hour : [%s]\n"
      (aux t.hours);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "min : [%s]\n"
      (aux t.minutes);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "sec : [%s]\n"
      (aux t.seconds);
    Buffer.contents buffer

  let debug_print_pattern ?(indent_level = 0) t =
    print_string (debug_string_of_pattern ~indent_level t)
end
