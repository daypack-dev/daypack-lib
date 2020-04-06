open Time_expr_ast

let time_pattern_of_day_expr ?(base : Time_pattern.t = Time_pattern.empty) (e : day_expr) : Time_pattern.t =
  {
    base with
    days =
      (match e with
       | Weekday x -> `Weekdays [ x ]
       | Month_day x -> `Month_days [ x ]);
  }

let time_pattern_of_month_expr ?(base : Time_pattern.t = Time_pattern.empty) (e : month_expr) : Time_pattern.t =
  {
    base with
    months = [ e ];
  }

let paired_hour_minute_of_range_expr (e : hour_minute_expr range_expr) : hour_minute_expr * hour_minute_expr  =
  match e with
  | Single x -> x, x
  | Ranged (x, y) -> x, y

let add_hour_minute_to_time_pattern (e : hour_minute_expr) (pat : Time_pattern.t) : Time_pattern.t =
  {
    pat with
    hours = e.hour :: pat.hours;
    minutes = e.minute :: pat.minutes;
  }

let days_of_day_range_expr (e : day_range_expr) : day_expr list =
  match e with
  | Weekday_range (start, end_inc) ->
    Time.weekday_list_of_weekday_range ~start ~end_inc
    |> List.map (fun x -> Weekday x)
  | Month_day_range (start, end_inc) ->
    OSeq.(start -- end_inc)
    |> Seq.map (fun x -> Month_day x)
    |> List.of_seq

let paired_time_patterns_list_of_time_slots_expr (e : time_slots_expr) : (Time_pattern.t * Time_pattern.t) list =
  match e with
  | Hour_minutes_of_day_list { hour_minutes; days } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    days
    |> List.to_seq
    |> Seq.map time_pattern_of_day_expr
    |> Seq.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
    |> List.of_seq
  | Hour_minutes_of_day_range { hour_minutes; days } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    days
    |> days_of_day_range_expr
    |> List.to_seq
    |> Seq.map time_pattern_of_day_expr
    |> Seq.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
    |> List.of_seq
  | Hour_minutes_of_next_n_days { hour_minutes; day_count } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    let cur_tm = Time.Current.cur_tm_local () in
    let cur_mday = cur_tm.tm_mday in
    OSeq.(cur_mday --^ (cur_mday + day_count))
    |> Seq.map (fun mday -> Month_day mday)
    |> Seq.map time_pattern_of_day_expr
    |> Seq.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
    |> List.of_seq
  | Hour_minutes_of_day_list_of_month_list { hour_minutes; days; months } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    months
    |> List.to_seq
    |> Seq.map time_pattern_of_month_expr
    |> Seq.flat_map (fun base ->
        days
        |> List.to_seq
        |> Seq.map (time_pattern_of_day_expr ~base)
      )
    |> Seq.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
    |> List.of_seq
  | Hour_minutes_of_every_weekday_list_of_month_list { hour_minutes; weekdays; months } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    months
    |> List.to_seq
    |> Seq.map time_pattern_of_month_expr
    |> Seq.flat_map (fun base ->
        weekdays
        |> List.to_seq
        |> Seq.map (fun weekday -> time_pattern_of_day_expr ~base (Weekday weekday))
      )
    |> Seq.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
    |> List.of_seq

(* let time_range_exprs_of_complex_time_range_expr (e : complex_time_range_expr) : time_range_expr list =
 *   match e with
 *   | Hour_minutes_of_days { hour_minutes; days } -> *)
