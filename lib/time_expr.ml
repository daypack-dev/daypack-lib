open Time_expr_ast

let time_pattern_of_day_expr (e : day_expr) : Time_pattern.t =
  {
    years = [];
    months = [];
    days =
      (match e with
       | Weekday x -> `Weekdays [ x ]
       | Month_day x -> `Month_days [ x ]);
    hours = [];
    minutes = [];
    seconds = [];
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

(* let days_of_day_range_expr (e : day_range_expr) : day_expr list =
 *   match e with
 *   | Weekday_range  *)

let paired_time_patterns_list_of_time_slots_expr (e : time_slots_expr) : (Time_pattern.t * Time_pattern.t) list =
  match e with
  | Hour_minutes_of_day_list { hour_minutes; days } ->
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in
    days
    |> List.map time_pattern_of_day_expr
    |> List.map (fun pat ->
        (add_hour_minute_to_time_pattern hm_start pat,
         add_hour_minute_to_time_pattern hm_end_exc pat)
      )
  (* | Hour_minutes_of_day_range { hour_minutes; days } ->
   *   let hm_start, hm_end_exc = paired_hour_minute_of_range_expr hour_minutes in *)
  | _ -> failwith "Unimplemented"

(* let time_range_exprs_of_complex_time_range_expr (e : complex_time_range_expr) : time_range_expr list =
 *   match e with
 *   | Hour_minutes_of_days { hour_minutes; days } -> *)
