open Time_expr_ast

exception Invalid_time_expr

let next_hour_minute_expr ({ hour; minute } : hour_minute_expr) :
  hour_minute_expr =
  match Time.next_hour_minute ~hour ~minute with
  | Ok (hour, minute) -> { hour; minute }
  | Error () -> raise Invalid_time_expr

let time_pattern_of_day_expr ?(base : Time_pattern.t = Time_pattern.empty)
    (e : day_expr) : Time_pattern.t =
  match e with
  | Weekday x -> { base with days = `Weekdays [ x ] }
  | Month_day x ->
    if 1 <= x && x <= 31 then { base with days = `Month_days [ x ] }
    else raise Invalid_time_expr

let time_pattern_of_month_expr ?(base : Time_pattern.t = Time_pattern.empty)
    (e : month_expr) : Time_pattern.t =
  let month =
    match e with
    | Direct_pick_month x -> x
    | Human_int_month n -> (
        match Time.month_of_human_int n with
        | Ok x -> x
        | Error () -> raise Invalid_time_expr )
  in
  { base with months = [ month ] }

let paired_hour_minute_of_range_expr (e : hour_minute_expr range_expr) :
  hour_minute_expr * hour_minute_expr =
  match e with
  | Range_inc (x, y) -> (x, next_hour_minute_expr y)
  | Range_exc (x, y) -> (x, y)

let time_pattern_of_hour_minute_expr
    ?(base : Time_pattern.t = Time_pattern.empty) (e : hour_minute_expr) :
  Time_pattern.t =
  { base with hours = [ e.hour ]; minutes = [ e.minute ] }

let paired_time_pattern_of_hour_minute_range_expr
    ?(base : Time_pattern.t = Time_pattern.empty)
    (e : hour_minute_expr range_expr) : Time_pattern.t * Time_pattern.t =
  let hm_start, hm_end_exc = paired_hour_minute_of_range_expr e in
  ( time_pattern_of_hour_minute_expr ~base hm_start,
    time_pattern_of_hour_minute_expr ~base hm_end_exc )

let days_of_day_range_expr (e : day_range_expr) : day_expr list =
  match e with
  | Weekday_range (start, end_inc) ->
    Time.weekday_list_of_weekday_range ~start ~end_inc
    |> List.map (fun x -> Weekday x)
  | Month_day_range (start, end_inc) ->
    OSeq.(start -- end_inc) |> Seq.map (fun x -> Month_day x) |> List.of_seq

let paired_time_patterns_of_time_slots_expr (e : time_slots_expr) :
  ((Time_pattern.t * Time_pattern.t) list, unit) result =
  try
    Ok
      ( match e with
        | Hour_minutes_of_day_list { hour_minutes; days } ->
          let day_pats = List.map time_pattern_of_day_expr days in
          day_pats
          |> List.to_seq
          |> Seq.map (fun pat ->
              paired_time_pattern_of_hour_minute_range_expr ~base:pat
                hour_minutes)
          |> List.of_seq
        | Hour_minutes_of_day_range { hour_minutes; days } ->
          let day_pats =
            days |> days_of_day_range_expr |> List.map time_pattern_of_day_expr
          in
          day_pats
          |> List.to_seq
          |> Seq.map (fun pat ->
              paired_time_pattern_of_hour_minute_range_expr ~base:pat
                hour_minutes)
          |> List.of_seq
        | Hour_minutes_of_next_n_days { hour_minutes; day_count } ->
          let cur_tm = Time.Current.cur_tm_local () in
          let cur_mday = cur_tm.tm_mday in
          let day_pats =
            OSeq.(cur_mday --^ (cur_mday + day_count))
            |> Seq.map (fun mday -> Month_day mday)
            |> Seq.map time_pattern_of_day_expr
            |> List.of_seq
          in
          day_pats
          |> List.to_seq
          |> Seq.map (fun pat ->
              paired_time_pattern_of_hour_minute_range_expr ~base:pat
                hour_minutes)
          |> List.of_seq
        | Hour_minutes_of_day_list_of_month_list { hour_minutes; days; months } ->
          let month_pats = List.map time_pattern_of_month_expr months in
          let day_pats =
            month_pats
            |> List.to_seq
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
  with Invalid_time_expr -> Error ()

module Interpret_string = struct
  let parse lexbuf : t = Time_expr_parser.parse Time_expr_lexer.read lexbuf

  let of_string (s : string) : t =
    let lexbuf = Lexing.from_string s in
    parse lexbuf
end
