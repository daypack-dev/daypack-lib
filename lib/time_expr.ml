open Time_expr_ast

module Interpret_string = struct
  let parse lexbuf : t = Time_expr_parser.parse Time_expr_lexer.read lexbuf

  let lexbuf_to_pos_str lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol - 1)

  let of_string (s : string) : (t, string) result =
    let lexbuf = Lexing.from_string s in
    try Ok (parse lexbuf) with
    | Time_expr_lexer.Syntax_error msg ->
      Error (Printf.sprintf "%s: %s" (lexbuf_to_pos_str lexbuf) msg)
    | Time_expr_parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (lexbuf_to_pos_str lexbuf))
end

module To_time_pattern = struct
  exception Invalid_time_expr of string

  let check_hour_minute_expr ({ hour; minute } : hour_minute_expr)
    : unit =
    if Time.check_hour_minute ~hour ~minute then ()
    else
      raise
        (Invalid_time_expr
           (Printf.sprintf "Invalid hour minute: %d:%d" hour minute))

  let check_hour_minute_range_expr
      (hour_minute_range : hour_minute_range_expr) : unit =
    match hour_minute_range with
    | Range_inc (x, y) | Range_exc (x, y) ->
      check_hour_minute_expr x;
      check_hour_minute_expr y

  let check_hour_minutes
      (hour_minutes : hour_minute_range_expr list) : unit =
    List.iter check_hour_minute_range_expr hour_minutes

  let next_hour_minute_expr ({ hour; minute } : hour_minute_expr)
    : hour_minute_expr =
    match Time.next_hour_minute ~hour ~minute with
    | Ok (hour, minute) -> { hour; minute }
    | Error () ->
      raise
        (Invalid_time_expr
           (Printf.sprintf "Invalid hour minute: %d:%d" hour minute))

  let days_of_day_range_expr (e : day_range_expr) :
    day_expr list =
    match e with
    | Weekday_range (start, end_inc) ->
      Time.weekday_list_of_weekday_range ~start ~end_inc
      |> List.map (fun x -> Time_expr_ast.Weekday x)
    | Month_day_range (start, end_inc) ->
      OSeq.(start -- end_inc)
      |> Seq.map (fun x -> Time_expr_ast.Month_day x)
      |> List.of_seq

  let time_pattern_of_day_expr ?(base : Time_pattern.t = Time_pattern.empty) (e : day_expr)
    : Time_pattern.t =
    match e with
    | Weekday x -> { base with days = `Weekdays [ x ] }
    | Month_day x ->
      if 1 <= x && x <= 31 then { base with days = `Month_days [ x ] }
      else
        raise
          (Invalid_time_expr (Printf.sprintf "Invalid day of month: %d" x))

  let time_pattern_of_month_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : month_expr) : Time_pattern.t =
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

  let time_pattern_of_year_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_ast.year_expr) : Time_pattern.t =
    { base with years = [ e ] }

  let paired_hour_minute_of_range_expr
      (e : hour_minute_expr range_expr) :
    hour_minute_expr * hour_minute_expr =
    match e with
    | Range_inc (x, y) -> (x, next_hour_minute_expr y)
    | Range_exc (x, y) -> (x, y)

  let time_pattern_of_hour_minute_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_ast.hour_minute_expr) : Time_pattern.t =
    { base with hours = [ e.hour ]; minutes = [ e.minute ] }

  let paired_time_pattern_of_hour_minute_range_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : hour_minute_range_expr) : Time_pattern.t * Time_pattern.t =
    let hm_start, hm_end_exc = paired_hour_minute_of_range_expr e in
    ( time_pattern_of_hour_minute_expr ~base hm_start,
      time_pattern_of_hour_minute_expr ~base hm_end_exc )

  let paired_time_pattern_seq_of_hour_minutes ?(base : Time_pattern.t = Time_pattern.empty)
      (l : hour_minute_range_expr list) : (Time_pattern.t * Time_pattern.t) Seq.t =
    List.to_seq l
    |> Seq.map (paired_time_pattern_of_hour_minute_range_expr ~base)

  let time_pattern_of_time_point_expr (e : time_point_expr) :
    (Time_pattern.t, string) result =
    try
      Ok
        ( match e with
          | Year_month_day_hour_minute { year; month; month_day; hour_minute } ->
            time_pattern_of_year_expr year
            |> (fun base -> time_pattern_of_month_expr ~base month)
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
          | Month_day_hour_minute { month; month_day; hour_minute } ->
            time_pattern_of_month_expr month
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
          | Day_hour_minute { day; hour_minute } ->
            time_pattern_of_day_expr day
            |> (fun base -> time_pattern_of_hour_minute_expr ~base hour_minute)
          | Hour_minute hour_minute ->
            time_pattern_of_hour_minute_expr hour_minute
        )
    with Invalid_time_expr msg -> Error msg

  let paired_time_patterns_of_time_slots_expr
      (e : time_slots_expr) : ((Time_pattern.t * Time_pattern.t) list, string) result =
    try
      Ok
        ( match e with
          | Single_time_slot (start, end_exc) -> (
              match time_pattern_of_time_point_expr start with
              | Error msg -> raise (Invalid_time_expr msg)
              | Ok start -> (
                  match time_pattern_of_time_point_expr end_exc with
                  | Error msg -> raise (Invalid_time_expr msg)
                  | Ok end_exc -> [ (start, end_exc) ] ) )
          | Day_list_and_hour_minutes { hour_minutes; days } ->
            check_hour_minutes hour_minutes;
            List.map time_pattern_of_day_expr days
            |> List.to_seq
            |> Seq.flat_map (fun pat ->
                paired_time_pattern_seq_of_hour_minutes ~base:pat
                  hour_minutes)
            |> List.of_seq
          | Day_range_and_hour_minutes { hour_minutes; days } ->
            days
            |> days_of_day_range_expr
            |> List.to_seq
            |> Seq.map time_pattern_of_day_expr
            |> Seq.flat_map (fun pat ->
                paired_time_pattern_seq_of_hour_minutes ~base:pat
                  hour_minutes)
            |> List.of_seq
          | Month_list_and_month_day_list_and_hour_minutes
              { hour_minutes; month_days; months } ->
            let month_pats = List.map time_pattern_of_month_expr months in
            let day_pats =
              month_pats
              |> List.to_seq
              |> Seq.flat_map (fun base ->
                  month_days
                  |> List.to_seq
                  |> Seq.map (fun x ->
                      time_pattern_of_day_expr ~base
                        (Time_expr_ast.Month_day x)))
              |> List.of_seq
            in
            day_pats
            |> List.to_seq
            |> Seq.flat_map (fun pat ->
                paired_time_pattern_seq_of_hour_minutes ~base:pat
                  hour_minutes)
            |> List.of_seq
          | Month_list_and_weekday_list_and_hour_minutes
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
            |> Seq.flat_map (fun pat ->
                paired_time_pattern_seq_of_hour_minutes ~base:pat
                  hour_minutes)
            |> List.of_seq )
    with Invalid_time_expr msg -> Error msg

  let single_or_pairs_of_time_expr (e : t) :
    (Time_pattern.single_or_pairs, string) result =
    match e with
    | Time_expr_ast.Time_point_expr e -> (
        match time_pattern_of_time_point_expr e with
        | Ok x -> Ok (Single_time_pattern x)
        | Error msg -> Error msg )
    | Time_expr_ast.Time_slots_expr e -> (
        match paired_time_patterns_of_time_slots_expr e with
        | Ok x -> Ok (Paired_time_patterns x)
        | Error msg -> Error msg )

  let time_pattern_of_time_expr (e : t) :
    (Time_pattern.t, string) result =
    match single_or_pairs_of_time_expr e with
    | Ok (Time_pattern.Single_time_pattern x) -> Ok x
    | Ok (Time_pattern.Paired_time_patterns _) -> Error "Time expression translates to paired time patterns"
    | Error msg -> Error msg

  let paired_time_pattern_of_time_expr (e : t) :
    (Time_pattern.t * Time_pattern.t, string) result =
    match single_or_pairs_of_time_expr e with
    | Ok (Time_pattern.Single_time_pattern _) -> Error "Time expression translates to single time pattern"
    | Ok (Time_pattern.Paired_time_patterns l) ->(
        match l with
        | [] -> Error "Time expression translates to empty list of paired time patterns"
        | [ x ] -> Ok x
        | _ -> Error "Time expression translates to more than one paired time patterns"
      )
    | Error msg -> Error msg

  let paired_time_patterns_of_time_expr (e : t) :
    ((Time_pattern.t * Time_pattern.t) list, string) result =
    match single_or_pairs_of_time_expr e with
    | Ok (Time_pattern.Single_time_pattern _) -> Error "Time expression translates to single time pattern"
    | Ok (Time_pattern.Paired_time_patterns l) -> Ok l
    | Error msg -> Error msg
end
