type search_param = Time_pattern.search_param

module To_string = struct
  let debug_string_of_hour_minutes
      ({ hour; minute; mode } : Time_expr_ast.hour_minute_expr) : string =
    Printf.sprintf "%d:%d%s" hour minute
      ( match mode with
        | Hour_in_AM -> " am"
        | Hour_in_PM -> " pm"
        | Hour_in_24_hours -> "" )
end

exception Invalid_time_expr of string

module Validate_and_normalize = struct
  let hour_minute_expr
      ({ hour; minute; mode } as e : Time_expr_ast.hour_minute_expr) :
    Time_expr_normalized_ast.hour_minute_expr =
    if 0 <= minute && minute < 60 then
      match mode with
      | Hour_in_AM ->
        if 0 <= hour && hour < 12 then { hour; minute }
        else
          raise
            (Invalid_time_expr
               ("Invalid hour : " ^ To_string.debug_string_of_hour_minutes e))
      | Hour_in_PM ->
        if 0 <= hour && hour < 12 then { hour = hour + 12; minute }
        else
          raise
            (Invalid_time_expr
               ("Invalid hour : " ^ To_string.debug_string_of_hour_minutes e))
      | Hour_in_24_hours ->
        if 0 <= hour && hour < 24 then { hour; minute }
        else
          raise
            (Invalid_time_expr
               ("Invalid hour : " ^ To_string.debug_string_of_hour_minutes e))
    else
      raise
        (Invalid_time_expr
           ("Invalid minute : " ^ To_string.debug_string_of_hour_minutes e))

  let hour_minute_range_expr (e : Time_expr_ast.hour_minute_range_expr) :
    Time_expr_normalized_ast.hour_minute_range_expr =
    match e with
    | `Range_inc (t1, t2) ->
      `Range_inc (hour_minute_expr t1, hour_minute_expr t2)
    | `Range_exc (t1, t2) ->
      `Range_exc (hour_minute_expr t1, hour_minute_expr t2)

  let month_day_expr (n : int) : int =
    if 1 <= n && n <= 31 then n
    else raise (Invalid_time_expr (Printf.sprintf "Invalid month day : %d" n))

  let day_expr (e : Time_expr_ast.day_expr) : Time_expr_normalized_ast.day_expr
    =
    match e with Weekday _ -> e | Month_day n -> Month_day (month_day_expr n)

  let day_range_expr (e : Time_expr_ast.day_range_expr) :
    Time_expr_normalized_ast.day_range_expr =
    match e with
    | Time_expr_ast.Weekday_range (d1, d2) ->
      Time_expr_ast.Weekday_range (d1, d2)
    | Time_expr_ast.Month_day_range (d1, d2) ->
      Time_expr_ast.Month_day_range (month_day_expr d1, month_day_expr d2)

  let month_expr (e : Time_expr_ast.month_expr) :
    Time_expr_normalized_ast.month_expr =
    match e with
    | Direct_pick_month m -> m
    | Human_int_month n -> (
        match Time.month_of_human_int n with
        | Error () ->
          raise (Invalid_time_expr (Printf.sprintf "Invalid month : %d" n))
        | Ok m -> m )

  let year_expr (e : Time_expr_ast.year_expr) :
    Time_expr_normalized_ast.year_expr =
    e

  let time_point_expr (e : Time_expr_ast.time_point_expr) :
    Time_expr_normalized_ast.time_point_expr =
    match e with
    | Time_expr_ast.Year_month_day_hour_minute
        { year; month; month_day; hour_minute } ->
      Time_expr_normalized_ast.Year_month_day_hour_minute
        {
          year = year_expr year;
          month = month_expr month;
          month_day = month_day_expr month_day;
          hour_minute = hour_minute_expr hour_minute;
        }
    | Time_expr_ast.Month_day_hour_minute { month; month_day; hour_minute } ->
      Time_expr_normalized_ast.Month_day_hour_minute
        {
          month = month_expr month;
          month_day = month_day_expr month_day;
          hour_minute = hour_minute_expr hour_minute;
        }
    | Time_expr_ast.Day_hour_minute { day; hour_minute } ->
      Time_expr_normalized_ast.Day_hour_minute
        { day = day_expr day; hour_minute = hour_minute_expr hour_minute }
    | Time_expr_ast.Hour_minute hour_minute ->
      Time_expr_normalized_ast.Hour_minute (hour_minute_expr hour_minute)

  let time_slots_expr (e : Time_expr_ast.time_slots_expr) :
    Time_expr_normalized_ast.time_slots_expr =
    match e with
    | Time_expr_ast.Single_time_slot (e1, e2) ->
      Time_expr_normalized_ast.Single_time_slot
        (time_point_expr e1, time_point_expr e2)
    | Time_expr_ast.Day_list_and_hour_minutes { hour_minutes; days } ->
      Time_expr_normalized_ast.Day_list_and_hour_minutes
        {
          days = List.map day_expr days;
          hour_minutes = List.map hour_minute_range_expr hour_minutes;
        }
    | Time_expr_ast.Day_range_and_hour_minutes { hour_minutes; days } ->
      Time_expr_normalized_ast.Day_range_and_hour_minutes
        {
          days = day_range_expr days;
          hour_minutes = List.map hour_minute_range_expr hour_minutes;
        }
    | Time_expr_ast.Month_list_and_month_day_list_and_hour_minutes
        { months; month_days; hour_minutes } ->
      Time_expr_normalized_ast.Month_list_and_month_day_list_and_hour_minutes
        {
          months = List.map month_expr months;
          month_days = List.map month_day_expr month_days;
          hour_minutes = List.map hour_minute_range_expr hour_minutes;
        }
    | Time_expr_ast.Month_list_and_weekday_list_and_hour_minutes
        { months; weekdays; hour_minutes } ->
      Time_expr_normalized_ast.Month_list_and_weekday_list_and_hour_minutes
        {
          months = List.map month_expr months;
          weekdays;
          hour_minutes = List.map hour_minute_range_expr hour_minutes;
        }

  let time_expr (e : Time_expr_ast.t) :
    (Time_expr_normalized_ast.t, string) result =
    try
      match e with
      | Time_expr_ast.Time_point_expr e ->
        Ok (Time_expr_normalized_ast.Time_point_expr (time_point_expr e))
      | Time_expr_ast.Time_slots_expr e ->
        Ok (Time_expr_normalized_ast.Time_slots_expr (time_slots_expr e))
    with Invalid_time_expr msg -> Error msg
end

module Interpret_string = struct
  let parse lexbuf : Time_expr_ast.t =
    Time_expr_parser.parse Time_expr_lexer.read lexbuf

  let lexbuf_to_pos_str lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 1)

  let of_string (s : string) : (Time_expr_normalized_ast.t, string) result =
    let lexbuf = Lexing.from_string s in
    try Validate_and_normalize.time_expr (parse lexbuf) with
    | Time_expr_lexer.Syntax_error msg ->
      Error (Printf.sprintf "%s: %s" (lexbuf_to_pos_str lexbuf) msg)
    | Time_expr_parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (lexbuf_to_pos_str lexbuf))

  let time_point_expr_of_string (s : string) :
    (Time_expr_normalized_ast.time_point_expr, string) result =
    match of_string s with
    | Ok (Time_point_expr e) -> Ok e
    | Ok (Time_slots_expr _) ->
      Error "String translates to time slots expression"
    | Error msg -> Error msg

  let time_slots_expr_of_string (s : string) :
    (Time_expr_normalized_ast.time_slots_expr, string) result =
    match of_string s with
    | Ok (Time_point_expr _) ->
      Error "String translates to time point expression"
    | Ok (Time_slots_expr e) -> Ok e
    | Error msg -> Error msg
end

module To_time_pattern_lossy = struct
  let check_hour_minute_expr
      ({ hour; minute } : Time_expr_normalized_ast.hour_minute_expr) : unit =
    if Time.check_hour_minute ~hour ~minute then ()
    else
      raise
        (Invalid_time_expr
           (Printf.sprintf "Invalid hour minute: %d:%d" hour minute))

  let check_hour_minute_range_expr
      (hour_minute_range : Time_expr_normalized_ast.hour_minute_range_expr) :
    unit =
    match hour_minute_range with
    | `Range_inc (x, y) | `Range_exc (x, y) ->
      check_hour_minute_expr x;
      check_hour_minute_expr y

  let check_hour_minutes
      (hour_minutes : Time_expr_normalized_ast.hour_minute_range_expr list) :
    unit =
    List.iter check_hour_minute_range_expr hour_minutes

  (* let next_hour_minute_expr (e : hour_minute_expr) :
   *   hour_minute_expr =
   *   match Validate_and_normalize.hour_minutes_expr e with
   *   | Error msg -> raise (Invalid_time_expr msg)
   *   | Ok {hour; minute; mode} ->
   *     match Time.next_hour_minute ~hour ~minute with
   *     | Ok (hour, minute) -> { hour; minute;  mode }
   *     | Error () ->
   *       raise
   *         (Invalid_time_expr
   *            (To_string.debug_string_of_hour_minutes e)) *)

  let days_of_day_range_expr (e : Time_expr_normalized_ast.day_range_expr) :
    Time_expr_normalized_ast.day_expr list =
    match e with
    | Weekday_range (start, end_inc) ->
      Time.weekday_list_of_weekday_range ~start ~end_inc
      |> List.map (fun x -> Time_expr_ast.Weekday x)
    | Month_day_range (start, end_inc) ->
      OSeq.(start -- end_inc)
      |> Seq.map (fun x -> Time_expr_ast.Month_day x)
      |> List.of_seq

  let time_pattern_of_day_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_normalized_ast.day_expr) : Time_pattern.t =
    match e with
    | Weekday x -> { base with days = `Weekdays [ x ] }
    | Month_day x ->
      if 1 <= x && x <= 31 then { base with days = `Month_days [ x ] }
      else
        raise
          (Invalid_time_expr (Printf.sprintf "Invalid day of month: %d" x))

  let time_pattern_of_month_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_normalized_ast.month_expr) : Time_pattern.t =
    { base with months = [ e ] }

  let time_pattern_of_year_expr ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_normalized_ast.year_expr) : Time_pattern.t =
    { base with years = [ e ] }

  let time_pattern_of_hour_minute_expr
      ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_normalized_ast.hour_minute_expr) : Time_pattern.t =
    { base with hours = [ e.hour ]; minutes = [ e.minute ] }

  let time_range_pattern_of_hour_minute_range_expr
      ?(base : Time_pattern.t = Time_pattern.empty)
      (e : Time_expr_normalized_ast.hour_minute_range_expr) :
    Time_pattern.time_range_pattern =
    match e with
    | `Range_inc (x, y) ->
      `Range_inc
        ( time_pattern_of_hour_minute_expr ~base x,
          time_pattern_of_hour_minute_expr ~base y )
    | `Range_exc (x, y) ->
      `Range_exc
        ( time_pattern_of_hour_minute_expr ~base x,
          time_pattern_of_hour_minute_expr ~base y )

  let time_range_pattern_seq_of_hour_minutes
      ?(base : Time_pattern.t = Time_pattern.empty)
      (l : Time_expr_normalized_ast.hour_minute_range_expr list) :
    Time_pattern.time_range_pattern Seq.t =
    List.to_seq l
    |> Seq.map (time_range_pattern_of_hour_minute_range_expr ~base)

  let time_pattern_of_time_point_expr
      (e : Time_expr_normalized_ast.time_point_expr) :
    (Time_pattern.t, string) result =
    try
      Ok
        ( match e with
          | Year_month_day_hour_minute { year; month; month_day; hour_minute } ->
            time_pattern_of_year_expr year
            |> (fun base -> time_pattern_of_month_expr ~base month)
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> fun base -> time_pattern_of_hour_minute_expr ~base hour_minute
          | Month_day_hour_minute { month; month_day; hour_minute } ->
            time_pattern_of_month_expr month
            |> (fun base ->
                time_pattern_of_day_expr ~base (Month_day month_day))
            |> fun base -> time_pattern_of_hour_minute_expr ~base hour_minute
          | Day_hour_minute { day; hour_minute } ->
            time_pattern_of_day_expr day
            |> fun base -> time_pattern_of_hour_minute_expr ~base hour_minute
          | Hour_minute hour_minute ->
            time_pattern_of_hour_minute_expr hour_minute )
    with Invalid_time_expr msg -> Error msg

  let time_range_patterns_of_time_slots_expr
      (e : Time_expr_normalized_ast.time_slots_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    try
      Ok
        ( match e with
          | Single_time_slot (start, end_exc) -> (
              match time_pattern_of_time_point_expr start with
              | Error msg -> raise (Invalid_time_expr msg)
              | Ok start -> (
                  match time_pattern_of_time_point_expr end_exc with
                  | Error msg -> raise (Invalid_time_expr msg)
                  | Ok end_exc -> [ `Range_exc (start, end_exc) ] ) )
          | Day_list_and_hour_minutes { hour_minutes; days } ->
            check_hour_minutes hour_minutes;
            List.map time_pattern_of_day_expr days
            |> List.to_seq
            |> Seq.flat_map (fun pat ->
                time_range_pattern_seq_of_hour_minutes ~base:pat hour_minutes)
            |> List.of_seq
          | Day_range_and_hour_minutes { hour_minutes; days } ->
            days
            |> days_of_day_range_expr
            |> List.to_seq
            |> Seq.map time_pattern_of_day_expr
            |> Seq.flat_map (fun pat ->
                time_range_pattern_seq_of_hour_minutes ~base:pat hour_minutes)
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
                time_range_pattern_seq_of_hour_minutes ~base:pat hour_minutes)
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
                time_range_pattern_seq_of_hour_minutes ~base:pat hour_minutes)
            |> List.of_seq )
    with Invalid_time_expr msg -> Error msg

  let single_or_ranges_of_time_expr (e : Time_expr_normalized_ast.t) :
    (Time_pattern.single_or_ranges, string) result =
    match e with
    | Time_expr_normalized_ast.Time_point_expr e -> (
        match time_pattern_of_time_point_expr e with
        | Ok x -> Ok (Single_time_pattern x)
        | Error msg -> Error msg )
    | Time_expr_normalized_ast.Time_slots_expr e -> (
        match time_range_patterns_of_time_slots_expr e with
        | Ok x -> Ok (Time_range_patterns x)
        | Error msg -> Error msg )

  let time_pattern_of_time_expr (e : Time_expr_normalized_ast.t) :
    (Time_pattern.t, string) result =
    match single_or_ranges_of_time_expr e with
    | Ok (Time_pattern.Single_time_pattern x) -> Ok x
    | Ok (Time_pattern.Time_range_patterns _) ->
      Error "Time expression translates to time pattern pairs"
    | Error msg -> Error msg

  let time_range_patterns_of_time_expr (e : Time_expr_normalized_ast.t) :
    (Time_pattern.time_range_pattern list, string) result =
    match single_or_ranges_of_time_expr e with
    | Ok (Time_pattern.Single_time_pattern _) ->
      Error "Time expression translates to single time pattern"
    | Ok (Time_pattern.Time_range_patterns l) -> Ok l
    | Error msg -> Error msg

  let time_range_pattern_of_time_expr (e : Time_expr_normalized_ast.t) :
    (Time_pattern.time_range_pattern, string) result =
    match time_range_patterns_of_time_expr e with
    | Ok l -> (
        match l with
        | [] ->
          Error
            "Time expression translates to empty list of time range patterns"
        | [ x ] -> Ok x
        | _ ->
          Error
            "Time expression translates to more than one time range patterns"
      )
    | Error msg -> Error msg
end

let next_match_unix_time_time_point_expr (search_param : search_param)
    (e : Time_expr_normalized_ast.time_point_expr) :
  (int64 option, string) result =
  match To_time_pattern_lossy.time_pattern_of_time_point_expr e with
  | Error msg -> Error msg
  | Ok pat -> Ok (Time_pattern.Single_pattern.next_match_unix_time search_param pat)

let matching_time_slots (search_param : search_param)
    (e : Time_expr_normalized_ast.t) : (Time_slot_ds.t Seq.t, string) result =
  match e with
  | Time_point_expr e -> (
      match To_time_pattern_lossy.time_pattern_of_time_point_expr e with
      | Error msg -> Error msg
      | Ok pat ->
        Time_pattern.Single_pattern.matching_time_slots search_param pat
        |> OSeq.take 1
        |> Result.ok )
  | Time_slots_expr e -> (
      let take_count =
        match e with
        | Single_time_slot _ | Day_list_and_hour_minutes _
        | Day_range_and_hour_minutes _
        | Month_list_and_month_day_list_and_hour_minutes _
        | Month_list_and_weekday_list_and_hour_minutes _ ->
          Some 1
      in
      match To_time_pattern_lossy.time_range_patterns_of_time_slots_expr e with
      | Error msg -> Error msg
      | Ok l ->
        l
        |> List.map
          (Time_pattern.Range_pattern.matching_time_slots
             search_param)
        |> Time_slot_ds.collect_round_robin_non_decreasing
        |> (match take_count with None -> fun x -> x | Some n -> OSeq.take n)
        |> OSeq.take_while (List.for_all Option.is_some)
        |> Seq.flat_map List.to_seq
        |> Seq.map Option.get
        |> Result.ok )

let next_match_time_slot (search_param : search_param)
    (e : Time_expr_normalized_ast.t) : ((int64 * int64) option, string) result =
  match matching_time_slots search_param e with
  | Error msg -> Error msg
  | Ok seq -> (
      match seq () with Seq.Nil -> Ok None | Seq.Cons (x, _) -> Ok (Some x) )
