type search_param = Time_pattern.search_param

module To_string = struct
  let debug_string_of_hour_minutes
      ({ hour; minute; mode } : Time_expr_ast.hour_minute_expr) : string =
    Printf.sprintf "%02d:%02d%s" hour minute
      ( match mode with
        | Hour_in_AM -> " am"
        | Hour_in_PM -> " pm"
        | Hour_in_24_hours -> "" )
end

exception Invalid_time_expr of string

module Validate_and_normalize = struct
  module Hour_minute = struct
    let hour_minute_expr
        ({ hour; minute; mode } as e : Time_expr_ast.hour_minute_expr) :
      Time_expr_normalized_ast.hour_minute_expr =
      if 0 <= minute && minute < 60 then
        match mode with
        | Hour_in_AM ->
          if 1 <= hour && hour <= 12 then
            let hour = if hour = 12 then 0 else hour in
            { hour; minute }
          else
            raise
              (Invalid_time_expr
                 ("Invalid hour : " ^ To_string.debug_string_of_hour_minutes e))
        | Hour_in_PM ->
          if 1 <= hour && hour <= 12 then
            let hour = if hour = 12 then 0 else hour in
            { hour = hour + 12; minute }
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

    let hour_minute_ranges_expr (l : Time_expr_ast.hour_minute_range_expr list)
      : Time_expr_normalized_ast.hour_minute_range_expr list =
      List.map hour_minute_range_expr l
  end

  module Month_day = struct
    let month_day_expr (n : int) : int =
      if 1 <= n && n <= 31 then n
      else raise (Invalid_time_expr (Printf.sprintf "Invalid month day : %d" n))

    let month_day_range_expr (x : int Range.t) : int Range.t =
      match x with
      | `Range_inc (x, y) -> `Range_inc (month_day_expr x, month_day_expr y)
      | `Range_exc (x, y) ->
        `Range_inc (month_day_expr x, month_day_expr (y - 1))

    let month_day_ranges_expr (l : int Range.t list) : int Range.t list =
      List.map month_day_range_expr l
  end

  module Weekday = struct
    let weekday_expr (x : Time.weekday) : Time.weekday = x

    let weekday_range_expr (x : Time.weekday Range.t) : Time.weekday Range.t = x

    let weekday_ranges_expr (l : Time.weekday Range.t list) :
      Time.weekday Range.t list =
      List.map weekday_range_expr l
  end

  module Day = struct
    let day_expr (e : Time_expr_ast.day_expr) :
      Time_expr_normalized_ast.day_expr =
      match e with
      | Weekday _ -> e
      | Month_day n -> Month_day (Month_day.month_day_expr n)
  end

  module Month = struct
    let month_expr (e : Time_expr_ast.month_expr) :
      Time_expr_normalized_ast.month_expr =
      match e with
      | Direct_pick_month m -> m
      | Human_int_month n -> (
          match Time.month_of_human_int n with
          | Error () ->
            raise (Invalid_time_expr (Printf.sprintf "Invalid month : %d" n))
          | Ok m -> m )

    let month_range_expr (e : Time_expr_ast.month_expr Range.t) :
      Time_expr_normalized_ast.month_expr Range.t =
      match e with
      | `Range_inc (x, y) -> `Range_inc (month_expr x, month_expr y)
      | `Range_exc (x, y) -> `Range_exc (month_expr x, month_expr y)

    let month_ranges_expr (l : Time_expr_ast.month_expr Range.t list) :
      Time_expr_normalized_ast.month_expr Range.t list =
      List.map month_range_expr l
  end

  module Year = struct
    let year_expr (e : Time_expr_ast.year_expr) :
      Time_expr_normalized_ast.year_expr =
      e

    let year_range_expr (e : Time_expr_ast.year_expr Range.t) :
      Time_expr_normalized_ast.year_expr Range.t =
      e

    let year_ranges_expr (l : Time_expr_ast.year_expr Range.t list) :
      Time_expr_normalized_ast.year_expr Range.t list =
      List.map year_range_expr l
  end

  let time_point_expr (e : Time_expr_ast.time_point_expr) :
    Time_expr_normalized_ast.time_point_expr =
    match e with
    | Time_expr_ast.Year_month_day_hour_minute
        { year; month; month_day; hour_minute } ->
      Time_expr_normalized_ast.Year_month_day_hour_minute
        {
          year = Year.year_expr year;
          month = Month.month_expr month;
          month_day = Month_day.month_day_expr month_day;
          hour_minute = Hour_minute.hour_minute_expr hour_minute;
        }
    | Time_expr_ast.Month_day_hour_minute { month; month_day; hour_minute } ->
      Time_expr_normalized_ast.Month_day_hour_minute
        {
          month = Month.month_expr month;
          month_day = Month_day.month_day_expr month_day;
          hour_minute = Hour_minute.hour_minute_expr hour_minute;
        }
    | Time_expr_ast.Day_hour_minute { day; hour_minute } ->
      Time_expr_normalized_ast.Day_hour_minute
        {
          day = Day.day_expr day;
          hour_minute = Hour_minute.hour_minute_expr hour_minute;
        }
    | Time_expr_ast.Hour_minute hour_minute ->
      Time_expr_normalized_ast.Hour_minute
        (Hour_minute.hour_minute_expr hour_minute)

  let time_slots_expr (e : Time_expr_ast.time_slots_expr) :
    Time_expr_normalized_ast.time_slots_expr =
    match e with
    | Time_expr_ast.Single_time_slot { start; end_exc; match_mode } ->
      Time_expr_normalized_ast.Single_time_slot
        {start = time_point_expr start;
         end_exc = time_point_expr end_exc;
         match_mode;
        }
    | Time_expr_ast.Month_days_and_hour_minutes { month_days; hour_minutes; match_mode  } ->
      Time_expr_normalized_ast.Month_days_and_hour_minutes
        {
          month_days = Month_day.month_day_ranges_expr month_days;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
        }
    | Time_expr_ast.Weekdays_and_hour_minutes { weekdays; hour_minutes; match_mode  } ->
      Time_expr_normalized_ast.Weekdays_and_hour_minutes
        {
          weekdays = Weekday.weekday_ranges_expr weekdays;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
        }
    | Time_expr_ast.Months_and_month_days_and_hour_minutes
        { months; month_days; hour_minutes; match_mode  } ->
      Time_expr_normalized_ast.Months_and_month_days_and_hour_minutes
        {
          months = Month.month_ranges_expr months;
          month_days = Month_day.month_day_ranges_expr month_days;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
        }
    | Time_expr_ast.Months_and_weekdays_and_hour_minutes
        { months; weekdays; hour_minutes; match_mode  } ->
      Time_expr_normalized_ast.Months_and_weekdays_and_hour_minutes
        {
          months = Month.month_ranges_expr months;
          weekdays = Weekday.weekday_ranges_expr weekdays;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
        }
    | Time_expr_ast.Months_and_weekday_and_hour_minutes
        { months; weekday; hour_minutes; month_weekday_mode; match_mode  } ->
      Time_expr_normalized_ast.Months_and_weekday_and_hour_minutes
        {
          months = Month.month_ranges_expr months;
          weekday = Weekday.weekday_expr weekday;
          month_weekday_mode;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
        }
    | Time_expr_ast.Years_and_months_and_month_days_and_hour_minutes
        { years; months; month_days; hour_minutes; match_mode  } ->
      Time_expr_normalized_ast
      .Years_and_months_and_month_days_and_hour_minutes
        {
          years = Year.year_ranges_expr years;
          months = Month.month_ranges_expr months;
          month_days = Month_day.month_day_ranges_expr month_days;
          hour_minutes = Hour_minute.hour_minute_ranges_expr hour_minutes;
         match_mode;
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
  (* let check_hour_minute_expr
   *     ({ hour; minute } : Time_expr_normalized_ast.hour_minute_expr) : unit =
   *   if Time.check_hour_minute ~hour ~minute then ()
   *   else
   *     raise
   *       (Invalid_time_expr
   *          (Printf.sprintf "Invalid hour minute: %d:%d" hour minute)) *)

  (* let check_hour_minute_range_expr
   *     (hour_minute_range : Time_expr_normalized_ast.hour_minute_range_expr) :
   *   unit =
   *   match hour_minute_range with
   *   | `Range_inc (x, y) | `Range_exc (x, y) ->
   *     check_hour_minute_expr x;
   *     check_hour_minute_expr y *)

  (* let check_hour_minutes
   *     (hour_minutes : Time_expr_normalized_ast.hour_minute_range_expr list) :
   *   unit =
   *   List.iter check_hour_minute_range_expr hour_minutes *)

  module Hour_minute = struct
    let update_time_pattern_using_hour_minute_expr
        (e : Time_expr_normalized_ast.hour_minute_expr) (base : Time_pattern.t)
      : Time_pattern.t =
      if Time.check_hour_minute ~hour:e.hour ~minute:e.minute then
        { base with hours = [ e.hour ]; minutes = [ e.minute ] }
      else
        raise
          (Invalid_time_expr
             (Printf.sprintf "Invalid hour minute: %d:%d" e.hour e.minute))

    let time_range_pattern_of_hour_minute_range_expr_and_base_time_pattern
        (e : Time_expr_normalized_ast.hour_minute_range_expr)
        (base : Time_pattern.t) : Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_hour_minute_expr x base,
            update_time_pattern_using_hour_minute_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_hour_minute_expr x base,
            update_time_pattern_using_hour_minute_expr y base )

    let time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
        (l : Time_expr_normalized_ast.hour_minute_range_expr list)
        (base : Time_pattern.t) : Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_hour_minute_range_expr_and_base_time_pattern
            e base)

    (* let update_time_range_pattern_seq_of_hour_minutes
     *     ?(base : Time_pattern.t = Time_pattern.empty)
     *     (l : Time_expr_normalized_ast.hour_minute_range_expr list) :
     *   Time_pattern.time_range_pattern Seq.t =
     *   List.to_seq l
     *   |> Seq.map (time_range_pattern_of_hour_minute_range_expr ~base) *)
  end

  module Month_day = struct
    let update_time_pattern_using_month_day_expr (x : int)
        (base : Time_pattern.t) : Time_pattern.t =
      if 1 <= x && x <= 31 then { base with month_days = [ x ] }
      else
        raise (Invalid_time_expr (Printf.sprintf "Invalid day of month: %d" x))

    let time_pattern_of_month_day_expr x =
      update_time_pattern_using_month_day_expr x Time_pattern.empty

    let time_patterns_of_month_days_and_base_time_pattern (l : int list)
        (base : Time_pattern.t) : Time_pattern.t Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_month_day_expr e base)

    let flatten_month_day_ranges (l : int Range.t list) : int Seq.t =
      List.to_seq l
      |> Seq.flat_map
        (Range.flatten_into_seq ~of_int:(fun x -> x) ~to_int:(fun x -> x))
  end

  module Weekday = struct
    let update_time_pattern_using_weekday_expr (x : Time.weekday)
        (base : Time_pattern.t) : Time_pattern.t =
      { base with weekdays = [ x ] }

    let time_pattern_of_weekday_expr x =
      update_time_pattern_using_weekday_expr x Time_pattern.empty

    let time_patterns_of_weekdays_and_base_time_pattern (l : Time.weekday list)
        (base : Time_pattern.t) : Time_pattern.t Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_weekday_expr e base)

    let flatten_weekday_ranges (l : Time.weekday Range.t list) :
      Time.weekday Seq.t =
      List.to_seq l
      |> Seq.flat_map
        (Range.flatten_into_seq ~modulo:7 ~of_int:Time.weekday_of_tm_int
           ~to_int:Time.tm_int_of_weekday)
  end

  module Day = struct
    (* let days_of_day_range_expr (e : Time_expr_normalized_ast.day_range_expr) :
     *   Time_expr_normalized_ast.day_expr list =
     *   match e with
     *   | Weekday_range (start, end_inc) ->
     *     Time.weekday_list_of_weekday_range ~start ~end_inc
     *     |> List.map (fun x -> Time_expr_ast.Weekday x)
     *   | Month_day_range (start, end_inc) ->
     *     OSeq.(start -- end_inc)
     *     |> Seq.map (fun x -> Time_expr_ast.Month_day x)
     *     |> List.of_seq *)

    let update_time_pattern_using_day_expr
        (e : Time_expr_normalized_ast.day_expr) (base : Time_pattern.t) :
      Time_pattern.t =
      match e with
      | Month_day e -> Month_day.update_time_pattern_using_month_day_expr e base
      | Weekday e -> Weekday.update_time_pattern_using_weekday_expr e base
  end

  module Month = struct
    let update_time_pattern_using_month_expr
        (e : Time_expr_normalized_ast.month_expr) (base : Time_pattern.t) :
      Time_pattern.t =
      { base with months = [ e ] }

    let time_pattern_of_month_expr x =
      update_time_pattern_using_month_expr x Time_pattern.empty

    let time_patterns_of_months_and_base_time_pattern (l : Time.month list)
        (base : Time_pattern.t) : Time_pattern.t Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_month_expr e base)

    let flatten_month_ranges (l : Time.month Range.t list) : Time.month Seq.t =
      List.to_seq l
      |> Seq.flat_map
        (Range.flatten_into_seq
           ~of_int:(fun x -> Time.month_of_tm_int x |> Result.get_ok)
           ~to_int:Time.tm_int_of_month)
  end

  module Year = struct
    let update_time_pattern_using_year_expr
        (e : Time_expr_normalized_ast.year_expr) (base : Time_pattern.t) :
      Time_pattern.t =
      { base with years = [ e ] }

    let time_pattern_of_year_expr x =
      update_time_pattern_using_year_expr x Time_pattern.empty

    let flatten_year_ranges (l : int Range.t list) : int Seq.t =
      List.to_seq l
      |> Seq.flat_map
        (Range.flatten_into_seq ~of_int:(fun x -> x) ~to_int:(fun x -> x))
  end

  let time_pattern_of_time_point_expr
      (e : Time_expr_normalized_ast.time_point_expr) :
    (Time_pattern.t, string) result =
    try
      Ok
        ( match e with
          | Year_month_day_hour_minute { year; month; month_day; hour_minute } ->
            Time_pattern.empty
            |> Year.update_time_pattern_using_year_expr year
            |> Month.update_time_pattern_using_month_expr month
            |> Month_day.update_time_pattern_using_month_day_expr month_day
            |> Hour_minute.update_time_pattern_using_hour_minute_expr
              hour_minute
          | Month_day_hour_minute { month; month_day; hour_minute } ->
            Time_pattern.empty
            |> Month.update_time_pattern_using_month_expr month
            |> Month_day.update_time_pattern_using_month_day_expr month_day
            |> Hour_minute.update_time_pattern_using_hour_minute_expr
              hour_minute
          | Day_hour_minute { day; hour_minute } ->
            Time_pattern.empty
            |> Day.update_time_pattern_using_day_expr day
            |> Hour_minute.update_time_pattern_using_hour_minute_expr
              hour_minute
          | Hour_minute hour_minute ->
            Hour_minute.update_time_pattern_using_hour_minute_expr hour_minute
              Time_pattern.empty )
    with Invalid_time_expr msg -> Error msg

  let time_range_patterns_of_time_slots_expr
      (e : Time_expr_normalized_ast.time_slots_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    try
      Ok
        ( match e with
          | Single_time_slot {start; end_exc; match_mode = _ } -> (
              match time_pattern_of_time_point_expr start with
              | Error msg -> raise (Invalid_time_expr msg)
              | Ok start -> (
                  match time_pattern_of_time_point_expr end_exc with
                  | Error msg -> raise (Invalid_time_expr msg)
                  | Ok end_exc -> [ `Range_exc (start, end_exc) ] ) )
          | Month_days_and_hour_minutes { month_days; hour_minutes; match_mode = _ } ->
            (* check_hour_minutes hour_minutes; *)
            Month_day.flatten_month_day_ranges month_days
            |> Seq.map Month_day.time_pattern_of_month_day_expr
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
            |> List.of_seq
          | Weekdays_and_hour_minutes { weekdays; hour_minutes; match_mode = _ } ->
            Weekday.flatten_weekday_ranges weekdays
            |> Seq.map Weekday.time_pattern_of_weekday_expr
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
            |> List.of_seq
          | Months_and_month_days_and_hour_minutes
              { months; month_days; hour_minutes; match_mode = _ } ->
            let month_days =
              Month_day.flatten_month_day_ranges month_days |> List.of_seq
            in
            Month.flatten_month_ranges months
            |> Seq.map Month.time_pattern_of_month_expr
            |> Seq.flat_map
              (Month_day.time_patterns_of_month_days_and_base_time_pattern
                 month_days)
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
            |> List.of_seq
          | Months_and_weekdays_and_hour_minutes
              { months; weekdays; hour_minutes; match_mode = _ } ->
            let weekdays =
              Weekday.flatten_weekday_ranges weekdays |> List.of_seq
            in
            Month.flatten_month_ranges months
            |> Seq.map Month.time_pattern_of_month_expr
            |> Seq.flat_map
              (Weekday.time_patterns_of_weekdays_and_base_time_pattern
                 weekdays)
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
            |> List.of_seq
          | Months_and_weekday_and_hour_minutes
              { months; weekday; hour_minutes; month_weekday_mode = _; match_mode = _ } ->
            Month.flatten_month_ranges months
            |> Seq.map Month.time_pattern_of_month_expr
            |> Seq.map
              (Weekday.update_time_pattern_using_weekday_expr weekday)
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
            |> List.of_seq
          | Years_and_months_and_month_days_and_hour_minutes
              { years; months; month_days; hour_minutes; match_mode = _ } ->
            let months = Month.flatten_month_ranges months |> List.of_seq in
            let month_days =
              Month_day.flatten_month_day_ranges month_days |> List.of_seq
            in
            Year.flatten_year_ranges years
            |> Seq.map Year.time_pattern_of_year_expr
            |> Seq.flat_map
              (Month.time_patterns_of_months_and_base_time_pattern months)
            |> Seq.flat_map
              (Month_day.time_patterns_of_month_days_and_base_time_pattern
                 month_days)
            |> Seq.flat_map
              (Hour_minute
               .time_range_patterns_of_hour_minute_ranges_and_base_time_pattern
                 hour_minutes)
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

module Time_point_expr = struct
  let next_match_unix_time (search_param : search_param)
      (e : Time_expr_normalized_ast.time_point_expr) :
    (int64 option, string) result =
    match To_time_pattern_lossy.time_pattern_of_time_point_expr e with
    | Error msg -> Error msg
    | Ok pat ->
      Ok (Time_pattern.Single_pattern.next_match_unix_time search_param pat)

  let matching_time_slots (search_param : search_param)
      (e : Time_expr_normalized_ast.time_point_expr) :
    (Time_slot_ds.t Seq.t, string) result =
    match To_time_pattern_lossy.time_pattern_of_time_point_expr e with
    | Error msg -> Error msg
    | Ok pat ->
      Time_pattern.Single_pattern.matching_time_slots search_param pat
      |> OSeq.take 1
      |> Result.ok

  let next_match_time_slot (search_param : search_param)
      (e : Time_expr_normalized_ast.time_point_expr) :
    ((int64 * int64) option, string) result =
    match matching_time_slots search_param e with
    | Error msg -> Error msg
    | Ok seq -> (
        match seq () with Seq.Nil -> Ok None | Seq.Cons (x, _) -> Ok (Some x) )
end

module Time_slots_expr = struct
  let get_first_or_last_n_matches_of_same_month_tm_pair_seq
      ~(first_or_last : [ `First | `Last ]) ~(n : int)
      (s : (Unix.tm * Unix.tm) Seq.t) : (Unix.tm * Unix.tm) Seq.t =
    let flush_acc first_or_last (n : int) (acc : (Unix.tm * Unix.tm) list) :
      (Unix.tm * Unix.tm) Seq.t =
      (match first_or_last with `First -> List.rev acc | `Last -> acc)
      |> List.to_seq
      |> OSeq.take n
    in
    let rec aux first_or_last (n : int) (acc : (Unix.tm * Unix.tm) list)
        (s : (Unix.tm * Unix.tm) Seq.t) : (Unix.tm * Unix.tm) Seq.t =
      match s () with
      | Seq.Nil -> flush_acc first_or_last n acc
      | Seq.Cons ((start, end_exc), rest) -> (
          match acc with
          | [] -> aux first_or_last n [ (start, end_exc) ] rest
          | (tm, _) :: _ ->
            if tm.tm_mon = start.tm_mon then
              aux first_or_last n ((start, end_exc) :: acc) rest
            else
              OSeq.append
                (flush_acc first_or_last n acc)
                (aux first_or_last n [ (start, end_exc) ] rest) )
    in
    aux first_or_last n [] s

  let get_first_or_last_n_matches_of_same_month
      ~(first_or_last : [ `First | `Last ]) ~(n : int)
      (search_param : search_param) (s : Time_slot_ds.t Seq.t) :
    Time_slot_ds.t Seq.t =
    let time_zone_of_tm =
      Time_pattern.search_in_time_zone_of_search_param search_param
    in
    s
    |> Seq.map (fun (x, y) ->
        ( Time.tm_of_unix_time ~time_zone_of_tm x,
          Time.tm_of_unix_time ~time_zone_of_tm y ))
    |> get_first_or_last_n_matches_of_same_month_tm_pair_seq ~first_or_last ~n
    |> Seq.map (fun (x, y) ->
        ( Time.unix_time_of_tm ~time_zone_of_tm x,
          Time.unix_time_of_tm ~time_zone_of_tm y ))

  let matching_time_slots ?(force_match_mode : Time_expr_ast.match_mode option) (search_param : search_param)
      (e : Time_expr_normalized_ast.time_slots_expr) :
    (Time_slot_ds.t Seq.t, string) result =
    let list_selector =
      match e with
      | Single_time_slot { match_mode; _ } | Month_days_and_hour_minutes { match_mode; _ }
      | Weekdays_and_hour_minutes { match_mode; _ } | Months_and_month_days_and_hour_minutes { match_mode; _ }
      | Months_and_weekdays_and_hour_minutes { match_mode; _ }
      | Months_and_weekday_and_hour_minutes {match_mode; _ }
      | Years_and_months_and_month_days_and_hour_minutes { match_mode; _ } ->
        (match Option.value ~default:match_mode force_match_mode with
         | Next -> OSeq.take 1
         | Every -> fun x -> x
        )
    in
    let flat_selector =
      match e with
      | Single_time_slot _ | Month_days_and_hour_minutes _
      | Weekdays_and_hour_minutes _ | Months_and_month_days_and_hour_minutes _
      | Months_and_weekdays_and_hour_minutes _
      | Years_and_months_and_month_days_and_hour_minutes _ ->
        fun x -> x
      | Months_and_weekday_and_hour_minutes { month_weekday_mode; _ } -> (
          match month_weekday_mode with
          | None -> fun x -> x
          | Some (First_n n) ->
            get_first_or_last_n_matches_of_same_month ~first_or_last:`First ~n
              search_param
          | Some (Last_n n) ->
            get_first_or_last_n_matches_of_same_month ~first_or_last:`Last ~n
              search_param )
    in
    match To_time_pattern_lossy.time_range_patterns_of_time_slots_expr e with
    | Error msg -> Error msg
    | Ok l ->
      Time_pattern.Range_pattern
      .matching_time_slots_round_robin_non_decreasing search_param l
      |> list_selector
      |> Seq.flat_map List.to_seq
      |> flat_selector
      |> Result.ok

  let next_match_time_slot (search_param : search_param)
      (e : Time_expr_normalized_ast.time_slots_expr) :
    ((int64 * int64) option, string) result =
    match matching_time_slots search_param e with
    | Error msg -> Error msg
    | Ok seq -> (
        match seq () with Seq.Nil -> Ok None | Seq.Cons (x, _) -> Ok (Some x) )
end

let matching_time_slots (search_param : search_param)
    (e : Time_expr_normalized_ast.t) : (Time_slot_ds.t Seq.t, string) result =
  match e with
  | Time_point_expr e -> Time_point_expr.matching_time_slots search_param e
  | Time_slots_expr e -> Time_slots_expr.matching_time_slots search_param e

let next_match_time_slot (search_param : search_param)
    (e : Time_expr_normalized_ast.t) : ((int64 * int64) option, string) result =
  match e with
  | Time_point_expr e -> Time_point_expr.next_match_time_slot search_param e
  | Time_slots_expr e -> Time_slots_expr.next_match_time_slot search_param e
