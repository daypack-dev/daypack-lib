type search_param = Time_pattern.search_param

type f_resolve_tse_name =
  string -> Time_expr_ast.unbounded_time_slots_expr option

type f_resolve_tpe_name =
  string -> Time_expr_ast.unbounded_time_points_expr option

let default_f_resolve_tse_name (_ : string) :
  Time_expr_ast.unbounded_time_slots_expr option =
  None

let default_f_resolve_tpe_name (_ : string) :
  Time_expr_ast.unbounded_time_points_expr option =
  None

module To_string = struct
  let debug_string_of_hour_minute_second_ranges
      ({ hour; minute; second } : Time_expr_ast.hour_minute_second_expr) :
    string =
    Printf.sprintf "%02d:%02d:%02d" hour minute second
end

exception Invalid_time_expr of string

let max_resolve_depth = 1000

let resolve_unbounded_time_points_expr
    ~(f_resolve_tpe_name : f_resolve_tpe_name)
    (e : Time_expr_ast.unbounded_time_points_expr) :
  (Time_expr_ast.unbounded_time_points_expr, string) result =
  let rec aux f_resolve_tpe_name remaining_resolve_depth name e =
    if remaining_resolve_depth <= 0 then Error "Maximum resolve depth reached"
    else
      match e with
      | Time_expr_ast.Tpe_name s -> (
          if name = Some s then
            Error
              (Printf.sprintf "Name resolution loop detected for name: %s" s)
          else
            let name = match name with None -> Some s | Some x -> Some x in
            match f_resolve_tpe_name s with
            | None ->
              Error (Printf.sprintf "Name resolution failed for name: %s" s)
            | Some e ->
              aux f_resolve_tpe_name (pred remaining_resolve_depth) name e )
      | e -> Ok e
  in
  aux f_resolve_tpe_name max_resolve_depth None e

let resolve_unbounded_time_slots_expr ~(f_resolve_tse_name : f_resolve_tse_name)
    ~(f_resolve_tpe_name : f_resolve_tpe_name)
    (e : Time_expr_ast.unbounded_time_slots_expr) :
  (Time_expr_ast.unbounded_time_slots_expr, string) result =
  let rec aux f_resolve_tse_name f_resolve_tpe_name remaining_resolve_depth name
      e =
    if remaining_resolve_depth <= 0 then Error "Maximum resolve depth reached"
    else
      match e with
      | Time_expr_ast.Tse_name s -> (
          if name = Some s then
            Error
              (Printf.sprintf "Name resolution loop detected for name: %s" s)
          else
            let name = match name with None -> Some s | Some x -> Some x in
            match f_resolve_tse_name s with
            | None ->
              Error (Printf.sprintf "Name resolution failed for name: %s" s)
            | Some e ->
              aux f_resolve_tse_name f_resolve_tpe_name
                (pred remaining_resolve_depth)
                name e )
      | Single_time_slot { start; end_exc } -> (
          match
            resolve_unbounded_time_points_expr ~f_resolve_tpe_name start
          with
          | Error msg -> Error msg
          | Ok start -> (
              match
                resolve_unbounded_time_points_expr ~f_resolve_tpe_name end_exc
              with
              | Error msg -> Error msg
              | Ok end_exc ->
                Ok (Time_expr_ast.Single_time_slot { start; end_exc }) ) )
      | e -> Ok e
  in
  aux f_resolve_tse_name f_resolve_tpe_name max_resolve_depth None e

module Interpret_string = struct
  open Angstrom
  open Parser_components

  let to_string = string_ci "to"

  let first_string = string_ci "first"

  let last_string = string_ci "last"

  let bound =
    option `Next
      (choice
         [
           string_ci "coming" *> return `Next;
           char '?' *> return `Next;
           string_ci "every" *> return `Every;
           char '!' *> return `Every;
         ])

  let sep_by_comma1 (p : 'a t) : 'a list t = sep_by1 (space *> comma *> space) p

  let range_inc_expr (p : 'a t) : 'a Range.t t =
    p
    >>= (fun x ->
        space *> to_string *> space *> p >>| fun y -> `Range_inc (x, y))
        <|> (p >>| fun x -> `Range_inc (x, x))

  let range_exc_expr (p : 'a t) : 'a Range.t t =
    p
    >>= (fun x ->
        space *> to_string *> space *> p >>| fun y -> `Range_exc (x, y))
        <|> (p >>| fun x -> `Range_inc (x, x))

  let ranges_expr ~to_int (p : 'a Range.t t) : 'a Range.t list t =
    sep_by_comma1 p >>| Range.compress_list ~to_int

  module Second = struct
    let second_expr : Time_expr_ast.second_expr t =
      string "::" *> nat_zero
      >>= fun second ->
      if second >= 60 then fail (Printf.sprintf "Invalid second: %d" second)
      else return second
  end

  module Minute_second = struct
    let minute_second_expr : Time_expr_ast.minute_second_expr t =
      char ':' *> nat_zero
      >>= fun minute ->
      if minute >= 60 then fail (Printf.sprintf "Invalid minute: %d" minute)
      else
        option 0 (char ':' *> nat_zero)
        >>= fun second ->
        if second >= 60 then fail (Printf.sprintf "Invalid second: %d" second)
        else return Time_expr_ast.{ minute; second }
  end

  module Hour_minute_second = struct
    let hour_minute_second_mode_expr =
      option `Hour_in_24_hours
        ( string_ci "am" *> return `Hour_in_AM
          <|> string_ci "pm" *> return `Hour_in_PM )

    let hour_minute_second_expr : Time_expr_ast.hour_minute_second_expr t =
      nat_zero
      >>= fun hour ->
      char ':' *> nat_zero
      >>= fun minute ->
      if minute >= 60 then fail (Printf.sprintf "Invalid minute: %d" minute)
      else
        option 0 (char ':' *> nat_zero)
        >>= fun second ->
        if second >= 60 then fail (Printf.sprintf "Invalid second: %d" second)
        else
          space *> hour_minute_second_mode_expr
          >>= fun mode ->
          match mode with
          | `Hour_in_24_hours ->
            if hour >= 24 then fail (Printf.sprintf "Invalid hour: %d" hour)
            else return Time_expr_ast.{ hour; minute; second }
          | `Hour_in_AM ->
            if 1 <= hour && hour <= 12 then
              let hour = if hour = 12 then 0 else hour in
              return Time_expr_ast.{ hour; minute; second }
            else fail (Printf.sprintf "Invalid hour: %d" hour)
          | `Hour_in_PM ->
            if 1 <= hour && hour <= 12 then
              let hour = if hour = 12 then 0 else hour in
              return Time_expr_ast.{ hour = hour + 12; minute; second }
            else fail (Printf.sprintf "Invalid hour: %d" hour)

    let hour_minute_second_range_expr :
      Time_expr_ast.hour_minute_second_range_expr t =
      range_exc_expr hour_minute_second_expr

    let hour_minute_second_ranges_expr :
      Time_expr_ast.hour_minute_second_range_expr list t =
      sep_by_comma1 hour_minute_second_range_expr
  end

  module Month_day = struct
    let month_day_expr : int t =
      nat_zero
      >>= fun x ->
      if 1 <= x && x <= 31 then return x
      else fail (Printf.sprintf "Invalid month day: %d" x)

    let month_day_range_expr : int Range.t t = range_inc_expr month_day_expr

    let month_day_ranges_expr : int Range.t list t =
      ranges_expr ~to_int:(fun x -> x) month_day_range_expr
  end

  module Weekday = struct
    let weekday_expr : Time.weekday t =
      alpha_string
      >>= fun x ->
      match Time.Interpret_string.weekday_of_string x with
      | Ok x -> return x
      | Error _ -> fail "Failed to interpret weekday string"

    let weekday_range_expr : Time.weekday Range.t t =
      range_inc_expr weekday_expr

    let weekday_ranges_expr : Time.weekday Range.t list t =
      ranges_expr ~to_int:Time.tm_int_of_weekday weekday_range_expr
  end

  module Day = struct
    let day_expr : Time_expr_ast.day_expr t =
      Month_day.month_day_expr
      >>| (fun x -> Time_expr_ast.Month_day x)
          <|> (Weekday.weekday_expr >>| fun x -> Time_expr_ast.Weekday x)
  end

  module Month = struct
    let human_int_month_expr : Time_expr_ast.month_expr t =
      nat_zero
      >>= fun x ->
      match Time.month_of_human_int x with
      | Ok m -> return m
      | Error () -> fail (Printf.sprintf "Invalid month: %d" x)

    let human_int_month_range_expr = range_inc_expr human_int_month_expr

    let human_int_month_ranges_expr = sep_by_comma1 human_int_month_range_expr

    let direct_pick_month_expr : Time_expr_ast.month_expr t =
      alpha_string
      >>= fun x ->
      match Time.Interpret_string.month_of_string x with
      | Ok x -> return x
      | Error _ ->
        fail (Printf.sprintf "Failed to interpret month string: %s" x)

    let direct_pick_month_range_expr = range_inc_expr direct_pick_month_expr

    let direct_pick_month_ranges_expr =
      sep_by_comma1 direct_pick_month_range_expr

    let month_expr = human_int_month_expr <|> direct_pick_month_expr

    let month_range_expr = range_inc_expr month_expr

    let month_ranges_expr = sep_by_comma1 month_range_expr
  end

  module Year = struct
    let year_expr : int t = nat_zero

    let year_range_expr = range_inc_expr year_expr

    let year_ranges_expr = sep_by_comma1 year_range_expr
  end

  module Time_points_expr = struct
    let tp_ymd_hour_minute_second =
      nat_zero
      >>= fun year ->
      hyphen *> Month.month_expr
      >>= fun month ->
      hyphen *> nat_zero
      >>= fun month_day ->
      space *> Hour_minute_second.hour_minute_second_expr
      >>= fun hour_minute_second ->
      return
        (Time_expr_ast.Year_month_day_hour_minute_second
           { year; month; month_day; hour_minute_second })

    let tp_md_hour_minute_second =
      Month.month_expr
      >>= fun month ->
      hyphen *> nat_zero
      >>= fun month_day ->
      space *> Hour_minute_second.hour_minute_second_expr
      >>= fun hour_minute_second ->
      return
        (Time_expr_ast.Month_day_hour_minute_second
           { month; month_day; hour_minute_second })

    let tp_d_hour_minute_second =
      Day.day_expr
      >>= fun day ->
      space *> Hour_minute_second.hour_minute_second_expr
      >>= fun hour_minute_second ->
      return (Time_expr_ast.Day_hour_minute_second { day; hour_minute_second })

    let tp_hour_minute_second =
      Hour_minute_second.hour_minute_second_expr
      >>= fun hour_minute_second ->
      return (Time_expr_ast.Hour_minute_second hour_minute_second)

    let tp_minute_second =
      Minute_second.minute_second_expr
      >>= fun minute_second ->
      return (Time_expr_ast.Minute_second minute_second)

    let tp_second =
      Second.second_expr >>= fun second -> return (Time_expr_ast.Second second)

    let unbounded_time_points_expr : Time_expr_ast.unbounded_time_points_expr t
      =
      choice
        [
          tp_ymd_hour_minute_second;
          tp_md_hour_minute_second;
          tp_d_hour_minute_second;
          tp_hour_minute_second;
          tp_minute_second;
          tp_second;
        ]

    let time_points_expr : Time_expr_ast.time_points_expr t =
      bound
      >>= fun bound ->
      space *> unbounded_time_points_expr >>= fun e -> return (bound, e)
  end

  module Time_slots_expr = struct
    let ts_single =
      Time_points_expr.unbounded_time_points_expr
      >>= fun start ->
      space *> to_string *> space *> Time_points_expr.unbounded_time_points_expr
      >>= fun end_exc ->
      return (Time_expr_ast.Single_time_slot { start; end_exc })

    let ts_days_hour_minute_second_ranges =
      Month_day.month_day_ranges_expr
      >>= (fun month_days ->
          space
          *> dot
          *> space
          *> Hour_minute_second.hour_minute_second_ranges_expr
          >>= fun hour_minute_second_ranges ->
          return
            (Time_expr_ast.Month_days_and_hour_minute_second_ranges
               { month_days; hour_minute_second_ranges }))
          <|> ( Weekday.weekday_ranges_expr
                >>= fun weekdays ->
                space
                *> dot
                *> space
                *> Hour_minute_second.hour_minute_second_ranges_expr
                >>= fun hour_minute_second_ranges ->
                return
                  (Time_expr_ast.Weekdays_and_hour_minute_second_ranges
                     { weekdays; hour_minute_second_ranges }) )

    let ts_months_mdays_hour_minute_second =
      Month.month_ranges_expr
      >>= fun months ->
      space *> dot *> space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      space *> dot *> space *> Hour_minute_second.hour_minute_second_ranges_expr
      >>= fun hour_minute_second_ranges ->
      return
        (Time_expr_ast.Months_and_month_days_and_hour_minute_second_ranges
           { months; month_days; hour_minute_second_ranges })

    let ts_months_wdays_hour_minute_second =
      Month.month_ranges_expr
      >>= fun months ->
      space *> dot *> space *> Weekday.weekday_ranges_expr
      >>= fun weekdays ->
      space *> dot *> space *> Hour_minute_second.hour_minute_second_ranges_expr
      >>= fun hour_minute_second_ranges ->
      return
        (Time_expr_ast.Months_and_weekdays_and_hour_minute_second_ranges
           { months; weekdays; hour_minute_second_ranges })

    let month_weekday_mode_expr =
      choice
        [
          ( first_string *> space *> nat_zero
            >>| fun n -> Some (Time_expr_ast.First_n n) );
          ( last_string *> space *> nat_zero
            >>| fun n -> Some (Time_expr_ast.Last_n n) );
        ]

    let ts_months_wday_hour_minute_second =
      Month.month_ranges_expr
      >>= fun months ->
      space *> dot *> space *> month_weekday_mode_expr
      >>= fun month_weekday_mode ->
      space *> Weekday.weekday_expr
      >>= fun weekday ->
      space *> dot *> space *> Hour_minute_second.hour_minute_second_ranges_expr
      >>= fun hour_minute_second_ranges ->
      return
        (Time_expr_ast.Months_and_weekday_and_hour_minute_second_ranges
           { months; weekday; hour_minute_second_ranges; month_weekday_mode })

    let ts_years_months_mdays_hour_minute_second =
      Year.year_ranges_expr
      >>= fun years ->
      space *> dot *> space *> Month.month_ranges_expr
      >>= fun months ->
      space *> dot *> space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      space *> dot *> space *> Hour_minute_second.hour_minute_second_ranges_expr
      >>= fun hour_minute_second_ranges ->
      return
        (Time_expr_ast
         .Years_and_months_and_month_days_and_hour_minute_second_ranges
           { years; months; month_days; hour_minute_second_ranges })

    let unbounded_time_slots_expr : Time_expr_ast.unbounded_time_slots_expr t =
      choice
        [
          ts_single;
          ts_days_hour_minute_second_ranges;
          ts_months_mdays_hour_minute_second;
          ts_months_wdays_hour_minute_second;
          ts_months_wday_hour_minute_second;
          ts_years_months_mdays_hour_minute_second;
        ]

    let time_slots_expr : Time_expr_ast.time_slots_expr t =
      bound
      >>= fun bound ->
      space *> unbounded_time_slots_expr >>= fun e -> return (bound, e)
  end

  let time_expr =
    Time_points_expr.time_points_expr
    >>| (fun e -> Time_expr_ast.Time_points_expr e)
        <|> ( Time_slots_expr.time_slots_expr
              >>| fun e -> Time_expr_ast.Time_slots_expr e )

  let of_string (s : string) : (Time_expr_ast.t, string) result =
    parse_string (time_expr <* end_of_input) s

  let time_points_expr_of_string (s : string) :
    (Time_expr_ast.time_points_expr, string) result =
    match of_string s with
    | Ok (Time_points_expr e) -> Ok e
    | Ok (Time_slots_expr _) ->
      Error "String translates to time slots expression"
    | Error msg -> Error msg

  let time_slots_expr_of_string (s : string) :
    (Time_expr_ast.time_slots_expr, string) result =
    match of_string s with
    | Ok (Time_points_expr _) ->
      Error "String translates to time point expression"
    | Ok (Time_slots_expr e) -> Ok e
    | Error msg -> Error msg
end

module To_time_pattern_lossy = struct
  module Second = struct
    let update_time_pattern_using_second_expr (e : Time_expr_ast.second_expr)
        (base : Time_pattern.t) : Time_pattern.t =
      if Time.check_second ~second:e then { base with seconds = [ e ] }
      else
        raise (Invalid_time_expr (Printf.sprintf "Invalid second: XX:XX:%d" e))

    let time_range_pattern_of_second_range_expr_and_base_time_pattern
        (e : Time_expr_ast.second_range_expr) (base : Time_pattern.t) :
      Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_second_expr x base,
            update_time_pattern_using_second_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_second_expr x base,
            update_time_pattern_using_second_expr y base )

    let time_range_patterns_of_second_ranges_and_base_time_pattern
        (l : Time_expr_ast.second_range_expr list) (base : Time_pattern.t) :
      Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_second_range_expr_and_base_time_pattern e
            base)
  end

  module Minute_second = struct
    let update_time_pattern_using_minute_second_expr
        (e : Time_expr_ast.minute_second_expr) (base : Time_pattern.t) :
      Time_pattern.t =
      if Time.check_minute_second ~minute:e.minute ~second:e.second then
        { base with minutes = [ e.minute ] }
      else
        raise
          (Invalid_time_expr
             (Printf.sprintf "Invalid minute second: XX:%d:%d" e.minute
                e.second))

    let time_range_pattern_of_minute_second_range_expr_and_base_time_pattern
        (e : Time_expr_ast.minute_second_range_expr) (base : Time_pattern.t) :
      Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_minute_second_expr x base,
            update_time_pattern_using_minute_second_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_minute_second_expr x base,
            update_time_pattern_using_minute_second_expr y base )

    let time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
        (l : Time_expr_ast.minute_second_range_expr list)
        (base : Time_pattern.t) : Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_minute_second_range_expr_and_base_time_pattern
            e base)
  end

  module Hour_minute_second = struct
    let update_time_pattern_using_hour_minute_second_expr
        (e : Time_expr_ast.hour_minute_second_expr) (base : Time_pattern.t) :
      Time_pattern.t =
      if
        Time.check_hour_minute_second ~hour:e.hour ~minute:e.minute
          ~second:e.second
      then { base with hours = [ e.hour ]; minutes = [ e.minute ] }
      else
        raise
          (Invalid_time_expr
             (Printf.sprintf "Invalid hour minute: %d:%d" e.hour e.minute))

    let time_range_pattern_of_hour_minute_second_range_expr_and_base_time_pattern
        (e : Time_expr_ast.hour_minute_second_range_expr)
        (base : Time_pattern.t) : Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_hour_minute_second_expr x base,
            update_time_pattern_using_hour_minute_second_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_hour_minute_second_expr x base,
            update_time_pattern_using_hour_minute_second_expr y base )

    let time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
        (l : Time_expr_ast.hour_minute_second_range_expr list)
        (base : Time_pattern.t) : Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_hour_minute_second_range_expr_and_base_time_pattern
            e base)
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
  end

  module Day = struct
    let update_time_pattern_using_day_expr (e : Time_expr_ast.day_expr)
        (base : Time_pattern.t) : Time_pattern.t =
      match e with
      | Month_day e -> Month_day.update_time_pattern_using_month_day_expr e base
      | Weekday e -> Weekday.update_time_pattern_using_weekday_expr e base
  end

  module Month = struct
    let update_time_pattern_using_month_expr (e : Time_expr_ast.month_expr)
        (base : Time_pattern.t) : Time_pattern.t =
      { base with months = [ e ] }

    let time_pattern_of_month_expr x =
      update_time_pattern_using_month_expr x Time_pattern.empty

    let time_patterns_of_months_and_base_time_pattern (l : Time.month list)
        (base : Time_pattern.t) : Time_pattern.t Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_month_expr e base)
  end

  module Year = struct
    let update_time_pattern_using_year_expr (e : Time_expr_ast.year_expr)
        (base : Time_pattern.t) : Time_pattern.t =
      { base with years = [ e ] }

    let time_pattern_of_year_expr x =
      update_time_pattern_using_year_expr x Time_pattern.empty
  end

  let time_pattern_of_unbounded_time_points_expr
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (e : Time_expr_ast.unbounded_time_points_expr) :
    (Time_pattern.t, string) result =
    try
      match resolve_unbounded_time_points_expr ~f_resolve_tpe_name e with
      | Error msg -> Error msg
      | Ok e ->
        Ok
          ( match e with
            | Tpe_name _ -> failwith "Unexpected case"
            | Year_month_day_hour_minute_second
                { year; month; month_day; hour_minute_second } ->
              Time_pattern.empty
              |> Year.update_time_pattern_using_year_expr year
              |> Month.update_time_pattern_using_month_expr month
              |> Month_day.update_time_pattern_using_month_day_expr month_day
              |> Hour_minute_second
                 .update_time_pattern_using_hour_minute_second_expr
                hour_minute_second
            | Month_day_hour_minute_second
                { month; month_day; hour_minute_second } ->
              Time_pattern.empty
              |> Month.update_time_pattern_using_month_expr month
              |> Month_day.update_time_pattern_using_month_day_expr month_day
              |> Hour_minute_second
                 .update_time_pattern_using_hour_minute_second_expr
                hour_minute_second
            | Day_hour_minute_second { day; hour_minute_second } ->
              Time_pattern.empty
              |> Day.update_time_pattern_using_day_expr day
              |> Hour_minute_second
                 .update_time_pattern_using_hour_minute_second_expr
                hour_minute_second
            | Hour_minute_second hour_minute_second ->
              Hour_minute_second
              .update_time_pattern_using_hour_minute_second_expr
                hour_minute_second Time_pattern.empty
            | Minute_second second ->
              Minute_second.update_time_pattern_using_minute_second_expr
                second Time_pattern.empty
            | Second second ->
              Second.update_time_pattern_using_second_expr second
                Time_pattern.empty )
    with Invalid_time_expr msg -> Error msg

  let time_pattern_of_time_points_expr
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      ((_, e) : Time_expr_ast.time_points_expr) :
    (Time_pattern.t, string) result =
    time_pattern_of_unbounded_time_points_expr ~f_resolve_tpe_name e

  let time_range_patterns_of_unbounded_time_slots_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (e : Time_expr_ast.unbounded_time_slots_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    try
      match
        resolve_unbounded_time_slots_expr ~f_resolve_tse_name
          ~f_resolve_tpe_name e
      with
      | Error msg -> Error msg
      | Ok e ->
        Ok
          ( match e with
            | Tse_name _ -> failwith "Unexpected case"
            | Single_time_slot { start; end_exc } -> (
                match
                  time_pattern_of_unbounded_time_points_expr ~f_resolve_tpe_name
                    start
                with
                | Error msg -> raise (Invalid_time_expr msg)
                | Ok start -> (
                    match
                      time_pattern_of_unbounded_time_points_expr
                        ~f_resolve_tpe_name end_exc
                    with
                    | Error msg -> raise (Invalid_time_expr msg)
                    | Ok end_exc -> [ `Range_exc (start, end_exc) ] ) )
            | Month_days_and_hour_minute_second_ranges
                { month_days; hour_minute_second_ranges } ->
              (* check_hour_minute_second_ranges hour_minute_second_ranges; *)
              Time.flatten_month_day_ranges month_days
              |> Seq.map Month_day.time_pattern_of_month_day_expr
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq
            | Weekdays_and_hour_minute_second_ranges
                { weekdays; hour_minute_second_ranges } ->
              Time.flatten_weekday_ranges weekdays
              |> Seq.map Weekday.time_pattern_of_weekday_expr
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq
            | Months_and_month_days_and_hour_minute_second_ranges
                { months; month_days; hour_minute_second_ranges } ->
              let month_days =
                Time.flatten_month_day_ranges month_days |> List.of_seq
              in
              Time.flatten_month_ranges months
              |> Seq.map Month.time_pattern_of_month_expr
              |> Seq.flat_map
                (Month_day
                 .time_patterns_of_month_days_and_base_time_pattern
                   month_days)
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq
            | Months_and_weekdays_and_hour_minute_second_ranges
                { months; weekdays; hour_minute_second_ranges } ->
              let weekdays =
                Time.flatten_weekday_ranges weekdays |> List.of_seq
              in
              Time.flatten_month_ranges months
              |> Seq.map Month.time_pattern_of_month_expr
              |> Seq.flat_map
                (Weekday.time_patterns_of_weekdays_and_base_time_pattern
                   weekdays)
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq
            | Months_and_weekday_and_hour_minute_second_ranges
                {
                  months;
                  weekday;
                  hour_minute_second_ranges;
                  month_weekday_mode = _;
                } ->
              Time.flatten_month_ranges months
              |> Seq.map Month.time_pattern_of_month_expr
              |> Seq.map
                (Weekday.update_time_pattern_using_weekday_expr weekday)
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq
            | Years_and_months_and_month_days_and_hour_minute_second_ranges
                { years; months; month_days; hour_minute_second_ranges } ->
              let months = Time.flatten_month_ranges months |> List.of_seq in
              let month_days =
                Time.flatten_month_day_ranges month_days |> List.of_seq
              in
              Time.flatten_year_ranges years
              |> Seq.map Year.time_pattern_of_year_expr
              |> Seq.flat_map
                (Month.time_patterns_of_months_and_base_time_pattern
                   months)
              |> Seq.flat_map
                (Month_day
                 .time_patterns_of_month_days_and_base_time_pattern
                   month_days)
              |> Seq.flat_map
                (Hour_minute_second
                 .time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
                   hour_minute_second_ranges)
              |> List.of_seq )
    with Invalid_time_expr msg -> Error msg

  let time_range_patterns_of_time_slots_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      ((_, e) : Time_expr_ast.time_slots_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    time_range_patterns_of_unbounded_time_slots_expr ~f_resolve_tse_name
      ~f_resolve_tpe_name e

  let single_or_ranges_of_time_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name) (e : Time_expr_ast.t) :
    (Time_pattern.single_or_ranges, string) result =
    match e with
    | Time_expr_ast.Time_points_expr e -> (
        match time_pattern_of_time_points_expr ~f_resolve_tpe_name e with
        | Ok x -> Ok (Single_time_pattern x)
        | Error msg -> Error msg )
    | Time_expr_ast.Time_slots_expr e -> (
        match
          time_range_patterns_of_time_slots_expr ~f_resolve_tse_name
            ~f_resolve_tpe_name e
        with
        | Ok x -> Ok (Time_range_patterns x)
        | Error msg -> Error msg )

  let time_pattern_of_time_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name) (e : Time_expr_ast.t) :
    (Time_pattern.t, string) result =
    match
      single_or_ranges_of_time_expr ~f_resolve_tse_name ~f_resolve_tpe_name e
    with
    | Ok (Time_pattern.Single_time_pattern x) -> Ok x
    | Ok (Time_pattern.Time_range_patterns _) ->
      Error "Time expression translates to time pattern pairs"
    | Error msg -> Error msg

  let time_range_patterns_of_time_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name) (e : Time_expr_ast.t) :
    (Time_pattern.time_range_pattern list, string) result =
    match
      single_or_ranges_of_time_expr ~f_resolve_tse_name ~f_resolve_tpe_name e
    with
    | Ok (Time_pattern.Single_time_pattern _) ->
      Error "Time expression translates to single time pattern"
    | Ok (Time_pattern.Time_range_patterns l) -> Ok l
    | Error msg -> Error msg

  let time_range_pattern_of_time_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name) (e : Time_expr_ast.t) :
    (Time_pattern.time_range_pattern, string) result =
    match
      time_range_patterns_of_time_expr ~f_resolve_tse_name ~f_resolve_tpe_name e
    with
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

module Time_points_expr = struct
  let next_match_unix_time ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (search_param : search_param) (e : Time_expr_ast.time_points_expr) :
    (int64 option, string) result =
    match
      To_time_pattern_lossy.time_pattern_of_time_points_expr ~f_resolve_tpe_name
        e
    with
    | Error msg -> Error msg
    | Ok pat ->
      Ok (Time_pattern.Single_pattern.next_match_unix_time search_param pat)

  let matching_unix_times ?(force_bound : Time_expr_ast.bound option)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (search_param : search_param)
      ((bound, e) : Time_expr_ast.time_points_expr) :
    (int64 Seq.t, string) result =
    match
      To_time_pattern_lossy.time_pattern_of_unbounded_time_points_expr
        ~f_resolve_tpe_name e
    with
    | Error msg -> Error msg
    | Ok pat -> (
        match resolve_unbounded_time_points_expr ~f_resolve_tpe_name e with
        | Error msg -> Error msg
        | Ok e ->
          let selector =
            match e with
            | Tpe_name _ -> failwith "Unexpected case"
            | Year_month_day_hour_minute_second _
            | Month_day_hour_minute_second _ | Day_hour_minute_second _
            | Hour_minute_second _ | Minute_second _ | Second _ -> (
                match Option.value ~default:bound force_bound with
                | `Next -> OSeq.take 1
                | `Every -> fun x -> x )
          in
          Time_pattern.Single_pattern.matching_time_slots search_param pat
          |> Seq.map (fun (x, _) -> x)
          |> selector
          |> Result.ok )
end

module Time_slots_expr = struct
  let get_first_or_last_n_matches_of_same_month_tm_pair_seq
      ~(first_or_last : [ `First | `Last ]) ~(n : int)
      (s : (Unix.tm * Unix.tm) Seq.t) : (Unix.tm * Unix.tm) Seq.t =
    let flush_acc first_or_last (n : int) (acc : (Unix.tm * Unix.tm) list) :
      (Unix.tm * Unix.tm) Seq.t =
      ( match first_or_last with
        | `First -> acc |> List.rev |> Misc_utils.take_first_n_list n
        | `Last -> acc |> List.rev |> Misc_utils.take_last_n_list n )
      |> List.to_seq
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

  let matching_time_slots ?(force_bound : Time_expr_ast.bound option)
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (search_param : search_param) ((bound, e) : Time_expr_ast.time_slots_expr)
    : (Time_slot_ds.t Seq.t, string) result =
    match
      resolve_unbounded_time_slots_expr ~f_resolve_tse_name ~f_resolve_tpe_name
        e
    with
    | Error msg -> Error msg
    | Ok e -> (
        let list_selector =
          match e with
          | Tse_name _ -> failwith "Unexpected case"
          | Single_time_slot _ | Month_days_and_hour_minute_second_ranges _
          | Weekdays_and_hour_minute_second_ranges _
          | Months_and_month_days_and_hour_minute_second_ranges _
          | Months_and_weekdays_and_hour_minute_second_ranges _
          | Years_and_months_and_month_days_and_hour_minute_second_ranges _ -> (
              match Option.value ~default:bound force_bound with
              | `Next -> OSeq.take 1
              | `Every -> fun x -> x )
          | Months_and_weekday_and_hour_minute_second_ranges _ -> (
              match Option.value ~default:bound force_bound with
              | `Next -> OSeq.take 4
              | `Every -> fun x -> x )
        in
        let flat_selector =
          match e with
          | Tse_name _ -> failwith "Unexpected case"
          | Single_time_slot _ | Month_days_and_hour_minute_second_ranges _
          | Weekdays_and_hour_minute_second_ranges _
          | Months_and_month_days_and_hour_minute_second_ranges _
          | Months_and_weekdays_and_hour_minute_second_ranges _
          | Years_and_months_and_month_days_and_hour_minute_second_ranges _ ->
            fun x -> x
          | Months_and_weekday_and_hour_minute_second_ranges
              { month_weekday_mode; _ } -> (
              match month_weekday_mode with
              | None -> fun x -> x
              | Some (First_n n) ->
                get_first_or_last_n_matches_of_same_month
                  ~first_or_last:`First ~n search_param
              | Some (Last_n n) ->
                get_first_or_last_n_matches_of_same_month ~first_or_last:`Last
                  ~n search_param )
        in
        match
          To_time_pattern_lossy.time_range_patterns_of_unbounded_time_slots_expr
            ~f_resolve_tse_name ~f_resolve_tpe_name e
        with
        | Error msg -> Error msg
        | Ok l ->
          Time_pattern.Range_pattern
          .matching_time_slots_round_robin_non_decreasing search_param l
          |> list_selector
          |> Seq.flat_map List.to_seq
          |> flat_selector
          |> Result.ok )

  let next_match_time_slot ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (search_param : search_param) (e : Time_expr_ast.time_slots_expr) :
    ((int64 * int64) option, string) result =
    match
      matching_time_slots ~f_resolve_tse_name ~f_resolve_tpe_name search_param e
    with
    | Error msg -> Error msg
    | Ok seq -> (
        match seq () with Seq.Nil -> Ok None | Seq.Cons (x, _) -> Ok (Some x) )
end
