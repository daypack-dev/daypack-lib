type hms_expr = {
  hour : int;
  minute : int;
  second : int;
}

type hms_range_expr = hms_expr Range.t

type day_expr = Time_expr_ast.day_expr

type day_range_expr = Time_expr_ast.day_range_expr

type month_expr = Time.month

type year_expr = int

type match_mode = Time_expr_ast.match_mode

type time_point_expr =
  | Year_month_day_hms of {
      year : year_expr;
      month : month_expr;
      month_day : int;
      hms : hms_expr;
      match_mode : match_mode;
    }
  | Month_day_hms of {
      month : month_expr;
      month_day : int;
      hms : hms_expr;
      match_mode : match_mode;
    }
  | Day_hms of {
      day : day_expr;
      hms : hms_expr;
      match_mode : match_mode;
    }
  | Hms of {
      hms : hms_expr;
      match_mode : match_mode;
    }

type month_weekday_mode = Time_expr_ast.month_weekday_mode

type time_slots_expr =
  | Single_time_slot of {
      start : time_point_expr;
      end_exc : time_point_expr;
      match_mode : match_mode;
    }
  | Month_days_and_hms_ranges of {
      month_days : int Range.t list;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
    }
  | Weekdays_and_hms_ranges of {
      weekdays : Time.weekday Range.t list;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_month_days_and_hms_ranges of {
      months : month_expr Range.t list;
      month_days : int Range.t list;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_weekdays_and_hms_ranges of {
      months : month_expr Range.t list;
      weekdays : Time.weekday Range.t list;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_weekday_and_hms_ranges of {
      months : month_expr Range.t list;
      weekday : Time.weekday;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
      month_weekday_mode : month_weekday_mode option;
    }
  | Years_and_months_and_month_days_and_hms_ranges of {
      years : int Range.t list;
      months : month_expr Range.t list;
      month_days : int Range.t list;
      hms_ranges : hms_range_expr list;
      match_mode : match_mode;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
