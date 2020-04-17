type hour_minute_expr = {
  hour : int;
  minute : int;
}

type hour_minute_range_expr = hour_minute_expr Range.t

type day_expr = Time_expr_ast.day_expr

type day_range_expr = Time_expr_ast.day_range_expr

type month_expr = Time.month

type year_expr = int

type match_mode = Time_expr_ast.match_mode

type time_point_expr =
  | Year_month_day_hour_minute of {
      year : year_expr;
      month : month_expr;
      month_day : int;
      hour_minute : hour_minute_expr;
      match_mode : match_mode;
    }
  | Month_day_hour_minute of {
      month : month_expr;
      month_day : int;
      hour_minute : hour_minute_expr;
      match_mode : match_mode;
    }
  | Day_hour_minute of {
      day : day_expr;
      hour_minute : hour_minute_expr;
      match_mode : match_mode;
    }
  | Hour_minute of {
      hour_minute : hour_minute_expr;
      match_mode : match_mode;
    }

type month_weekday_mode = Time_expr_ast.month_weekday_mode

type time_slots_expr =
  | Single_time_slot of {
      start : time_point_expr;
      end_exc : time_point_expr;
      match_mode : match_mode;
    }
  | Month_days_and_hour_minutes of {
      month_days : int Range.t list;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
    }
  | Weekdays_and_hour_minutes of {
      weekdays : Time.weekday Range.t list;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_month_days_and_hour_minutes of {
      months : month_expr Range.t list;
      month_days : int Range.t list;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_weekdays_and_hour_minutes of {
      months : month_expr Range.t list;
      weekdays : Time.weekday Range.t list;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
    }
  | Months_and_weekday_and_hour_minutes of {
      months : month_expr Range.t list;
      weekday : Time.weekday;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
      month_weekday_mode : month_weekday_mode option;
    }
  | Years_and_months_and_month_days_and_hour_minutes of {
      years : int Range.t list;
      months : month_expr Range.t list;
      month_days : int Range.t list;
      hour_minutes : hour_minute_range_expr list;
      match_mode : match_mode;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
