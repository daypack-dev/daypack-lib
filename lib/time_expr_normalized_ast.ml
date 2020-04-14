type hour_minute_expr = {
  hour : int;
  minute : int;
}

type hour_minute_range_expr = hour_minute_expr Range.t

type day_expr = Time_expr_ast.day_expr

type day_range_expr = Time_expr_ast.day_range_expr

type month_expr = Time.month

type year_expr = int

type time_point_expr =
  | Year_month_day_hour_minute of {
      year : year_expr;
      month : month_expr;
      month_day : int;
      hour_minute : hour_minute_expr;
    }
  | Month_day_hour_minute of {
      month : month_expr;
      month_day : int;
      hour_minute : hour_minute_expr;
    }
  | Day_hour_minute of {
      day : day_expr;
      hour_minute : hour_minute_expr;
    }
  | Hour_minute of hour_minute_expr

type month_weekday_mode = Time_expr_ast.month_weekday_mode

type time_slots_expr =
  | Single_time_slot of time_point_expr * time_point_expr
  | Day_list_and_hour_minutes of {
      day_list : day_expr list;
      day_range : day_expr Range.t;
      hour_minutes : hour_minute_range_expr list;
    }
  | Months_and_month_days_and_hour_minutes of {
      month_list : month_expr list;
      month_range : month_expr Range.t;
      month_day_list : int list;
      month_day_range : int Range.t;
      hour_minutes : hour_minute_range_expr list;
    }
  | Months_and_weekdays_and_hour_minutes of {
      month_list : month_expr list;
      month_range : month_expr Range.t;
      weekdays : Time.weekday list;
      weekday_range : Time.weekday Range.t;
      month_weekday_mode : month_weekday_mode;
      hour_minutes : hour_minute_range_expr list;
    }
  | Years_and_months_and_month_days_and_hour_minutes of {
      year_list : int list;
      year_range : int Range.t;
      month_list : month_expr list;
      month_range : month_expr Range.t;
      month_day_list : int list;
      month_day_range : int Range.t;
      hour_minutes : hour_minute_range_expr list;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
