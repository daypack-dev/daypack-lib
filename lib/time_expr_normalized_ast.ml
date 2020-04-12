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

type time_slots_expr =
  | Single_time_slot of time_point_expr * time_point_expr
  | Day_list_and_hour_minutes of {
      days : day_expr list;
      hour_minutes : hour_minute_range_expr list;
    }
  | Day_range_and_hour_minutes of {
      days : day_range_expr;
      hour_minutes : hour_minute_range_expr list;
    }
  | Month_list_and_month_day_list_and_hour_minutes of {
      months : month_expr list;
      month_days : int list;
      hour_minutes : hour_minute_range_expr list;
    }
  | Month_list_and_weekday_list_and_hour_minutes of {
      months : month_expr list;
      weekdays : Time.weekday list;
      hour_minutes : hour_minute_range_expr list;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
