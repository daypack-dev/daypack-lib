type 'a range_expr =
  | Single of 'a
  | Ranged of 'a * 'a

type hour_minute_expr = {
  hour : int;
  minute : int;
}

type hour_minutes_expr =
  | Hour_minute_range of hour_minute_expr range_expr

type day_expr =
  | Weekday of Time.weekday
  | Month_day of int

type days_expr =
  | Next_n_days of int
  | Every_x_day of day_expr
  | Day_list of day_expr list
  | Day_range of day_expr range_expr

type month_expr = Time.month

type months_expr =
  | Next_n_months of int
  | Every_x_month of month_expr
  | Month_list of month_expr list
  | Month_range of month_expr range_expr

type year_expr = int

type time_point_expr =
  | Year_month_day_hour_minute of {
      year : year_expr;
      month : months_expr;
      day : day_expr;
      hour_minute : hour_minute_expr;
    }
  | Month_day_hour_minute of {
      month : months_expr;
      day : day_expr;
      hour_minute : hour_minute_expr;
    }
  | Day_hour_minute of {
      day : day_expr;
      hour_minute : hour_minute_expr;
    }
  | Hour_minute of hour_minute_expr

type time_range_expr =
  time_point_expr range_expr

type complex_time_range_expr =
  | Hour_minutes_of_days of {
      hour_minutes : hour_minutes_expr;
      days : days_expr;
    }

(* let time_range_exprs_of_complex_time_range_expr (e : complex_time_range_expr) : time_range_expr list =
 *   match e with
 *   | Hour_minutes_of_days { hour_minutes; days } -> *)
