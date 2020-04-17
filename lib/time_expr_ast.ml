type hour_minute_mode =
  | Hour_in_AM
  | Hour_in_PM
  | Hour_in_24_hours

type hour_minute_expr = {
  hour : int;
  minute : int;
  mode : hour_minute_mode;
}

type hour_minute_range_expr = hour_minute_expr Range.t

type day_expr =
  | Weekday of Time.weekday
  | Month_day of int

type day_range_expr =
  | Weekday_range of Time.weekday_range
  | Month_day_range of Time.month_day_range

(* type days_expr =
 *   | Next_n_days of int
 *   | Every_x_day of day_expr
 *   | Day_list of day_expr list
 *   | Day_range of day_expr range_expr *)

type month_expr =
  | Direct_pick_month of Time.month
  | Human_int_month of int

(* type months_expr =
 *   | Next_n_months of int
 *   | Every_x_month of month_expr
 *   | Month_list of month_expr list
 *   | Month_range of month_expr range_expr *)

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

type month_weekday_mode =
  | First_n of int
  | Last_n of int

type match_mode = [
  | `Every
  | `Next
]

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
