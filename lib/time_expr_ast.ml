type 'a range_expr =
  | Range_inc of 'a * 'a
  | Range_exc of 'a * 'a

type hour_minute_mode = Hour_in_AM | Hour_in_PM | Hour_in_24_hours

type hour_minute_expr = {
  hour : int;
  minute : int;
  mode : hour_minute_mode;
}

type hour_minute_range_expr = hour_minute_expr range_expr

type day_expr =
  | Weekday of Time.weekday
  | Month_day of int

type day_range_expr =
  | Weekday_range of Time.weekday * Time.weekday
  | Month_day_range of int * int

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

type time_slots_expr =
  | Single_time_slot of time_point_expr * time_point_expr
  | Day_list_and_hour_minutes of {
      hour_minutes : hour_minute_range_expr list;
      days : day_expr list;
    }
  | Day_range_and_hour_minutes of {
      hour_minutes : hour_minute_range_expr list;
      days : day_range_expr;
    }
  | Month_list_and_month_day_list_and_hour_minutes of {
      hour_minutes : hour_minute_range_expr list;
      month_days : int list;
      months : month_expr list;
    }
  | Month_list_and_weekday_list_and_hour_minutes of {
      hour_minutes : hour_minute_range_expr list;
      weekdays : Time.weekday list;
      months : month_expr list;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
