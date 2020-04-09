type 'a range_expr =
  | Range_inc of 'a * 'a
  | Range_exc of 'a * 'a

type hour_minute_expr = {
  hour : int;
  minute : int;
}

type hour_minutes_expr = hour_minute_expr range_expr

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
  | Hour_minutes_of_day_list of {
      hour_minutes : hour_minutes_expr;
      days : day_expr list;
    }
  | Hour_minutes_of_day_range of {
      hour_minutes : hour_minutes_expr;
      days : day_range_expr;
    }
  (* | Hour_minutes_of_next_n_days of {
   *     hour_minutes : hour_minutes_expr;
   *     day_count : int;
   *   } *)
  | Hour_minutes_of_month_day_list_of_month_list of {
      hour_minutes : hour_minutes_expr;
      month_days : int list;
      months : month_expr list;
    }
  | Hour_minutes_of_every_weekday_list_of_month_list of {
      hour_minutes : hour_minutes_expr;
      weekdays : Time.weekday list;
      months : month_expr list;
    }

type t =
  | Time_point_expr of time_point_expr
  | Time_slots_expr of time_slots_expr
