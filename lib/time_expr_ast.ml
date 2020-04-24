type hms_mode =
  | Hour_in_AM
  | Hour_in_PM
  | Hour_in_24_hours

type hms_expr = {
  hour : int;
  minute : int;
  second : int;
  mode : hms_mode;
}

type hms_range_expr = hms_expr Range.t

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

type match_mode =
  [ `Every
  | `Next
  ]

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

type month_weekday_mode =
  | First_n of int
  | Last_n of int

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
