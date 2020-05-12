type search_param =
  | Time_slots of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      time_slots : Time_slot.t list;
    }
  | Years_ahead_start_unix_time of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_date_time of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : Time.date_time;
      search_years_ahead : int;
    }

type search_param_error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead

type t = {
  years : int list;
  months : Time.month list;
  month_days : int list;
  weekdays : Time.weekday list;
  hours : int list;
  minutes : int list;
  seconds : int list;
  unix_times : int64 list;
}

type time_pattern_error =
  | Invalid_years of int list
  | Invalid_month_days of int list
  | Invalid_hours of int list
  | Invalid_minutes of int list
  | Invalid_seconds of int list
  | Invalid_unix_times of int64 list

type error =
  | Invalid_search_param of search_param_error
  | Invalid_time_pattern of time_pattern_error

type time_range_pattern = t Range.range

type single_or_ranges =
  | Single_time_pattern of t
  | Time_range_patterns of time_range_pattern list

val search_using_tz_offset_s_of_search_param :
  search_param -> Time.tz_offset_s option

val empty : t

module Check : sig
  val check_search_param : search_param -> (unit, search_param_error) result

  val check_time_pattern : t -> (unit, time_pattern_error) result

  val check_time_range_pattern : time_range_pattern -> (unit, time_pattern_error) result
end

module Single_pattern : sig
  val matching_date_time_seq : search_param -> t -> (Time.date_time Seq.t, error) result

  val matching_time_slots : search_param -> t -> (Time_slot.t Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing :
    search_param -> t list -> (Time_slot.t list Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing_flat :
    search_param -> t list -> (Time_slot.t Seq.t, error) result

  val next_match_date_time : search_param -> t -> (Time.date_time option, error) result

  val next_match_unix_time : search_param -> t -> (int64 option, error) result

  val next_match_time_slot : search_param -> t -> ((int64 * int64) option, error) result
end

module Range_pattern : sig
  val matching_time_slots :
    search_param -> time_range_pattern -> (Time_slot.t Seq.t, error) result

  val next_match_time_slot :
    search_param -> time_range_pattern -> ((int64 * int64) option, error) result

  val matching_time_slots_multi :
    search_param -> time_range_pattern list -> (Time_slot.t Seq.t, error) result

  val next_match_time_slot_multi :
    search_param -> time_range_pattern list -> ((int64 * int64) option, error) result

  val matching_time_slots_round_robin_non_decreasing :
    search_param -> time_range_pattern list -> (Time_slot.t list Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing_flat :
    search_param -> time_range_pattern list -> (Time_slot.t Seq.t, error) result
end

module Single_or_ranges : sig
  val matching_time_slots :
    search_param -> single_or_ranges -> (Time_slot.t Seq.t, error) result

  val next_match_time_slot :
    search_param -> single_or_ranges -> (Time_slot.t option, error) result

  val matching_time_slots_round_robin_non_decreasing :
    search_param -> single_or_ranges -> (Time_slot.t list Seq.t, error) result

  val matching_time_slots_round_robin_non_decreasing_flat :
    search_param -> single_or_ranges -> (Time_slot.t Seq.t, error) result
end

module Equal : sig
  val equal : t -> t -> bool
end

module To_string : sig
  val debug_string_of_weekdays : Time.weekday list -> string

  val debug_string_of_month_days : int list -> string

  val debug_string_of_time_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_string_of_time_range_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> time_range_pattern -> string

  val debug_string_of_single_or_ranges :
    ?indent_level:int -> ?buffer:Buffer.t -> single_or_ranges -> string
end

module Print : sig
  val debug_print_time_pattern : ?indent_level:int -> t -> unit

  val debug_print_time_range_pattern :
    ?indent_level:int -> time_range_pattern -> unit

  val debug_print_single_or_ranges :
    ?indent_level:int -> single_or_ranges -> unit
end

module Serialize : sig
  val pack_pattern : t -> Time_pattern_t.t
end

module Deserialize : sig
  val unpack_pattern : Time_pattern_t.t -> t
end
