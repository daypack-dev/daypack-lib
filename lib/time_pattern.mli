type search_param =
  | Time_slots of {
      search_in_time_zone : Time.time_zone;
      time_slots : Time_slot_ds.t list;
    }
  | Years_ahead_start_unix_time of {
      search_in_time_zone : Time.time_zone;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_tm of {
      search_in_time_zone : Time.time_zone;
      time_zone_of_tm : Time.time_zone;
      start : Unix.tm;
      search_years_ahead : int;
    }

type days =
  [ `Weekdays of Time.weekday list
  | `Month_days of int list
  ]

type t = {
  years : int list;
  months : Time.month list;
  days : days;
  hours : int list;
  minutes : int list;
  seconds : int list;
}

type time_range_pattern = t Range.t

type single_or_ranges =
  | Single_time_pattern of t
  | Time_range_patterns of time_range_pattern list

val empty : t

val matching_tm_seq : search_param -> t -> Unix.tm Seq.t

val matching_time_slots : search_param -> t -> Time_slot_ds.t Seq.t

val next_match_tm : search_param -> t -> Unix.tm option

val next_match_unix_time : search_param -> t -> int64 option

val next_match_time_slot : search_param -> t -> (int64 * int64) option

val matching_time_slots_time_range_pattern :
  search_param -> time_range_pattern -> Time_slot_ds.t Seq.t

val next_match_time_slot_time_range_pattern :
  search_param -> time_range_pattern -> (int64 * int64) option

val matching_time_slots_time_range_patterns :
  search_param -> time_range_pattern list -> Time_slot_ds.t Seq.t

val next_match_time_slot_time_range_patterns :
  search_param -> time_range_pattern list -> (int64 * int64) option

val matching_time_slots_single_or_ranges :
  search_param -> single_or_ranges -> Time_slot_ds.t Seq.t

val next_match_time_slot_single_or_ranges :
  search_param -> single_or_ranges -> Time_slot_ds.t option

module Equal : sig
  val equal : t -> t -> bool
end

module To_string : sig
  val debug_string_of_days : days -> string

  val debug_string_of_time_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string
  val debug_string_of_time_range_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> time_range_pattern -> string
  val debug_string_of_single_or_ranges :
    ?indent_level:int -> ?buffer:Buffer.t -> single_or_ranges -> string
end

module Print : sig
  val debug_print_time_pattern : ?indent_level:int -> t -> unit
  val debug_print_time_range_pattern : ?indent_level:int -> time_range_pattern -> unit
  val debug_print_single_or_ranges : ?indent_level:int -> single_or_ranges -> unit
end

module Serialize : sig
  val pack_days : days -> Time_pattern_t.days

  val pack_pattern : t -> Time_pattern_t.t
end

module Deserialize : sig
  val unpack_days : Time_pattern_t.days -> days

  val unpack_pattern : Time_pattern_t.t -> t
end
