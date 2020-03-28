type search_type =
  | Time_slots of Time_slot_ds.t list
  | Years_ahead_start_int64 of {
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_tm of {
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
}

val matching_tm_seq :
  search_years_ahead:int -> start:Unix.tm -> t -> Unix.tm Seq.t

val matching_time_slots : t -> search_type -> Time_slot_ds.t Seq.t

val next_match_tm :
  search_years_ahead:int -> start:Unix.tm -> t -> Unix.tm option

val next_match_int64 :
  search_years_ahead:int -> start:int64 -> t -> int64 option

val next_match_time_slot :
  search_years_ahead:int -> start:int64 -> t -> (int64 * int64) option

val next_match_time_slot_paired_pattern :
  search_years_ahead:int -> start:int64 -> t -> t -> (int64 * int64) option

module Interpret_string : sig
  val of_string : string -> (t, string) result
end

module Equal : sig
  val equal : t -> t -> bool
end

module Print : sig
  val debug_string_of_days : days -> string

  val debug_string_of_pattern :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_print_pattern : ?indent_level:int -> t -> unit
end

module Serialize : sig
  val pack_days : days -> Time_pattern_t.days

  val pack_pattern : t -> Time_pattern_t.t
end

module Deserialize : sig
  val unpack_days : Time_pattern_t.days -> days

  val unpack_pattern : Time_pattern_t.t -> t
end
