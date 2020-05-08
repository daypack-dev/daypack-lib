module Filter : sig
  val filter_invalid : Time_slot.t Seq.t -> Time_slot.t Seq.t

  val filter_invalid_list : Time_slot.t list -> Time_slot.t list

  val filter_empty : Time_slot.t Seq.t -> Time_slot.t Seq.t

  val filter_empty_list : Time_slot.t list -> Time_slot.t list
end

module Sort : sig
  val sort_time_slots_list : Time_slot.t list -> Time_slot.t list

  val sort_time_slots : Time_slot.t Seq.t -> Time_slot.t Seq.t

  val sort_uniq_time_slots_list : Time_slot.t list -> Time_slot.t list

  val sort_uniq_time_slots : Time_slot.t Seq.t -> Time_slot.t Seq.t
end

module Normalize : sig
  val normalize :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    Time_slot.t Seq.t ->
    Time_slot.t Seq.t

  val normalize_list_in_seq_out :
    ?skip_filter_invalid:bool ->
    ?skip_filter_empty:bool ->
    ?skip_sort:bool ->
    Time_slot.t list ->
    Time_slot.t Seq.t
end

module Slice : sig
  val slice :
    ?start:int64 -> ?end_exc:int64 -> Time_slot.t Seq.t -> Time_slot.t Seq.t

  val slice_rev :
    ?start:int64 -> ?end_exc:int64 -> Time_slot.t Seq.t -> Time_slot.t Seq.t
end

val invert :
  start:int64 -> end_exc:int64 -> Time_slot.t Seq.t -> Time_slot.t Seq.t

val relative_complement :
  mem_of:Time_slot.t Seq.t -> not_mem_of:Time_slot.t Seq.t -> Time_slot.t Seq.t

module Merge : sig
  val merge : Time_slot.t Seq.t -> Time_slot.t Seq.t -> Time_slot.t Seq.t

  val merge_multi_seq : Time_slot.t Seq.t Seq.t -> Time_slot.t Seq.t

  val merge_multi_list : Time_slot.t Seq.t list -> Time_slot.t Seq.t
end

module Round_robin : sig
  val collect_round_robin_non_decreasing :
    Time_slot.t Seq.t list -> Time_slot.t option list Seq.t

  val merge_multi_seq_round_robin_non_decreasing :
    Time_slot.t Seq.t Seq.t -> Time_slot.t Seq.t

  val merge_multi_list_round_robin_non_decreasing :
    Time_slot.t Seq.t list -> Time_slot.t Seq.t
end

val intersect : Time_slot.t Seq.t -> Time_slot.t Seq.t -> Time_slot.t Seq.t

module Union : sig
  val union : Time_slot.t Seq.t -> Time_slot.t Seq.t -> Time_slot.t Seq.t

  val union_multi_seq : Time_slot.t Seq.t Seq.t -> Time_slot.t Seq.t

  val union_multi_list : Time_slot.t Seq.t list -> Time_slot.t Seq.t
end

val chunk :
  chunk_size:int64 ->
  ?drop_partial:bool ->
  Time_slot.t Seq.t ->
  Time_slot.t Seq.t

module Sum : sig
  val sum_length : Time_slot.t Seq.t -> int64

  val sum_length_list : Time_slot.t list -> int64
end

module Bound : sig
  val min_start_and_max_end_exc : Time_slot.t Seq.t -> (int64 * int64) option

  val min_start_and_max_end_exc_list :
    Time_slot.t list -> (int64 * int64) option
end

val shift_list : offset:int64 -> Time_slot.t list -> Time_slot.t list

val equal : Time_slot.t list -> Time_slot.t list -> bool

val a_is_subset_of_b : a:Time_slot.t Seq.t -> b:Time_slot.t Seq.t -> bool

val count_overlap : Time_slot.t Seq.t -> (Time_slot.t * int) Seq.t

module Serialize : sig
  val pack_time_slots :
    (int64 * int64) list -> ((int32 * int32) * (int32 * int32)) list
end

module Deserialize : sig
  val unpack_time_slots :
    ((int32 * int32) * (int32 * int32)) list -> (int64 * int64) list
end
