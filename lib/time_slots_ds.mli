module Normalize : sig
  val normalize :
    ?skip_filter:bool ->
    ?skip_sort:bool ->
    Time_slot_ds.t Seq.t ->
    Time_slot_ds.t Seq.t

  val normalize_list_in_seq_out :
    ?skip_filter:bool ->
    ?skip_sort:bool ->
    Time_slot_ds.t list ->
    Time_slot_ds.t Seq.t
end

val seq_of_unix_time_seq :
  ?skip_sort:bool -> int64 Seq.t -> Time_slot_ds.t Seq.t

module Slice : sig
  val slice :
    ?start:int64 ->
    ?end_exc:int64 ->
    Time_slot_ds.t Seq.t ->
    Time_slot_ds.t Seq.t

  val slice_rev :
    ?start:int64 ->
    ?end_exc:int64 ->
    Time_slot_ds.t Seq.t ->
    Time_slot_ds.t Seq.t
end

val invert :
  start:int64 -> end_exc:int64 -> Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t

val relative_complement :
  mem_of:Time_slot_ds.t Seq.t ->
  not_mem_of:Time_slot_ds.t Seq.t ->
  Time_slot_ds.t Seq.t

module Merge : sig
  val merge :
    Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t

  val merge_multi_seq : Time_slot_ds.t Seq.t Seq.t -> Time_slot_ds.t Seq.t

  val merge_multi_list : Time_slot_ds.t Seq.t list -> Time_slot_ds.t Seq.t
end

module Round_robin : sig
  val collect_round_robin_non_decreasing :
    Time_slot_ds.t Seq.t list -> Time_slot_ds.t option list Seq.t

  val merge_multi_seq_round_robin_non_decreasing :
    Time_slot_ds.t Seq.t Seq.t -> Time_slot_ds.t Seq.t

  val merge_multi_list_round_robin_non_decreasing :
    Time_slot_ds.t Seq.t list -> Time_slot_ds.t Seq.t
end

val intersect :
  Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t

module Union : sig
  val union :
    Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t -> Time_slot_ds.t Seq.t

  val union_multi_seq : Time_slot_ds.t Seq.t Seq.t -> Time_slot_ds.t Seq.t

  val union_multi_list : Time_slot_ds.t Seq.t list -> Time_slot_ds.t Seq.t
end

val chunk :
  chunk_size:int64 ->
  ?drop_partial:bool ->
  Time_slot_ds.t Seq.t ->
  Time_slot_ds.t Seq.t

module Sum : sig
  val sum_length : Time_slot_ds.t Seq.t -> int64

  val sum_length_list : Time_slot_ds.t list -> int64
end

module Bound : sig
  val min_start_and_max_end_exc : Time_slot_ds.t Seq.t -> (int64 * int64) option

  val min_start_and_max_end_exc_list :
    Time_slot_ds.t list -> (int64 * int64) option
end

val shift_list : offset:int64 -> Time_slot_ds.t list -> Time_slot_ds.t list

val equal : Time_slot_ds.t list -> Time_slot_ds.t list -> bool

val a_is_subset_of_b : a:Time_slot_ds.t Seq.t -> b:Time_slot_ds.t Seq.t -> bool

module Serialize : sig
  val pack_time_slots :
    (int64 * int64) list -> ((int32 * int32) * (int32 * int32)) list
end

module Deserialize : sig
  val unpack_time_slots :
    ((int32 * int32) * (int32 * int32)) list -> (int64 * int64) list
end
