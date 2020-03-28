type t = int64 * int64

val normalize : ?skip_filter:bool -> ?skip_sort:bool -> t Seq.t -> t Seq.t

val normalize_list_in_seq_out :
  ?skip_filter:bool -> ?skip_sort:bool -> t list -> t Seq.t

val slice : ?start:int64 -> ?end_exc:int64 -> t Seq.t -> t Seq.t

val slice_rev : ?start:int64 -> ?end_exc:int64 -> t Seq.t -> t Seq.t

val invert : start:int64 -> end_exc:int64 -> t Seq.t -> t Seq.t

val relative_complement : mem_of:t Seq.t -> not_mem_of:t Seq.t -> t Seq.t

val merge : t Seq.t -> t Seq.t -> t Seq.t

val intersect : t Seq.t -> t Seq.t -> t Seq.t

val union : t Seq.t -> t Seq.t -> t Seq.t

val chunk : chunk_size:int64 -> ?drop_partial:bool -> t Seq.t -> t Seq.t

val sum_length : t Seq.t -> int64

val sum_length_list : t list -> int64

val min_start_and_max_end_exc : t Seq.t -> (int64 * int64) option

val min_start_and_max_end_exc_list : t list -> (int64 * int64) option

val shift_list : offset:int64 -> t list -> t list

val equal : t list -> t list -> bool

val a_is_subset_of_b : a:t Seq.t -> b:t Seq.t -> bool

val to_string : t -> string

module Check : sig
  val check_time_slot : t -> bool
end

module Serialize : sig
  val pack_time_slot : int64 * int64 -> (int32 * int32) * (int32 * int32)

  val pack_time_slots :
    (int64 * int64) list -> ((int32 * int32) * (int32 * int32)) list
end

module Deserialize : sig
  val unpack_time_slot : (int32 * int32) * (int32 * int32) -> int64 * int64

  val unpack_time_slots :
    ((int32 * int32) * (int32 * int32)) list -> (int64 * int64) list
end
