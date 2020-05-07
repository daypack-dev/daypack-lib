type t = int64 * int64

val compare : t -> t -> int

val to_string : t -> string

val join : t -> t -> t option

val overlap_of_a_over_b : a:t -> b:t -> t option * t option * t option

module Check : sig
  val check_time_slot : t -> bool
end

module Serialize : sig
  val pack_time_slot : int64 * int64 -> (int32 * int32) * (int32 * int32)
end

module Deserialize : sig
  val unpack_time_slot : (int32 * int32) * (int32 * int32) -> int64 * int64
end
