type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

val map :
  f_inc:('a * 'a -> 'b * 'b) -> f_exc:('a * 'a -> 'b * 'b) -> 'a t -> 'b t

module Merge : sig
  val merge_big : to_int64:('a -> int64) -> 'a t -> 'a t -> 'a t option

  val merge : to_int:('a -> int) -> 'a t -> 'a t -> 'a t option
end
