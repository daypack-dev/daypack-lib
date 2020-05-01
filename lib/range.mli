type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

val map :
  f_inc:('a * 'a -> 'b * 'b) ->
  f_exc:('a * 'a -> 'b * 'b) ->
  'a range ->
  'b range

val int64_range_of_range :
  to_int64:('a -> int64) ->
  'a range ->
  int64 range

val int64_exc_range_of_range :
  to_int64:('a -> int64) ->
  'a range ->
  int64 * int64

val inc_range_of_range :
  to_int64:('a -> int64) ->
  of_int64:(int64 -> 'a) ->
  'a range ->
  'a * 'a

val exc_range_of_range :
  to_int64:('a -> int64) ->
  of_int64:(int64 -> 'a) ->
  'a range ->
  'a * 'a

val join :
  to_int64:('a -> int64) ->
  of_int64:(int64 -> 'a) ->
  'a range ->
  'a range ->
  'a range option

module Flatten : sig
  val flatten_into_seq :
    ?modulo:int64 ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a range ->
    'a Seq.t

  val flatten_into_list :
    ?modulo:int64 ->
    to_int64:('a -> int64) ->
    of_int64:(int64 -> 'a) ->
    'a range ->
    'a list
end

module type B = sig
  type t

  val to_int64 : t -> int64

  val of_int64 : int64 -> t

  val modulo : int64 option
end

module type S = sig
  type t

  val int64_range_of_range : t range -> int64 range

  val int64_exc_range_of_range : t range -> int64 * int64

  val inc_range_of_range : t range -> t * t

  val exc_range_of_range : t range -> t * t

  val join : t range -> t range -> t range option

  module Flatten : sig
    val flatten_into_seq : t range -> t Seq.t

    val flatten_into_list : t range -> t list
  end
end

module Make (B : B) : S with type t := B.t
