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

module Multi : sig
  module Normalize : sig
    val normalize_to_inc :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a t -> 'a * 'a

    val normalize_to_exc :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a t -> 'a * 'a

    val normalize :
      ?skip_filter:bool ->
      ?skip_sort:bool ->
      to_int64:('a -> int64) ->
      of_int64:(int64 -> 'a) ->
      'a t Seq.t ->
      'a t Seq.t
  end

  module Flatten : sig
    val flatten_into_seq_big :
      ?modulo:int64 ->
      to_int64:('a -> int64) ->
      of_int64:(int64 -> 'a) ->
      'a t ->
      'a Seq.t

    val flatten_into_list_big :
      ?modulo:int64 ->
      to_int64:('a -> int64) ->
      of_int64:(int64 -> 'a) ->
      'a t ->
      'a list

    val flatten_into_seq :
      ?modulo:int ->
      to_int:('a -> int) ->
      of_int:(int -> 'a) ->
      'a t ->
      'a Seq.t

    val flatten_into_list :
      ?modulo:int -> to_int:('a -> int) -> of_int:(int -> 'a) -> 'a t -> 'a list
  end

  module Of_seq : sig
    val range_seq_of_seq_big :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a Seq.t -> 'a t Seq.t

    val range_list_of_seq_big :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a Seq.t -> 'a t list

    val range_seq_of_seq :
      to_int:('a -> int) -> of_int:(int -> 'a) -> 'a Seq.t -> 'a t Seq.t

    val range_list_of_seq :
      to_int:('a -> int) -> of_int:(int -> 'a) -> 'a Seq.t -> 'a t list
  end

  module Of_list : sig
    val range_seq_of_list_big :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a list -> 'a t Seq.t

    val range_list_of_list_big :
      to_int64:('a -> int64) -> of_int64:(int64 -> 'a) -> 'a list -> 'a t list

    val range_seq_of_list :
      to_int:('a -> int) -> of_int:(int -> 'a) -> 'a list -> 'a t Seq.t

    val range_list_of_list :
      to_int:('a -> int) -> of_int:(int -> 'a) -> 'a list -> 'a t list
  end

  module Compress : sig
    val compress_seq_big : to_int64:('a -> int64) -> 'a t Seq.t -> 'a t Seq.t

    val compress_list_big : to_int64:('a -> int64) -> 'a t list -> 'a t list

    val compress_seq : to_int:('a -> int) -> 'a t Seq.t -> 'a t Seq.t

    val compress_list : to_int:('a -> int) -> 'a t list -> 'a t list
  end
end