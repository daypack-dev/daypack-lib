type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

val flatten_to_seq : ?modulo:int -> of_int:(int -> 'a) -> to_int:('a -> int) -> 'a t -> 'a Seq.t

val flatten_to_list : ?modulo:int -> of_int:(int -> 'a) -> to_int:('a -> int) -> 'a t -> 'a list

val range_seq_of_seq : to_int:('a -> int) -> 'a Seq.t -> 'a t Seq.t

val range_list_of_seq : to_int:('a -> int) -> 'a Seq.t -> 'a t list

val range_seq_of_list : to_int:('a -> int) -> 'a list -> 'a t Seq.t

val range_list_of_list : to_int:('a -> int) -> 'a list -> 'a t list

val merge : to_int:('a -> int) -> 'a t -> 'a t -> 'a t option

val compress_seq : to_int:('a -> int) -> 'a t Seq.t -> 'a t Seq.t

val compress_list : to_int:('a -> int) -> 'a t list -> 'a t list
