type 'a range = 'a Range.range

let map = Range.map

let int_range_of_range (type a) ~(to_int : a -> int) (x : a range) : int range =
  let f (x, y) =
    (to_int x,
     to_int y)
  in
  map ~f_inc:f ~f_exc:f x

let int_exc_range_of_range (type a) ~(to_int : a -> int) (x : a range) : int * int =
  match x with
  | `Range_inc (x, y) -> (to_int x, y |> to_int |> Int.succ)
  | `Range_exc (x, y) -> (to_int x, to_int y)

let inc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a) (x : a range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y)
  | `Range_exc (x, y) -> (x, y |> to_int |> Int.pred |> of_int)

let exc_range_of_range (type a) ~(to_int : a -> int) ~(of_int : int -> a) (x : a range) : a * a =
  match x with
  | `Range_inc (x, y) -> (x, y |> to_int |> Int.succ |> of_int)
  | `Range_exc (x, y) -> (x, y)

let join (type a) ~(to_int : a -> int) ~(of_int : int -> a) (x : a range) (y : a range) : a range option =
  let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
  let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
  Range.join ~to_int64 ~of_int64 x y

module Flatten = struct
  let flatten_into_seq_internal (type a) ~(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a) (t : a range) : a Seq.t =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    match modulo with
    | None ->
      Range.Flatten.flatten_into_seq ~to_int64 ~of_int64 t
    | Some modulo ->
      let modulo = Int64.of_int modulo in
      Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64 t

  let flatten_into_seq (type a) ?(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a) (t : a range) : a Seq.t =
    flatten_into_seq_internal ~modulo ~to_int ~of_int t

  let flatten_into_list (type a) ?(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a) (t : a range) : a list =
    match modulo with
    | None ->
      flatten_into_seq ~to_int ~of_int t |> List.of_seq
    | Some modulo ->
      flatten_into_seq ~modulo ~to_int ~of_int t |> List.of_seq
end

module type B = sig
  type t

  val to_int : t -> int

  val of_int : int -> t

  val modulo : int option
end

module type S = sig
  type t

  val int_range_of_range : t range -> int range

  val int_exc_range_of_range : t range -> int * int

  val inc_range_of_range : t range -> t * t

  val exc_range_of_range : t range -> t * t

  val join : t range -> t range -> t range option

  module Flatten : sig
    val flatten_into_seq : t range -> t Seq.t

    val flatten_into_list : t range -> t list
  end
end

module Make (B : B) : S with type t := B.t = struct
  open B

  let int_range_of_range (x : t range) : int range =
    int_range_of_range ~to_int x

  let int_exc_range_of_range (x : t range) : int * int =
    int_exc_range_of_range ~to_int x

  let inc_range_of_range (x : t range) : t * t =
    inc_range_of_range ~to_int ~of_int x

  let exc_range_of_range (x : t range) : t * t =
    exc_range_of_range ~to_int ~of_int x

  let join (x : t range) (y : t range) : t range option =
    join ~to_int ~of_int x y

  module Flatten = struct
    let flatten_into_seq (t : t range) : t Seq.t =
      Flatten.flatten_into_seq_internal ~modulo ~to_int ~of_int t

    let flatten_into_list (t : t range) : t list =
      Flatten.flatten_into_seq_internal ~modulo ~to_int ~of_int t |> List.of_seq
  end
end
