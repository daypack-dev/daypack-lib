type 'a range =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let map ~(f_inc : 'a * 'a -> 'b * 'b) ~(f_exc : 'a * 'a -> 'b * 'b) (t : 'a range) :
  'b range =
  match t with
  | `Range_inc (x, y) ->
    let x, y = f_inc (x, y) in
    `Range_inc (x, y)
  | `Range_exc (x, y) ->
    let x, y = f_exc (x, y) in
    `Range_exc (x, y)

module type B_big = sig
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
end

module Make_big (B : B_big) : S with type t := B.t = struct
  open B

  let int64_range_of_range (x : t range) : int64 range =
    let f (x, y) =
      (to_int64 x,
       to_int64 y)
    in
    map ~f_inc:f ~f_exc:f x

  let int64_exc_range_of_range (x : t range) : int64 * int64 =
    match x with
    | `Range_inc (x, y) -> (to_int64 x, y |> to_int64 |> Int64.succ)
    | `Range_exc (x, y) -> (to_int64 x, to_int64 y)

  let inc_range_of_range (x : t range) : t * t =
    match x with
    | `Range_inc (x, y) -> (x, y)
    | `Range_exc (x, y) -> (x, y |> to_int64 |> Int64.pred |> of_int64)

  let exc_range_of_range (x : t range) : t * t =
    match x with
    | `Range_inc (x, y) -> (x, y |> to_int64 |> Int64.succ |> of_int64)
    | `Range_exc (x, y) -> (x, y)

  let join (x : t range) (y : t range) : t range option =
    let x = int64_exc_range_of_range x in
    let y = int64_exc_range_of_range y in
    Time_slot.join x y
    |> Option.map (fun (x, y) -> `Range_exc (of_int64 x, of_int64 y))
end

