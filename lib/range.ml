module type B_big = sig
  type t

  val to_int64 : t -> int64

  val of_int64 : int64 -> t
end

module type S = sig end

module Make_big (B : B_big) : S = struct
  open B

  let b_is_next_of_a_int64 x y =
    let x = to_int64 x in
    let y = to_int64 y in
    Int64.succ x = y

  module Merge = struct
    let merge_big (x : t Range_ds.t) (y : t Range_ds.t) : t Range_ds.t option =
      match (x, y) with
      | `Range_inc (x_start, x_end_inc), `Range_inc (y_start, y_end_inc) ->
        if b_is_next_of_a_int64 x_end_inc y_start then
          Some (`Range_inc (x_start, y_end_inc))
        else None
      | `Range_inc (x_start, x_end_inc), `Range_exc (y_start, y_end_exc) ->
        if b_is_next_of_a_int64 x_end_inc y_start then
          Some (`Range_exc (x_start, y_end_exc))
        else None
      | `Range_exc (x_start, x_end_exc), `Range_inc (y_start, y_end_inc) ->
        if x_end_exc = y_start then Some (`Range_inc (x_start, y_end_inc))
        else None
      | `Range_exc (x_start, x_end_exc), `Range_exc (y_start, y_end_exc) ->
        if x_end_exc = y_start then Some (`Range_exc (x_start, y_end_exc))
        else None
  end
end
