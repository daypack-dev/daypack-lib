type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let b_is_next_of_a_int64 (type a) ~(to_int64 : a -> int64) x y =
  let x = to_int64 x in
  let y = to_int64 y in
  Int64.succ x = y

let b_is_next_of_a (type a) ~(to_int : a -> int) x y =
  let x = to_int x in
  let y = to_int y in
  succ x = y

let map ~(f_inc : 'a * 'a -> 'b * 'b) ~(f_exc : 'a * 'a -> 'b * 'b) (t : 'a t) :
  'b t =
  match t with
  | `Range_inc (x, y) ->
    let x, y = f_inc (x, y) in
    `Range_inc (x, y)
  | `Range_exc (x, y) ->
    let x, y = f_exc (x, y) in
    `Range_exc (x, y)

module Merge = struct
  let merge_big (type a) ~(to_int64 : a -> int64) (x : a t) (y : a t) :
    a t option =
    match (x, y) with
    | `Range_inc (x_start, x_end_inc), `Range_inc (y_start, y_end_inc) ->
      if b_is_next_of_a_int64 ~to_int64 x_end_inc y_start then
        Some (`Range_inc (x_start, y_end_inc))
      else None
    | `Range_inc (x_start, x_end_inc), `Range_exc (y_start, y_end_exc) ->
      if b_is_next_of_a_int64 ~to_int64 x_end_inc y_start then
        Some (`Range_exc (x_start, y_end_exc))
      else None
    | `Range_exc (x_start, x_end_exc), `Range_inc (y_start, y_end_inc) ->
      if x_end_exc = y_start then Some (`Range_inc (x_start, y_end_inc))
      else None
    | `Range_exc (x_start, x_end_exc), `Range_exc (y_start, y_end_exc) ->
      if x_end_exc = y_start then Some (`Range_exc (x_start, y_end_exc))
      else None

  let merge (type a) ~(to_int : a -> int) (x : a t) (y : a t) : a t option =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    merge_big ~to_int64 x y
end
