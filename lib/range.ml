type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let flatten_seq ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : 'a Seq.t =
  match t with
  | `Range_inc (start, end_inc) ->
    let start_int = to_int start in
    let end_inc_int = to_int end_inc in
    if start_int <= end_inc_int then
      OSeq.(start_int -- end_inc_int)
      |> Seq.map of_int
    else
      raise (Invalid_argument "End is before start")
  | `Range_exc (start, end_exc) ->
    let start_int = to_int start in
    let end_exc_int = to_int end_exc in
    if start_int <= end_exc_int then
      OSeq.(start_int --^ end_exc_int)
      |> Seq.map of_int
    else
      raise (Invalid_argument "End is before start")

let flatten_list ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : 'a list =
  flatten_seq ~of_int ~to_int t
  |> List.of_seq
