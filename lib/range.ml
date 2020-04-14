type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let flatten ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : ('a list, unit) result =
  match t with
  | `Range_inc (start, end_inc) ->
    let start_int = to_int start in
    let end_inc_int = to_int end_inc in
    if start_int <= end_inc_int then
      OSeq.(start_int -- end_inc_int)
      |> Seq.map of_int
      |> List.of_seq
      |> Result.ok
    else
      Error ()
  | `Range_exc (start, end_exc) ->
    let start_int = to_int start in
    let end_exc_int = to_int end_exc in
    if start_int <= end_exc_int then
      OSeq.(start_int --^ end_exc_int)
      |> Seq.map of_int
      |> List.of_seq
      |> Result.ok
    else
      Error ()
