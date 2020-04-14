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

let range_seq_of_seq (type a) ~(to_int : a -> int) (s : a Seq.t) : a t Seq.t =
  let b_is_next_of_a (to_int : a -> int) x y =
    let x_int = to_int x in
    let y_int = to_int y in
    x_int + 1 = y_int
  in
  let rec aux to_int (acc_inc : (a * a) option) (s : a Seq.t) : a t Seq.t =
    match s () with
    | Seq.Nil -> (
        match acc_inc with
        | None -> Seq.empty
        | Some (start, end_inc) -> Seq.return (`Range_inc (start, end_inc))
      )
    | Seq.Cons (x, rest) ->
      match acc_inc with
      | None -> aux to_int (Some (x, x)) rest
      | Some (start, end_inc) ->
        if b_is_next_of_a to_int end_inc x then
          aux to_int (Some (start, x)) rest
        else
          fun () ->
            Seq.Cons (`Range_inc (start, end_inc),
                      aux to_int None rest
                     )
  in
  aux to_int None s

let range_list_of_seq (type a) ~(to_int : a -> int) (s : a Seq.t) : a t list =
  range_seq_of_seq ~to_int s
  |> List.of_seq

let range_seq_of_list (type a) ~(to_int : a -> int) (l : a list) : a t Seq.t =
  List.to_seq l
  |> range_seq_of_seq ~to_int

let range_list_of_list (type a) ~(to_int : a -> int) (l : a list) : a t list =
  List.to_seq l
  |> range_seq_of_seq ~to_int
  |> List.of_seq
