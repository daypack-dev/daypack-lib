type 'a t =
  [ `Range_inc of 'a * 'a
  | `Range_exc of 'a * 'a
  ]

let b_is_next_of_a (type a) ~(to_int : a -> int) x y =
  let x_int = to_int x in
  let y_int = to_int y in
  x_int + 1 = y_int

let flatten_to_seq_internal ~(modulo : int option) ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : 'a Seq.t =
  match t with
  | `Range_inc (start, end_inc) ->
    let start_int = to_int start in
    let end_inc_int = to_int end_inc in
    if start_int <= end_inc_int then
      OSeq.(start_int -- end_inc_int)
      |> Seq.map of_int
    else (
      match modulo with
      | None -> raise (Invalid_argument "End is before start")
      | Some modulo ->
        if modulo <= 0 then
          raise (Invalid_argument "Modulo is <= 0")
        else
          OSeq.append OSeq.(start_int --^ modulo)
            OSeq.(0 -- end_inc_int)
          |> Seq.map of_int
    )
  | `Range_exc (start, end_exc) ->
    let start_int = to_int start in
    let end_exc_int = to_int end_exc in
    if start_int <= end_exc_int then
      OSeq.(start_int --^ end_exc_int)
      |> Seq.map of_int
    else
      match modulo with
      | None -> raise (Invalid_argument "End is before start")
      | Some modulo ->
        if modulo <= 0 then
          raise (Invalid_argument "Modulo is <= 0")
        else
          OSeq.append OSeq.(start_int --^ modulo)
            OSeq.(0 --^ end_exc_int)
          |> Seq.map of_int

let flatten_to_seq ?(modulo : int option) ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : 'a Seq.t =
  flatten_to_seq_internal ~modulo ~of_int ~to_int t

let flatten_to_list ?(modulo : int option) ~(of_int : int -> 'a) ~(to_int : 'a -> int) (t : 'a t) : 'a list =
  flatten_to_seq_internal ~modulo ~of_int ~to_int t
  |> List.of_seq

let range_seq_of_seq (type a) ~(to_int : a -> int) (s : a Seq.t) : a t Seq.t =
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
        if b_is_next_of_a ~to_int end_inc x then
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

let merge (type a) ~(to_int : a -> int) (x : a t) (y : a t) : a t option =
  match x, y with
  | `Range_inc (x_start, x_end_inc), `Range_inc (y_start, y_end_inc) ->
    if b_is_next_of_a ~to_int x_end_inc y_start then
      Some (`Range_inc (x_start, y_end_inc))
    else
      None
  | `Range_inc (x_start, x_end_inc), `Range_exc (y_start, y_end_exc) ->
    if b_is_next_of_a ~to_int x_end_inc y_start then
      Some (`Range_exc (x_start, y_end_exc))
    else
      None
  | `Range_exc (x_start, x_end_exc), `Range_inc (y_start, y_end_inc) ->
    if x_end_exc = y_start then
      Some (`Range_inc (x_start, y_end_inc))
    else
      None
  | `Range_exc (x_start, x_end_exc), `Range_exc (y_start, y_end_exc) ->
    if x_end_exc = y_start then
      Some (`Range_exc (x_start, y_end_exc))
    else
      None

let compress_seq (type a) ~(to_int : a -> int) (s : a t Seq.t) : a t Seq.t =
  let rec aux (to_int : a -> int) (acc : a t option) (s : a t Seq.t) : a t Seq.t =
    match s () with
    | Seq.Nil -> (
        match acc with
        | None -> Seq.empty
        | Some x -> Seq.return x
      )
    | Seq.Cons (x, rest) ->
      (
        match acc with
        | None -> aux to_int (Some x) rest
        | Some acc ->
          match merge ~to_int acc x with
          | None -> fun () -> Seq.Cons (acc, aux to_int (Some x) rest)
          | Some res -> aux to_int (Some res) rest
      )
  in
  aux to_int None s

let compress_list (type a) ~(to_int : a -> int) (l : a t list) : a t list =
  List.to_seq l
  |> compress_seq ~to_int
  |> List.of_seq
