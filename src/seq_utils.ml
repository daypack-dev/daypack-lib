open Int64_utils

let nat : int Seq.t =
  let open OSeq.Generator in
  let rec aux n = yield n >>= fun () -> aux (n + 1) in
  run (aux 0)

(* let nat_int64 : int64 Seq.t =
 *   let open OSeq.Generator in
 *   let rec aux n = yield n >>= fun () -> aux (n ^+ 1L) in
 *   run (aux 0L) *)

let zero_to_n_exc n : int Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else Seq.empty
  in
  aux 0 n

let zero_to_n_inc n = zero_to_n_exc (n + 1)

let a_to_b_exc_int64 ~a ~b : int64 Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur +^ 1L) n) else Seq.empty
  in
  aux a b

let a_to_b_inc_int64 ~a ~b : int64 Seq.t = a_to_b_exc_int64 ~a ~b:(b +^ 1L)

let zero_to_n_exc_int64 n : int64 Seq.t = a_to_b_exc_int64 ~a:0L ~b:n

let zero_to_n_inc_int64 n = zero_to_n_exc_int64 (n +^ 1L)

let mod_int n =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else aux 0 n
  in
  aux 0 n

(* let mapi (f : int -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
 *   let rec aux f s i =
 *     match s () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons (x, rest) -> fun () -> Seq.Cons (f i x, aux f rest (i + 1))
 *   in
 *   aux f s 0 *)

(* let mapi_int64 (f : int64 -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
 *   let rec aux f s i =
 *     match s () with
 *     | Seq.Nil -> Seq.empty
 *     | Seq.Cons (x, rest) -> fun () -> Seq.Cons (f i x, aux f rest (i ^+ 1L))
 *   in
 *   aux f s 0L *)

let collect_round_robin_non_decreasing (type a) (compare : a -> a -> int)
    (batches : a Seq.t list) : a option list Seq.t =
  let rec get_usable_part compare (cur : a) (seq : a Seq.t) : a Seq.t =
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (x, rest) as s ->
      let cmp_res = compare cur x in
      if cmp_res <= 0 then fun () -> s else get_usable_part compare cur rest
  in
  let rec aux compare (cur : a option) (batches : a Seq.t list) :
    a option list Seq.t =
    let cur, acc, new_batches =
      List.fold_left
        (fun (cur, acc, new_batches) seq ->
           let usable =
             match cur with
             | None -> seq
             | Some cur_start -> get_usable_part compare cur_start seq
           in
           match usable () with
           | Seq.Nil -> (cur, None :: acc, new_batches)
           | Seq.Cons (x, rest) -> (Some x, Some x :: acc, rest :: new_batches))
        (cur, [], []) batches
    in
    let acc = List.rev acc in
    let new_batches = List.rev new_batches in
    fun () -> Seq.Cons (acc, aux compare cur new_batches)
  in
  aux compare None batches