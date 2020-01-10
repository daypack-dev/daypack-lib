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

let zero_to_n_exc_int64 n : int64 Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur +^ 1L) n) else Seq.empty
  in
  aux 0L n

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
