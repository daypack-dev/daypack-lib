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

module type S_ranges = sig

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

module Make_ranges_big (B : B_big) (S : S with type t = B.t) : S_ranges = struct
  open B
  open S

  let normalize ?(skip_filter = false) ?(skip_sort = false)
      (s : t range Seq.t)
    =
    s
    |> Seq.map S.int64_exc_range_of_range
    |> Time_slots.Normalize.normalize ~skip_filter ~skip_sort
    |> Seq.map (fun (x, y) -> (of_int64 x, of_int64 y))
    |> Seq.map (fun (x, y) -> `Range_exc (x, y))

  module Flatten = struct
    let flatten_into_seq_internal (t : t range) :
      t Seq.t =
      match t with
      | `Range_inc (start, end_inc) -> (
          let start = to_int64 start in
          let end_inc = to_int64 end_inc in
          if start <= end_inc then
            Seq_utils.a_to_b_inc_int64 ~a:start ~b:end_inc |> Seq.map of_int64
          else
            match modulo with
            | None -> raise (Invalid_argument "End is before start")
            | Some modulo ->
              if modulo <= 0L then raise (Invalid_argument "Modulo is <= 0")
              else
                OSeq.append
                  (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                  (Seq_utils.a_to_b_inc_int64 ~a:0L ~b:end_inc)
                |> Seq.map of_int64 )
      | `Range_exc (start, end_exc) -> (
          let start = to_int64 start in
          let end_exc = to_int64 end_exc in
          if start <= end_exc then
            Seq_utils.a_to_b_exc_int64 ~a:start ~b:end_exc |> Seq.map of_int64
          else
            match modulo with
            | None -> raise (Invalid_argument "End is before start")
            | Some modulo ->
              if modulo <= 0L then raise (Invalid_argument "Modulo is <= 0")
              else
                OSeq.append
                  (Seq_utils.a_to_b_exc_int64 ~a:start ~b:modulo)
                  (Seq_utils.a_to_b_exc_int64 ~a:0L ~b:end_exc)
                |> Seq.map of_int64 )

    let flatten_into_seq (t : 'a Range_ds.t) : 'a Seq.t =
      flatten_into_seq_internal t

    let flatten_into_list (t : 'a Range_ds.t) : 'a list =
      flatten_into_seq_internal t |> List.of_seq
  end

  module Of_seq = struct
    let range_seq_of_seq (s : t Seq.t) : t range Seq.t =
      s
      |> Seq.map (fun x -> `Range_inc (x, x))
      |> normalize ~skip_filter:true ~skip_sort:true

    let range_list_of_seq (s : t Seq.t) : t range list =
      range_seq_of_seq s |> List.of_seq
  end

  module Of_list = struct
    let range_seq_of_list (l : t list) : t range Seq.t =
      List.to_seq l |> Of_seq.range_seq_of_seq

    let range_list_of_list (l : t list) : t range list =
      List.to_seq l
      |> Of_seq.range_seq_of_seq
      |> List.of_seq
  end

  module Compress = struct
    let compress_seq
        (s : t range Seq.t) : t range Seq.t =
      let rec aux (acc : t range option)
          (s : t range Seq.t) : t range Seq.t =
        match s () with
        | Seq.Nil -> (
            match acc with None -> Seq.empty | Some x -> Seq.return x )
        | Seq.Cons (x, rest) -> (
            match acc with
            | None -> aux (Some x) rest
            | Some acc -> (
                match Range_ds.Merge.merge_big ~to_int64 acc x with
                | None -> fun () -> Seq.Cons (acc, aux (Some x) rest)
                | Some res -> aux (Some res) rest ) )
      in
      aux None s

    let compress_list
        (l : t range list) : t range list =
      List.to_seq l |> compress_seq |> List.of_seq
  end
end
