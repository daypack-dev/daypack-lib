open Range

module type S = sig end

module Make_big (B : Range.B_big) (S : Range.S with type t = B.t) : S = struct
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
