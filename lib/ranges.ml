let normalize (type a) ?(skip_filter = false) ?(skip_sort = false)
    ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a) (s : a Range.range Seq.t)
  : a Range.range Seq.t =
  s
  |> Seq.map (Range.int64_exc_range_of_range ~to_int64)
  |> Time_slots.Normalize.normalize ~skip_filter ~skip_sort
  |> Seq.map (fun (x, y) -> (of_int64 x, of_int64 y))
  |> Seq.map (fun (x, y) -> `Range_exc (x, y))

module Flatten = struct
  let flatten_internal (type a) ~(modulo : int64 option) ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
      (s : a Range.range Seq.t) : a Seq.t =
    match modulo with
    | None ->
      Seq.flat_map (Range.Flatten.flatten_into_seq ~to_int64 ~of_int64) s
    | Some modulo ->
      Seq.flat_map (Range.Flatten.flatten_into_seq ~modulo ~to_int64 ~of_int64) s

  let flatten (type a) ?(modulo : int64 option) ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
      (s : a Range.range Seq.t) : a Seq.t =
    flatten_internal ~modulo ~to_int64 ~of_int64 s

  let flatten_list (type a) ?(modulo : int64 option) ~(to_int64 : a -> int64) ~(of_int64 : int64 -> a)
      (l : a Range.range list) : a list =
    l
    |> List.to_seq
    |> flatten_internal ~modulo ~to_int64 ~of_int64
    |> List.of_seq
end

module Of_seq = struct
  let range_seq_of_seq (type a) ?(skip_filter = false) ?(skip_sort = false) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (s : a Seq.t) : a Range.range Seq.t =
    s
    |> Seq.map (fun x -> `Range_inc (x, x))
    |> normalize ~skip_filter ~skip_sort ~to_int64 ~of_int64

  let range_list_of_seq (type a) ?(skip_filter = false) ?(skip_sort = false) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (s : a Seq.t) : a Range.range list =
    range_seq_of_seq ~skip_filter ~skip_sort ~to_int64 ~of_int64 s |> List.of_seq
end

module Of_list = struct
  let range_seq_of_list (type a) ?(skip_filter = false) ?(skip_sort = false) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (l : a list) : a Range.range Seq.t =
    List.to_seq l |> Of_seq.range_seq_of_seq ~skip_filter ~skip_sort ~to_int64 ~of_int64

  let range_list_of_list (type a) ?(skip_filter = false) ?(skip_sort = false) ~(to_int64 : a -> int64)
      ~(of_int64 : int64 -> a) (l : a list) : a Range.range list =
    List.to_seq l |> Of_seq.range_seq_of_seq ~skip_filter ~skip_sort ~to_int64 ~of_int64 |> List.of_seq
end

module type S = sig
  type t

  val normalize :
    ?skip_filter:bool ->
    ?skip_sort:bool ->
    t Range.range Seq.t ->
    t Range.range Seq.t

  module Of_seq : sig
    val range_seq_of_seq : t Seq.t -> t Range.range Seq.t

    val range_list_of_seq : t Seq.t -> t Range.range list
  end

  module Of_list : sig
    val range_seq_of_list : t list -> t Range.range Seq.t

    val range_list_of_list : t list -> t Range.range list
  end
end

module Make (B : Range.B) : S with type t := B.t = struct
  open Range
  open B

  let normalize ?(skip_filter = false) ?(skip_sort = false) (s : t range Seq.t)
    =
    normalize ~skip_filter ~skip_sort ~to_int64 ~of_int64 s

  module Of_seq = struct
    let range_seq_of_seq (s : t Seq.t) : t range Seq.t =
      Of_seq.range_seq_of_seq ~to_int64 ~of_int64 s

    let range_list_of_seq (s : t Seq.t) : t range list =
      Of_seq.range_list_of_seq ~to_int64 ~of_int64 s
  end

  module Of_list = struct
    let range_seq_of_list (l : t list) : t range Seq.t =
      List.to_seq l |> Of_seq.range_seq_of_seq

    let range_list_of_list (l : t list) : t range list =
      List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
  end
end
