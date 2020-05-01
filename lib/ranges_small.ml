let normalize (type a) ?(skip_filter = false) ?(skip_sort = false)
    ~(to_int : a -> int) ~(of_int : int -> a) (s : a Range.range Seq.t) :
  a Range.range Seq.t =
  let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
  let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
  Ranges.normalize ~skip_filter ~skip_sort ~to_int64 ~of_int64 s

module Flatten = struct
  let flatten_internal (type a) ~(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a)
      (s : a Range.range Seq.t) : a Seq.t =
    let to_int64 = Misc_utils.convert_to_int_to_int64 to_int in
    let of_int64 = Misc_utils.convert_of_int_to_int64 of_int in
    match modulo with
    | None ->
      Ranges.Flatten.flatten ~to_int64 ~of_int64 s
    | Some modulo ->
      let modulo = Int64.of_int modulo in
      Ranges.Flatten.flatten ~modulo ~to_int64 ~of_int64 s

  let flatten (type a) ?(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a)
      (s : a Range.range Seq.t) : a Seq.t =
    flatten_internal ~modulo ~to_int ~of_int s

  let flatten_list (type a) ?(modulo : int option) ~(to_int : a -> int) ~(of_int : int -> a)
      (l : a Range.range list) : a list =
    l
    |> List.to_seq
    |> flatten_internal ~modulo ~to_int ~of_int
    |> List.of_seq
end

module Of_seq = struct
  let range_seq_of_seq (type a) ?(skip_filter = false) ?(skip_sort = false)~(to_int : a -> int) ~(of_int : int -> a)
      (s : a Seq.t) : a Range.range Seq.t =
    s
    |> Seq.map (fun x -> `Range_inc (x, x))
    |> normalize ~skip_filter ~skip_sort ~to_int ~of_int

  let range_list_of_seq (type a) ?(skip_filter = false) ?(skip_sort = false)~(to_int : a -> int) ~(of_int : int -> a)
      (s : a Seq.t) : a Range.range list =
    range_seq_of_seq ~skip_filter ~skip_sort ~to_int ~of_int s |> List.of_seq
end

module Of_list = struct
  let range_seq_of_list (type a) ?(skip_filter = false) ?(skip_sort = false)~(to_int : a -> int) ~(of_int : int -> a)
      (l : a list) : a Range.range Seq.t =
    List.to_seq l |> Of_seq.range_seq_of_seq ~skip_filter ~skip_sort ~to_int ~of_int

  let range_list_of_list (type a) ?(skip_filter = false) ?(skip_sort = false)~(to_int : a -> int) ~(of_int : int -> a)
      (l : a list) : a Range.range list =
    List.to_seq l |> Of_seq.range_seq_of_seq ~skip_filter ~skip_sort ~to_int ~of_int |> List.of_seq
end

module Make (B : Range_small.B) : Ranges.S with type t := B.t = struct
  open B

  let normalize ?(skip_filter = false) ?(skip_sort = false)
      (s : t Range.range Seq.t) =
    normalize ~skip_filter ~skip_sort ~to_int ~of_int s

  module Of_seq = struct
    let range_seq_of_seq (s : t Seq.t) : t Range.range Seq.t =
      Of_seq.range_seq_of_seq ~to_int ~of_int s

    let range_list_of_seq (s : t Seq.t) : t Range.range list =
      Of_seq.range_list_of_seq ~to_int ~of_int s
  end

  module Of_list = struct
    let range_seq_of_list (l : t list) : t Range.range Seq.t =
      List.to_seq l |> Of_seq.range_seq_of_seq

    let range_list_of_list (l : t list) : t Range.range list =
      List.to_seq l |> Of_seq.range_seq_of_seq |> List.of_seq
  end
end
