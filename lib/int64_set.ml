include Set.Make (Int64)

module Serialize = struct
  let pack (set : t) : float list =
    set |> to_seq |> Seq.map Int64.to_float |> List.of_seq
end

module Deserialize = struct
  let unpack (l : float list) : t =
    l |> List.to_seq |> Seq.map Int64.of_float |> of_seq
end
