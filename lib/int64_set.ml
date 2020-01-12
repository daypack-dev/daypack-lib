include Set.Make (Int64)

module Serialize = struct
  let pack (set : t) : int64 list = set |> to_seq |> List.of_seq
end

module Deserialize = struct
  let unpack (l : int64 list) : t = l |> List.to_seq |> of_seq
end
