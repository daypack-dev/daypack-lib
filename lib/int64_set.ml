include Set.Make (Int64)

module Serialize = struct
  let pack (set : t) : int64 list = set |> to_seq |> List.of_seq
end
