include Set.Make (struct
    type t = int64 * int64 option

    let compare = compare
  end)

module Serialize = struct
  let pack (set : t) : (int64 * int64 option) list =
    set |> to_seq |> List.of_seq
end

module Deserialize = struct
  let unpack (l : (int64 * int64 option) list) : t = l |> List.to_seq |> of_seq
end
