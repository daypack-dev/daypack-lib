include Set.Make (struct
    type t = int64 * int64

    let compare = compare
  end)

module Serialize = struct
  let pack (set : t) : ((int32 * int32) * (int32 * int32)) list =
    set
    |> to_seq
    |> Seq.map (fun (x, y) ->
        (Misc_utils.int64_to_int32_int32 x, Misc_utils.int64_to_int32_int32 y))
    |> List.of_seq
end

module Deserialize = struct
  let unpack (l : ((int32 * int32) * (int32 * int32)) list) : t =
    l
    |> List.to_seq
    |> Seq.map (fun (x, y) ->
        (Misc_utils.int32_int32_to_int64 x, Misc_utils.int32_int32_to_int64 y))
    |> of_seq
end
