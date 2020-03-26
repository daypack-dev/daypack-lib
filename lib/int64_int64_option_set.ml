include Set.Make (struct
    type t = int64 * int64 option

    let compare = compare
  end)

module Serialize = struct
  let pack (set : t) : (float * float option) list =
    set
    |> to_seq
    |> Seq.map (fun (x, y) -> (Int64.to_float x, Option.map Int64.to_float y))
    |> List.of_seq
end

module Deserialize = struct
  let unpack (l : (float * float option) list) : t =
    l
    |> List.to_seq
    |> Seq.map (fun (x, y) -> (Int64.of_float x, Option.map Int64.of_float y))
    |> of_seq
end
