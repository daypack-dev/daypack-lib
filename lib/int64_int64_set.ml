include Set.Make (struct
    type t = int64 * int64

    let compare = compare
  end)

module Serialize = struct
  let pack (set : t) : (float * float) list =
    set
    |> to_seq
    |> Seq.map (fun (x, y) -> (Int64.to_float x, Int64.to_float y))
    |> List.of_seq
end

module Deserialize = struct
  let unpack (l : (float * float) list) : t =
    l
    |> List.to_seq
    |> Seq.map (fun (x, y) -> (Int64.of_float x, Int64.of_float y))
    |> of_seq
end
