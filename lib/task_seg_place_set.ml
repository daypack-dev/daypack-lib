include Set.Make (struct
    type t = Task_ds.task_seg_place

    let compare = compare
  end)

module Serialize = struct
  let pack (t : t) : Task_ds.task_seg_place list = t |> to_seq |> List.of_seq
end

module Deserialize = struct
  let unpack (l : Task_ds.task_seg_place list) : t = l |> List.to_seq |> of_seq
end
