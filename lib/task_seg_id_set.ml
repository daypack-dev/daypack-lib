include Set.Make (struct
    type t = Task_ds.task_seg_id

    let compare = compare
  end)

module Serialize = struct
  let pack (t : t) : Task_ds_t.task_seg_id list =
    t |> to_seq |> Seq.map Task_ds.Serialize.pack_task_seg_id |> List.of_seq
end

module Deserialize = struct
  let unpack (l : Task_ds_t.task_seg_id list) : t =
    l |> List.to_seq |> Seq.map Task_ds.Deserialize.unpack_task_seg_id |> of_seq
end
