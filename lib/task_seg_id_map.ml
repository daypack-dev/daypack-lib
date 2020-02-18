include Map.Make (struct
    type t = Task_ds.task_seg_id

    let compare = compare
  end)
