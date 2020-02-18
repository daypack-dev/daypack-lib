include Map.Make (struct
    type t = Task_ds.task_id

    let compare = compare
  end)
