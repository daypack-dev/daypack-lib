include Map.Make (struct
    type t = Task_ds.task_inst_id

    let compare = compare
  end)
