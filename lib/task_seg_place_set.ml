include Set.Make (struct
    type t = Task.task_seg_place

    let compare = compare
  end)
