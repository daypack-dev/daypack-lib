include Map.Make (struct
    type t = Task_ds.user_id

    let compare = compare
  end)
