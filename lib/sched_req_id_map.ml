include Map.Make (struct
    type t = Sched_req_ds.sched_req_id

    let compare = compare
  end)
