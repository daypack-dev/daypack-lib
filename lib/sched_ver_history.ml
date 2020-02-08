type t = { history : Sched.sched list }

let fold_head ~none:(f_none : unit -> Sched.sched)
    ~some:(f : Sched.sched -> Sched.sched) (t : t) : t =
  match t.history with
  | [] -> { history = [ f_none () ] }
  | hd :: tl ->
    let hd = f hd in
    { history = hd :: tl }

module In_place_head = struct
  let reg_task ~parent_user_id (data : Task.task_data)
      (task_inst_data_list : Task.task_inst_data list) (t : t) =
    fold_head
      ~none:(fun () ->
          let _, _, sched =
            Sched.empty
            |> Sched.Task_store.add_task ~parent_user_id data task_inst_data_list
          in
          sched)
      ~some:(fun sched -> sched)
      t
end

module Serialize = struct
  let to_base_and_diffs (l : Sched.sched list) :
    (Sched.sched * Sched.sched_diff list) option =
    let rec aux
        (base_and_last_and_diffs :
           (Sched.sched * Sched.sched * Sched.sched_diff list) option)
        (l : Sched.sched list) =
      match l with
      | [] -> (
          match base_and_last_and_diffs with
          | None -> None
          | Some (base, _, diffs) -> Some (base, List.rev diffs) )
      | sched :: rest -> (
          match base_and_last_and_diffs with
          | None -> aux (Some (sched, sched, [])) rest
          | Some (base, last, diffs) ->
            let diff = Sched.Diff.diff_sched ~old:last sched in
            aux (Some (base, sched, diff :: diffs)) rest )
    in
    aux None (List.rev l)
end

module Deserialize = struct
  let of_base_and_diffs (base : Sched.sched) (diffs : Sched.sched_diff list) :
    Sched.sched list =
    let rec aux (acc : Sched.sched list) (cur : Sched.sched)
        (diffs : Sched.sched_diff list) : Sched.sched list =
      match diffs with
      | [] -> acc
      | diff :: diffs ->
        let next = Sched.Diff.add_diff_sched diff cur in
        aux (next :: acc) next diffs
    in
    aux [ base ] base diffs
end
