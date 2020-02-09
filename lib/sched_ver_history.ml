type t = { mutable history : Sched.sched list }

type head_choice =
  [ `In_place
  | `New_head
  ]

(* let fold_head ~none:(f_none : unit -> Sched.sched)
 *     ~some:(f : Sched.sched -> Sched.sched) (t : t) : unit =
 *   match t.history with
 *   | [] -> t.history <- [ f_none () ]
 *   | hd :: tl ->
 *     let hd = f hd in
 *     t.history <- hd :: tl *)

let map_head (f : Sched.sched -> 'a * head_choice * Sched.sched) (t : t) : 'a =
  match t.history with
  | [] ->
    let ret, _, sched = f Sched.empty in
    t.history <- [ sched ];
    ret
  | hd :: tl ->
    let ret, choice, x = f hd in
    ( match choice with
      | `In_place -> t.history <- x :: tl
      | `New_head -> t.history <- x :: hd :: tl );
    ret

module In_place_head = struct
  let add_task ~parent_user_id (data : Task.task_data)
      (task_inst_data_list : Task.task_inst_data list) (t : t) : Task.task =
    map_head
      (fun sched ->
         let task, _, sched =
           Sched.Task_store.add_task ~parent_user_id data task_inst_data_list
             sched
         in
         (task, `In_place, sched))
      t

  let queue_sched_req (data : Sched_req.sched_req_data) (t : t) :
    Sched_req.sched_req =
    map_head
      (fun sched ->
         let sched_req, sched =
           Sched.Sched_req_store.queue_sched_req_data data sched
         in
         (sched_req, `In_place, sched))
      t
end

module Maybe_append_to_head = struct
  let remove_task (task_id : Task.task_id) (t : t) : unit =
    match t.history with
    | [] -> ()
    | hd :: tl -> (
        let task_seg_place_seq =
          Sched.Task_seg_place_map.find_task_seg_place_seq_by_task_id task_id hd
        in
        match task_seg_place_seq () with
        | Seq.Nil ->
          let hd = Sched.Task_store.remove_task task_id hd in
          t.history <- hd :: tl
        | _ ->
          let hd =
            hd
            |> Sched.Task_store.remove_task task_id
            |> Sched.Task_seg_place_map.remove_task_seg_place_seq
              task_seg_place_seq
          in
          t.history <- hd :: tl )
end

module Append_to_head = struct end

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
