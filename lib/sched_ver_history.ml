type t = { mutable history : Sched.sched list }

let make_empty () = { history = [] }

let of_sched_list history = { history }

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
      (task_inst_data_list : Task.task_inst_data list) (t : t) :
    Task.task * Task.task_inst list =
    map_head
      (fun sched ->
         let task, task_inst_list, sched =
           Sched.Task_store.add_task ~parent_user_id data task_inst_data_list
             sched
         in
         ((task, task_inst_list), `In_place, sched))
      t

  let add_task_inst ~parent_task_id (data : Task.task_inst_data) (t : t) :
    Task.task_inst =
    map_head
      (fun sched ->
         let task_inst, sched =
           Sched.Task_inst_store.add_task_inst ~parent_task_id data sched
         in
         (task_inst, `In_place, sched))
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

  let instantiate ~start ~end_exc (t : t) : unit =
    map_head
      (fun sched ->
         let sched = Sched.Recur.instantiate ~start ~end_exc sched in
         ((), `In_place, sched))
      t
end

module Maybe_append_to_head = struct
  let remove_task (task_id : Task.task_id) (t : t) : unit =
    match t.history with
    | [] -> ()
    | hd :: tl -> (
        let hd' =
          hd
          |> Sched.Task_store.remove_task task_id
          |> Sched.Sched_req_store.remove_pending_sched_req_by_task_id task_id
        in
        let task_seg_place_seq =
          Sched.Agenda.find_task_seg_place_seq_by_task_id task_id hd
        in
        match task_seg_place_seq () with
        | Seq.Nil -> t.history <- hd' :: tl
        | _ ->
          let hd' =
            hd'
            |> Sched.Sched_req_store.remove_sched_req_record_by_task_id
              task_id
            |> Sched.Agenda.remove_task_seg_place_seq task_seg_place_seq
          in
          t.history <- hd' :: hd :: tl )

  let remove_task_inst (task_inst_id : Task.task_inst_id) (t : t) : unit =
    match t.history with
    | [] -> ()
    | hd :: tl -> (
        let hd' =
          hd
          |> Sched.Task_inst_store.remove_task_inst task_inst_id
          |> Sched.Sched_req_store.remove_pending_sched_req_by_task_inst_id
            task_inst_id
        in
        let task_seg_place_seq =
          Sched.Agenda.find_task_seg_place_seq_by_task_inst_id task_inst_id hd
        in
        match task_seg_place_seq () with
        | Seq.Nil -> t.history <- hd' :: tl
        | _ ->
          let hd' =
            hd'
            |> Sched.Sched_req_store.remove_sched_req_record_by_task_inst_id
              task_inst_id
            |> Sched.Agenda.remove_task_seg_place_seq task_seg_place_seq
          in
          t.history <- hd' :: hd :: tl )

  let sched ~start ~end_exc ~include_sched_reqs_partially_within_time_period
      ~up_to_sched_req_id_inc (t : t) : (unit, unit) result =
    match t.history with
    | [] -> Ok ()
    | hd :: tl -> (
        let sched_req_records, hd' =
          hd
          |> Sched.Recur.instantiate ~start ~end_exc
          |> Sched.Sched_req_store.allocate_task_segs_for_pending_sched_reqs
            ~start ~end_exc ~include_sched_reqs_partially_within_time_period
            ~up_to_sched_req_id_inc
        in
        match sched_req_records with
        | [] -> Ok ()
        | _ -> (
            let possible_scheds =
              Sched_search.backtracking_search_multi ~start ~end_exc ~base:hd'
                sched_req_records
            in
            match possible_scheds () with
            | Seq.Nil -> Error ()
            | Seq.Cons (hd', _) ->
              t.history <- hd' :: hd :: tl;
              Ok () ) )
end

module Equal = struct
  let equal t1 t2 =
    List.for_all2
      (fun s1 s2 -> Sched.Equal.sched_equal s1 s2)
      t1.history t2.history
end

module Serialize = struct
  let list_to_base_and_diffs (l : Sched.sched list) :
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

  let to_base_and_diffs (t : t) : (Sched.sched * Sched.sched_diff list) option =
    list_to_base_and_diffs t.history
end

module Deserialize = struct
  let list_of_base_and_diffs (base : Sched.sched)
      (diffs : Sched.sched_diff list) : Sched.sched list =
    let rec aux (acc : Sched.sched list) (cur : Sched.sched)
        (diffs : Sched.sched_diff list) : Sched.sched list =
      match diffs with
      | [] -> acc
      | diff :: diffs ->
        let next = Sched.Diff.add_diff_sched diff cur in
        aux (next :: acc) next diffs
    in
    aux [ base ] base diffs

  let of_base_and_diffs base diffs : t =
    let history = list_of_base_and_diffs base diffs in
    { history }
end

module Print = struct
  let debug_string_of_sched_ver_history ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : t) =
    Debug_print.bprintf ~indent_level buffer "sched ver history\n";
    List.iteri
      (fun i sched ->
         Debug_print.bprintf ~indent_level buffer "ver : %d\n" i |> ignore;
         Sched.Print.debug_string_of_sched ~indent_level:(indent_level + 1)
           ~buffer sched
         |> ignore)
      (List.rev t.history);
    Buffer.contents buffer

  let debug_print_sched_ver_history ?(indent_level = 0) (t : t) =
    print_string (debug_string_of_sched_ver_history ~indent_level t)
end
