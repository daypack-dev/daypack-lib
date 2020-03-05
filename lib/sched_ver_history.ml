type t = { mutable history : Sched.sched list }

let make_empty () = { history = [] }

let of_sched_list history = { history }

type head_choice =
  | Replace_head of Sched.sched
  | New_head of Sched.sched
  | Do_nothing

(* let fold_head ~none:(f_none : unit -> Sched.sched)
 *     ~some:(f : Sched.sched -> Sched.sched) (t : t) : unit =
 *   match t.history with
 *   | [] -> t.history <- [ f_none () ]
 *   | hd :: tl ->
 *     let hd = f hd in
 *     t.history <- hd :: tl *)

let map_head (f : Sched.sched -> 'a * head_choice) (t : t) : 'a =
  match t.history with
  | [] ->
    let ret, choice = f Sched.empty in
    ( match choice with
      | Replace_head x -> t.history <- [ x ]
      | New_head x -> t.history <- [ x ]
      | Do_nothing -> () );
    ret
  | hd :: tl ->
    let ret, choice = f hd in
    ( match choice with
      | Replace_head x -> t.history <- x :: tl
      | New_head x -> t.history <- x :: hd :: tl
      | Do_nothing -> () );
    ret

module In_place_head = struct
  let add_task ~parent_user_id (data : Task_ds.task_data)
      (task_inst_data_list : Task_ds.task_inst_data list) (t : t) :
    Task_ds.task * Task_ds.task_inst list =
    map_head
      (fun sched ->
         let task, task_inst_list, sched =
           Sched.Task.add_task ~parent_user_id data task_inst_data_list sched
         in
         ((task, task_inst_list), Replace_head sched))
      t

  let add_task_inst ~parent_task_id (data : Task_ds.task_inst_data) (t : t) :
    Task_ds.task_inst =
    map_head
      (fun sched ->
         let task_inst, sched =
           Sched.Task_inst.add_task_inst ~parent_task_id data sched
         in
         (task_inst, Replace_head sched))
      t

  let queue_sched_req (data : Sched_req_ds.sched_req_data) (t : t) :
    Sched_req_ds.sched_req =
    map_head
      (fun sched ->
         let sched_req, sched =
           Sched.Sched_req.queue_sched_req_data data sched
         in
         (sched_req, Replace_head sched))
      t

  let instantiate ~start ~end_exc (t : t) : unit =
    map_head
      (fun sched ->
         let sched = Sched.Recur.instantiate ~start ~end_exc sched in
         ((), Replace_head sched))
      t

  let move_task_seg_internal
      ~(move_task_seg_by_id : Task_ds.task_seg_id -> Sched.sched -> Sched.sched)
      (task_seg_id : Task_ds.task_seg_id) (t : t) : unit =
    map_head
      (fun sched ->
         let sched = move_task_seg_by_id task_seg_id sched in
         ((), Replace_head sched))
      t

  let move_task_seg_to_completed (task_seg_id : Task_ds.task_seg_id) (t : t) :
    unit =
    move_task_seg_internal
      ~move_task_seg_by_id:Sched.Progress.move_task_seg_to_completed task_seg_id
      t

  let move_task_seg_to_uncompleted (task_seg_id : Task_ds.task_seg_id) (t : t) :
    unit =
    move_task_seg_internal
      ~move_task_seg_by_id:Sched.Progress.move_task_seg_to_uncompleted
      task_seg_id t

  let move_task_seg_to_discarded (task_seg_id : Task_ds.task_seg_id) (t : t) :
    unit =
    move_task_seg_internal
      ~move_task_seg_by_id:Sched.Progress.move_task_seg_to_discarded task_seg_id
      t

  let move_task_inst_internal
      ~(move_task_inst_by_id :
          Task_ds.task_inst_id -> Sched.sched -> Sched.sched)
      (task_inst_id : Task_ds.task_inst_id) (t : t) : unit =
    map_head
      (fun sched ->
         let sched = move_task_inst_by_id task_inst_id sched in
         ((), Replace_head sched))
      t

  let move_task_inst_to_completed (task_inst_id : Task_ds.task_inst_id) (t : t)
    : unit =
    move_task_inst_internal
      ~move_task_inst_by_id:Sched.Progress.move_task_inst_to_completed
      task_inst_id t

  let move_task_inst_to_uncompleted (task_inst_id : Task_ds.task_inst_id)
      (t : t) : unit =
    move_task_inst_internal
      ~move_task_inst_by_id:Sched.Progress.move_task_inst_to_uncompleted
      task_inst_id t

  let move_task_inst_to_discarded (task_inst_id : Task_ds.task_inst_id) (t : t)
    : unit =
    move_task_inst_internal
      ~move_task_inst_by_id:Sched.Progress.move_task_inst_to_discarded
      task_inst_id t

  let add_task_seg_progress_chunk (task_seg_id : Task_ds.task_seg_id)
      (chunk : int64 * int64) (t : t) : unit =
    map_head
      (fun sched ->
         let sched =
           Sched.Progress.add_task_seg_progress_chunk task_seg_id chunk sched
         in
         ((), Replace_head sched))
      t

  let add_task_inst_progress_chunk (task_inst_id : Task_ds.task_inst_id)
      (chunk : int64 * int64) (t : t) : unit =
    map_head
      (fun sched ->
         let sched =
           Sched.Progress.add_task_inst_progress_chunk task_inst_id chunk sched
         in
         ((), Replace_head sched))
      t
end

module Maybe_append_to_head = struct
  let remove_task (task_id : Task_ds.task_id) (t : t) : unit =
    map_head
      (fun hd ->
         let hd' =
           hd
           |> Sched.Task.remove_task_all task_id
           |> Sched.Sched_req.remove_pending_sched_req_by_task_id task_id
           |> Sched.Sched_req.remove_sched_req_record_by_task_id task_id
         in
         let task_seg_place_seq =
           Sched.Agenda.find_task_seg_place_seq_by_task_id task_id hd
         in
         match task_seg_place_seq () with
         | Seq.Nil -> ((), Replace_head hd')
         | _ ->
           let hd' =
             hd'
             |> Sched.Sched_req.remove_sched_req_record_by_task_id task_id
             |> Sched.Agenda.remove_task_seg_place_seq task_seg_place_seq
           in
           ((), New_head hd'))
      t

  let remove_task_inst (task_inst_id : Task_ds.task_inst_id) (t : t) : unit =
    match t.history with
    | [] -> ()
    | hd :: tl -> (
        let hd' =
          hd
          |> Sched.Task_inst.remove_task_inst_all task_inst_id
          |> Sched.Sched_req.remove_pending_sched_req_by_task_inst_id
            task_inst_id
          |> Sched.Sched_req.remove_sched_req_record_by_task_inst_id
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
            |> Sched.Sched_req.remove_sched_req_record_by_task_inst_id
              task_inst_id
            |> Sched.Agenda.remove_task_seg_place_seq task_seg_place_seq
          in
          t.history <- hd' :: hd :: tl )

  let remove_task_seg_progress_chunk (task_seg_id : Task_ds.task_seg_id)
      (chunk : int64 * int64) (t : t) : unit =
    map_head
      (fun sched ->
         let chunks =
           Sched.Progress.find_task_seg_progress_chunk_set task_seg_id sched
         in
         if Int64_int64_set.mem chunk chunks then
           let hd' =
             sched
             |> Sched.Progress.remove_task_seg_progress_chunk task_seg_id chunk
           in
           ((), Replace_head hd')
         else ((), Do_nothing))
      t

  let remove_task_inst_progress_chunk (task_inst_id : Task_ds.task_inst_id)
      (chunk : int64 * int64) (t : t) : unit =
    map_head
      (fun sched ->
         let chunks =
           Sched.Progress.find_task_inst_progress_chunk_set task_inst_id sched
         in
         if Int64_int64_set.mem chunk chunks then
           let hd' =
             sched
             |> Sched.Progress.remove_task_inst_progress_chunk task_inst_id chunk
           in
           ((), Replace_head hd')
         else ((), Do_nothing))
      t

  let sched ~start ~end_exc ~include_sched_reqs_partially_within_time_period
      ~up_to_sched_req_id_inc (t : t) : (unit, unit) result =
    match t.history with
    | [] -> Ok ()
    | hd :: tl -> (
        let sched_req_records, hd' =
          hd
          |> Sched.Recur.instantiate ~start ~end_exc
          |> Sched.Leftover.sched_for_leftover_task_segs ~start ~end_exc
          |> Sched.Sched_req.allocate_task_segs_for_pending_sched_reqs ~start
            ~end_exc ~include_sched_reqs_partially_within_time_period
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

module Append_to_head = struct
  let snapshot (t : t) : unit = map_head (fun sched -> ((), New_head sched)) t
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

  let write_to_dir ~(dir : string) (t : t) : (unit, string) result =
    try
      if Sys.is_directory dir then (
        match to_base_and_diffs t with
        | None -> Ok ()
        | Some (base, diffs) ->
          (let base_str = Sched.Serialize.json_string_of_sched base in
           let oc = open_out (Filename.concat dir "sched_v0.json") in
           Fun.protect
             ~finally:(fun () -> close_out oc)
             (fun () -> output_string oc base_str));
          diffs
          |> List.to_seq
          |> Seq.map Sched.Serialize.json_string_of_sched_diff
          |> OSeq.iteri (fun i sched_diff_str ->
              let oc =
                open_out
                  (Filename.concat dir
                     (Printf.sprintf "sched_v%d.json" (i + 1)))
              in
              Fun.protect
                ~finally:(fun () -> close_out oc)
                (fun () -> output_string oc sched_diff_str));
          Ok () )
      else Error "File is not a directory"
    with Sys_error msg -> Error msg
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

  let read_from_dir ~(dir : string) : (t, string) result =
    try
      let base =
        let ic = open_in (Filename.concat dir "sched_v0.json") in
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () -> really_input_string ic (in_channel_length ic))
        |> Sched.Deserialize.sched_of_json_string
      in
      let diffs =
        Sys.readdir dir
        |> Array.to_seq
        |> Seq.filter_map (fun s ->
            try
              Scanf.sscanf s "sched_v%d.json" (fun i ->
                  if i = 0 then None else Some (i, s))
            with Stdlib.Scanf.Scan_failure _ -> None)
        |> Seq.map (fun (i, s) ->
            let ic = open_in (Filename.concat dir s) in
            ( i,
              Fun.protect
                ~finally:(fun () -> close_in ic)
                (fun () -> really_input_string ic (in_channel_length ic)) ))
        |> OSeq.sort ~cmp:(fun (i1, _) (i2, _) -> compare i1 i2)
        |> Seq.map (fun (_i, s) ->
            Sched.Deserialize.sched_diff_of_json_string s)
        |> List.of_seq
      in
      Ok (of_base_and_diffs base diffs)
    with Sys_error msg -> Error msg
end

module Print = struct
  let debug_string_of_sched_ver_history ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : t) =
    Debug_print.bprintf ~indent_level buffer "sched ver history\n";
    List.iteri
      (fun i sched ->
         Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "ver : %d\n"
           i
         |> ignore;
         Sched.Print.debug_string_of_sched ~indent_level:(indent_level + 2)
           ~buffer sched
         |> ignore)
      (List.rev t.history);
    Buffer.contents buffer

  let debug_print_sched_ver_history ?(indent_level = 0) (t : t) =
    print_string (debug_string_of_sched_ver_history ~indent_level t)
end
