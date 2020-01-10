open Int64_utils

type sched_id = int

type task_store = Task.task_data Task_id_map.t

type task_store_diff = Task.task_data Task_id_map_utils.diff

type task_inst_store = Task.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task.task_seg_size Task_seg_id_map_utils.diff

type transit_time_slice = {
  start : int64;
  len : int64;
}

type transit_time_record = transit_time_slice Int64_map.t

type transit_time_store = transit_time_record Transit_time_map.t

type transit_time_store_diff = transit_time_record Transit_time_map_utils.diff

type sched_req_store = Sched_req.sched_req_data Sched_req_id_map.t

type sched_req_store_diff = Sched_req.sched_req_data Sched_req_id_map.t

type sched_req_record_store = Sched_req.sched_req_record_data Sched_req_id_map.t

type sched_req_record_store_diff =
  Sched_req.sched_req_record_data Sched_req_id_map.t

type task_seg_place_map = Task_seg_place_set.t Int64_map.t

type task_seg_place_map_diff = Task_seg_place_set.t Int64_map_utils.diff

type store = {
  task_store : task_store;
  task_inst_store : task_inst_store;
  task_seg_store : task_seg_store;
  transit_time_store : transit_time_store;
  user_id_to_task_ids : Int64_set.t User_id_map.t;
  task_id_to_task_inst_ids : Int64_set.t Task_id_map.t;
  task_inst_id_to_task_seg_ids : Int64_set.t Task_inst_id_map.t;
  sched_req_ids : Int64_set.t;
  sched_req_pending_store : sched_req_store;
  sched_req_record_store : sched_req_record_store;
  quota : int64 Task_inst_id_map.t;
}

type store_diff = {
  task_store_diff : task_store_diff;
  task_inst_store_diff : task_inst_store_diff;
  task_seg_store_diff : task_seg_store_diff;
  transit_time_store_diff : transit_time_store_diff;
}

type agenda = {
  start_and_end_exc : (int64 * int64) option;
  indexed_by_start : task_seg_place_map;
}

type sched_data = {
  store : store;
  agenda : agenda;
}

let store_empty =
  {
    task_store = Task_id_map.empty;
    task_inst_store = Task_inst_id_map.empty;
    task_seg_store = Task_seg_id_map.empty;
    transit_time_store = Transit_time_map.empty;
    user_id_to_task_ids = User_id_map.empty;
    task_id_to_task_inst_ids = Task_id_map.empty;
    task_inst_id_to_task_seg_ids = Task_inst_id_map.empty;
    sched_req_ids = Int64_set.empty;
    sched_req_pending_store = Sched_req_id_map.empty;
    sched_req_record_store = Sched_req_id_map.empty;
    quota = Task_inst_id_map.empty;
  }

let agenda_empty =
  { start_and_end_exc = None; indexed_by_start = Int64_map.empty }

let sched_data_empty = { store = store_empty; agenda = agenda_empty }

type sched = sched_id * sched_data

let empty : sched = (0, sched_data_empty)

module Id = struct
  let sched_id_to_string (id : sched_id) = string_of_int id

  let incre_int64_id x = match x with None -> 0L | Some x -> Int64.succ x

  let get_new_task_id (user_id : Task.user_id) ((sid, sd) : sched) :
    Task.task_id * sched =
    let task_ids =
      User_id_map.find_opt user_id sd.store.user_id_to_task_ids
      |> Option.value ~default:Int64_set.empty
    in
    let task_part = Int64_set.max_elt_opt task_ids |> incre_int64_id in
    let task_ids = Int64_set.add task_part task_ids in
    ( (user_id, task_part),
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              user_id_to_task_ids =
                User_id_map.add user_id task_ids sd.store.user_id_to_task_ids;
            };
        } ) )

  let remove_task_id ((user_id, task_part) : Task.task_id) ((sid, sd) : sched) :
    sched =
    match User_id_map.find_opt user_id sd.store.user_id_to_task_ids with
    | None -> (sid, sd)
    | Some task_ids ->
      let task_ids = Int64_set.remove task_part task_ids in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              user_id_to_task_ids =
                User_id_map.add user_id task_ids sd.store.user_id_to_task_ids;
            };
        } )

  let get_new_task_inst_id (task_id : Task.task_id) ((sid, sd) : sched) :
    Task.task_inst_id * sched =
    let task_inst_ids =
      Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids
      |> Option.value ~default:Int64_set.empty
    in
    let task_inst_part =
      Int64_set.max_elt_opt task_inst_ids |> incre_int64_id
    in
    let task_inst_ids = Int64_set.add task_inst_part task_inst_ids in
    let user_id, task_part = task_id in
    ( (user_id, task_part, task_inst_part),
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_id_to_task_inst_ids =
                Task_id_map.add task_id task_inst_ids
                  sd.store.task_id_to_task_inst_ids;
            };
        } ) )

  let remove_task_inst_id
      ((user_id, task_part, task_inst_part) : Task.task_inst_id)
      ((sid, sd) : sched) : sched =
    let task_id = (user_id, task_part) in
    match Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids with
    | None -> (sid, sd)
    | Some task_inst_ids ->
      let task_inst_ids = Int64_set.remove task_inst_part task_inst_ids in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_id_to_task_inst_ids =
                Task_id_map.add task_id task_inst_ids
                  sd.store.task_id_to_task_inst_ids;
            };
        } )

  let get_new_task_seg_id (task_inst_id : Task.task_inst_id) ((sid, sd) : sched)
    : Task.task_seg_id * sched =
    let task_seg_ids =
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
      |> Option.value ~default:Int64_set.empty
    in
    let task_seg_part = Int64_set.max_elt_opt task_seg_ids |> incre_int64_id in
    let task_seg_ids = Int64_set.add task_seg_part task_seg_ids in
    let user_id, task_part, task_inst_part = task_inst_id in
    ( (user_id, task_part, task_inst_part, task_seg_part, None),
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_id_to_task_seg_ids =
                Task_inst_id_map.add task_inst_id task_seg_ids
                  sd.store.task_inst_id_to_task_seg_ids;
            };
        } ) )

  let remove_task_seg_id
      ((user_id, task_part, task_inst_part, task_seg_part, _) :
         Task.task_seg_id) ((sid, sd) : sched) : sched =
    let task_inst_id = (user_id, task_part, task_inst_part) in
    match
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
    with
    | None -> (sid, sd)
    | Some task_seg_ids ->
      let task_seg_ids = Int64_set.remove task_seg_part task_seg_ids in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_id_to_task_seg_ids =
                Task_inst_id_map.add task_inst_id task_seg_ids
                  sd.store.task_inst_id_to_task_seg_ids;
            };
        } )

  let get_new_sched_req_id ((sid, sd) : sched) : Sched_req.sched_req_id * sched
    =
    let sched_req_id =
      Int64_set.max_elt_opt sd.store.sched_req_ids |> incre_int64_id
    in
    let sched_req_ids = Int64_set.add sched_req_id sd.store.sched_req_ids in
    (sched_req_id, (sid, { sd with store = { sd.store with sched_req_ids } }))

  let remove_sched_req_id (sched_req_id : Sched_req.sched_req_id)
      ((sid, sd) : sched) : sched =
    let sched_req_ids = Int64_set.remove sched_req_id sd.store.sched_req_ids in
    (sid, { sd with store = { sd.store with sched_req_ids } })
end

module Time_slot = struct
  let get_occupied_time_slots ?start ?end_exc ((_sid, sd) : sched) :
    (int64 * int64) Seq.t =
    sd.agenda.indexed_by_start |> Int64_map.to_seq
    |> Seq.flat_map (fun (_start, bucket) ->
        bucket |> Task_seg_place_set.to_seq
        |> Seq.map (fun (_, start, end_exc) -> (start, end_exc)))
    |> (fun l ->
        Option.fold ~none:l ~some:(fun start -> Time_slot.slice ~start l) start)
    |> (fun l ->
        Option.fold ~none:l
          ~some:(fun end_exc -> Time_slot.slice ~end_exc l)
          end_exc)
    |> Time_slot.normalize ~skip_sort:true

  let get_free_time_slots ~start ~end_exc (sched : sched) :
    (int64 * int64) Seq.t =
    get_occupied_time_slots ~start ~end_exc sched
    |> Time_slot.invert ~start ~end_exc
end

module Quota_store = struct
  let update_quota quota ((sid, sd) : sched) : sched =
    (sid, { sd with store = { sd.store with quota } })

  let add_quota quota ((sid, sd) : sched) : sched =
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            quota =
              Task_inst_id_map.union
                (fun _ x y -> Some (x +^ y))
                sd.store.quota quota;
          };
      } )
end

module Task_seg_store = struct
  let add_task_seg ~(parent_task_inst_id : Task.task_inst_id)
      (size : Task.task_seg_size) ((sid, sd) : sched) : Task.task_seg * sched =
    let task_seg_id, (sid, sd) =
      Id.get_new_task_seg_id parent_task_inst_id (sid, sd)
    in
    ( (task_seg_id, size),
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_store =
                Task_seg_id_map.add task_seg_id size sd.store.task_seg_store;
            };
        } ) )

  let add_task_seg_via_task_seg_alloc_req
      ((parent_task_inst_id, task_seg_size) : Task.task_seg_alloc_req)
      (sched : sched) : Task.task_seg * sched =
    let task_seg, sched =
      add_task_seg ~parent_task_inst_id task_seg_size sched
    in
    (task_seg, sched)

  let add_task_segs_via_task_seg_alloc_req_list
      (reqs : Task.task_seg_alloc_req list) (sched : sched) :
    Task.task_seg list * sched =
    List.fold_left
      (fun (acc, sched) req ->
         let task_seg, sched = add_task_seg_via_task_seg_alloc_req req sched in
         (task_seg :: acc, sched))
      ([], sched) reqs
    |> fun (l, t) -> (List.rev l, t)

  let remove_task_seg (task_seg_id : Task.task_seg_id) (sched : sched) : sched =
    let sid, sd = Id.remove_task_seg_id task_seg_id sched in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_seg_store =
              Task_seg_id_map.remove task_seg_id sd.store.task_seg_store;
          };
      } )

  let diff = Task_seg_id_map_utils.diff
end

module Task_inst_store = struct
  let add_task_inst ~(parent_task_id : Task.task_id)
      (data : Task.task_inst_data) ((sid, sd) : sched) : Task.task_inst * sched
    =
    let task_inst_id, (sid, sd) =
      Id.get_new_task_inst_id parent_task_id (sid, sd)
    in
    let task_inst_store =
      Task_inst_id_map.add task_inst_id data sd.store.task_inst_store
    in
    let quota =
      match data.task_inst_type with
      | Task.Reminder_quota_counting { quota } ->
        Task_inst_id_map.add task_inst_id quota sd.store.quota
      | Task.Reminder | Passing -> sd.store.quota
    in
    ( (task_inst_id, data),
      (sid, { sd with store = { sd.store with task_inst_store; quota } }) )

  let add_task_inst_list ~(parent_task_id : Task.task_id)
      (data_list : Task.task_inst_data list) (sched : sched) :
    Task.task_inst list * sched =
    List.fold_left
      (fun (acc, sched) data ->
         let inst, sched = add_task_inst ~parent_task_id data sched in
         (inst :: acc, sched))
      ([], sched) data_list
    |> fun (l, t) -> (List.rev l, t)

  let diff = Task_inst_id_map_utils.diff
end

module Task_store = struct
  let add_task ~(parent_user_id : Task.user_id) (data : Task.task_data)
      (task_inst_data_list : Task.task_inst_data list) ((sid, sd) : sched) :
    Task.task * Task.task_inst list * sched =
    let parent_task_id, (sid, sd) =
      Id.get_new_task_id parent_user_id (sid, sd)
    in
    let sd =
      {
        sd with
        store =
          {
            sd.store with
            task_store = Task_id_map.add parent_task_id data sd.store.task_store;
          };
      }
    in
    let inst_list, (sid, sd) =
      Task_inst_store.add_task_inst_list ~parent_task_id task_inst_data_list
        (sid, sd)
    in
    ((parent_task_id, data), inst_list, (sid, sd))

  let diff = Task_id_map_utils.diff
end

module Task_seg_place_map = struct
  let add_task_seg_place
      (((id1, id2, id3, id4, id5), start, end_exc) : Task.task_seg_place)
      ((sid, sd) : sched) : sched =
    let indexed_by_start =
      Int64_map.update start
        (fun bucket ->
           Some
             (Task_seg_place_set.add
                ((id1, id2, id3, id4, id5), start, end_exc)
                ( match bucket with
                  | None -> Task_seg_place_set.empty
                  | Some s -> s )))
        sd.agenda.indexed_by_start
    in
    let quota =
      Task_inst_id_map.update (id1, id2, id3)
        (Option.map (fun x -> x -^ (end_exc -^ start)))
        sd.store.quota
    in
    ( sid,
      {
        store = { sd.store with quota };
        agenda = { sd.agenda with indexed_by_start };
      } )

  let add_task_seg_place_list (task_seg_place_s : Task.task_seg_place list)
      (sched : sched) : sched =
    List.fold_left
      (fun acc task_seg_place -> add_task_seg_place task_seg_place acc)
      sched task_seg_place_s

  let add_task_seg_place_seq (task_seg_place_s : Task.task_seg_place Seq.t)
      (sched : sched) : sched =
    Seq.fold_left
      (fun acc task_seg_place -> add_task_seg_place task_seg_place acc)
      sched task_seg_place_s

  let remove_task_seg_place
      ((task_seg_id, start, end_exc) : Task.task_seg_place) ((sid, sd) : sched)
    : sched =
    let id1, id2, id3, _, _ = task_seg_id in
    let indexed_by_start =
      Int64_map.update start
        (fun bucket ->
           Option.map
             (fun bucket ->
                Task_seg_place_set.filter
                  (fun (x, _, _) -> x <> task_seg_id)
                  bucket)
             bucket)
        sd.agenda.indexed_by_start
    in
    let quota =
      Task_inst_id_map.update (id1, id2, id3)
        (Option.map (fun x -> x +^ (end_exc -^ start)))
        sd.store.quota
    in
    ( sid,
      {
        store = { sd.store with quota };
        agenda = { sd.agenda with indexed_by_start };
      } )

  let diff = Int64_map_utils.Int64_bucketed.diff_bucketed
end

module Sched_req_store = struct
  let queue_sched_req_data (sched_req_data : Sched_req.sched_req_data)
      (sched : sched) : sched =
    let sched_req_id, (sid, sd) = Id.get_new_sched_req_id sched in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            sched_req_pending_store =
              Sched_req_id_map.add sched_req_id sched_req_data
                sd.store.sched_req_pending_store;
          };
      } )

  let queue_sched_req_data_list
      (sched_req_data_list : Sched_req.sched_req_data list) (sched : sched) :
    sched =
    List.fold_left
      (fun sched sched_req_data -> queue_sched_req_data sched_req_data sched)
      sched sched_req_data_list

  let partition_pending_sched_reqs_based_on_time_period ~start ~end_exc
      ((_sid, sd) : sched) : sched_req_store * sched_req_store * sched_req_store
    =
    let fully_within, leftover =
      Sched_req_id_map.partition
        (fun id req_record_data ->
           Sched_req.sched_req_fully_within_time_period ~start ~end_exc
             (id, req_record_data))
        sd.store.sched_req_pending_store
    in
    let partially_within, leftover =
      Sched_req_id_map.partition
        (fun id req_record_data ->
           Sched_req.sched_req_fully_within_time_period ~start ~end_exc
             (id, req_record_data))
        leftover
    in
    (fully_within, partially_within, leftover)

  let allocate_task_segs_for_sched_req_data
      (sched_req_data : Sched_req.sched_req_data) (sched : sched) :
    Sched_req.sched_req_record_data * sched =
    match sched_req_data with
    | Sched_req_data_skeleton.Fixed { task_seg_related_data; start } ->
      let task_seg_related_data, sched =
        Task_seg_store.add_task_seg_via_task_seg_alloc_req
          task_seg_related_data sched
      in
      (Fixed { task_seg_related_data; start }, sched)
    | Shift (task_seg_alloc_reqs, time_slots) ->
      let task_segs, sched =
        Task_seg_store.add_task_segs_via_task_seg_alloc_req_list
          task_seg_alloc_reqs sched
      in
      (Shift (task_segs, time_slots), sched)
    | Split_and_shift (task_seg_alloc_req, time_slots) ->
      let task_seg, sched =
        Task_seg_store.add_task_seg_via_task_seg_alloc_req task_seg_alloc_req
          sched
      in
      (Split_and_shift (task_seg, time_slots), sched)
    | Split_even { task_seg_related_data; time_slots; buckets } ->
      let task_seg_related_data, sched =
        Task_seg_store.add_task_seg_via_task_seg_alloc_req
          task_seg_related_data sched
      in
      (Split_even { task_seg_related_data; time_slots; buckets }, sched)
    | Time_share (task_seg_alloc_reqs, time_slots) ->
      let task_segs, sched =
        Task_seg_store.add_task_segs_via_task_seg_alloc_req_list
          task_seg_alloc_reqs sched
      in
      (Time_share (task_segs, time_slots), sched)
    | Push_to (direction, task_seg_alloc_req, time_slots) ->
      let task_seg, sched =
        Task_seg_store.add_task_seg_via_task_seg_alloc_req task_seg_alloc_req
          sched
      in
      (Push_to (direction, task_seg, time_slots), sched)

  let allocate_task_segs_for_sched_req_list
      (sched_req_list : Sched_req.sched_req list) (sched : sched) :
    Sched_req.sched_req_record list * sched =
    List.fold_left
      (fun (acc, sched) (sched_req_id, sched_req_data) ->
         let sched_req_record_data, (sid, sd) =
           allocate_task_segs_for_sched_req_data sched_req_data sched
         in
         ( (sched_req_id, sched_req_record_data) :: acc,
           ( sid,
             {
               sd with
               store =
                 {
                   sd.store with
                   sched_req_record_store =
                     Sched_req_id_map.add sched_req_id sched_req_record_data
                       sd.store.sched_req_record_store;
                 };
             } ) ))
      ([], sched) sched_req_list
    |> fun (l, sched) -> (List.rev l, sched)

  let allocate_task_segs_for_pending_sched_reqs ~start ~end_exc
      ~(include_sched_reqs_partially_within_time_period : bool)
      ~(up_to_sched_req_id_inc : Sched_req.sched_req_id option)
      ((sid, sd) : sched) : Sched_req.sched_req_record list * sched =
    let fully_within, partially_within, leftover =
      partition_pending_sched_reqs_based_on_time_period ~start ~end_exc (sid, sd)
    in
    let to_be_scheduled_candidates, leftover =
      if include_sched_reqs_partially_within_time_period then
        ( Sched_req_id_map.union
            (fun _ _ _ -> None)
            fully_within partially_within,
          leftover )
      else
        ( fully_within,
          Sched_req_id_map.union (fun _ _ _ -> None) partially_within leftover
        )
    in
    let to_be_scheduled, leftover =
      match up_to_sched_req_id_inc with
      | None -> (to_be_scheduled_candidates, leftover)
      | Some up_to_id_inc -> (
          let lt, eq, gt =
            Sched_req_id_map.split up_to_id_inc to_be_scheduled_candidates
          in
          let leftover =
            Sched_req_id_map.union (fun _ _ _ -> None) gt leftover
          in
          match eq with
          | None -> (lt, leftover)
          | Some eq -> (Sched_req_id_map.add up_to_id_inc eq lt, leftover) )
    in
    let to_be_scheduled_sched_reqs =
      to_be_scheduled |> Sched_req_id_map.to_seq |> List.of_seq
    in
    let sd =
      { sd with store = { sd.store with sched_req_pending_store = leftover } }
    in
    allocate_task_segs_for_sched_req_list to_be_scheduled_sched_reqs (sid, sd)
end

module Sched_req_utils = struct
end

module Recur = struct
  let instantiate_raw_seq ~start ~end_exc (task_data : Task.task_data) :
    (Task.task_inst_data * Task.task_seg_size Sched_req_data_skeleton.t list)
      Seq.t =
    match task_data.task_type with
    | Task.One_off -> Seq.empty
    | Task.Recurring recur -> (
        match recur with
        | Task.Arithemtic_seq
            ( { start = seq_start; end_exc = seq_end_exc; diff },
              { task_inst_data; sched_req_templates } ) ->
          let rec aux cur end_exc diff task_inst_data sched_req_templates =
            if cur < end_exc then
              let sched_reqs =
                Sched_req_data_skeleton.shift_time_list ~offset:cur
                  sched_req_templates
              in
              fun () ->
                Seq.Cons
                  ( (task_inst_data, sched_reqs),
                    aux (cur +^ diff) end_exc diff task_inst_data
                      sched_req_templates )
            else Seq.empty
          in
          let start =
            if start < seq_start then seq_start
            else seq_start +^ ((start -^ seq_start) /^ diff *^ diff)
          in
          let end_exc = min seq_end_exc end_exc in
          aux start end_exc diff task_inst_data sched_req_templates
        | Task.Time_pattern_match _ -> failwith "Unimplemented" )

  let instance_recorded_already (task_id : Task.task_id)
      (task_inst_data : Task.task_inst_data)
      (sched_req_data_list : Task.task_seg_size Sched_req_data_skeleton.t list)
      ((_sid, sd) : sched) : bool =
    let user_id, task_part = task_id in
    match Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids with
    | None -> false
    | Some task_inst_ids ->
      Int64_set.exists
        (fun task_inst_part ->
           let task_inst_id = (user_id, task_part, task_inst_part) in
           Printf.printf "Checking for task inst : %s\n" (Task.task_inst_id_to_string task_inst_id);
           let stored_task_inst_data =
             Task_inst_id_map.find task_inst_id sd.store.task_inst_store
           in
           task_inst_data = stored_task_inst_data
           && List.exists
             (fun sched_req_data ->
                Sched_req_id_map.exists
                  (fun _sched_req_id sched_req_record_data ->
                     match (sched_req_data, sched_req_record_data) with
                     | ( Sched_req_data_skeleton.Fixed
                           { task_seg_related_data = size1; start = start1 },
                         Sched_req_data_skeleton.Fixed
                           { task_seg_related_data = _id, size2; start = start2 }
                       ) ->
                       size1 = size2 && start1 = start2
                     | Sched_req_data_skeleton.Shift (sizes1, time_slots1),
                       Sched_req_data_skeleton.Shift (task_segs, time_slots2)
                     | Sched_req_data_skeleton.Time_share (sizes1, time_slots1),
                       Sched_req_data_skeleton.Time_share (task_segs, time_slots2) ->
                       let sizes2 = List.map (fun (_, size) -> size) task_segs in
                       List.sort_uniq compare sizes1 = List.sort_uniq compare sizes2 &&
                       List.sort_uniq compare time_slots1 = List.sort_uniq compare time_slots2
                     | Sched_req_data_skeleton.Split_and_shift (size1, time_slots1),
                       Sched_req_data_skeleton.Split_and_shift ((_id, size2), time_slots2) ->
                       size1 = size2 &&
                       List.sort_uniq compare time_slots1 = List.sort_uniq compare time_slots2
                     | Sched_req_data_skeleton.Split_even { task_seg_related_data = size1;
                                                            time_slots = time_slots1;
                                                            buckets = buckets1 },
                       Sched_req_data_skeleton.Split_even { task_seg_related_data = (_id, size2);
                                                            time_slots = time_slots2;
                                                            buckets = buckets2 } ->
                       size1 = size2 &&
                       List.sort_uniq compare time_slots1 = List.sort_uniq compare time_slots2 &&
                       List.sort_uniq compare buckets1 = List.sort_uniq compare buckets2
                     | Push_to (dir1, size1, time_slots1), Push_to (dir2, (_id, size2), time_slots2) ->
                       dir1 = dir2 && size1 = size2 && List.sort_uniq compare time_slots1 = List.sort_uniq compare time_slots2
                     | _ -> false)
                  sd.store.sched_req_record_store)
             sched_req_data_list)
        task_inst_ids

  let instantiate ~start ~end_exc ((sid, sd) : sched) : sched =
    Task_id_map.fold
      (fun task_id task_data sched ->
         let raw_seq = instantiate_raw_seq ~start ~end_exc task_data in
         raw_seq
         |> Seq.filter (fun (task_inst_data, sched_req_data_list) ->
             not (instance_recorded_already task_id task_inst_data sched_req_data_list sched)
           )
         |> Seq.fold_left
           (fun sched (task_inst_data, sched_req_templates) ->
              let (task_inst_id, _), sched =
                Task_inst_store.add_task_inst ~parent_task_id:task_id
                  task_inst_data sched
              in
              let sched_req_data_list =
                Sched_req_data_skeleton.map_list
                  (fun task_seg_size -> (task_inst_id, task_seg_size))
                  sched_req_templates
              in
              Sched_req_store.queue_sched_req_data_list sched_req_data_list
                sched)
           sched)
      sd.store.task_store (sid, sd)
end

module Print = struct
  let debug_print_sched ?(indent_level = 0) (sid, sd) =
    Debug_print.printf ~indent_level "schedule id : %s\n"
      (Id.sched_id_to_string sid);
    Debug_print.printf ~indent_level:(indent_level + 1)
      "pending scheduling requests :\n";
    Sched_req_id_map.iter
      (fun id data ->
         Sched_req.Print.debug_print_sched_req ~indent_level:(indent_level + 2)
           (id, data))
      sd.store.sched_req_pending_store;
    Debug_print.printf ~indent_level:(indent_level + 1) "tasks :\n";
    Task_id_map.iter
      (fun id data ->
         Task.Print.debug_print_task ~indent_level:(indent_level + 2) (id, data))
      sd.store.task_store;
    Debug_print.printf ~indent_level:(indent_level + 1) "task insts :\n";
    Task_inst_id_map.iter
      (fun id data ->
         Task.Print.debug_print_task_inst ~indent_level:(indent_level + 2)
           (id, data))
      sd.store.task_inst_store;
    Debug_print.printf ~indent_level:(indent_level + 1) "task segs :\n";
    Task_seg_id_map.iter
      (fun id data ->
         Task.Print.debug_print_task_seg ~indent_level:(indent_level + 2)
           (id, data))
      sd.store.task_seg_store;
    Debug_print.printf ~indent_level:(indent_level + 1) "agenda :\n";
    Int64_map.iter
      (fun _start bucket ->
         Task_seg_place_set.iter
           (fun (id, start, end_exc) ->
              Debug_print.printf ~indent_level:(indent_level + 2)
                "%Ld - %Ld | %s\n" start end_exc
                (Task.task_seg_id_to_string id))
           bucket)
      sd.agenda.indexed_by_start;
    Debug_print.printf ~indent_level:(indent_level + 1) "leftover quota :\n";
    Task_inst_id_map.iter
      (fun id quota ->
         Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
           (Task.task_inst_id_to_string id)
           quota)
      sd.store.quota
end