open Int64_utils

module TS = Time_slot

type sched_id = int

type task_store = Task.task_data Task_id_map.t

type task_store_diff = Task.task_data Task_id_map_utils.diff

type task_inst_store = Task.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task.task_seg_size Task_seg_id_map_utils.diff

(* type transit_time_slice = {
 *   start : int64;
 *   len : int64;
 * } *)

(* type transit_time_record = transit_time_slice Int64_map.t
 * 
 * type transit_time_store = transit_time_record Transit_time_map.t
 * 
 * type transit_time_store_diff = transit_time_record Transit_time_map_utils.diff *)

type sched_req_store = Sched_req.sched_req_data Sched_req_id_map.t

type sched_req_store_diff = Sched_req.sched_req_data Sched_req_id_map_utils.diff

type sched_req_record_store = Sched_req.sched_req_record_data Sched_req_id_map.t

type sched_req_record_store_diff =
  Sched_req.sched_req_record_data Sched_req_id_map_utils.diff

type task_seg_place_map = Task_seg_place_set.t Int64_map.t

type task_seg_place_map_diff =
  Int64_map_utils.Task_seg_place_bucketed.diff_bucketed

type store = {
  task_store : task_store;
  task_inst_store : task_inst_store;
  task_seg_store : task_seg_store;
  (* transit_time_store : transit_time_store; *)
  user_id_to_task_ids : Int64_set.t User_id_map.t;
  task_id_to_task_inst_ids : Int64_set.t Task_id_map.t;
  task_inst_id_to_task_seg_ids : Int64_set.t Task_inst_id_map.t;
  sched_req_ids : Int64_set.t;
  sched_req_pending_store : sched_req_store;
  sched_req_record_store : sched_req_record_store;
  quota : int64 Task_inst_id_map.t;
  progress_indexed_by_start : task_seg_place_map;
}

type store_diff = {
  task_store_diff : task_store_diff;
  task_inst_store_diff : task_inst_store_diff;
  task_seg_store_diff : task_seg_store_diff;
  (* transit_time_store_diff : transit_time_store_diff; *)
  user_id_to_task_ids_diff : User_id_map_utils.Int64_bucketed.diff_bucketed;
  task_id_to_task_inst_ids_diff :
    Task_id_map_utils.Int64_bucketed.diff_bucketed;
  task_inst_id_to_task_seg_ids_diff :
    Task_inst_id_map_utils.Int64_bucketed.diff_bucketed;
  sched_req_ids_diff : Int64_set_utils.diff;
  sched_req_pending_store_diff : sched_req_store_diff;
  sched_req_record_store_diff : sched_req_record_store_diff;
  quota_diff : int64 Task_inst_id_map_utils.diff;
  progress_indexed_by_start_diff : task_seg_place_map_diff;
}

type agenda = {
  (* start_and_end_exc : (int64 * int64) option; *)
  indexed_by_start : task_seg_place_map;
}

type agenda_diff = { indexed_by_start_diff : task_seg_place_map_diff }

type sched_data = {
  store : store;
  agenda : agenda;
}

type sched_data_diff = {
  store_diff : store_diff;
  agenda_diff : agenda_diff;
}

type sched = sched_id * sched_data

type sched_diff = sched_id * sched_id * sched_data_diff

let store_empty =
  {
    task_store = Task_id_map.empty;
    task_inst_store = Task_inst_id_map.empty;
    task_seg_store = Task_seg_id_map.empty;
    (* transit_time_store = Transit_time_map.empty; *)
    user_id_to_task_ids = User_id_map.empty;
    task_id_to_task_inst_ids = Task_id_map.empty;
    task_inst_id_to_task_seg_ids = Task_inst_id_map.empty;
    sched_req_ids = Int64_set.empty;
    sched_req_pending_store = Sched_req_id_map.empty;
    sched_req_record_store = Sched_req_id_map.empty;
    quota = Task_inst_id_map.empty;
    progress_indexed_by_start = Int64_map.empty;
  }

let agenda_empty =
  { (* start_and_end_exc = None; *)
    indexed_by_start = Int64_map.empty }

let sched_data_empty = { store = store_empty; agenda = agenda_empty }

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

  let get_task_inst_id_seq ((id1, id2) : Task.task_id) ((_, sd) : sched) :
    Task.task_inst_id Seq.t =
    match Task_id_map.find_opt (id1, id2) sd.store.task_id_to_task_inst_ids with
    | None -> Seq.empty
    | Some s -> s |> Int64_set.to_seq |> Seq.map (fun id -> (id1, id2, id))

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

  let get_task_seg_id_seq ((id1, id2, id3) : Task.task_inst_id)
      ((_, sd) : sched) : Task.task_seg_id Seq.t =
    match
      Task_inst_id_map.find_opt (id1, id2, id3)
        sd.store.task_inst_id_to_task_seg_ids
    with
    | None -> Seq.empty
    | Some s ->
      s |> Int64_set.to_seq |> Seq.map (fun id -> (id1, id2, id3, id, None))

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
    sd.agenda.indexed_by_start
    |> Int64_map.to_seq
    |> Seq.flat_map (fun (_start, bucket) ->
        bucket
        |> Task_seg_place_set.to_seq
        |> Seq.map (fun (_, start, end_exc) -> (start, end_exc)))
    |> (fun l ->
        Option.fold ~none:l ~some:(fun start -> TS.slice ~start l) start)
    |> (fun l ->
        Option.fold ~none:l
          ~some:(fun end_exc -> TS.slice ~end_exc l)
          end_exc)
    |> TS.normalize ~skip_sort:true

  let get_free_time_slots ~start ~end_exc (sched : sched) :
    (int64 * int64) Seq.t =
    get_occupied_time_slots ~start ~end_exc sched
    |> TS.invert ~start ~end_exc
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

  let find_task_seg_opt (task_seg_id : Task.task_seg_id) ((_, sd) : sched) :
    Task.task_seg_size option =
    Task_seg_id_map.find_opt task_seg_id sd.store.task_seg_store

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

  let remove_task_seg_strict (task_seg_id : Task.task_seg_id) (sched : sched) :
    (sched, unit) result =
    match find_task_seg_opt task_seg_id sched with
    | None -> Error ()
    | Some _ -> Ok (remove_task_seg task_seg_id sched)

  let remove_task_seg_seq (task_seg_ids : Task.task_seg_id Seq.t)
      (sched : sched) : sched =
    Seq.fold_left
      (fun sched task_seg_id -> remove_task_seg task_seg_id sched)
      sched task_seg_ids
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

  let find_task_inst_opt (task_inst_id : Task.task_inst_id) ((_, sd) : sched) :
    Task.task_inst_data option =
    Task_inst_id_map.find_opt task_inst_id sd.store.task_inst_store

  let remove_task_inst (task_inst_id : Task.task_inst_id) (sched : sched) :
    sched =
    let children_task_seg_ids = Id.get_task_seg_id_seq task_inst_id sched in
    sched
    |> Task_seg_store.remove_task_seg_seq children_task_seg_ids
    |> fun (sid, sd) ->
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_inst_store =
              Task_inst_id_map.remove task_inst_id sd.store.task_inst_store;
            task_inst_id_to_task_seg_ids =
              Task_inst_id_map.remove task_inst_id
                sd.store.task_inst_id_to_task_seg_ids;
          };
      } )

  let remove_task_inst_strict (task_inst_id : Task.task_inst_id) (sched : sched)
    : (sched, unit) result =
    match find_task_inst_opt task_inst_id sched with
    | None -> Error ()
    | Some _ -> Ok (remove_task_inst task_inst_id sched)

  let remove_task_inst_seq (task_inst_ids : Task.task_inst_id Seq.t)
      (sched : sched) : sched =
    Seq.fold_left
      (fun sched task_inst_id -> remove_task_inst task_inst_id sched)
      sched task_inst_ids
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

  let find_task_opt (task_id : Task.task_id) ((_, sd) : sched) :
    Task.task_data option =
    Task_id_map.find_opt task_id sd.store.task_store

  let remove_task (task_id : Task.task_id) (sched : sched) : sched =
    let children_task_inst_ids = Id.get_task_inst_id_seq task_id sched in
    sched
    |> Task_inst_store.remove_task_inst_seq children_task_inst_ids
    |> fun (sid, sd) ->
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_store = Task_id_map.remove task_id sd.store.task_store;
            task_id_to_task_inst_ids =
              Task_id_map.remove task_id sd.store.task_id_to_task_inst_ids;
          };
      } )

  let remove_task_strict task_id sched : (sched, unit) result =
    match find_task_opt task_id sched with
    | None -> Error ()
    | Some _ -> Ok (remove_task task_id sched)
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
        (* agenda = { sd.agenda with indexed_by_start }; *)
        agenda = { indexed_by_start };
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

  let filter_task_seg_place_seq (f : Task.task_seg_place -> bool)
      ((_, sd) : sched) : Task.task_seg_place Seq.t =
    sd.agenda.indexed_by_start
    |> Int64_map.to_seq
    |> Seq.flat_map (fun (_, s) -> Task_seg_place_set.to_seq s)
    |> Seq.filter f

  let find_task_seg_place_seq_by_task_id (task_id : Task.task_id)
      (sched : sched) : Task.task_seg_place Seq.t =
    filter_task_seg_place_seq
      (fun ((id1, id2, _id3, _id4, _id5), _, _) -> (id1, id2) = task_id)
      sched

  let find_task_seg_place_seq_by_task_inst_id (task_inst_id : Task.task_inst_id)
      (sched : sched) : Task.task_seg_place Seq.t =
    filter_task_seg_place_seq
      (fun ((id1, id2, id3, _id4, _id5), _, _) ->
         (id1, id2, id3) = task_inst_id)
      sched

  let find_task_seg_place_seq_by_task_seg_id (task_seg_id : Task.task_seg_id)
      ((_, sd) : sched) : Task.task_seg_place Seq.t =
    sd.agenda.indexed_by_start
    |> Int64_map.to_seq
    |> Seq.flat_map (fun (_, s) -> Task_seg_place_set.to_seq s)
    |> Seq.filter (fun (id, _, _) -> task_seg_id = id)

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
    (sid, { store = { sd.store with quota }; agenda = { indexed_by_start } })

  let remove_task_seg_place_seq (task_seg_place_seq : Task.task_seg_place Seq.t)
      (sched : sched) : sched =
    Seq.fold_left
      (fun sched task_seg_place -> remove_task_seg_place task_seg_place sched)
      sched task_seg_place_seq

  let remove_task_seg_place_by_task_id task_id sched =
    let s = find_task_seg_place_seq_by_task_id task_id sched in
    remove_task_seg_place_seq s sched

  let remove_task_seg_place_by_task_inst_id task_inst_id sched =
    let s = find_task_seg_place_seq_by_task_inst_id task_inst_id sched in
    remove_task_seg_place_seq s sched

  let remove_task_seg_place_by_task_seg_id task_seg_id sched =
    let s = find_task_seg_place_seq_by_task_seg_id task_seg_id sched in
    remove_task_seg_place_seq s sched
end

module Sched_req_store = struct
  let queue_sched_req_data (sched_req_data : Sched_req.sched_req_data)
      (sched : sched) : Sched_req.sched_req * sched =
    let sched_req_id, (sid, sd) = Id.get_new_sched_req_id sched in
    ( (sched_req_id, sched_req_data),
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
        } ) )

  let queue_sched_req_data_list
      (sched_req_data_list : Sched_req.sched_req_data list) (sched : sched) :
    Sched_req.sched_req list * sched =
    List.fold_left
      (fun (sched_reqs, sched) sched_req_data ->
         let sched_req, sched = queue_sched_req_data sched_req_data sched in
         (sched_req :: sched_reqs, sched))
      ([], sched) sched_req_data_list
    |> fun (l, s) -> (List.rev l, s)

  let unqueue_sched_req (sched_req_id : int64) ((sid, sd) : sched) : sched =
    match
      Sched_req_id_map.find_opt sched_req_id sd.store.sched_req_pending_store
    with
    | None -> (sid, sd)
    | Some _ ->
      let sid, sd = Id.remove_sched_req_id sched_req_id (sid, sd) in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              sched_req_pending_store =
                Sched_req_id_map.remove sched_req_id
                  sd.store.sched_req_pending_store;
            };
        } )

  let filter_sched_req_record_seq (f : Sched_req.sched_req_record -> bool)
      ((_, sd) : sched) : Sched_req.sched_req_record Seq.t =
    sd.store.sched_req_record_store |> Sched_req_id_map.to_seq |> Seq.filter f

  let find_sched_req_record_by_task_id ((id1, id2) : Task.task_id)
      (sched : sched) : Sched_req.sched_req_record Seq.t =
    filter_sched_req_record_seq
      (fun (_, l) ->
         List.exists
           (fun x ->
              List.exists
                (fun ((id1', id2', _id3', _id4', _id5'), _) ->
                   id1 = id1' && id2 = id2')
                (Sched_req_data_unit_skeleton.get_data x))
           l)
      sched

  let find_sched_req_record_by_task_inst_id
      ((id1, id2, id3) : Task.task_inst_id) (sched : sched) :
    Sched_req.sched_req_record Seq.t =
    filter_sched_req_record_seq
      (fun (_, l) ->
         List.exists
           (fun x ->
              List.exists
                (fun ((id1', id2', id3', _id4', _id5'), _) ->
                   id1 = id1' && id2 = id2' && id3 = id3')
                (Sched_req_data_unit_skeleton.get_data x))
           l)
      sched

  let find_sched_req_record_by_task_seg_id
      ((id1, id2, id3, id4, _id5) : Task.task_seg_id) (sched : sched) :
    Sched_req.sched_req_record Seq.t =
    filter_sched_req_record_seq
      (fun (_, l) ->
         List.exists
           (fun x ->
              List.exists
                (fun ((id1', id2', id3', id4', _id5'), _) ->
                   id1 = id1' && id2 = id2' && id3 = id3' && id4 = id4')
                (Sched_req_data_unit_skeleton.get_data x))
           l)
      sched

  let remove_sched_req_record_if_contains_matching_task_seg
      (f : Task.task_seg -> bool) ((sid, sd) : sched) : sched =
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            sched_req_record_store =
              sd.store.sched_req_record_store
              |> Sched_req_id_map.to_seq
              |> Seq.filter_map (fun (id, l) ->
                  let l =
                    List.filter
                      (fun sched_req_record ->
                         not
                           (List.exists f
                              (Sched_req_data_unit_skeleton.get_data
                                 sched_req_record)))
                      l
                  in
                  match l with [] -> None | _ -> Some (id, l))
              |> Sched_req_id_map.of_seq;
          };
      } )

  let remove_sched_req_record_by_task_id ((id1, id2) : Task.task_id)
      (sched : sched) : sched =
    remove_sched_req_record_if_contains_matching_task_seg
      (fun ((id1', id2', _id3', _id4', _id5'), _data) ->
         id1 = id1' && id2 = id2')
      sched

  let remove_sched_req_record_by_task_inst_id
      ((id1, id2, id3) : Task.task_inst_id) (sched : sched) : sched =
    remove_sched_req_record_if_contains_matching_task_seg
      (fun ((id1', id2', id3', _id4', _id5'), _data) ->
         id1 = id1' && id2 = id2' && id3 = id3')
      sched

  let remove_sched_req_record_by_task_seg_id
      ((id1, id2, id3, id4, _id5) : Task.task_seg_id) (sched : sched) : sched =
    remove_sched_req_record_if_contains_matching_task_seg
      (fun ((id1', id2', id3', id4', _id5'), _data) ->
         id1 = id1' && id2 = id2' && id3 = id3' && id4 = id4')
      sched

  let partition_pending_sched_reqs_based_on_time_period ~start ~end_exc
      ((_sid, sd) : sched) : sched_req_store * sched_req_store * sched_req_store
    =
    let fully_within, leftover =
      Sched_req_id_map.partition
        (fun id req_record_data_list ->
           Sched_req.sched_req_fully_within_time_period ~start ~end_exc
             (id, req_record_data_list))
        sd.store.sched_req_pending_store
    in
    let partially_within, leftover =
      Sched_req_id_map.partition
        (fun id req_record_data ->
           Sched_req.sched_req_partially_within_time_period ~start ~end_exc
             (id, req_record_data))
        leftover
    in
    (fully_within, partially_within, leftover)

  let allocate_task_segs_for_sched_req_data
      (sched_req_data : Sched_req.sched_req_data) (sched : sched) :
    Sched_req.sched_req_record_data * sched =
    List.fold_left
      (fun (acc, sched) sched_req_data_unit ->
         match sched_req_data_unit with
         | Sched_req_data_unit_skeleton.Fixed { task_seg_related_data; start } ->
           let task_seg_related_data, sched =
             Task_seg_store.add_task_seg_via_task_seg_alloc_req
               task_seg_related_data sched
           in
           ( Sched_req_data_unit_skeleton.Fixed { task_seg_related_data; start }
             :: acc,
             sched )
         | Shift x ->
           let task_seg_related_data_list, sched =
             Task_seg_store.add_task_segs_via_task_seg_alloc_req_list
               x.task_seg_related_data_list sched
           in
           (Shift { x with task_seg_related_data_list } :: acc, sched)
         | Split_and_shift x ->
           let task_seg_related_data, sched =
             Task_seg_store.add_task_seg_via_task_seg_alloc_req
               x.task_seg_related_data sched
           in
           (Split_and_shift { x with task_seg_related_data } :: acc, sched)
         | Split_even x ->
           let task_seg_related_data, sched =
             Task_seg_store.add_task_seg_via_task_seg_alloc_req
               x.task_seg_related_data sched
           in
           (Split_even { x with task_seg_related_data } :: acc, sched)
         | Time_share x ->
           let task_seg_related_data_list, sched =
             Task_seg_store.add_task_segs_via_task_seg_alloc_req_list
               x.task_seg_related_data_list sched
           in
           (Time_share { x with task_seg_related_data_list } :: acc, sched)
         | Push_toward x ->
           let task_seg_related_data, sched =
             Task_seg_store.add_task_seg_via_task_seg_alloc_req
               x.task_seg_related_data sched
           in
           (Push_toward { x with task_seg_related_data } :: acc, sched))
      ([], sched) sched_req_data

  let allocate_task_segs_for_sched_req_list
      (sched_req_list : Sched_req.sched_req list) (sched : sched) :
    Sched_req.sched_req_record list * sched =
    List.fold_left
      (fun (acc, sched) (sched_req_id, sched_req_data) ->
         let sched_req_record_data_list, (sid, sd) =
           allocate_task_segs_for_sched_req_data sched_req_data sched
         in
         ( (sched_req_id, sched_req_record_data_list) :: acc,
           ( sid,
             {
               sd with
               store =
                 {
                   sd.store with
                   sched_req_record_store =
                     Sched_req_id_map.add sched_req_id sched_req_record_data_list
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

module Recur = struct
  let instantiate_raw_seq ~start ~end_exc (task_data : Task.task_data) :
    (Task.task_inst_data * Task.sched_req_template) Seq.t =
    match task_data.task_type with
    | Task.One_off -> Seq.empty
    | Task.Recurring recur -> (
        let usable_time_slots = TS.invert ~start ~end_exc (List.to_seq recur.excluded_time_slots) in
        match recur.recur_type with
        | Task.Arithemtic_seq
            ( { start = seq_start; end_exc = seq_end_exc; diff },
              { task_inst_data; sched_req_template } ) ->
          let rec aux cur end_exc diff task_inst_data sched_req_templates =
            if cur < end_exc then
              let sched_reqs =
                Sched_req_data_unit_skeleton.shift_time_list ~offset:cur
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
          aux start end_exc diff task_inst_data sched_req_template
        | Task.Time_pattern_match _ -> failwith "Unimplemented" )

  let instance_recorded_already (task_id : Task.task_id)
      (task_inst_data : Task.task_inst_data)
      (sched_req_template : Task.sched_req_template)
      ((_sid, sd) : sched) : bool =
    let user_id, task_part = task_id in
    match Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids with
    | None -> false
    | Some task_inst_ids ->
      Int64_set.exists
        (fun task_inst_part ->
           let task_inst_id = (user_id, task_part, task_inst_part) in
           let stored_task_inst_data =
             Task_inst_id_map.find task_inst_id sd.store.task_inst_store
           in
           task_inst_data = stored_task_inst_data
           && ( List.exists
                  (fun sched_req_template_data_unit ->
                     Sched_req_id_map.exists
                       (fun _sched_req_id sched_req_record_data ->
                          Sched_req_utils
                          .sched_req_template_data_unit_matches_sched_req_record_data
                            sched_req_template_data_unit sched_req_record_data
                       )
                       sd.store.sched_req_record_store)
                  sched_req_template
                || List.exists
                  (fun sched_req_template ->
                     Sched_req_id_map.exists
                       (fun _sched_req_id sched_req_data_unit ->
                          Sched_req_utils
                          .sched_req_template_matches_sched_req_data
                            sched_req_template sched_req_data_unit)
                       sd.store.sched_req_pending_store)
                  sched_req_template ))
        task_inst_ids

  let instantiate ~start ~end_exc ((sid, sd) : sched) : sched =
    Task_id_map.fold
      (fun task_id task_data sched ->
         let raw_seq = instantiate_raw_seq ~start ~end_exc task_data in
         raw_seq
         |> Seq.filter (fun (task_inst_data, sched_req_template) ->
             not
               (instance_recorded_already task_id task_inst_data
                  sched_req_template sched))
         |> Seq.fold_left
           (fun sched (task_inst_data, sched_req_templates) ->
              let (task_inst_id, _), sched =
                Task_inst_store.add_task_inst ~parent_task_id:task_id
                  task_inst_data sched
              in
              let sched_req_data =
                Sched_req_data_unit_skeleton.map_list
                  ~f_data:(fun task_seg_size -> (task_inst_id, task_seg_size))
                  ~f_time:(fun x -> x)
                  ~f_time_slot:(fun x -> x)
                  sched_req_templates
              in
              let _, sched =
                Sched_req_store.queue_sched_req_data sched_req_data sched
              in
              sched)
           sched)
      sd.store.task_store (sid, sd)
end

module Serialize = struct
  (*$ #use "lib/sched.cinaps";;

    Store.print_pack_related_functions ()
  *)

  let pack_task_store (x : task_store) : Sched_t.task list =
    x |> Task_id_map.to_seq |> Seq.map Task.Serialize.pack_task |> List.of_seq

  let pack_task_store_diff (x : task_store_diff) :
    (Task_t.task_id, Task_t.task_data) Map_utils_t.diff =
    { added = pack_task_store x.added; removed = pack_task_store x.removed }

  let pack_task_inst_store (x : task_inst_store) : Sched_t.task_inst list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map Task.Serialize.pack_task_inst
    |> List.of_seq

  let pack_task_inst_store_diff (x : task_inst_store_diff) :
    (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff =
    {
      added = pack_task_inst_store x.added;
      removed = pack_task_inst_store x.removed;
    }

  let pack_task_seg_store (x : task_seg_store) : Sched_t.task_seg list =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map Task.Serialize.pack_task_seg
    |> List.of_seq

  let pack_task_seg_store_diff (x : task_seg_store_diff) :
    (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff =
    {
      added = pack_task_seg_store x.added;
      removed = pack_task_seg_store x.removed;
    }

  let pack_sched_req_pending_store (x : sched_req_store) :
    Sched_req_t.sched_req list =
    x
    |> Sched_req_id_map.to_seq
    |> Seq.map Sched_req.Serialize.pack_sched_req
    |> List.of_seq

  let pack_sched_req_pending_store_diff (x : sched_req_store_diff) :
    (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff =
    {
      added = pack_sched_req_pending_store x.added;
      removed = pack_sched_req_pending_store x.removed;
    }

  let pack_sched_req_record_store (x : sched_req_record_store) :
    Sched_req_t.sched_req_record list =
    x
    |> Sched_req_id_map.to_seq
    |> Seq.map Sched_req.Serialize.pack_sched_req_record
    |> List.of_seq

  let pack_sched_req_record_store_diff (x : sched_req_record_store_diff) :
    ( Sched_req_t.sched_req_id,
      Sched_req_t.sched_req_record_data )
      Map_utils_t.diff =
    {
      added = pack_sched_req_record_store x.added;
      removed = pack_sched_req_record_store x.removed;
    }

  let pack_quota (x : int64 Task_inst_id_map.t) :
    (Task.task_inst_id * int64) list =
    x |> Task_inst_id_map.to_seq |> List.of_seq

  let pack_quota_diff (x : int64 Task_inst_id_map_utils.diff) :
    (Task_t.task_inst_id, int64) Map_utils_t.diff =
    { added = pack_quota x.added; removed = pack_quota x.removed }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    Bucket_store.print_pack_related_functions ()
  *)

  let pack_user_id_to_task_ids (x : Int64_set.t User_id_map.t) :
    (Task_t.user_id * int64 list) list =
    x
    |> User_id_map.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Serialize.pack y))
    |> List.of_seq

  let pack_user_id_to_task_ids_diff
      (x : User_id_map_utils.Int64_bucketed.diff_bucketed) :
    (Task_t.user_id, int64) Map_utils_t.diff_bucketed =
    {
      added = pack_user_id_to_task_ids x.added;
      removed = pack_user_id_to_task_ids x.removed;
    }

  let pack_task_id_to_task_inst_ids (x : Int64_set.t Task_id_map.t) :
    (Task_t.task_id * int64 list) list =
    x
    |> Task_id_map.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Serialize.pack y))
    |> List.of_seq

  let pack_task_id_to_task_inst_ids_diff
      (x : Task_id_map_utils.Int64_bucketed.diff_bucketed) :
    (Task_t.task_id, int64) Map_utils_t.diff_bucketed =
    {
      added = pack_task_id_to_task_inst_ids x.added;
      removed = pack_task_id_to_task_inst_ids x.removed;
    }

  let pack_task_inst_id_to_task_seg_ids (x : Int64_set.t Task_inst_id_map.t) :
    (Task_t.task_inst_id * int64 list) list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Serialize.pack y))
    |> List.of_seq

  let pack_task_inst_id_to_task_seg_ids_diff
      (x : Task_inst_id_map_utils.Int64_bucketed.diff_bucketed) :
    (Task_t.task_inst_id, int64) Map_utils_t.diff_bucketed =
    {
      added = pack_task_inst_id_to_task_seg_ids x.added;
      removed = pack_task_inst_id_to_task_seg_ids x.removed;
    }

  let pack_progress_indexed_by_start (x : task_seg_place_map) :
    (int64 * Task_t.task_seg_place list) list =
    x
    |> Int64_map.to_seq
    |> Seq.map (fun (id, y) -> (id, Task_seg_place_set.Serialize.pack y))
    |> List.of_seq

  let pack_progress_indexed_by_start_diff (x : task_seg_place_map_diff) :
    (int64, Task_t.task_seg_place) Map_utils_t.diff_bucketed =
    {
      added = pack_progress_indexed_by_start x.added;
      removed = pack_progress_indexed_by_start x.removed;
    }

  let pack_indexed_by_start (x : task_seg_place_map) :
    (int64 * Task_t.task_seg_place list) list =
    x
    |> Int64_map.to_seq
    |> Seq.map (fun (id, y) -> (id, Task_seg_place_set.Serialize.pack y))
    |> List.of_seq

  let pack_indexed_by_start_diff (x : task_seg_place_map_diff) :
    (int64, Task_t.task_seg_place) Map_utils_t.diff_bucketed =
    {
      added = pack_indexed_by_start x.added;
      removed = pack_indexed_by_start x.removed;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    Set_store.print_pack_related_functions ()
  *)

  let pack_sched_req_ids (x : Int64_set.t) : int64 list =
    x |> Int64_set.to_seq |> List.of_seq

  let pack_sched_req_ids_diff (x : Int64_set_utils.diff) :
    int64 Set_utils_t.diff =
    {
      added = pack_sched_req_ids x.added;
      removed = pack_sched_req_ids x.removed;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    print_pack_store ();
    print_pack_store_diff ()
  *)

  let pack_store (store : store) : Sched_t.store =
    {
      task_list = pack_task_store store.task_store;
      task_inst_list = pack_task_inst_store store.task_inst_store;
      task_seg_list = pack_task_seg_store store.task_seg_store;
      user_id_to_task_ids = pack_user_id_to_task_ids store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        pack_task_id_to_task_inst_ids store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        pack_task_inst_id_to_task_seg_ids store.task_inst_id_to_task_seg_ids;
      sched_req_ids = pack_sched_req_ids store.sched_req_ids;
      sched_req_pending_list =
        pack_sched_req_pending_store store.sched_req_pending_store;
      sched_req_record_list =
        pack_sched_req_record_store store.sched_req_record_store;
      quota = pack_quota store.quota;
      progress_indexed_by_start =
        pack_progress_indexed_by_start store.progress_indexed_by_start;
    }

  let pack_store_diff (diff : store_diff) : Sched_t.store_diff =
    {
      task_list_diff = pack_task_store_diff diff.task_store_diff;
      task_inst_list_diff = pack_task_inst_store_diff diff.task_inst_store_diff;
      task_seg_list_diff = pack_task_seg_store_diff diff.task_seg_store_diff;
      user_id_to_task_ids_diff =
        pack_user_id_to_task_ids_diff diff.user_id_to_task_ids_diff;
      task_id_to_task_inst_ids_diff =
        pack_task_id_to_task_inst_ids_diff diff.task_id_to_task_inst_ids_diff;
      task_inst_id_to_task_seg_ids_diff =
        pack_task_inst_id_to_task_seg_ids_diff
          diff.task_inst_id_to_task_seg_ids_diff;
      sched_req_ids_diff = pack_sched_req_ids_diff diff.sched_req_ids_diff;
      sched_req_pending_list_diff =
        pack_sched_req_pending_store_diff diff.sched_req_pending_store_diff;
      sched_req_record_list_diff =
        pack_sched_req_record_store_diff diff.sched_req_record_store_diff;
      quota_diff = pack_quota_diff diff.quota_diff;
      progress_indexed_by_start_diff =
        pack_progress_indexed_by_start_diff diff.progress_indexed_by_start_diff;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    print_pack_agenda ();

    print_pack_agenda_diff ()
  *)

  let pack_agenda (agenda : agenda) : Sched_t.agenda =
    { indexed_by_start = pack_indexed_by_start agenda.indexed_by_start }

  let pack_agenda_diff (diff : agenda_diff) : Sched_t.agenda_diff =
    {
      indexed_by_start_diff =
        pack_indexed_by_start_diff diff.indexed_by_start_diff;
    }

  (*$*)

  let pack_sched ((sid, sd) : sched) : Sched_t.sched =
    (sid, { store = pack_store sd.store; agenda = pack_agenda sd.agenda })

  let pack_sched_diff ((sid_old, sid_new, sd_diff) : sched_diff) :
    Sched_t.sched_diff =
    ( sid_old,
      sid_new,
      {
        store_diff = pack_store_diff sd_diff.store_diff;
        agenda_diff = pack_agenda_diff sd_diff.agenda_diff;
      } )

  let json_string_of_sched (sched : sched) : string =
    sched |> pack_sched |> Sched_j.string_of_sched

  let json_string_of_sched_diff (diff : sched_diff) : string =
    diff |> pack_sched_diff |> Sched_j.string_of_sched_diff
end

module Deserialize = struct
  (*$ #use "lib/sched.cinaps";;

    Store.print_unpack_related_functions ()
  *)

  let unpack_task_list (x : Sched_t.task list) : task_store =
    x
    |> List.to_seq
    |> Seq.map Task.Deserialize.unpack_task
    |> Task_id_map.of_seq

  let unpack_task_list_diff
      (x : (Task_t.task_id, Task_t.task_data) Map_utils_t.diff) :
    task_store_diff =
    { added = unpack_task_list x.added; removed = unpack_task_list x.removed }

  let unpack_task_inst_list (x : Sched_t.task_inst list) : task_inst_store =
    x
    |> List.to_seq
    |> Seq.map Task.Deserialize.unpack_task_inst
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_list_diff
      (x : (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff) :
    task_inst_store_diff =
    {
      added = unpack_task_inst_list x.added;
      removed = unpack_task_inst_list x.removed;
    }

  let unpack_task_seg_list (x : Sched_t.task_seg list) : task_seg_store =
    x
    |> List.to_seq
    |> Seq.map Task.Deserialize.unpack_task_seg
    |> Task_seg_id_map.of_seq

  let unpack_task_seg_list_diff
      (x : (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff) :
    task_seg_store_diff =
    {
      added = unpack_task_seg_list x.added;
      removed = unpack_task_seg_list x.removed;
    }

  let unpack_sched_req_pending_list (x : Sched_req_t.sched_req list) :
    sched_req_store =
    x
    |> List.to_seq
    |> Seq.map Sched_req.Deserialize.unpack_sched_req
    |> Sched_req_id_map.of_seq

  let unpack_sched_req_pending_list_diff
      (x :
         (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff)
    : sched_req_store_diff =
    {
      added = unpack_sched_req_pending_list x.added;
      removed = unpack_sched_req_pending_list x.removed;
    }

  let unpack_sched_req_record_list (x : Sched_req_t.sched_req_record list) :
    sched_req_record_store =
    x
    |> List.to_seq
    |> Seq.map Sched_req.Deserialize.unpack_sched_req_record
    |> Sched_req_id_map.of_seq

  let unpack_sched_req_record_list_diff
      (x :
         ( Sched_req_t.sched_req_id,
           Sched_req_t.sched_req_record_data )
           Map_utils_t.diff) : sched_req_record_store_diff =
    {
      added = unpack_sched_req_record_list x.added;
      removed = unpack_sched_req_record_list x.removed;
    }

  let unpack_quota (x : (Task.task_inst_id * int64) list) :
    int64 Task_inst_id_map.t =
    x |> List.to_seq |> Task_inst_id_map.of_seq

  let unpack_quota_diff (x : (Task_t.task_inst_id, int64) Map_utils_t.diff) :
    int64 Task_inst_id_map_utils.diff =
    { added = unpack_quota x.added; removed = unpack_quota x.removed }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    Bucket_store.print_unpack_related_functions ()
  *)

  let unpack_user_id_to_task_ids (x : (Task_t.user_id * int64 list) list) :
    Int64_set.t User_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Deserialize.unpack y))
    |> User_id_map.of_seq

  let unpack_user_id_to_task_ids_diff
      (x : (Task_t.user_id, int64) Map_utils_t.diff_bucketed) :
    User_id_map_utils.Int64_bucketed.diff_bucketed =
    {
      added = unpack_user_id_to_task_ids x.added;
      removed = unpack_user_id_to_task_ids x.removed;
    }

  let unpack_task_id_to_task_inst_ids (x : (Task_t.task_id * int64 list) list) :
    Int64_set.t Task_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Deserialize.unpack y))
    |> Task_id_map.of_seq

  let unpack_task_id_to_task_inst_ids_diff
      (x : (Task_t.task_id, int64) Map_utils_t.diff_bucketed) :
    Task_id_map_utils.Int64_bucketed.diff_bucketed =
    {
      added = unpack_task_id_to_task_inst_ids x.added;
      removed = unpack_task_id_to_task_inst_ids x.removed;
    }

  let unpack_task_inst_id_to_task_seg_ids
      (x : (Task_t.task_inst_id * int64 list) list) :
    Int64_set.t Task_inst_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) -> (id, Int64_set.Deserialize.unpack y))
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_id_to_task_seg_ids_diff
      (x : (Task_t.task_inst_id, int64) Map_utils_t.diff_bucketed) :
    Task_inst_id_map_utils.Int64_bucketed.diff_bucketed =
    {
      added = unpack_task_inst_id_to_task_seg_ids x.added;
      removed = unpack_task_inst_id_to_task_seg_ids x.removed;
    }

  let unpack_progress_indexed_by_start
      (x : (int64 * Task_t.task_seg_place list) list) : task_seg_place_map =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) -> (id, Task_seg_place_set.Deserialize.unpack y))
    |> Int64_map.of_seq

  let unpack_progress_indexed_by_start_diff
      (x : (int64, Task_t.task_seg_place) Map_utils_t.diff_bucketed) :
    task_seg_place_map_diff =
    {
      added = unpack_progress_indexed_by_start x.added;
      removed = unpack_progress_indexed_by_start x.removed;
    }

  let unpack_indexed_by_start (x : (int64 * Task_t.task_seg_place list) list) :
    task_seg_place_map =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) -> (id, Task_seg_place_set.Deserialize.unpack y))
    |> Int64_map.of_seq

  let unpack_indexed_by_start_diff
      (x : (int64, Task_t.task_seg_place) Map_utils_t.diff_bucketed) :
    task_seg_place_map_diff =
    {
      added = unpack_indexed_by_start x.added;
      removed = unpack_indexed_by_start x.removed;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    Set_store.print_unpack_related_functions ()
  *)

  let unpack_sched_req_ids (x : int64 list) : Int64_set.t =
    x |> List.to_seq |> Int64_set.of_seq

  let unpack_sched_req_ids_diff (x : int64 Set_utils_t.diff) :
    Int64_set_utils.diff =
    {
      added = unpack_sched_req_ids x.added;
      removed = unpack_sched_req_ids x.removed;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    print_unpack_store ();
    print_unpack_store_diff ();
  *)

  let unpack_store (store : Sched_t.store) : store =
    {
      task_store = unpack_task_list store.task_list;
      task_inst_store = unpack_task_inst_list store.task_inst_list;
      task_seg_store = unpack_task_seg_list store.task_seg_list;
      user_id_to_task_ids = unpack_user_id_to_task_ids store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        unpack_task_id_to_task_inst_ids store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        unpack_task_inst_id_to_task_seg_ids store.task_inst_id_to_task_seg_ids;
      sched_req_ids = unpack_sched_req_ids store.sched_req_ids;
      sched_req_pending_store =
        unpack_sched_req_pending_list store.sched_req_pending_list;
      sched_req_record_store =
        unpack_sched_req_record_list store.sched_req_record_list;
      quota = unpack_quota store.quota;
      progress_indexed_by_start =
        unpack_progress_indexed_by_start store.progress_indexed_by_start;
    }

  let unpack_store_diff (diff : Sched_t.store_diff) : store_diff =
    {
      task_store_diff = unpack_task_list_diff diff.task_list_diff;
      task_inst_store_diff = unpack_task_inst_list_diff diff.task_inst_list_diff;
      task_seg_store_diff = unpack_task_seg_list_diff diff.task_seg_list_diff;
      user_id_to_task_ids_diff =
        unpack_user_id_to_task_ids_diff diff.user_id_to_task_ids_diff;
      task_id_to_task_inst_ids_diff =
        unpack_task_id_to_task_inst_ids_diff diff.task_id_to_task_inst_ids_diff;
      task_inst_id_to_task_seg_ids_diff =
        unpack_task_inst_id_to_task_seg_ids_diff
          diff.task_inst_id_to_task_seg_ids_diff;
      sched_req_ids_diff = unpack_sched_req_ids_diff diff.sched_req_ids_diff;
      sched_req_pending_store_diff =
        unpack_sched_req_pending_list_diff diff.sched_req_pending_list_diff;
      sched_req_record_store_diff =
        unpack_sched_req_record_list_diff diff.sched_req_record_list_diff;
      quota_diff = unpack_quota_diff diff.quota_diff;
      progress_indexed_by_start_diff =
        unpack_progress_indexed_by_start_diff
          diff.progress_indexed_by_start_diff;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    print_unpack_agenda ();
    print_unpack_agenda_diff ();
  *)

  let unpack_agenda (agenda : Sched_t.agenda) : agenda =
    { indexed_by_start = unpack_indexed_by_start agenda.indexed_by_start }

  let unpack_agenda_diff (diff : Sched_t.agenda_diff) : agenda_diff =
    {
      indexed_by_start_diff =
        unpack_indexed_by_start_diff diff.indexed_by_start_diff;
    }

  (*$*)

  let unpack_sched ((sid, sd) : Sched_t.sched) : sched =
    (sid, { store = unpack_store sd.store; agenda = unpack_agenda sd.agenda })

  let unpack_sched_diff ((sid_old, sid_new, sd_diff) : Sched_t.sched_diff) :
    sched_diff =
    ( sid_old,
      sid_new,
      {
        store_diff = unpack_store_diff sd_diff.store_diff;
        agenda_diff = unpack_agenda_diff sd_diff.agenda_diff;
      } )

  let sched_of_json_string string : sched =
    string |> Sched_j.sched_of_string |> unpack_sched

  let sched_diff_of_json_string string : sched_diff =
    string |> Sched_j.sched_diff_of_string |> unpack_sched_diff
end

module Equal = struct
  (*$ #use "lib/sched.cinaps";;

    print_store_equal ();
    print_agenda_equal ();
  *)

  let store_equal (store1 : store) (store2 : store) : bool =
    Task_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_store store2.task_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_inst_store store2.task_inst_store
    && Task_seg_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_seg_store store2.task_seg_store
    && User_id_map.equal Int64_set.equal store1.user_id_to_task_ids
      store2.user_id_to_task_ids
    && Task_id_map.equal Int64_set.equal store1.task_id_to_task_inst_ids
      store2.task_id_to_task_inst_ids
    && Task_inst_id_map.equal Int64_set.equal
      store1.task_inst_id_to_task_seg_ids store2.task_inst_id_to_task_seg_ids
    && Int64_set.equal store1.sched_req_ids store2.sched_req_ids
    && Sched_req_id_map.equal
      (fun x y -> compare x y = 0)
      store1.sched_req_pending_store store2.sched_req_pending_store
    && Sched_req_id_map.equal
      (fun x y -> compare x y = 0)
      store1.sched_req_record_store store2.sched_req_record_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.quota store2.quota
    && Int64_map.equal Task_seg_place_set.equal store1.progress_indexed_by_start
      store2.progress_indexed_by_start

  let agenda_equal (agenda1 : agenda) (agenda2 : agenda) : bool =
    Int64_map.equal Task_seg_place_set.equal agenda1.indexed_by_start
      agenda2.indexed_by_start

  (*$*)

  let sched_data_equal (sd1 : sched_data) (sd2 : sched_data) =
    store_equal sd1.store sd2.store && agenda_equal sd1.agenda sd2.agenda

  let sched_equal ((sid1, sd1) : sched) ((sid2, sd2) : sched) =
    sid1 = sid2 && sched_data_equal sd1 sd2
end

module Diff = struct
  (*$ #use "lib/sched.cinaps";;

    print_diff_store ();
    print_add_diff_store ();
    print_sub_diff_store ();
  *)

  let diff_store (store1 : store) (store2 : store) : store_diff =
    {
      task_store_diff =
        Task_id_map_utils.diff ~old:store1.task_store store2.task_store;
      task_inst_store_diff =
        Task_inst_id_map_utils.diff ~old:store1.task_inst_store
          store2.task_inst_store;
      task_seg_store_diff =
        Task_seg_id_map_utils.diff ~old:store1.task_seg_store
          store2.task_seg_store;
      user_id_to_task_ids_diff =
        User_id_map_utils.Int64_bucketed.diff_bucketed
          ~old:store1.user_id_to_task_ids store2.user_id_to_task_ids;
      task_id_to_task_inst_ids_diff =
        Task_id_map_utils.Int64_bucketed.diff_bucketed
          ~old:store1.task_id_to_task_inst_ids store2.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids_diff =
        Task_inst_id_map_utils.Int64_bucketed.diff_bucketed
          ~old:store1.task_inst_id_to_task_seg_ids
          store2.task_inst_id_to_task_seg_ids;
      sched_req_ids_diff =
        Int64_set_utils.diff ~old:store1.sched_req_ids store2.sched_req_ids;
      sched_req_pending_store_diff =
        Sched_req_id_map_utils.diff ~old:store1.sched_req_pending_store
          store2.sched_req_pending_store;
      sched_req_record_store_diff =
        Sched_req_id_map_utils.diff ~old:store1.sched_req_record_store
          store2.sched_req_record_store;
      quota_diff = Task_inst_id_map_utils.diff ~old:store1.quota store2.quota;
      progress_indexed_by_start_diff =
        Int64_map_utils.Task_seg_place_bucketed.diff_bucketed
          ~old:store1.progress_indexed_by_start store2.progress_indexed_by_start;
    }

  let add_diff_store (diff : store_diff) (store : store) : store =
    {
      task_store =
        Task_id_map_utils.add_diff diff.task_store_diff store.task_store;
      task_inst_store =
        Task_inst_id_map_utils.add_diff diff.task_inst_store_diff
          store.task_inst_store;
      task_seg_store =
        Task_seg_id_map_utils.add_diff diff.task_seg_store_diff
          store.task_seg_store;
      user_id_to_task_ids =
        User_id_map_utils.Int64_bucketed.add_diff_bucketed
          diff.user_id_to_task_ids_diff store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        Task_id_map_utils.Int64_bucketed.add_diff_bucketed
          diff.task_id_to_task_inst_ids_diff store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        Task_inst_id_map_utils.Int64_bucketed.add_diff_bucketed
          diff.task_inst_id_to_task_seg_ids_diff
          store.task_inst_id_to_task_seg_ids;
      sched_req_ids =
        Int64_set_utils.add_diff diff.sched_req_ids_diff store.sched_req_ids;
      sched_req_pending_store =
        Sched_req_id_map_utils.add_diff diff.sched_req_pending_store_diff
          store.sched_req_pending_store;
      sched_req_record_store =
        Sched_req_id_map_utils.add_diff diff.sched_req_record_store_diff
          store.sched_req_record_store;
      quota = Task_inst_id_map_utils.add_diff diff.quota_diff store.quota;
      progress_indexed_by_start =
        Int64_map_utils.Task_seg_place_bucketed.add_diff_bucketed
          diff.progress_indexed_by_start_diff store.progress_indexed_by_start;
    }

  let sub_diff_store (diff : store_diff) (store : store) : store =
    {
      task_store =
        Task_id_map_utils.sub_diff diff.task_store_diff store.task_store;
      task_inst_store =
        Task_inst_id_map_utils.sub_diff diff.task_inst_store_diff
          store.task_inst_store;
      task_seg_store =
        Task_seg_id_map_utils.sub_diff diff.task_seg_store_diff
          store.task_seg_store;
      user_id_to_task_ids =
        User_id_map_utils.Int64_bucketed.sub_diff_bucketed
          diff.user_id_to_task_ids_diff store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        Task_id_map_utils.Int64_bucketed.sub_diff_bucketed
          diff.task_id_to_task_inst_ids_diff store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        Task_inst_id_map_utils.Int64_bucketed.sub_diff_bucketed
          diff.task_inst_id_to_task_seg_ids_diff
          store.task_inst_id_to_task_seg_ids;
      sched_req_ids =
        Int64_set_utils.sub_diff diff.sched_req_ids_diff store.sched_req_ids;
      sched_req_pending_store =
        Sched_req_id_map_utils.sub_diff diff.sched_req_pending_store_diff
          store.sched_req_pending_store;
      sched_req_record_store =
        Sched_req_id_map_utils.sub_diff diff.sched_req_record_store_diff
          store.sched_req_record_store;
      quota = Task_inst_id_map_utils.sub_diff diff.quota_diff store.quota;
      progress_indexed_by_start =
        Int64_map_utils.Task_seg_place_bucketed.sub_diff_bucketed
          diff.progress_indexed_by_start_diff store.progress_indexed_by_start;
    }

  (*$*)

  (*$ #use "lib/sched.cinaps";;

    print_diff_agenda ();
    print_add_diff_agenda ();
    print_sub_diff_agenda ();
  *)

  let diff_agenda (agenda1 : agenda) (agenda2 : agenda) : agenda_diff =
    {
      indexed_by_start_diff =
        Int64_map_utils.Task_seg_place_bucketed.diff_bucketed
          ~old:agenda1.indexed_by_start agenda2.indexed_by_start;
    }

  let add_diff_agenda (diff : agenda_diff) (agenda : agenda) : agenda =
    {
      indexed_by_start =
        Int64_map_utils.Task_seg_place_bucketed.add_diff_bucketed
          diff.indexed_by_start_diff agenda.indexed_by_start;
    }

  let sub_diff_agenda (diff : agenda_diff) (agenda : agenda) : agenda =
    {
      indexed_by_start =
        Int64_map_utils.Task_seg_place_bucketed.sub_diff_bucketed
          diff.indexed_by_start_diff agenda.indexed_by_start;
    }

  (*$*)

  let diff_sched_data ~(old : sched_data) (sd : sched_data) : sched_data_diff =
    {
      store_diff = diff_store old.store sd.store;
      agenda_diff = diff_agenda old.agenda sd.agenda;
    }

  let diff_sched ~(old : sched) ((sid, sd) : sched) : sched_diff =
    let sid_old, sd_old = old in
    (sid_old, sid, diff_sched_data ~old:sd_old sd)

  let add_diff_sched_data (diff : sched_data_diff) (sd : sched_data) :
    sched_data =
    {
      store = add_diff_store diff.store_diff sd.store;
      agenda = add_diff_agenda diff.agenda_diff sd.agenda;
    }

  let sub_diff_sched_data (diff : sched_data_diff) (sd : sched_data) :
    sched_data =
    {
      store = sub_diff_store diff.store_diff sd.store;
      agenda = sub_diff_agenda diff.agenda_diff sd.agenda;
    }

  let add_diff_sched ((sid_old, sid_new, sd_diff) : sched_diff)
      ((sid, sd) : sched) : sched =
    if sid_old <> sid then raise Exceptions.Invalid_diff
    else (sid_new, add_diff_sched_data sd_diff sd)

  let sub_diff_sched ((sid_old, sid_new, sd_diff) : sched_diff)
      ((sid, sd) : sched) : sched =
    if sid_new <> sid then raise Exceptions.Invalid_diff
    else (sid_old, sub_diff_sched_data sd_diff sd)
end

module Print = struct
  let debug_string_of_sched ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (sid, sd) =
    Debug_print.bprintf ~indent_level buffer "schedule id : %s\n"
      (Id.sched_id_to_string sid);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "pending scheduling requests :\n";
    Sched_req_id_map.iter
      (fun id data ->
         Sched_req.Print.debug_string_of_sched_req
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.sched_req_pending_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "tasks :\n";
    Task_id_map.iter
      (fun id data ->
         Task.Print.debug_string_of_task ~indent_level:(indent_level + 2) ~buffer
           (id, data)
         |> ignore)
      sd.store.task_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "task insts :\n";
    Task_inst_id_map.iter
      (fun id data ->
         Task.Print.debug_string_of_task_inst ~indent_level:(indent_level + 2)
           ~buffer (id, data)
         |> ignore)
      sd.store.task_inst_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "task segs :\n";
    Task_seg_id_map.iter
      (fun id data ->
         Task.Print.debug_string_of_task_seg ~indent_level:(indent_level + 2)
           ~buffer (id, data)
         |> ignore)
      sd.store.task_seg_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "agenda :\n";
    Int64_map.iter
      (fun _start bucket ->
         Task_seg_place_set.iter
           (fun (id, start, end_exc) ->
              Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
                "%Ld - %Ld | %s\n" start end_exc
                (Task.task_seg_id_to_string id))
           bucket)
      sd.agenda.indexed_by_start;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "leftover quota :\n";
    Task_inst_id_map.iter
      (fun id quota ->
         Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s : %Ld\n"
           (Task.task_inst_id_to_string id)
           quota)
      sd.store.quota;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "progress :\n";
    Int64_map.iter
      (fun _start bucket ->
         Task_seg_place_set.iter
           (fun (id, start, end_exc) ->
              Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
                "%Ld - %Ld | %s\n" start end_exc
                (Task.task_seg_id_to_string id))
           bucket)
      sd.store.progress_indexed_by_start;
    Buffer.contents buffer

  let debug_print_sched ?(indent_level = 0) sched =
    print_string (debug_string_of_sched ~indent_level sched)
end
