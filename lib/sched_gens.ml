open Int64_utils

let backtracking_search ~start ~end_exc ~(base : Sched.sched)
    ((_sched_req_id, sched_req_record_data_list) : Sched_req.sched_req_record) :
  Sched.sched Seq.t =
  let free_time_slots =
    Sched.Time_slot.get_free_time_slots ~start ~end_exc base
  in
  let get_usable_time_slots time_slots =
    time_slots |> Time_slot.normalize_list_in_seq_out
    |> Time_slot.intersect free_time_slots
  in
  Seq.flat_map
    (fun sched_req_record_data ->
       match sched_req_record_data with
       | Sched_req_data_unit_skeleton.Fixed
           { task_seg_related_data = task_seg; start } ->
         let _, size = task_seg in
         Task_seg_place_gens.multi_task_segs_shift ~incre:1L
           ~task_segs:[ task_seg ]
           (Seq.return (start, size))
         |> OSeq.map (fun place_s ->
             base
             |> Sched.Task_seg_place_map.add_task_seg_place_list place_s)
       | Shift (task_segs, time_slots) ->
         let usable_time_slots = get_usable_time_slots time_slots in
         Task_seg_place_gens.multi_task_segs_shift ~incre:15L ~task_segs
           usable_time_slots
         |> OSeq.map (fun place_s ->
             base
             |> Sched.Task_seg_place_map.add_task_seg_place_list place_s)
       | Split_and_shift (task_seg, time_slots) ->
         let usable_time_slots = get_usable_time_slots time_slots in
         Task_seg_place_gens.single_task_seg_multi_splits_max_shift
           ~min_seg_size:5L ~max_seg_size:None ~split_count:4L ~incre:15L
           ~task_seg usable_time_slots
         |> OSeq.map (fun place_s ->
             base
             |> Sched.Task_seg_place_map.add_task_seg_place_list place_s)
       | Split_even { task_seg_related_data = task_seg; time_slots; buckets } ->
         let usable_time_slots = get_usable_time_slots time_slots in
         Task_seg_place_gens.single_task_seg_multi_even_splits ~incre:15L
           ~task_seg ~buckets ~usable_time_slots
         |> OSeq.map (fun place_s ->
             base
             |> Sched.Task_seg_place_map.add_task_seg_place_list place_s)
       | Time_share (task_segs, time_slots) ->
         let usable_time_slots = get_usable_time_slots time_slots in
         let s =
           Task_seg_place_gens.multi_task_segs_interleave ~interval_size:15L
             ~task_segs usable_time_slots
         in
         Seq.return (base |> Sched.Task_seg_place_map.add_task_seg_place_seq s)
       | Push_toward (task_seg, target, time_slots) ->
         let usable_time_slots = get_usable_time_slots time_slots in
         let s1 =
           Task_seg_place_gens.single_task_seg_shift ~cur_pos:target ~incre:15L
             ~task_seg usable_time_slots
           |> OSeq.take 1
         in
         let s2 =
           Task_seg_place_gens.single_task_seg_shift_rev
             ~cur_end_pos_exc:target ~incre:15L ~task_seg usable_time_slots
           |> OSeq.take 1
         in
         let s =
           OSeq.append s1 s2
           |> OSeq.sort
             ~cmp:(fun (_id1, start1, end_exc1) (_id2, start2, end_exc2) ->
                 let distance1 =
                   let mid1 = (end_exc1 +^ start1) /^ 2L in
                   Int64.abs (mid1 -^ target)
                 in
                 let distance2 =
                   let mid2 = (end_exc2 +^ start2) /^ 2L in
                   Int64.abs (mid2 -^ target)
                 in
                 compare distance1 distance2)
           |> OSeq.take 1
         in
         Seq.return (base |> Sched.Task_seg_place_map.add_task_seg_place_seq s))
    (sched_req_record_data_list |> List.to_seq)

let backtracking_search_multi ~start ~end_exc ~base
    (sched_req_records : Sched_req.sched_req_record list) : Sched.sched Seq.t =
  sched_req_records |> Sched_req.sort_sched_req_record_list_by_flexibility_score
  |> List.fold_left
    (fun sched_seq sched_req ->
       Seq.flat_map
         (fun sched ->
            backtracking_search ~start ~end_exc ~base:sched sched_req)
         sched_seq)
    (Seq.return base)

let backtracking_search_pending ~start ~end_exc
    ~include_sched_reqs_partially_within_time_period ~up_to_sched_req_id_inc
    ~base : Sched.sched Seq.t =
  let sched_req_records, base =
    Sched.Sched_req_store.allocate_task_segs_for_pending_sched_reqs ~start
      ~end_exc ~include_sched_reqs_partially_within_time_period
      ~up_to_sched_req_id_inc base
  in
  backtracking_search_multi ~start ~end_exc ~base sched_req_records
