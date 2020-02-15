open Daypack_lib

let debug_single_task_seg_shift () =
  print_endline "Debug print for Task_seg_place_gens.single_task_seg_shift";
  let incre = 2L in
  let cur_pos = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 4L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  Printf.printf "cur_pos : %Ld\n" cur_pos;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_shift ~incre ~cur_pos ~task_seg time_slots
  |> Seq.iter (fun (id, start, end_exc) ->
      Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
        (Task.task_seg_id_to_string id)
        start end_exc)

let debug_single_task_seg_shift_rev () =
  print_endline "Debug print for Task_seg_place_gens.single_task_seg_shift_rev";
  let incre = 2L in
  let cur_end_pos_exc = 50L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 4L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  Printf.printf "cur_end_pos_exc : %Ld\n" cur_end_pos_exc;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_shift_rev ~incre ~cur_end_pos_exc
    ~task_seg time_slots
  |> Seq.iter (fun (id, start, end_exc) ->
      Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
        (Task.task_seg_id_to_string id)
        start end_exc)

let debug_multi_task_segs_shift () =
  print_endline "Debug print for Task_seg_place_gens.multi_task_segs_shift";
  let incre = 2L in
  let task_segs =
    [ ((0L, 0L, 0L, 0L, None), 4L); ((0L, 0L, 0L, 1L, None), 2L) ]
  in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  List.iter
    (fun (id, size) ->
       Printf.printf "time seg id : %s size : %Ld\n"
         (Task.task_seg_id_to_string id)
         size)
    task_segs;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.multi_task_segs_shift ~incre ~task_segs time_slots
  |> Seq.iter (fun pos_s ->
      Printf.printf "possible schedule :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.task_seg_id_to_string id)
             start end_exc)
        pos_s)

let debug_single_task_seg_single_split () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_single_split";
  let min_seg_size = 2L in
  let max_seg_size = 20L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_single_split ~min_seg_size ~max_seg_size
    ~cur_split_pos:0L ~task_seg
  |> Seq.iter (fun ((_, size1), (_, size2)) ->
      Printf.printf "splits : %Ld, %Ld\n" size1 size2)

let debug_single_task_seg_multi_splits_exact () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_exact";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let split_count = 2L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_multi_splits_exact ~min_seg_size
    ~max_seg_size ~split_count ~task_seg
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, x) ->
           Printf.printf "  %s - %Ld\n" (Task.task_seg_id_to_string id) x)
        splits)

let debug_single_task_seg_multi_splits_max () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_max";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let split_count = 2L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_multi_splits_max ~min_seg_size
    ~max_seg_size ~split_count ~task_seg
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, x) ->
           Printf.printf "  %s - %Ld\n" (Task.task_seg_id_to_string id) x)
        splits)

let debug_single_task_seg_multi_splits_exact_shift () =
  print_endline
    "Debug print for \
     Task_seg_place_gens.single_task_seg_multi_splits_exact_shift";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let incre = 1L in
  let split_count = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 7L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (11L, 15L) ] in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_multi_splits_exact_shift ~min_seg_size
    ~max_seg_size ~split_count ~incre ~task_seg time_slots
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.task_seg_id_to_string id)
             start end_exc)
        splits)

let debug_single_task_seg_multi_splits_max_shift () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_max_shift";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let incre = 1L in
  let split_count = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 7L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (11L, 15L) ] in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_multi_splits_max_shift ~min_seg_size
    ~max_seg_size ~split_count ~incre ~task_seg time_slots
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.task_seg_id_to_string id)
             start end_exc)
        splits)

let debug_multi_tasks_interleave () =
  print_endline "Debug print for Task_seg_place_gens.multi_task_segs_interleave";
  let task_segs =
    [
      ((0L, 0L, 0L, 0L, None), 2L);
      ((0L, 0L, 0L, 1L, None), 5L);
      ((0L, 0L, 0L, 2L, None), 9L);
    ]
  in
  let time_slots = [ (0L, 100L) ] in
  let time_slots = List.to_seq time_slots in
  let interval_size = 2L in
  let s =
    Task_seg_place_gens.multi_task_segs_interleave ~interval_size ~task_segs
      time_slots
  in
  Seq.iter
    (fun (id, start, end_exc) ->
       Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
         (Task.task_seg_id_to_string id)
         start end_exc)
    s

let debug_single_task_seg_multi_even_splits () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_even_splits";
  let task_seg = ((0L, 0L, 0L, 0L, None), 230L) in
  let buckets =
    [
      (0L, 24L);
      (24L, 48L);
      (48L, 72L);
      (100L, 124L);
      (124L, 148L);
      (148L, 172L);
      (200L, 224L);
      (224L, 248L);
      (248L, 272L);
      (300L, 324L);
      (324L, 348L);
      (348L, 372L);
    ]
  in
  let usable_time_slots = [ (0L, 500L) ] in
  let usable_time_slots = List.to_seq usable_time_slots in
  let s =
    Task_seg_place_gens.single_task_seg_multi_even_splits ~incre:5L ~task_seg
      ~buckets ~usable_time_slots
  in
  s
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.task_seg_id_to_string id)
             start end_exc)
        splits)

let debug_slice_time_slots_start () =
  print_endline "Debug print for Time_slot.slice start";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slot.slice ~start:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_end_exc () =
  print_endline "Debug print for Time_slot.slice end_exc";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slot.slice ~end_exc:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_start_rev () =
  print_endline "Debug print for Time_slot.slice_rev start";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = time_slots |> List.rev |> List.to_seq in
  Time_slot.slice_rev ~start:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_end_exc_rev () =
  print_endline "Debug print for Time_slot.slice_rev end_exc";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = time_slots |> List.rev |> List.to_seq in
  Time_slot.slice_rev ~end_exc:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_normalize_time_slots () =
  print_endline "Debug print for Time_slot.normalize";
  let time_slots =
    [ (0L, 10L); (10L, 11L); (11L, 20L); (22L, 25L); (20L, 22L); (25L, 30L) ]
  in
  let time_slots = List.to_seq time_slots in
  Time_slot.normalize time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_invert_time_slots () =
  print_endline "Debug print for Time_slot.invert";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slot.invert ~start:0L ~end_exc:22L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_relative_complement_time_slots () =
  print_endline "Debug print for Time_slot.relative_complement";
  let mem_of_time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let not_mem_of_time_slots = [ (0L, 5L); (6L, 15L); (25L, 30L) ] in
  let mem_of_time_slots = List.to_seq mem_of_time_slots in
  let not_mem_of_time_slots = List.to_seq not_mem_of_time_slots in
  Time_slot.relative_complement ~mem_of:mem_of_time_slots
    ~not_mem_of:not_mem_of_time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_intersect_time_slots () =
  print_endline "Debug print for Time_slot.intersect";
  let time_slots1 = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots2 = [ (0L, 1L); (2L, 3L); (10L, 20L); (25L, 30L) ] in
  let time_slots1 = List.to_seq time_slots1 in
  let time_slots2 = List.to_seq time_slots2 in
  Time_slot.intersect time_slots1 time_slots2
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_union_time_slots () =
  print_endline "Debug print for Time_slot.union";
  let time_slots1 = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots2 = [ (0L, 1L); (2L, 3L); (10L, 20L); (25L, 30L) ] in
  let time_slots1 = List.to_seq time_slots1 in
  let time_slots2 = List.to_seq time_slots2 in
  Time_slot.union time_slots1 time_slots2
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_sched_backtracking_search_pending () =
  print_endline "Debug print for Sched_search.backtracking_search_pending";
  let sched_req_data_list =
    let open Sched_req_data_unit_skeleton in
    [
      [
        Split_even
          {
            task_seg_related_data = ((0L, 0L, 0L), 20L);
            time_slots = [ (0L, 50L) ];
            buckets = [ (0L, 10L); (10L, 20L) ];
            incre = 1L;
          };
        Split_even
          {
            task_seg_related_data = ((0L, 0L, 0L), 20L);
            time_slots = [ (50L, 100L) ];
            buckets = [ (50L, 60L); (60L, 70L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Split_and_shift
          {
            task_seg_related_data = ((0L, 0L, 2L), 15L);
            time_slots = [ (50L, 150L) ];
            split_count = Max_split 5L;
            incre = 1L;
            min_seg_size = 2L;
            max_seg_size = None;
          };
      ];
      [
        Time_share
          {
            task_seg_related_data_list =
              [ ((0L, 0L, 2L), 30L); ((0L, 0L, 3L), 20L) ];
            time_slots = [ (50L, 200L) ];
            interval_size = 30L;
          };
      ];
      [
        Push_toward
          {
            task_seg_related_data = ((0L, 0L, 4L), 10L);
            target = 100L;
            time_slots = [ (0L, 200L) ];
            incre = 1L;
          };
      ];
      [
        Push_toward
          {
            task_seg_related_data = ((0L, 0L, 5L), 10L);
            target = 75L;
            time_slots = [ (0L, 200L) ];
            incre = 1L;
          };
      ];
    ]
  in
  let quota =
    [
      ((0L, 0L, 0L), 20L);
      ((0L, 0L, 1L), 10L);
      ((0L, 0L, 2L), 50L);
      ((0L, 0L, 3L), 10L);
      ((0L, 0L, 4L), 10L);
      ((0L, 0L, 5L), 10L);
      ((0L, 0L, 6L), 10L);
      ((0L, 0L, 7L), 10L);
    ]
    |> List.to_seq
    |> Daypack_lib.Task_inst_id_map.of_seq
  in
  print_endline "scheduling requests";
  List.iter
    (Sched_req.Print.debug_print_sched_req_data ~indent_level:1)
    sched_req_data_list;
  print_newline ();
  let _, base =
    Sched.empty
    |> Sched.Quota_store.update_quota quota
    |> Sched.Sched_req_store.queue_sched_req_data_list sched_req_data_list
  in
  Sched_search.backtracking_search_pending ~start:0L ~end_exc:50L
    ~include_sched_reqs_partially_within_time_period:true
    ~up_to_sched_req_id_inc:None ~base
  |> OSeq.take 1
  |> Seq.iter (fun sched -> Sched.Print.debug_print_sched sched)

let debug_sched_usage_simulation () =
  let add_task ~parent_user_id task_data task_inst_data_list t : unit =
    let task, _task_inst_list =
      Sched_ver_history.In_place_head.add_task ~parent_user_id task_data
        task_inst_data_list t
    in
    print_endline "Added task";
    Task.Print.debug_print_task ~indent_level:1 task
  in
  let sched_ver_history = Sched_ver_history.make_empty () in
  add_task ~parent_user_id:0L
    Task.{ splittable = false; parallelizable = false; task_type = One_off }
    Task.[ { task_inst_type = Reminder } ]
    sched_ver_history;
  add_task ~parent_user_id:0L
    Task.
      {
        splittable = false;
        parallelizable = false;
        task_type =
          Recurring
            {
              excluded_time_slots = [];
              recur_type =
                Arithemtic_seq
                  ( { start = 0L; end_exc = 100L; diff = 50L },
                    {
                      task_inst_data = { task_inst_type = Reminder };
                      sched_req_template =
                        [ Fixed { task_seg_related_data = 1L; start = 0L } ];
                    } );
            };
      }
    [] sched_ver_history;
  add_task ~parent_user_id:0L
    Task.{ splittable = false; parallelizable = false; task_type = One_off }
    Task.[ { task_inst_type = Reminder } ]
    sched_ver_history;
  List.iter
    (fun sched_req_data ->
       Sched_ver_history.In_place_head.queue_sched_req sched_req_data
         sched_ver_history
       |> ignore)
    [
      [
        Split_even
          {
            task_seg_related_data = ((0L, 2L, 0L), 20L);
            time_slots = [ (0L, 50L) ];
            buckets = [ (0L, 10L); (10L, 20L) ];
            incre = 1L;
          };
        Split_even
          {
            task_seg_related_data = ((0L, 2L, 0L), 20L);
            time_slots = [ (50L, 100L) ];
            buckets = [ (50L, 60L); (60L, 70L); (90L, 100L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      (* [
       *   Split_and_shift
       *     {
       *       task_seg_related_data = ((0L, 0L, 2L), 15L);
       *       time_slots = [ (50L, 150L) ];
       *       split_count = Max_split 5L;
       *       incre = 1L;
       *       min_seg_size = 2L;
       *       max_seg_size = None;
       *     };
       * ]; *)
      (* [
       *   Time_share
       *     {
       *       task_seg_related_data_list =
       *         [ ((0L, 0L, 2L), 30L); ((0L, 0L, 3L), 20L) ];
       *       time_slots = [ (50L, 200L) ];
       *       interval_size = 30L;
       *     };
       * ]; *)
      (* [
       *   Push_toward
       *     {
       *       task_seg_related_data = ((0L, 0L, 4L), 10L);
       *       target = 100L;
       *       time_slots = [ (0L, 200L) ];
       *       incre = 1L;
       *     };
       * ];
       * [
       *   Push_toward
       *     {
       *       task_seg_related_data = ((0L, 0L, 5L), 10L);
       *       target = 75L;
       *       time_slots = [ (0L, 200L) ];
       *       incre = 1L;
       *     };
       * ]; *)
    ];
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  Sched_ver_history.In_place_head.instantiate ~start:0L ~end_exc:100L
    sched_ver_history;
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  ( match
      Sched_ver_history.Maybe_append_to_head.sched ~start:0L ~end_exc:100L
        ~include_sched_reqs_partially_within_time_period:true
        ~up_to_sched_req_id_inc:None sched_ver_history
    with
    | Ok () ->
      print_endline "Scheduled successfully";
      Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history
    | Error () -> print_endline "Scheduling failed" );
  print_endline "=====";
  print_endline "Removing task/task inst";
  Sched_ver_history.Maybe_append_to_head.remove_task (0L, 0L) sched_ver_history;
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  print_endline "Recording prgress";
  Sched_ver_history.In_place_head.mark_task_inst_completed (0L, 0L, 0L)
    sched_ver_history;
  Sched_ver_history.In_place_head.add_task_seg_progress_chunk
    (0L, 0L, 1L, 0L, None) (0L, 100L) sched_ver_history;
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_newline ()

(* let debug_time_pattern_normalize_pattern () =
 *   print_endline "Debug print for Time_pattern.normalize_pattern";
 *   let dir = `End in
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     { year = Some 2021; mon = None; day = None; hour = Some 2; min = None }
 *     |> normalize_pattern dir
 *   in
 *   Daypack_lib.Time_pattern.Print.debug_print_pattern pattern *)

let debug_time_pattern_matching_tm_seq () =
  print_endline "Debug print for Time_pattern.matching_tm_seq";
  let tm =
    (* (Some
     *    Unix.
     *      {
     *        tm_sec = 0;
     *        tm_min = 0;
     *        tm_hour = 0;
     *        tm_mday = 1;
     *        tm_mon = 0;
     *        tm_year = 0;
     *        tm_wday = 0;
     *        tm_yday = 0;
     *        tm_isdst = false;
     *      }) *)
    Unix.time () |> Unix.gmtime
  in
  let pattern =
    let open Daypack_lib.Time_pattern in
    {
      years = [];
      months = [ 5 ];
      days = `Month_days [];
      hours = [ 11 ];
      minutes = [ 0 ];
    }
  in
  let search_years_ahead = 100 in
  Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
  let s =
    Daypack_lib.Time_pattern.matching_tm_seq ~search_years_ahead pattern tm
  in
  s
  |> OSeq.take 10
  |> OSeq.iteri (fun i x ->
      let open Unix in
      Printf.printf "iter : %d\n" i;
      print_endline "  =====";
      Printf.printf "  tm_sec : %d\n" x.tm_sec;
      Printf.printf "  tm_min : %d\n" x.tm_min;
      Printf.printf "  tm_hour : %d\n" x.tm_hour;
      Printf.printf "  tm_mday : %d\n" x.tm_mday;
      Printf.printf "  tm_mon : %d\n" x.tm_mon;
      Printf.printf "  tm_year : %d\n" x.tm_year;
      Printf.printf "  tm_wday : %d\n" x.tm_wday;
      Printf.printf "  tm_yday : %d\n" x.tm_yday)

let debug_time_pattern_matching_time_slots () =
  print_endline "Debug print for Time_pattern.matching_time_slots";
  let tm =
    (* (Some
     *    Unix.
     *      {
     *        tm_sec = 0;
     *        tm_min = 0;
     *        tm_hour = 0;
     *        tm_mday = 1;
     *        tm_mon = 0;
     *        tm_year = 0;
     *        tm_wday = 0;
     *        tm_yday = 0;
     *        tm_isdst = false;
     *      }) *)
    Unix.time () |> Unix.gmtime
  in
  let start = Time.tm_to_time tm in
  let end_exc = Time.tm_to_time { tm with tm_year = tm.tm_year + 1 } in
  let time_slots = [ (start, end_exc) ] in
  let pattern =
    let open Daypack_lib.Time_pattern in
    {
      years = [];
      months = [ 5 ];
      days = `Month_days [];
      hours = [ 11 ];
      minutes = [];
    }
  in
  Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
  let s = Daypack_lib.Time_pattern.matching_time_slots pattern time_slots in
  s
  |> OSeq.take 100
  |> OSeq.iteri (fun i (start, end_exc) ->
      Printf.printf "iter : %d\n" i;
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

(* let debug_time_pattern_next_match_tm () =
 *   print_endline "Debug print for Time_pattern.next_match_tm";
 *   let tm =
 *     ref
 *       (\* (Some
 *        *    Unix.
 *        *      {
 *        *        tm_sec = 0;
 *        *        tm_min = 0;
 *        *        tm_hour = 0;
 *        *        tm_mday = 1;
 *        *        tm_mon = 0;
 *        *        tm_year = 0;
 *        *        tm_wday = 0;
 *        *        tm_yday = 0;
 *        *        tm_isdst = false;
 *        *      }) *\)
 *       (Unix.time () |> Unix.gmtime |> Option.some)
 *   in
 *   let normalize_dir = `Start in
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     { year = None; mon = None; day = None; hour = None; min = None }
 *     |> normalize_pattern normalize_dir
 *   in
 *   let search_years_ahead = 100 in
 *   Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
 *   for i = 0 to 10 do
 *     Printf.printf "iter : %d\n" i;
 *     match !tm with
 *     | Some x ->
 *       print_endline "  =====";
 *       Printf.printf "  tm_sec : %d\n" x.tm_sec;
 *       Printf.printf "  tm_min : %d\n" x.tm_min;
 *       Printf.printf "  tm_hour : %d\n" x.tm_hour;
 *       Printf.printf "  tm_mday : %d\n" x.tm_mday;
 *       Printf.printf "  tm_mon : %d\n" x.tm_mon;
 *       Printf.printf "  tm_year : %d\n" x.tm_year;
 *       Printf.printf "  tm_wday : %d\n" x.tm_wday;
 *       Printf.printf "  tm_yday : %d\n" x.tm_yday;
 *       tm :=
 *         Daypack_lib.Time_pattern.next_match_tm ~normalize_dir
 *           ~search_years_ahead pattern x
 *     | None -> print_endline "nothing"
 *   done *)

(* let debug_time_pattern_next_match_int64 () =
 *   print_endline "Debug print for Time_pattern.next_match_int64";
 *   let time =
 *     ref
 *       (Some
 *          ( Unix.
 *              {
 *                tm_sec = 0;
 *                tm_min = 0;
 *                tm_hour = 0;
 *                tm_mday = 1;
 *                tm_mon = 0;
 *                tm_year = 0;
 *                tm_wday = 0;
 *                tm_yday = 0;
 *                tm_isdst = false;
 *              }
 *            |> Daypack_lib.Time.tm_to_time ))
 *       (\* (Unix.time () |> Unix.gmtime |> Daypack_lib.Time.tm_to_time |> Option.some) *\)
 *   in
 *   let normalize_dir = `Start in
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     { year = None; mon = Some 1; day = None; hour = None; min = None }
 *     |> normalize_pattern normalize_dir
 *   in
 *   let search_years_ahead = 2 in
 *   let time_slots = [ (\* (0L, 36_347_213L) *\) ] in
 *   Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
 *   for i = 0 to 10 do
 *     Printf.printf "iter : %d\n" i;
 *     match !time with
 *     | Some x ->
 *       print_endline "  =====";
 *       Printf.printf "  time : %Ld\n" x;
 *       time :=
 *         Daypack_lib.Time_pattern.next_match_int64 ~time_slots ~normalize_dir
 *           ~search_years_ahead pattern x
 *     | None -> print_endline "nothing"
 *   done *)

(* let () = debug_single_task_seg_shift (); print_newline () *)

(* let () =
 *   debug_single_task_seg_shift_rev ();
 *   print_newline () *)

(* let () =
 *   debug_multi_task_segs_shift ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_single_split ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_exact ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_max ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_exact_shift ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_max_shift ();
 *   print_newline () *)

(* let () =
 *   debug_multi_tasks_interleave ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_even_splits ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_start ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_end_exc ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_start_rev ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_end_exc_rev ();
 *   print_newline () *)

(* let () =
 *   debug_normalize_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_invert_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_relative_complement_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_intersect_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_union_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_sched_backtracking_search_pending ();
 *   print_newline () *)

let () =
  debug_sched_usage_simulation ();
  print_newline ()

(* let () =
 *   debug_time_pattern_normalize_pattern ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_matching_tm_seq ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_matching_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_next_match_tm ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_next_match_int64 ();
 *   print_newline () *)
