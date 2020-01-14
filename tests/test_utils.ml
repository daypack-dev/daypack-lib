module Print_utils = struct
  let small_nat = QCheck.Print.int

  let int64 = Int64.to_string

  let time_slots = QCheck.Print.list (QCheck.Print.pair int64 int64)

  let task_seg_id = Daypack_lib.Task.task_seg_id_to_string

  let task_seg = QCheck.Print.(pair task_seg_id int64)

  let task_segs = QCheck.Print.list QCheck.Print.(pair task_seg_id int64)
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 0L |> min bound) ui64)

let nz_pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 1L |> min bound) ui64)

let small_pos_int64_gen = pos_int64_bound_gen 100L

let small_nz_pos_int64_gen = nz_pos_int64_bound_gen 100L

let pos_int64_gen = pos_int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64 = QCheck.make ~print:Print_utils.int64 pos_int64_gen

let small_pos_int64 = QCheck.make ~print:Print_utils.int64 small_pos_int64_gen

let small_nz_pos_int64 =
  QCheck.make ~print:Print_utils.int64 small_nz_pos_int64_gen

let nz_pos_int64_gen =
  QCheck.Gen.map (Int64.add 1L)
    (pos_int64_bound_gen (Int64.sub Int64.max_int 1L))

let nz_pos_int64 = QCheck.make ~print:Print_utils.int64 nz_pos_int64_gen

let tiny_sorted_time_slots_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x gap
            in
            let end_exc = Int64.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair
       (pos_int64_bound_gen 10_000L)
       (list_size (int_bound 5)
          (pair (pos_int64_bound_gen 20L) (pos_int64_bound_gen 20L))))

let tiny_sorted_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_sorted_time_slots_gen

let sorted_time_slots_maybe_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x (Int64.of_int gap)
            in
            let end_exc = Int64.add start (Int64.of_int size) in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair pos_int64_gen (list (pair nz_small_nat_gen small_nat)))

let sorted_time_slots_maybe_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_maybe_gaps_gen

let sorted_time_slots_with_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x (Int64.of_int gap)
            in
            let end_exc = Int64.add start (Int64.of_int size) in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair pos_int64_gen (list (pair nz_small_nat_gen nz_small_nat_gen)))

let sorted_time_slots_with_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_with_gaps_gen

let tiny_time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) -> (start, Int64.add start size)))
    (list_size (int_bound 5)
       (pair (pos_int64_bound_gen 10_000L) (pos_int64_bound_gen 20L)))

let tiny_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_time_slots_gen

let time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) ->
         (start, Int64.add start (Int64.of_int size))))
    (list (pair ui64 small_nat))

let time_slots = QCheck.make ~print:Print_utils.time_slots time_slots_gen

let task_seg_id_gen =
  let open QCheck.Gen in
  map
    (fun ((id1, id2, id3, id4), id5) -> (id1, id2, id3, id4, id5))
    (pair
       (quad pos_int64_gen pos_int64_gen pos_int64_gen pos_int64_gen)
       (opt pos_int64_gen))

let task_seg_id = QCheck.make task_seg_id_gen

let task_seg_size_gen = nz_pos_int64_bound_gen 20L

let task_seg_gen =
  let open QCheck in
  Gen.(pair task_seg_id_gen task_seg_size_gen)

let task_seg_sizes_gen =
  let open QCheck in
  Gen.(list_size (int_bound 5) task_seg_size_gen)

let task_segs_gen =
  let open QCheck in
  Gen.(list_size (int_bound 5) task_seg_gen)

let task_seg = QCheck.(make ~print:Print_utils.task_seg task_seg_gen)

let task_segs = QCheck.(make ~print:Print_utils.task_segs task_segs_gen)

let task_inst_id_gen =
  QCheck.Gen.(triple pos_int64_gen pos_int64_gen pos_int64_gen)

let task_inst_id = QCheck.make task_inst_id_gen

let task_inst_data_gen =
  let open QCheck.Gen in
  oneof
    [
      return Daypack_lib.Task.{ task_inst_type = Reminder };
      map
        (fun quota ->
           let open Daypack_lib.Task in
           { task_inst_type = Reminder_quota_counting { quota } })
        pos_int64_gen;
      return Daypack_lib.Task.{ task_inst_type = Passing };
    ]

let task_inst_gen = QCheck.Gen.(pair task_inst_id_gen task_inst_data_gen)

let task_inst =
  let open QCheck in
  make ~print:Daypack_lib.Task.Print.debug_string_of_task_inst task_inst_gen

let push_direction_gen = QCheck.Gen.(oneof [ return `Front; return `Back ])

let sched_req_template_gen =
  let open QCheck.Gen in
  oneof
    [
      map2
        (fun task_seg start ->
           Daypack_lib.Sched_req_data_skeleton.Fixed
             { task_seg_related_data = task_seg; start })
        task_seg_size_gen pos_int64_gen;
      map2
        (fun task_segs time_slots ->
           Daypack_lib.Sched_req_data_skeleton.Shift (task_segs, time_slots))
        task_seg_sizes_gen tiny_time_slots_gen;
      map2
        (fun task_seg time_slots ->
           Daypack_lib.Sched_req_data_skeleton.Split_and_shift
             (task_seg, time_slots))
        task_seg_size_gen tiny_time_slots_gen;
      map3
        (fun task_seg time_slots buckets ->
           Daypack_lib.Sched_req_data_skeleton.Split_even
             { task_seg_related_data = task_seg; time_slots; buckets })
        task_seg_size_gen tiny_time_slots_gen tiny_time_slots_gen;
      map2
        (fun task_segs time_slots ->
           Daypack_lib.Sched_req_data_skeleton.Time_share (task_segs, time_slots))
        task_seg_sizes_gen tiny_time_slots_gen;
      map3
        (fun direction task_seg time_slots ->
           Daypack_lib.Sched_req_data_skeleton.Push_to
             (direction, task_seg, time_slots))
        push_direction_gen task_seg_size_gen tiny_time_slots_gen;
    ]

let sched_req_templates_gen =
  QCheck.Gen.(list_size (int_bound 10) sched_req_template_gen)

let sched_req_data_gen : Daypack_lib.Sched_req.sched_req_data QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun task_inst_id sched_req_template ->
       Daypack_lib.Sched_req_data_skeleton.map
         (fun task_seg_size -> (task_inst_id, task_seg_size))
         sched_req_template)
    task_inst_id_gen sched_req_template_gen

let sched_req_gen : Daypack_lib.Sched_req.sched_req QCheck.Gen.t =
  QCheck.Gen.(pair pos_int64_gen sched_req_data_gen)

let sched_req_record_data_gen :
  Daypack_lib.Sched_req.sched_req_record_data QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun task_seg_id sched_req_template ->
       Daypack_lib.Sched_req_data_skeleton.map
         (fun task_seg_size -> (task_seg_id, task_seg_size))
         sched_req_template)
    task_seg_id_gen sched_req_template_gen

let sched_req_record_gen : Daypack_lib.Sched_req.sched_req_record QCheck.Gen.t =
  QCheck.Gen.(pair pos_int64_gen sched_req_record_data_gen)

let arith_seq_gen =
  let open QCheck.Gen in
  map3
    (fun start offset diff ->
       Daypack_lib.Task.{ start; end_exc = Int64.add start offset; diff })
    pos_int64_gen small_pos_int64_gen small_nz_pos_int64_gen

let arith_seq =
  QCheck.make ~print:Daypack_lib.Task.Print.debug_string_of_arith_seq
    arith_seq_gen

let recur_data_gen =
  let open QCheck.Gen in
  map2
    (fun task_inst_data sched_req_templates ->
       Daypack_lib.Task.{ task_inst_data; sched_req_templates })
    task_inst_data_gen sched_req_templates_gen

let recur_gen =
  let open QCheck.Gen in
  map2
    (fun arith_seq recur_data ->
       Daypack_lib.Task.Arithemtic_seq (arith_seq, recur_data))
    arith_seq_gen recur_data_gen

let task_type_gen =
  let open QCheck.Gen in
  oneof
    [
      return Daypack_lib.Task.One_off;
      map (fun recur -> Daypack_lib.Task.Recurring recur) recur_gen;
    ]

let task_id_gen =
  QCheck.Gen.map2 (fun id1 id2 -> (id1, id2)) pos_int64_gen pos_int64_gen

let task_id = QCheck.make task_id_gen

let task_data_gen =
  let open QCheck.Gen in
  map3
    (fun splittable parallelizable task_type ->
       Daypack_lib.Task.{ splittable; parallelizable; task_type })
    bool bool task_type_gen

let task_gen = QCheck.Gen.(pair task_id_gen task_data_gen)

let task =
  QCheck.make ~print:Daypack_lib.Task.Print.debug_string_of_task task_gen

let pos_int64_set_gen =
  let open QCheck.Gen in
  map
    (fun l -> Daypack_lib.Int64_set.of_list l)
    (list_size (int_bound 100) pos_int64_gen)

let pos_int64_set =
  QCheck.make
    ~print:(fun s ->
        s |> Daypack_lib.Int64_set.to_seq |> List.of_seq
        |> QCheck.Print.list Print_utils.int64)
    pos_int64_set_gen
