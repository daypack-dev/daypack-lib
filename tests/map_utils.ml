open Test_utils

(*$ #use "tests/map_utils.cinaps";;

  let store_list = [
    ("task_store",
     "task_store",
     "Daypack_lib.Task_id_map_utils.diff",
     "Daypack_lib.Task_id_map_utils.add_diff",
     "Daypack_lib.Task_id_map_utils.sub_diff",
     "Daypack_lib.Task_id_map.equal"
    );
    ("task_inst_store",
     "task_inst_store",
     "Daypack_lib.Task_inst_id_map_utils.diff",
     "Daypack_lib.Task_inst_id_map_utils.add_diff",
     "Daypack_lib.Task_inst_id_map_utils.sub_diff",
     "Daypack_lib.Task_inst_id_map.equal"
    );
    ("task_seg_store",
     "task_seg_store",
     "Daypack_lib.Task_seg_id_map_utils.diff",
     "Daypack_lib.Task_seg_id_map_utils.add_diff",
     "Daypack_lib.Task_seg_id_map_utils.sub_diff",
     "Daypack_lib.Task_seg_id_map.equal"
    );
    ("sched_req_pending_store",
     "sched_req_store",
     "Daypack_lib.Sched_req_id_map_utils.diff",
     "Daypack_lib.Sched_req_id_map_utils.add_diff",
     "Daypack_lib.Sched_req_id_map_utils.sub_diff",
     "Daypack_lib.Sched_req_id_map.equal"
    );
    ("sched_req_record_store",
     "sched_req_record_store",
     "Daypack_lib.Sched_req_id_map_utils.diff",
     "Daypack_lib.Sched_req_id_map_utils.add_diff",
     "Daypack_lib.Sched_req_id_map_utils.sub_diff",
     "Daypack_lib.Sched_req_id_map.equal"
    );
    ("quota",
     "quota",
     "Daypack_lib.Task_inst_id_map_utils.diff",
     "Daypack_lib.Task_inst_id_map_utils.add_diff",
     "Daypack_lib.Task_inst_id_map_utils.sub_diff",
     "Daypack_lib.Task_inst_id_map.equal"
    )
  ]
  in

  List.iter (fun (name, store_gen, f_diff, f_add_diff, _f_sub_diff, f_equal) ->
      Diff.print_add_diff_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_equal;
    )
    store_list;

  List.iter (fun (name, store_gen, f_diff, _f_add_diff, f_sub_diff, f_equal) ->
      Diff.print_sub_diff_test
        ~name
        ~store_gen
        ~f_diff
        ~f_sub_diff
        ~f_equal;
    )
    store_list;

  List.iter (fun (name, store_gen, f_diff, f_add_diff, f_sub_diff, f_equal) ->
      Diff.print_sub_diff_is_inverse_of_add_diff_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_sub_diff
        ~f_equal;
    )
    store_list;

  List.iter (fun (name, store_gen, f_diff, f_add_diff, f_sub_diff, f_equal) ->
      Diff.print_add_diff_is_inverse_of_sub_diff_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_sub_diff
        ~f_equal;
    )
    store_list;

  let bucket_store_list = [
    ("user_id_to_task_ids",
     "user_id_to_task_ids",
     "Daypack_lib.User_id_map_utils.Int64_bucketed.diff_bucketed",
     "Daypack_lib.User_id_map_utils.Int64_bucketed.add_diff_bucketed",
     "Daypack_lib.User_id_map_utils.Int64_bucketed.sub_diff_bucketed",
     "Daypack_lib.User_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
    ("task_id_to_task_inst_ids",
     "task_id_to_task_inst_ids",
     "Daypack_lib.Task_id_map_utils.Int64_bucketed.diff_bucketed",
     "Daypack_lib.Task_id_map_utils.Int64_bucketed.add_diff_bucketed",
     "Daypack_lib.Task_id_map_utils.Int64_bucketed.sub_diff_bucketed",
     "Daypack_lib.Task_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
    ("task_inst_id_to_task_seg_ids",
     "task_inst_id_to_task_seg_ids",
     "Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.diff_bucketed",
     "Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.add_diff_bucketed",
     "Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.sub_diff_bucketed",
     "Daypack_lib.Task_inst_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
    ("indexed_by_start",
     "task_seg_place_map",
     "Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.diff_bucketed",
     "Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.add_diff_bucketed",
     "Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.sub_diff_bucketed",
     "Daypack_lib.Int64_map.equal",
     "Daypack_lib.Task_seg_place_set.equal"
    );
  ] in

  List.iter (fun (name, store_gen, f_diff, f_add_diff, _f_sub_diff, f_equal, f_bucket_equal) ->
      Diff_bucketed.print_add_diff_bucketed_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_equal
        ~f_bucket_equal;
    )
    bucket_store_list;

  List.iter (fun (name, store_gen, f_diff, _f_add_diff, f_sub_diff, f_equal, f_bucket_equal) ->
      Diff_bucketed.print_sub_diff_bucketed_test
        ~name
        ~store_gen
        ~f_diff
        ~f_sub_diff
        ~f_equal
        ~f_bucket_equal;
    )
    bucket_store_list;

  List.iter (fun (name, store_gen, f_diff, f_add_diff, f_sub_diff, f_equal, f_bucket_equal) ->
      Diff_bucketed.print_sub_diff_bucketed_is_inverse_of_add_diff_bucketed_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_sub_diff
        ~f_equal
        ~f_bucket_equal;
    )
    bucket_store_list;

  List.iter (fun (name, store_gen, f_diff, f_add_diff, f_sub_diff, f_equal, f_bucket_equal) ->
      Diff_bucketed.print_add_diff_bucketed_is_inverse_of_sub_diff_bucketed_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_sub_diff
        ~f_equal
        ~f_bucket_equal;
    )
    bucket_store_list;

  print_endline "let suite = [";
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff.get_add_diff_test_name name);
    ) store_list;
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff.get_sub_diff_test_name name);
    ) store_list;
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff.get_sub_diff_is_inverse_of_add_diff_test_name name);
    ) store_list;
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff.get_add_diff_is_inverse_of_sub_diff_test_name name);
    ) store_list;
  List.iter (fun (name, _, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff_bucketed.get_add_diff_bucketed_test_name name);
    ) bucket_store_list;
  List.iter (fun (name, _, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff_bucketed.get_sub_diff_bucketed_test_name name);
    ) bucket_store_list;
  List.iter (fun (name, _, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff_bucketed.get_sub_diff_bucketed_is_inverse_of_add_diff_bucketed_test_name name);
    ) bucket_store_list;
  List.iter (fun (name, _, _, _, _, _, _) ->
      Printf.printf "%s;\n" (Diff_bucketed.get_add_diff_bucketed_is_inverse_of_sub_diff_bucketed_test_name name);
    ) bucket_store_list;
  print_endline "]"
*)

let qc_add_diff_test_task_store =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_task_store"
    QCheck.(pair task_store task_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_id_map_utils.diff ~old x in
       Daypack_lib.Task_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_id_map_utils.add_diff diff old)
         x)

let qc_add_diff_test_task_inst_store =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_task_inst_store"
    QCheck.(pair task_inst_store task_inst_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.add_diff diff old)
         x)

let qc_add_diff_test_task_seg_store =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_task_seg_store"
    QCheck.(pair task_seg_store task_seg_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_seg_id_map_utils.diff ~old x in
       Daypack_lib.Task_seg_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_seg_id_map_utils.add_diff diff old)
         x)

let qc_add_diff_test_sched_req_pending_store =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_sched_req_pending_store"
    QCheck.(pair sched_req_store sched_req_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.add_diff diff old)
         x)

let qc_add_diff_test_sched_req_record_store =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_sched_req_record_store"
    QCheck.(pair sched_req_record_store sched_req_record_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.add_diff diff old)
         x)

let qc_add_diff_test_quota =
  QCheck.Test.make ~count:5000 ~name:"qc_add_diff_test_quota"
    QCheck.(pair quota quota)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.add_diff diff old)
         x)

let qc_sub_diff_test_task_store =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_task_store"
    QCheck.(pair task_store task_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_id_map_utils.diff ~old x in
       Daypack_lib.Task_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_test_task_inst_store =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_task_inst_store"
    QCheck.(pair task_inst_store task_inst_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_test_task_seg_store =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_task_seg_store"
    QCheck.(pair task_seg_store task_seg_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_seg_id_map_utils.diff ~old x in
       Daypack_lib.Task_seg_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_seg_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_test_sched_req_pending_store =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_sched_req_pending_store"
    QCheck.(pair sched_req_store sched_req_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_test_sched_req_record_store =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_sched_req_record_store"
    QCheck.(pair sched_req_record_store sched_req_record_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_test_quota =
  QCheck.Test.make ~count:5000 ~name:"qc_sub_diff_test_quota"
    QCheck.(pair quota quota)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.sub_diff diff x)
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_task_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_task_store"
    QCheck.(pair task_store task_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_id_map_utils.diff ~old x in
       Daypack_lib.Task_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_id_map_utils.sub_diff diff
            (Daypack_lib.Task_id_map_utils.add_diff diff old))
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_task_inst_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_task_inst_store"
    QCheck.(pair task_inst_store task_inst_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.sub_diff diff
            (Daypack_lib.Task_inst_id_map_utils.add_diff diff old))
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_task_seg_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_task_seg_store"
    QCheck.(pair task_seg_store task_seg_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_seg_id_map_utils.diff ~old x in
       Daypack_lib.Task_seg_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_seg_id_map_utils.sub_diff diff
            (Daypack_lib.Task_seg_id_map_utils.add_diff diff old))
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_sched_req_pending_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_sched_req_pending_store"
    QCheck.(pair sched_req_store sched_req_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.sub_diff diff
            (Daypack_lib.Sched_req_id_map_utils.add_diff diff old))
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_sched_req_record_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_sched_req_record_store"
    QCheck.(pair sched_req_record_store sched_req_record_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.sub_diff diff
            (Daypack_lib.Sched_req_id_map_utils.add_diff diff old))
         old)

let qc_sub_diff_is_inverse_of_add_diff_test_quota =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_is_inverse_of_add_diff_test_quota"
    QCheck.(pair quota quota)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.sub_diff diff
            (Daypack_lib.Task_inst_id_map_utils.add_diff diff old))
         old)

let qc_add_diff_is_inverse_of_sub_diff_test_task_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_task_store"
    QCheck.(pair task_store task_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_id_map_utils.diff ~old x in
       Daypack_lib.Task_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_id_map_utils.add_diff diff
            (Daypack_lib.Task_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_is_inverse_of_sub_diff_test_task_inst_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_task_inst_store"
    QCheck.(pair task_inst_store task_inst_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.add_diff diff
            (Daypack_lib.Task_inst_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_is_inverse_of_sub_diff_test_task_seg_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_task_seg_store"
    QCheck.(pair task_seg_store task_seg_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_seg_id_map_utils.diff ~old x in
       Daypack_lib.Task_seg_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_seg_id_map_utils.add_diff diff
            (Daypack_lib.Task_seg_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_is_inverse_of_sub_diff_test_sched_req_pending_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_sched_req_pending_store"
    QCheck.(pair sched_req_store sched_req_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.add_diff diff
            (Daypack_lib.Sched_req_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_is_inverse_of_sub_diff_test_sched_req_record_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_sched_req_record_store"
    QCheck.(pair sched_req_record_store sched_req_record_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Sched_req_id_map_utils.diff ~old x in
       Daypack_lib.Sched_req_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Sched_req_id_map_utils.add_diff diff
            (Daypack_lib.Sched_req_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_is_inverse_of_sub_diff_test_quota =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_is_inverse_of_sub_diff_test_quota"
    QCheck.(pair quota quota)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_inst_id_map_utils.diff ~old x in
       Daypack_lib.Task_inst_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_inst_id_map_utils.add_diff diff
            (Daypack_lib.Task_inst_id_map_utils.sub_diff diff x))
         x)

let qc_add_diff_bucketed_test_user_id_to_task_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_bucketed_test_user_id_to_task_ids"
    QCheck.(pair user_id_to_task_ids user_id_to_task_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.User_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.User_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.User_id_map_utils.Int64_bucketed.add_diff_bucketed diff
            old)
         x)

let qc_add_diff_bucketed_test_task_id_to_task_inst_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_bucketed_test_task_id_to_task_inst_ids"
    QCheck.(pair task_id_to_task_inst_ids task_id_to_task_inst_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_id_map_utils.Int64_bucketed.add_diff_bucketed diff
            old)
         x)

let qc_add_diff_bucketed_test_task_inst_id_to_task_seg_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_bucketed_test_task_inst_id_to_task_seg_ids"
    QCheck.(pair task_inst_id_to_task_seg_ids task_inst_id_to_task_seg_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_inst_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.add_diff_bucketed
            diff old)
         x)

let qc_add_diff_bucketed_test_indexed_by_start =
  QCheck.Test.make ~count:5000
    ~name:"qc_add_diff_bucketed_test_indexed_by_start"
    QCheck.(pair task_seg_place_map task_seg_place_map)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Int64_map.equal Daypack_lib.Task_seg_place_set.equal
         (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.add_diff_bucketed
            diff old)
         x)

let qc_sub_diff_bucketed_test_user_id_to_task_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_bucketed_test_user_id_to_task_ids"
    QCheck.(pair user_id_to_task_ids user_id_to_task_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.User_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.User_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.User_id_map_utils.Int64_bucketed.sub_diff_bucketed diff x)
         old)

let qc_sub_diff_bucketed_test_task_id_to_task_inst_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_bucketed_test_task_id_to_task_inst_ids"
    QCheck.(pair task_id_to_task_inst_ids task_id_to_task_inst_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_id_map_utils.Int64_bucketed.sub_diff_bucketed diff x)
         old)

let qc_sub_diff_bucketed_test_task_inst_id_to_task_seg_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_bucketed_test_task_inst_id_to_task_seg_ids"
    QCheck.(pair task_inst_id_to_task_seg_ids task_inst_id_to_task_seg_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_inst_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.sub_diff_bucketed
            diff x)
         old)

let qc_sub_diff_bucketed_test_indexed_by_start =
  QCheck.Test.make ~count:5000
    ~name:"qc_sub_diff_bucketed_test_indexed_by_start"
    QCheck.(pair task_seg_place_map task_seg_place_map)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Int64_map.equal Daypack_lib.Task_seg_place_set.equal
         (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.sub_diff_bucketed
            diff x)
         old)

let qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_user_id_to_task_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_user_id_to_task_ids"
    QCheck.(pair user_id_to_task_ids user_id_to_task_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.User_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.User_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.User_id_map_utils.Int64_bucketed.sub_diff_bucketed diff
            (Daypack_lib.User_id_map_utils.Int64_bucketed.add_diff_bucketed diff
               old))
         old)

let qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_id_to_task_inst_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_id_to_task_inst_ids"
    QCheck.(pair task_id_to_task_inst_ids task_id_to_task_inst_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_id_map_utils.Int64_bucketed.sub_diff_bucketed diff
            (Daypack_lib.Task_id_map_utils.Int64_bucketed.add_diff_bucketed diff
               old))
         old)

let qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_inst_id_to_task_seg_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_inst_id_to_task_seg_ids"
    QCheck.(pair task_inst_id_to_task_seg_ids task_inst_id_to_task_seg_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_inst_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.sub_diff_bucketed
            diff
            (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.add_diff_bucketed
               diff old))
         old)

let qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_indexed_by_start =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_indexed_by_start"
    QCheck.(pair task_seg_place_map task_seg_place_map)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Int64_map.equal Daypack_lib.Task_seg_place_set.equal
         (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.sub_diff_bucketed
            diff
            (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed
             .add_diff_bucketed diff old))
         old)

let qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_user_id_to_task_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_user_id_to_task_ids"
    QCheck.(pair user_id_to_task_ids user_id_to_task_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.User_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.User_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.User_id_map_utils.Int64_bucketed.add_diff_bucketed diff
            (Daypack_lib.User_id_map_utils.Int64_bucketed.sub_diff_bucketed diff
               x))
         x)

let qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_id_to_task_inst_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_id_to_task_inst_ids"
    QCheck.(pair task_id_to_task_inst_ids task_id_to_task_inst_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_id_map_utils.Int64_bucketed.add_diff_bucketed diff
            (Daypack_lib.Task_id_map_utils.Int64_bucketed.sub_diff_bucketed diff
               x))
         x)

let qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_inst_id_to_task_seg_ids
  =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_inst_id_to_task_seg_ids"
    QCheck.(pair task_inst_id_to_task_seg_ids task_inst_id_to_task_seg_ids)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Task_inst_id_map.equal Daypack_lib.Int64_set.equal
         (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.add_diff_bucketed
            diff
            (Daypack_lib.Task_inst_id_map_utils.Int64_bucketed.sub_diff_bucketed
               diff x))
         x)

let qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_indexed_by_start =
  QCheck.Test.make ~count:5000
    ~name:
      "qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_indexed_by_start"
    QCheck.(pair task_seg_place_map task_seg_place_map)
    (fun (old, x) ->
       let diff =
         Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.diff_bucketed ~old x
       in
       Daypack_lib.Int64_map.equal Daypack_lib.Task_seg_place_set.equal
         (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed.add_diff_bucketed
            diff
            (Daypack_lib.Int64_map_utils.Task_seg_place_bucketed
             .sub_diff_bucketed diff x))
         x)

let suite =
  [
    qc_add_diff_test_task_store;
    qc_add_diff_test_task_inst_store;
    qc_add_diff_test_task_seg_store;
    qc_add_diff_test_sched_req_pending_store;
    qc_add_diff_test_sched_req_record_store;
    qc_add_diff_test_quota;
    qc_sub_diff_test_task_store;
    qc_sub_diff_test_task_inst_store;
    qc_sub_diff_test_task_seg_store;
    qc_sub_diff_test_sched_req_pending_store;
    qc_sub_diff_test_sched_req_record_store;
    qc_sub_diff_test_quota;
    qc_sub_diff_is_inverse_of_add_diff_test_task_store;
    qc_sub_diff_is_inverse_of_add_diff_test_task_inst_store;
    qc_sub_diff_is_inverse_of_add_diff_test_task_seg_store;
    qc_sub_diff_is_inverse_of_add_diff_test_sched_req_pending_store;
    qc_sub_diff_is_inverse_of_add_diff_test_sched_req_record_store;
    qc_sub_diff_is_inverse_of_add_diff_test_quota;
    qc_add_diff_is_inverse_of_sub_diff_test_task_store;
    qc_add_diff_is_inverse_of_sub_diff_test_task_inst_store;
    qc_add_diff_is_inverse_of_sub_diff_test_task_seg_store;
    qc_add_diff_is_inverse_of_sub_diff_test_sched_req_pending_store;
    qc_add_diff_is_inverse_of_sub_diff_test_sched_req_record_store;
    qc_add_diff_is_inverse_of_sub_diff_test_quota;
    qc_add_diff_bucketed_test_user_id_to_task_ids;
    qc_add_diff_bucketed_test_task_id_to_task_inst_ids;
    qc_add_diff_bucketed_test_task_inst_id_to_task_seg_ids;
    qc_add_diff_bucketed_test_indexed_by_start;
    qc_sub_diff_bucketed_test_user_id_to_task_ids;
    qc_sub_diff_bucketed_test_task_id_to_task_inst_ids;
    qc_sub_diff_bucketed_test_task_inst_id_to_task_seg_ids;
    qc_sub_diff_bucketed_test_indexed_by_start;
    qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_user_id_to_task_ids;
    qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_id_to_task_inst_ids;
    qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_task_inst_id_to_task_seg_ids;
    qc_sub_diff_bucketed_is_inverse_of_add_diff_test_bucketed_indexed_by_start;
    qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_user_id_to_task_ids;
    qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_id_to_task_inst_ids;
    qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_task_inst_id_to_task_seg_ids;
    qc_add_diff_bucketed_is_inverse_of_sub_diff_test_bucketed_indexed_by_start;
  ]

(*$*)
