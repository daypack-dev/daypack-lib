open Test_utils

(*$ #use "tests/serialization_related.cinaps";;
  #use "tests/sched.cinaps";;

  let unpack_pack_store_list = [
    ("task_store",
     "task",
     "Daypack_lib.Task_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_task_store",
     "Daypack_lib.Sched.Deserialize.unpack_task_list",
     "Daypack_lib.Task_id_map.equal"
    );
    ("task_inst_store",
     "task_inst",
     "Daypack_lib.Task_inst_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_task_inst_store",
     "Daypack_lib.Sched.Deserialize.unpack_task_inst_list",
     "Daypack_lib.Task_inst_id_map.equal"
    );
    ("task_seg_store",
     "task_seg",
     "Daypack_lib.Task_seg_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_task_seg_store",
     "Daypack_lib.Sched.Deserialize.unpack_task_seg_list",
     "Daypack_lib.Task_seg_id_map.equal"
    );
    ("sched_req_pending_store",
     "sched_req",
     "Daypack_lib.Sched_req_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_sched_req_pending_store",
     "Daypack_lib.Sched.Deserialize.unpack_sched_req_pending_list",
     "Daypack_lib.Sched_req_id_map.equal"
    );
    ("sched_req_record_store",
     "sched_req_record",
     "Daypack_lib.Sched_req_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_sched_req_record_store",
     "Daypack_lib.Sched.Deserialize.unpack_sched_req_record_list",
     "Daypack_lib.Sched_req_id_map.equal"
    );
    ("quota",
     "(pair task_inst_id pos_int64)",
     "Daypack_lib.Task_inst_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_quota",
     "Daypack_lib.Sched.Deserialize.unpack_quota",
     "Daypack_lib.Task_inst_id_map.equal"
    );
  ] in

  let unpack_pack_bucket_store_list = [
    ("user_id_to_task_ids",
     "pos_int64",
     "pos_int64_set",
     "Daypack_lib.User_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_user_id_to_task_ids",
     "Daypack_lib.Sched.Deserialize.unpack_user_id_to_task_ids",
     "Daypack_lib.User_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
    ("task_id_to_task_inst_ids",
     "task_id",
     "pos_int64_set",
     "Daypack_lib.Task_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_task_id_to_task_inst_ids",
     "Daypack_lib.Sched.Deserialize.unpack_task_id_to_task_inst_ids",
     "Daypack_lib.Task_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
    ("task_inst_id_to_task_seg_ids",
     "task_inst_id",
     "pos_int64_set",
     "Daypack_lib.Task_inst_id_map.of_seq",
     "Daypack_lib.Sched.Serialize.pack_task_inst_id_to_task_seg_ids",
     "Daypack_lib.Sched.Deserialize.unpack_task_inst_id_to_task_seg_ids",
     "Daypack_lib.Task_inst_id_map.equal",
     "Daypack_lib.Int64_set.equal"
    );
  ] in

  let unpack_pack_set_store_list = [
    ("sched_req_ids",
     "pos_int64_set",
     "Daypack_lib.Sched.Serialize.pack_sched_req_ids",
     "Daypack_lib.Sched.Deserialize.unpack_sched_req_ids",
     "Daypack_lib.Int64_set.equal"
    )
  ]
  in

  List.iter (fun (name, inner_typ_gen, f_of_seq, f_pack, f_unpack, f_equal) ->
      print_unpack_is_inverse_of_pack_test_store
        ~name
        ~inner_typ_gen
        ~f_of_seq
        ~f_pack
        ~f_unpack
        ~f_equal)
    unpack_pack_store_list;

  List.iter (fun (name, id_gen, bucket_gen, f_of_seq, f_pack, f_unpack, f_equal, f_bucket_equal) ->
      print_unpack_is_inverse_of_pack_test_bucket_store
        ~name
        ~id_gen
        ~bucket_gen
        ~f_of_seq
        ~f_pack
        ~f_unpack
        ~f_equal
        ~f_bucket_equal
    )
    unpack_pack_bucket_store_list;

  List.iter (fun (name, set_gen, f_pack, f_unpack, f_equal) ->
      print_unpack_is_inverse_of_pack_test_set_store
        ~name
        ~set_gen
        ~f_pack
        ~f_unpack
        ~f_equal)
    unpack_pack_set_store_list;

  let diff_test_list = [
    ("sched",
     "sched",
     "Daypack_lib.Sched.Diff.diff_sched",
     "Daypack_lib.Sched.Diff.add_diff_sched",
     "Daypack_lib.Sched.Diff.sub_diff_sched"
    )
  ] in

  print_endline "let suite = [";
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (unpack_is_inverse_of_pack_test_name name);
    ) unpack_pack_store_list;
  List.iter (fun (name, _, _, _, _, _, _, _) ->
      Printf.printf "%s;\n" (unpack_is_inverse_of_pack_test_name name);
    ) unpack_pack_bucket_store_list;
  List.iter (fun (name, _, _, _, _) ->
      Printf.printf "%s;\n" (unpack_is_inverse_of_pack_test_name name);
    )
    unpack_pack_set_store_list;
  print_endline "]";
*)

let qc_unpack_is_inverse_of_pack_task_store =
  QCheck.Test.make ~count:5000 ~name:"qc_unpack_is_inverse_of_pack_task_store"
    QCheck.(list_of_size Gen.(int_bound 100) task)
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_task_store
         |> Daypack_lib.Sched.Deserialize.unpack_task_list
       in
       Daypack_lib.Task_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_task_inst_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_task_inst_store"
    QCheck.(list_of_size Gen.(int_bound 100) task_inst)
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_task_inst_store
         |> Daypack_lib.Sched.Deserialize.unpack_task_inst_list
       in
       Daypack_lib.Task_inst_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_task_seg_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_task_seg_store"
    QCheck.(list_of_size Gen.(int_bound 100) task_seg)
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_seg_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_task_seg_store
         |> Daypack_lib.Sched.Deserialize.unpack_task_seg_list
       in
       Daypack_lib.Task_seg_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_sched_req_pending_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_sched_req_pending_store"
    QCheck.(list_of_size Gen.(int_bound 100) sched_req)
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Sched_req_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_sched_req_pending_store
         |> Daypack_lib.Sched.Deserialize.unpack_sched_req_pending_list
       in
       Daypack_lib.Sched_req_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_sched_req_record_store =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_sched_req_record_store"
    QCheck.(list_of_size Gen.(int_bound 100) sched_req_record)
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Sched_req_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_sched_req_record_store
         |> Daypack_lib.Sched.Deserialize.unpack_sched_req_record_list
       in
       Daypack_lib.Sched_req_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_quota =
  QCheck.Test.make ~count:5000 ~name:"qc_unpack_is_inverse_of_pack_quota"
    QCheck.(list_of_size Gen.(int_bound 100) (pair task_inst_id pos_int64))
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_quota
         |> Daypack_lib.Sched.Deserialize.unpack_quota
       in
       Daypack_lib.Task_inst_id_map.equal (fun x y -> compare x y = 0) x y)

let qc_unpack_is_inverse_of_pack_user_id_to_task_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_user_id_to_task_ids"
    QCheck.(list_of_size Gen.(int_bound 10) (pair pos_int64 pos_int64_set))
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.User_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_user_id_to_task_ids
         |> Daypack_lib.Sched.Deserialize.unpack_user_id_to_task_ids
       in
       Daypack_lib.User_id_map.equal Daypack_lib.Int64_set.equal x y)

let qc_unpack_is_inverse_of_pack_task_id_to_task_inst_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_task_id_to_task_inst_ids"
    QCheck.(list_of_size Gen.(int_bound 10) (pair task_id pos_int64_set))
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_task_id_to_task_inst_ids
         |> Daypack_lib.Sched.Deserialize.unpack_task_id_to_task_inst_ids
       in
       Daypack_lib.Task_id_map.equal Daypack_lib.Int64_set.equal x y)

let qc_unpack_is_inverse_of_pack_task_inst_id_to_task_seg_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_task_inst_id_to_task_seg_ids"
    QCheck.(list_of_size Gen.(int_bound 10) (pair task_inst_id pos_int64_set))
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq in
       let y =
         x |> Daypack_lib.Sched.Serialize.pack_task_inst_id_to_task_seg_ids
         |> Daypack_lib.Sched.Deserialize.unpack_task_inst_id_to_task_seg_ids
       in
       Daypack_lib.Task_inst_id_map.equal Daypack_lib.Int64_set.equal x y)

let qc_unpack_is_inverse_of_pack_sched_req_ids =
  QCheck.Test.make ~count:5000
    ~name:"qc_unpack_is_inverse_of_pack_sched_req_ids" pos_int64_set (fun x ->
        let y =
          x |> Daypack_lib.Sched.Serialize.pack_sched_req_ids
          |> Daypack_lib.Sched.Deserialize.unpack_sched_req_ids
        in
        Daypack_lib.Int64_set.equal x y)

let suite =
  [
    qc_unpack_is_inverse_of_pack_task_store;
    qc_unpack_is_inverse_of_pack_task_inst_store;
    qc_unpack_is_inverse_of_pack_task_seg_store;
    qc_unpack_is_inverse_of_pack_sched_req_pending_store;
    qc_unpack_is_inverse_of_pack_sched_req_record_store;
    qc_unpack_is_inverse_of_pack_quota;
    qc_unpack_is_inverse_of_pack_user_id_to_task_ids;
    qc_unpack_is_inverse_of_pack_task_id_to_task_inst_ids;
    qc_unpack_is_inverse_of_pack_task_inst_id_to_task_seg_ids;
    qc_unpack_is_inverse_of_pack_sched_req_ids;
  ]

(*$*)
