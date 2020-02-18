open Test_utils

(*$ #use "tests/serialization_related.cinaps";;

  let unpack_pack_list = [
    ("arith_seq",
     "Daypack_lib.Task_ds.Serialize.pack_arith_seq",
     "Daypack_lib.Task_ds.Deserialize.unpack_arith_seq"
    );
    ("task",
     "Daypack_lib.Task_ds.Serialize.pack_task",
     "Daypack_lib.Task_ds.Deserialize.unpack_task"
    );
    ("task_inst",
     "Daypack_lib.Task_ds.Serialize.pack_task_inst",
     "Daypack_lib.Task_ds.Deserialize.unpack_task_inst"
    );
    ("task_seg",
     "Daypack_lib.Task_ds.Serialize.pack_task_seg",
     "Daypack_lib.Task_ds.Deserialize.unpack_task_seg"
    );
  ] in

  List.iter (fun (typ, f_pack, f_unpack) ->
      print_unpack_is_inverse_of_pack_test ~typ
        ~f_pack
        ~f_unpack
    ) unpack_pack_list;

  print_endline "let suite = [";
  List.iter (fun (typ, _, _) ->
      Printf.printf "%s;" (unpack_is_inverse_of_pack_test_name typ);
    ) unpack_pack_list;
  print_endline "]";
*)

let qc_unpack_is_inverse_of_pack_arith_seq =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_arith_seq"
    arith_seq (fun x ->
        x
        |> Daypack_lib.Task_ds.Serialize.pack_arith_seq
        |> Daypack_lib.Task_ds.Deserialize.unpack_arith_seq
           = x)

let qc_unpack_is_inverse_of_pack_task =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_task" task
    (fun x ->
       x
       |> Daypack_lib.Task_ds.Serialize.pack_task
       |> Daypack_lib.Task_ds.Deserialize.unpack_task
          = x)

let qc_unpack_is_inverse_of_pack_task_inst =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_task_inst"
    task_inst (fun x ->
        x
        |> Daypack_lib.Task_ds.Serialize.pack_task_inst
        |> Daypack_lib.Task_ds.Deserialize.unpack_task_inst
           = x)

let qc_unpack_is_inverse_of_pack_task_seg =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_task_seg"
    task_seg (fun x ->
        x
        |> Daypack_lib.Task_ds.Serialize.pack_task_seg
        |> Daypack_lib.Task_ds.Deserialize.unpack_task_seg
           = x)

let suite =
  [
    qc_unpack_is_inverse_of_pack_arith_seq;
    qc_unpack_is_inverse_of_pack_task;
    qc_unpack_is_inverse_of_pack_task_inst;
    qc_unpack_is_inverse_of_pack_task_seg;
  ]

(*$*)
