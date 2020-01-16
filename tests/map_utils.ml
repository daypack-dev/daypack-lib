open Test_utils

(*$ #use "tests/map_utils.cinaps";;

  let store_list = [
    ("task_store",
     "task_store",
     "Daypack_lib.Task_id_map_utils.diff",
     "Daypack_lib.Task_id_map_utils.add_diff",
     "Daypack_lib.Task_id_map.equal"
    )
  ]
  in

  List.iter (fun (name, store_gen, f_diff, f_add_diff, f_equal) ->
      print_add_diff_test
        ~name
        ~store_gen
        ~f_diff
        ~f_add_diff
        ~f_equal)
    store_list;

  print_endline "let suite = [";
  List.iter (fun (name, _, _, _, _) ->
      Printf.printf "%s;\n" (get_add_diff_test_name name);
    ) store_list;
  print_endline "]"
*)

let add_diff_test_task_store =
  QCheck.Test.make ~count:10_000 ~name:"add_diff_test_task_store"
    QCheck.(pair task_store task_store)
    (fun (old, x) ->
       let diff = Daypack_lib.Task_id_map_utils.diff ~old x in
       Daypack_lib.Task_id_map.equal
         (fun x y -> compare x y = 0)
         (Daypack_lib.Task_id_map_utils.add_diff diff old)
         x)

let suite = [ add_diff_test_task_store ]

(*$*)
