let () =
  let suites =
    [
      (* ("Time_slot", Time_slot.suite);
       * ("Task_seg_gens", Task_seg_place_gens.suite); *)
      (* ("Task", Task.suite); *)
      ("Sched", Sched.suite);
      ("Map_utils", Map_utils.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  Alcotest.run "daypack_lib" suites

(* let () = QCheck.Test.check_exn Task_seg_place_gens.qc_single_task_seg_multi_splits_exact_shift_consistent *)
