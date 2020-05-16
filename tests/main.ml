let () =
  let alco_suites =
    [
      ("Range.Alco", Range.Alco.suite);
      ("Time_slots.Alco", Time_slots.Alco.suite);
    ]
  in
  let qc_suites =
    [
      ("Misc_utils", Misc_utils.Qc.suite);
      ("Map_utils", Map_utils.Qc.suite);
      ("Set_utils", Set_utils.Qc.suite);
      ("Time_slots.Qc", Time_slots.Qc.suite);
      ("Time_pattern", Time_pattern.Qc.suite);
      ("Time_profile", Time_profile.Qc.suite);
      ("Time_profile_store", Time_profile_store.Qc.suite);
      ("Task_seg_place_gens", Task_seg_place_gens.Qc.suite);
      ("Task", Task.Qc.suite);
      ("Sched", Sched.Qc.suite);
      ("Sched_ver_history", Sched_ver_history.Qc.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "daypack_lib" suites

(* let () =
 *   QCheck.Test.check_exn
 *     Sched_ver_history.qc_read_from_dir_is_inverse_of_write_to_dir *)
