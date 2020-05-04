let () =
  let alco_suites = [ ("Range", Range.alco_suite) ] in
  let qc_suites =
    [ (* ("Misc_utils", Misc_utils.suite);
       * ("Map_utils", Map_utils.suite);
       * ("Set_utils", Set_utils.suite); *)
      (* ("Time_slots", Time_slots.suite); *)
      (* ("Time_pattern", Time_pattern.suite);
       * ("Time_profile", Time_profile.suite);
       * ("Time_profile_store", Time_profile_store.suite);
       * ("Task_seg_place_gens", Task_seg_place_gens.suite);
       * ("Task", Task.suite);
       * ("Sched", Sched.suite);
       * ("Sched_ver_history", Sched_ver_history.suite); *) ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "daypack_lib" suites

(* let () =
 *   QCheck.Test.check_exn
 *     Sched_ver_history.qc_read_from_dir_is_inverse_of_write_to_dir *)
