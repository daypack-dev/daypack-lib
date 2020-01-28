open Test_utils

let qc_of_base_and_diffs_is_inverse_of_to_base_and_diffs =
  QCheck.Test.make ~count:500
    ~name:"qc_of_base_and_diffs_is_inverse_of_to_base_and_diffs"
    QCheck.(list_of_size Gen.(int_range 1 20) sched)
    (fun scheds ->
       match Daypack_lib.Sched_ver_list.to_base_and_diffs scheds with
       | None -> false
       | Some (base, diffs) ->
         let reconstructed_scheds =
           Daypack_lib.Sched_ver_list.of_base_and_diffs base diffs
         in
         List.for_all2
           (fun s1 s2 -> Daypack_lib.Sched.Equal.sched_equal s1 s2)
           scheds reconstructed_scheds)
