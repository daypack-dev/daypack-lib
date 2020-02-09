open Test_utils

let qc_list_of_base_and_diffs_is_inverse_of_list_to_base_and_diffs =
  QCheck.Test.make ~count:500
    ~name:"qc_of_base_and_diffs_is_inverse_of_to_base_and_diffs"
    QCheck.(list_of_size Gen.(int_range 1 10) sched)
    (fun scheds ->
       match
         Daypack_lib.Sched_ver_history.Serialize.list_to_base_and_diffs scheds
       with
       | None -> false
       | Some (base, diffs) ->
         let reconstructed_scheds =
           Daypack_lib.Sched_ver_history.Deserialize.list_of_base_and_diffs base
             diffs
         in
         List.for_all2
           (fun s1 s2 -> Daypack_lib.Sched.Equal.sched_equal s1 s2)
           scheds reconstructed_scheds)

let suite = [ qc_list_of_base_and_diffs_is_inverse_of_list_to_base_and_diffs]
