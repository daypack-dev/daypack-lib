open Test_utils

(*$ #use "tests/serialization_related.cinaps";;

     let unpack_pack_list = [
       
     ] in

     List.iter (fun (element_typ, ))
*)
(*$*)

let qc_unpack_is_inverse_of_pack_task_store =
  QCheck.Test.make ~count:1000 ~name:"qc_unpack_is_inverse_of_pack_task_store"
    QCheck.(list_of_size Gen.(int_bound 100) task)
    (fun l ->
       let x =
         l |> List.to_seq
         |> Daypack_lib.Task_id_map.of_seq
       in
       let y = x |> Daypack_lib.Sched.Serialize.pack_task_store |>
               Daypack_lib.Sched.Deserialize.unpack_task_list
       in
       Daypack_lib.Task_id_map.equal (fun x y -> compare x y = 0) x y
    )

let suite = [
  qc_unpack_is_inverse_of_pack_task_store
]
