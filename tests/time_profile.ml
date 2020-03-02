(*$ #use "tests/serialization_related.cinaps";;
  let unpack_pack_list = [
    ("period",
     "(pair time_pattern time_pattern)",
     "Daypack_lib.String_map.of_seq",
     "Daypack_lib.Time_profile_store.Serialize.pack_store",
     "Daypack_lib.Time_profile_store.Deserialize.unpack_store",
     "Daypack_lib.Time_profile_store.Equal.equal"
    )
  ] in

  List.iter (fun (name, inner_typ_gen, f_of_seq, f_pack, f_unpack, f_equal) ->
      print_unpack_is_inverse_of_pack_test_store
        ~name
        ~inner_typ_gen
        ~f_of_seq
        ~f_pack
        ~f_unpack
        ~f_equal)
    unpack_pack_list;

  print_endline "let suite = [";
  List.iter (fun (name, _, _, _, _, _) ->
      Printf.printf "%s;\n" (unpack_is_inverse_of_pack_test_name name);
    ) unpack_pack_list;
  print_endline "]";
*)

let qc_unpack_is_inverse_of_pack_period =
  QCheck.Test.make ~count:5000 ~name:"qc_unpack_is_inverse_of_pack_period"
    QCheck.(list_of_size Gen.(int_bound 100) (pair time_pattern time_pattern))
    (fun l ->
       let x = l |> List.to_seq |> Daypack_lib.String_map.of_seq in
       let y =
         x
         |> Daypack_lib.Time_profile_store.Serialize.pack_store
         |> Daypack_lib.Time_profile_store.Deserialize.unpack_store
       in
       Daypack_lib.Time_profile_store.Equal.equal
         (fun x y -> compare x y = 0)
         x y)

let suite = [ qc_unpack_is_inverse_of_pack_period ]

(*$*)
