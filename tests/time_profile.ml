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
*)

(*$*)
