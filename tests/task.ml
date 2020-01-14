open Test_utils

(*$ #use "tests/unpack_pack.cinaps";;

  print_unpack_is_inverse_of_pack_test ~typ:"arith_seq"
    ~f_pack_name:"Daypack_lib.Task.Serialize.pack_arith_seq"
    ~f_unpack_name:"Daypack_lib.Task.Deserialize.unpack_arith_seq";
*)

let qc_unpack_is_inverse_of_pack_arith_seq =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_arith_seq"
    arith_seq (fun x ->
        x |> Daypack_lib.Task.Serialize.pack_arith_seq
        |> Daypack_lib.Task.Deserialize.unpack_arith_seq = x)

(*$*)

let suite = [ qc_unpack_is_inverse_of_pack_arith_seq ]
