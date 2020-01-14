open Test_utils

let qc_unpack_is_inverse_of_pack_arith_seq =
  QCheck.Test.make ~count:10_000 ~name:"qc_unpack_is_inverse_of_pack_arith_seq"
    arith_seq (fun arith_seq ->
        arith_seq |> Daypack_lib.Task.Serialize.pack_arith_seq
        |> Daypack_lib.Task.Deserialize.unpack_arith_seq = arith_seq)

let suite = [ qc_unpack_is_inverse_of_pack_arith_seq ]
