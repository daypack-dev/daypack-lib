open Test_utils

let qc_int32_int32_to_int64_is_inverse_of_int32_int32_of_int64 =
  QCheck.Test.make ~count:10_000
    ~name:"qc_int32_int32_to_int64_is_inverse_of_int32_int32_of_int64" pos_int64
    (fun x ->
       let y =
         x
         |> Daypack_lib.Misc_utils.int32_int32_of_int64
         |> Daypack_lib.Misc_utils.int32_int32_to_int64
       in
       x = y)

let suite = [ qc_int32_int32_to_int64_is_inverse_of_int32_int32_of_int64 ]
