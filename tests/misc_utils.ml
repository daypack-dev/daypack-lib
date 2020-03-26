open Test_utils

let qc_int32_int32_to_int64_is_inverse_of_int64_to_int32_int32 =
  QCheck.Test.make ~count:5000
    ~name:"qc_int32_int32_to_int64_is_inverse_of_int64_to_int32_int32" pos_int64
    (fun x ->
       let y =
         x
         |> Daypack_lib.Misc_utils.int64_to_int32_int32
         |> Daypack_lib.Misc_utils.int32_int32_to_int64
       in
       x = y)

let suite = [ qc_int32_int32_to_int64_is_inverse_of_int64_to_int32_int32 ]
