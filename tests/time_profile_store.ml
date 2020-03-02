open Test_utils

let qc_read_from_dir_is_inverse_of_write_to_dir =
  QCheck.Test.make ~count:1000
    ~name:"qc_read_from_dir_is_inverse_of_write_to_dir" time_profile_store
    (fun store ->
       let dir = Core.Filename.temp_dir "daypack" "time_profile_store" in
       ( match
           Daypack_lib.Time_profile_store.Serialize.write_to_dir ~dir store
         with
         | Ok () -> ()
         | Error msg -> failwith msg );
       let store' =
         Daypack_lib.Time_profile_store.Deserialize.read_from_dir ~dir
         |> Stdlib.Result.get_ok
       in
       FileUtil.rm ~recurse:true [ dir ];
       Daypack_lib.Time_profile_store.Equal.equal store store')

let suite = [ qc_read_from_dir_is_inverse_of_write_to_dir ]
