type t = {
  sched_ver_history : Daypack_lib.Sched_ver_history.t;
  time_profile_store : Daypack_lib.Time_profile_store.t;
}

let load () : (t, string) result =
  match Config.time_profile_store_dir with
  | None -> Error "Environment variable HOME not declared"
  | Some time_profile_store_dir -> (
      match Config.sched_ver_history_dir with
      | None -> Error "Environment variable HOME not declared"
      | Some sched_ver_history_dir -> (
          let time_profile_store =
            if Sys.file_exists time_profile_store_dir then
              Daypack_lib.Time_profile_store.Deserialize.read_from_dir
                ~dir:time_profile_store_dir
            else Ok (Daypack_lib.Time_profile_store.make_empty ())
          in
          match time_profile_store with
          | Error msg -> Error msg
          | Ok time_profile_store -> (
              let sched_ver_history =
                if Sys.file_exists sched_ver_history_dir then
                  Daypack_lib.Sched_ver_history.Deserialize.read_from_dir
                    ~dir:sched_ver_history_dir
                else Ok (Daypack_lib.Sched_ver_history.make_empty ())
              in
              match sched_ver_history with
              | Error msg -> Error msg
              | Ok sched_ver_history ->
                Ok { sched_ver_history; time_profile_store } ) ) )

let save (t : t) : (unit, string) result =
  match Config.sched_ver_history_dir with
  | None -> Error "Environment variable HOME not declared"
  | Some sched_ver_history_dir ->
    if not (Sys.file_exists sched_ver_history_dir) then
      FileUtil.mkdir ~parent:true sched_ver_history_dir;
    Daypack_lib.Sched_ver_history.Serialize.write_to_dir
      ~dir:sched_ver_history_dir t.sched_ver_history
