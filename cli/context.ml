type t = {
  sched_ver_history : Daypack_lib.Sched_ver_history.t;
  time_profile_store : Daypack_lib.Time_profile_store.t;
}

let load () : (t, string) result =
  let sched_ver_history_dir =
    match Sys.getenv_opt Config.sched_ver_history_dir_env_var_name with
    | None -> Config.default_sched_ver_history_dir
    | Some s -> s
  in
  let time_profile_store_dir =
    match Sys.getenv_opt Config.time_profile_store_dir_env_var_name with
    | None -> Config.default_time_profile_store_dir
    | Some s -> s
  in
  let time_profile_store =
    if Sys.file_exists time_profile_store_dir then
      Daypack_lib.Time_profile_store.Deserialize.read_from_dir ~dir:time_profile_store_dir
    else
      Ok (Daypack_lib.Time_profile_store.make_empty ())
  in
  match time_profile_store with
  | Error msg -> Error msg
  | Ok time_profile_store ->
    let sched_ver_history =
      if Sys.file_exists sched_ver_history_dir then
        Daypack_lib.Sched_ver_history.Deserialize.read_from_dir ~dir:sched_ver_history_dir
      else
        Ok (Daypack_lib.Sched_ver_history.make_empty ())
    in
    match sched_ver_history with
    | Error msg -> Error msg
    | Ok sched_ver_history ->
      Ok { sched_ver_history; time_profile_store }
