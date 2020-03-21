let default_item_dir =
  Sys.getenv_opt "HOME"
  |> Option.map (fun home -> Filename.concat home ".daypc")

let default_sched_ver_history_dir =
  default_item_dir |> Option.map (fun dir -> Filename.concat dir "svh")

let default_time_profile_store_dir =
  default_item_dir |> Option.map (fun dir -> Filename.concat dir "tps")

let sched_ver_history_dir_env_var_name = "DAYPC_SVH_DIR"

let time_profile_store_dir_env_var_name = "DAYPC_TPS_DIR"

let sched_ver_history_dir =
  match Sys.getenv_opt sched_ver_history_dir_env_var_name with
  | None -> default_sched_ver_history_dir
  | Some s -> Some s

let time_profile_store_dir =
  match Sys.getenv_opt time_profile_store_dir_env_var_name with
  | None -> default_time_profile_store_dir
  | Some s -> Some s

let time_pattern_search_years_ahead = 5
