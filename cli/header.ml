let display_current_time () : unit =
  let cur_time_str =
    Daypack_lib.Time.Current.cur_unix_time ()
    |> Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_time
      ~display_in_time_zone:`Local
  in
  print_endline "Time right now:";
  Printf.printf "  - %s\n" cur_time_str

let display_pending_sched_reqs (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let cur_tm = Daypack_lib.Time.Current.cur_tm_local () in
  let end_exc_tm =
    { cur_tm with tm_mon = cur_tm.tm_mday + Config.sched_day_count }
  in
  let start = Daypack_lib.Time.unix_time_of_tm ~time_zone_of_tm:`Local cur_tm in
  let end_exc =
    Daypack_lib.Time.unix_time_of_tm ~time_zone_of_tm:`Local end_exc_tm
  in
  let expired_sched_reqs =
    Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq
      ~end_exc:start hd
    |> List.of_seq
  in
  let pending_sched_reqs_fully_within_time_slot =
    Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq ~start
      ~end_exc hd
    |> List.of_seq
  in
  let pending_sched_reqs_fully_or_partially_within_time_slot =
    Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq ~start
      ~end_exc ~include_sched_req_starting_within_time_slot:true
      ~include_sched_req_ending_within_time_slot:true hd
    |> List.of_seq
  in
  let count_expired = List.length expired_sched_reqs in
  let count_fully_within =
    List.length pending_sched_reqs_fully_within_time_slot
  in
  let count_fully_or_partially_within =
    List.length pending_sched_reqs_fully_or_partially_within_time_slot
  in
  if count_expired = 0 then print_endline "  - No expired scheduling requests"
  else Printf.printf "  - Expired scheduling requests: %d\n" count_expired;
  print_newline ();
  if count_fully_or_partially_within = 0 then
    Printf.printf "  - No pending scheduling requests within next %d days\n"
      Config.sched_day_count
  else (
    Printf.printf "  - Processable pending scheduling requests:\n";
    Printf.printf "    - Fully              within next %d days: %d\n"
      Config.sched_day_count count_fully_within;
    Printf.printf "    - Fully or partially within next %d days: %d\n"
      Config.sched_day_count count_fully_within )

let display_overdue_task_segs (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let overdue_task_seg_places =
    Daypack_lib.Sched.Overdue.get_overdue_task_seg_places
      ~deadline:(Daypack_lib.Time.Current.cur_unix_time ())
      hd
    |> List.of_seq
  in
  let count = List.length overdue_task_seg_places in
  if count = 0 then print_endline "  - No overdue task segments"
  else (
    print_endline "  - Overdue task segments:";
    List.iter
      (fun (task_seg_id, place_start, place_end_exc) ->
         let open Daypack_lib.Task in
         let task_id = Daypack_lib.Task.Id.task_id_of_task_seg_id task_seg_id in
         let task_data =
           Daypack_lib.Sched.Task.Find.find_task_any_opt task_id hd |> Option.get
         in
         let start_str =
           Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
             ~display_in_time_zone:`Local place_start
         in
         let end_exc_str =
           Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
             ~display_in_time_zone:`Local place_end_exc
         in
         Printf.printf "    - | %s - %s | %s | %s\n" start_str end_exc_str
           (Id.string_of_task_seg_id task_seg_id)
           task_data.name)
      overdue_task_seg_places )

let display_todos (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let uncompleted_tasks =
    Daypack_lib.Sched.Task.To_seq.task_seq_uncompleted hd |> List.of_seq
  in
  let count = List.length uncompleted_tasks in
  if count = 0 then print_endline "  - No TODOs"
  else (
    print_endline "  - TODOs:";
    List.iter
      (fun (task_id, task_data) ->
         let open Daypack_lib.Task in
         Printf.printf "    - | %s | %s\n"
           (Id.string_of_task_id task_id)
           task_data.name)
      uncompleted_tasks )

let display (context : Context.t) : unit =
  display_current_time ();
  print_newline ();
  print_endline "Notifications:";
  display_overdue_task_segs context;
  print_newline ();
  display_pending_sched_reqs context;
  print_newline ();
  display_todos context;
  print_newline ()
