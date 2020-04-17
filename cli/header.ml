let display_current_time () : unit =
  let cur_time_str =
    Daypack_lib.Time.Current.cur_unix_time ()
    |> Daypack_lib.Time.To_string.date_time_string_of_time
      ~display_in_time_zone:`Local
  in
  print_endline "Time right now:";
  Printf.printf "  - %s\n" cur_time_str

let display_pending_sched_reqs (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let pending_sched_reqs =
    Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq hd
    |> List.of_seq
  in
  let count = List.length pending_sched_reqs in
  if count = 0 then print_endline "  - No pending scheduling requests"
  else Printf.printf "  - Pending scheduling requests: %d\n" count

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
         let open Daypack_lib.Task_ds in
         let task_id =
           Daypack_lib.Task_ds.Id.task_id_of_task_seg_id task_seg_id
         in
         let task_data =
           Daypack_lib.Sched.Task.Find.find_task_uncompleted_opt task_id hd
           |> Option.get
         in
         let start_str =
           Daypack_lib.Time.To_string.date_time_string_of_time
             ~display_in_time_zone:`Local place_start
         in
         let end_exc_str =
           Daypack_lib.Time.To_string.date_time_string_of_time
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
         let open Daypack_lib.Task_ds in
         Printf.printf "    - | %s | %s\n"
           (Id.string_of_task_id task_id)
           task_data.name)
      uncompleted_tasks )

let display (context : Context.t) : unit =
  display_current_time ();
  print_newline ();
  print_endline "Notifications:";
  display_overdue_task_segs context;
  display_pending_sched_reqs context;
  display_todos context;
  print_newline ()
