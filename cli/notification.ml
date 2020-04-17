let display_pending_sched_reqs (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let pending_sched_reqs =
    Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq hd
    |> List.of_seq
  in
  let count = List.length pending_sched_reqs in
  if count = 0 then
    print_endline "  - No pending scheduling requests"
  else
    Printf.printf "  - Pending scheduling requests: %d\n" count

let display_todos (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let uncompleted_tasks =
    Daypack_lib.Sched.Task.To_seq.task_seq_uncompleted hd |> List.of_seq
  in
  let count = List.length uncompleted_tasks in
  if count = 0 then
    print_endline "  - No TODOs"
  else (
    print_endline "  - TODOs:";
    List.iter
      (fun (task_id, task_data) ->
         let open Daypack_lib.Task_ds in
         Printf.printf "    - | %s | %s\n" (Id.string_of_task_id task_id) task_data.name
      )
      uncompleted_tasks
  )

let display (context : Context.t) : unit =
  print_endline "Notifications:";
  display_pending_sched_reqs context;
  display_todos context;
  print_newline ()
