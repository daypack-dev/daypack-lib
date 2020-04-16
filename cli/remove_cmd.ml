open Cmdliner

let task_arg = Arg.(value & flag & info [ "task" ])

let task_inst_arg = Arg.(value & flag & info [ "task-inst" ])

let task_seg_arg = Arg.(value & flag & info [ "task-seg" ])

let pending_sched_req = Arg.(value & flag & info ["pending-sched-req"])

let sched_req_record = Arg.(value & flag & info ["sched-req-record"])

let run (remove_task : bool) (remove_task_inst : bool) (remove_task_seg : bool)
    (remove_pending_sched_req : bool) (remove_sched_req_record : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Notification.display context;
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    (if remove_task then
       let task_id =
         Dialog.ask_task_id ~indent_level:0
       in
       match Daypack_lib.Sched.Task.Find.find_task_any_with_status_opt task_id hd with
       | None ->
         Printf.printf "Failed to find task with ID: %s" (Daypack_lib.Task_ds.Id.string_of_task_id task_id)
       | Some (status, task_data) ->
         ()
    );
    ()

let cmd = (Term.(const run $ task_arg $ task_inst_arg $ task_seg_arg $ pending_sched_req $ sched_req_record), Term.info "remove")
