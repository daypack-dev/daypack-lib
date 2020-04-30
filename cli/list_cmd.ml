open Cmdliner

type list_task_item_choice =
  | Uncompleted
  | Completed
  | Discarded
  | All

let task_uncompleted_arg = Arg.(value & flag & info [ "task" ])

let task_inst_uncompleted_arg = Arg.(value & flag & info [ "task-inst" ])

let task_seg_uncompleted_arg = Arg.(value & flag & info [ "task-seg" ])

let pending_sched_req_arg = Arg.(value & flag & info [ "pending-sched-req" ])

let sched_req_record_arg = Arg.(value & flag & info [ "sched-req-record" ])

let run (list_task_uncompleted : bool) (list_task_inst_uncompleted : bool)
    (list_task_seg_uncompleted : bool) (list_pending_sched_req : bool)
    (list_sched_req_record : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    if list_task_uncompleted then (
      Printf.printf "Tasks :\n";
      Seq.iter
        (fun task ->
           Daypack_lib.Task.Print.debug_print_task ~indent_level:1 task)
        (Daypack_lib.Sched.Task.To_seq.task_seq_all hd) );
    if list_task_inst_uncompleted then (
      Printf.printf "Task instances :\n";
      Seq.iter
        (fun task_inst ->
           Daypack_lib.Task.Print.debug_print_task_inst ~indent_level:1
             task_inst)
        (Daypack_lib.Sched.Task_inst.To_seq.task_inst_seq_all hd) );
    if list_task_seg_uncompleted then (
      Printf.printf "Task segments :\n";
      Seq.iter
        (fun task_seg ->
           Daypack_lib.Task.Print.debug_print_task_seg ~indent_level:1 task_seg)
        (Daypack_lib.Sched.Task_seg.To_seq.task_seg_seq_all hd) );
    if list_pending_sched_req then (
      Printf.printf "Pending scheduling requests :\n";
      Seq.iter
        (fun sched_req ->
           Daypack_lib.Sched_req.Print.debug_print_sched_req ~indent_level:1
             sched_req)
        (Daypack_lib.Sched.Sched_req.To_seq.Pending.pending_sched_req_seq hd)
    );
    if list_sched_req_record then (
      Printf.printf "Scheduling request records :\n";
      Seq.iter
        (fun sched_req ->
           Daypack_lib.Sched_req.Print.debug_print_sched_req_record
             ~indent_level:1 sched_req)
        (Daypack_lib.Sched.Sched_req.To_seq.Record.sched_req_record_seq hd) )

let cmd =
  ( (let open Term in
     const run
     $ task_uncompleted_arg
     $ task_inst_uncompleted_arg
     $ task_seg_uncompleted_arg
     $ pending_sched_req_arg
     $ sched_req_record_arg),
    Term.info "list" )
