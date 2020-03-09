open Cmdliner

let task_arg =
  Arg.(value & flag & info ["task"])

let task_inst_arg =
  Arg.(value & flag & info ["task-inst"])

let task_seg_arg =
  Arg.(value & flag & info ["task-seg"])

let sched_req_arg =
  Arg.(value & flag & info ["sched-req"])

let run (list_task : bool) (list_task_inst : bool) (list_task_seg : bool) (list_sched_req : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    let hd = Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history in
    if list_task then (
        Seq.iter
          (fun task ->
             Daypack_lib.Task_ds.Print.debug_print_task task;
          )
          (Daypack_lib.Sched.Task.To_seq.task_seq_all hd)
      );
    if list_task_inst then (
        Seq.iter
          (fun task_inst ->
             Daypack_lib.Task_ds.Print.debug_print_task_inst task_inst;
          )
          (Daypack_lib.Sched.Task_inst.To_seq.task_inst_seq_all hd)
      );
    if list_task_seg then (
        Seq.iter
          (fun task_seg ->
             Daypack_lib.Task_ds.Print.debug_print_task_seg task_seg;
          )
          (Daypack_lib.Sched.Task_seg.To_seq.task_seg_seq_all hd)
      );
    if list_sched_req then (
      Seq.iter
        (fun sched_req ->
           Daypack_lib.Sched_req_ds.Print.debug_print_sched_req sched_req;
        )
        (Daypack_lib.Sched.Sched_req.To_seq.pending_sched_req_seq hd)
    )

let cmd =
  Term.(const run $ task_arg $ task_inst_arg $ task_seg_arg $ sched_req_arg),
  Term.info "list"
