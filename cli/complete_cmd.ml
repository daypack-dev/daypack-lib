open Cmdliner

let task_arg = Arg.(value & flag & info [ "task" ])

let task_inst_arg = Arg.(value & flag & info [ "task-inst" ])

let task_seg_arg = Arg.(value & flag & info [ "task-seg" ])

let run (complete_task : bool) (complete_task_inst : bool)
    (complete_task_seg : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Header.display context;
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    ( if complete_task then
        let task_id =
          Dialog.ask_task_id ~indent_level:0 ~exists_in_sched:(Some hd)
        in
        match
          Daypack_lib.Sched.Task.Find.find_task_any_with_status_opt task_id hd
          |> Option.get
        with
        | _, `Completed -> print_endline "Task is marked as completed already"
        | _, `Discarded -> print_endline "Task is marked as discarded"
        | data, `Uncompleted ->
          if
            Dialog.ask_yn ~indent_level:0
              ~prompt:
                (Printf.sprintf "Really mark task \"%s\" as completed?"
                   data.name)
            = `Yes
          then
            Daypack_lib.Sched_ver_history.In_place_head.Task.Move
            .move_task_to_completed task_id context.sched_ver_history
            |> Dialog.report_action_record );
    ( if complete_task_inst then
        let task_inst_id =
          Dialog.ask_task_inst_id ~indent_level:0 ~exists_in_sched:(Some hd)
        in
        match
          Daypack_lib.Sched.Task_inst.Find.find_task_inst_any_with_status_opt
            task_inst_id hd
          |> Option.get
        with
        | _, `Completed ->
          print_endline "Task instance is marked as completed already"
        | _, `Discarded -> print_endline "Task instance is marked as discarded"
        | _, `Uncompleted ->
          let task_id =
            Daypack_lib.Task.Id.task_id_of_task_inst_id task_inst_id
          in
          let task_data =
            Daypack_lib.Sched.Task.Find.find_task_any_opt task_id hd
            |> Option.get
          in
          if
            Dialog.ask_yn ~indent_level:0
              ~prompt:
                (Printf.sprintf
                   "Really mark instance under task \"%s\" as completed?"
                   task_data.name)
            = `Yes
          then
            Daypack_lib.Sched_ver_history.In_place_head.Task_inst.Move
            .move_task_inst_to_completed task_inst_id
              context.sched_ver_history
            |> Dialog.report_action_record );
    ( if complete_task_seg then
        let task_seg_id =
          Dialog.ask_task_seg_id ~indent_level:0 ~exists_in_sched:(Some hd)
        in
        match
          Daypack_lib.Sched.Task_seg.Find.find_task_seg_any_with_status_opt
            task_seg_id hd
          |> Option.get
        with
        | _, `Completed ->
          print_endline "Task segment is marked as completed already"
        | _, `Discarded -> print_endline "Task segment is marked as discarded"
        | _, `Uncompleted ->
          let task_id =
            Daypack_lib.Task.Id.task_id_of_task_seg_id task_seg_id
          in
          let task_data =
            Daypack_lib.Sched.Task.Find.find_task_any_opt task_id hd
            |> Option.get
          in
          if
            Dialog.ask_yn ~indent_level:0
              ~prompt:
                (Printf.sprintf
                   "Really mark segment under task \"%s\" as completed?"
                   task_data.name)
            = `Yes
          then
            Daypack_lib.Sched_ver_history.In_place_head.Task_seg.Move
            .move_task_seg_to_completed task_seg_id context.sched_ver_history
            |> Dialog.report_action_record );
    Context.save context |> Result.get_ok;
    ()

let cmd =
  ( Term.(const run $ task_arg $ task_inst_arg $ task_seg_arg),
    Term.info "complete" )
