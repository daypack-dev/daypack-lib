open Cmdliner

let task_arg = Arg.(value & flag & info [ "task" ])

let run (add_task : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    ( if add_task then
        let name =
          Dialog.ask ~indent_level:0 ~prompt:"Enter task name" (fun s ->
              if s = "" then Error "Task name cannot be empty" else Ok s)
        in
        let task_type_choice =
          Dialog.ask_pick_choice ~indent_level:0 ~prompt:"Pick task type"
            [ ("one-off", `One_off); ("recurring", `Recurring) ]
        in
        match task_type_choice with
        | `One_off -> (
            let task_data =
              let open Daypack_lib.Task_ds in
              {
                splittable = false;
                parallelizable = false;
                task_type = Daypack_lib.Task_ds.One_off;
                name;
              }
            in
            let task_inst_data_list =
              Daypack_lib.Task_ds.[ { task_inst_type = Reminder } ]
            in
            let (task_id, _task_data), task_inst_list =
              Daypack_lib.Sched_ver_history.In_place_head.Task.Add.add_task
                ~parent_user_id:0L task_data task_inst_data_list
                context.sched_ver_history
            in
            let task_inst_id, _task_inst_data = List.hd task_inst_list in
            match
              Dialog.ask_yn ~indent_level:0 ~prompt:"Lodge scheduling request?"
            with
            | `Yes -> (
                match
                  Dialog.ask_sched_req_data_unit ~indent_level:0 ~task_inst_id
                    ()
                with
                | Error msg -> print_endline msg
                | Ok sched_req_data_unit ->
                  let sched_req_data = [ sched_req_data_unit ] in
                  let _sched_req =
                    Daypack_lib.Sched_ver_history.In_place_head.Sched_req
                    .Enqueue
                    .enqueue_sched_req sched_req_data
                      context.sched_ver_history
                  in
                  Printf.printf "Allocated task under ID : %s\n"
                    (Daypack_lib.Task_ds.task_id_to_string task_id) )
            | `No -> () )
        | `Recurring -> () );
    Context.save context |> Result.get_ok;
    ()

let cmd = (Term.(const run $ task_arg), Term.info "add")
