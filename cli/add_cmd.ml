open Cmdliner

let task_arg = Arg.(value & flag & info [ "task" ])

let run (add_task : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Header.display context;
    ( if add_task then
        let name =
          Dialog.ask ~indent_level:0 ~prompt:"Enter task name" ~f_until:None
            (fun s ->
               if s = "" then Error "Task name cannot be empty" else Ok s)
        in
        let task_type_choice =
          Dialog.ask_pick_choice ~indent_level:0 ~prompt:"Pick task type"
            [ ("one-off", `One_off); ("recurring", `Recurring) ]
        in
        match task_type_choice with
        | `One_off ->
          let task_data =
            let open Daypack_lib.Task in
            {
              splittable = false;
              parallelizable = false;
              task_type = Daypack_lib.Task.One_off;
              name;
            }
          in
          let task_inst_data_list =
            Daypack_lib.Task.[ { task_inst_type = Reminder } ]
          in
          let (task_id, _task_data), task_inst_list, ar =
            Daypack_lib.Sched_ver_history.In_place_head.Task.Add.add_task
              ~parent_user_id:0L task_data task_inst_data_list
              context.sched_ver_history
          in
          Dialog.report_action_record ar;
          let task_inst_id, _task_inst_data = List.hd task_inst_list in
          ( if
            Dialog.ask_yn ~indent_level:0
              ~prompt:"Lodge scheduling request for above task?"
            = `Yes
            then
              match
                Dialog.ask_sched_req_data_unit ~indent_level:0 ~task_inst_id ()
              with
              | Error msg -> print_endline msg
              | Ok sched_req_data_unit -> (
                  let sched_req_data = [ sched_req_data_unit ] in
                  match
                    Daypack_lib.Sched_ver_history.In_place_head.Sched_req.Add
                    .add_sched_req sched_req_data context.sched_ver_history
                  with
                  | Ok _, ar ->
                    print_endline "Lodging was successful";
                    Dialog.report_action_record ar
                  | Error _, ar ->
                    print_endline "Lodging was successful";
                    Dialog.report_action_record ar ) );
          ( if
            Dialog.ask_yn ~indent_level:0
              ~prompt:"Resolve all pending scheduling requests now?"
            = `Yes
            then
              let cur_time = Daypack_lib.Time.Current.cur_unix_time () in
              match
                Daypack_lib.Sched_ver_history.Maybe_append_to_head.sched
                  ~start:cur_time
                  ~end_exc:
                    (Daypack_lib.Time.Add.add_days_unix_time
                       ~days:Config.sched_day_count cur_time)
                  ~include_sched_reqs_starting_within_time_slot:true
                  ~include_sched_reqs_ending_within_time_slot:true
                  ~up_to_sched_req_id_inc:None context.sched_ver_history
              with
              | Ok (), ar ->
                print_endline "Scheduling was successful";
                Dialog.report_action_record ar
              | Error (), ar ->
                print_endline "Failed to schedule";
                Dialog.report_action_record ar );
          Printf.printf "Allocated task under ID : %s\n"
            (Daypack_lib.Task.Id.string_of_task_id task_id);
          Printf.printf "Allocated task inst under ID : %s\n"
            (Daypack_lib.Task.Id.string_of_task_inst_id task_inst_id)
        | `Recurring -> print_endline "Not implemented" );
    Context.save context |> Result.get_ok;
    ()

let cmd = (Term.(const run $ task_arg), Term.info "add")
