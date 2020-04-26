open Cmdliner

let run () : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context -> (
      let cur_time = Daypack_lib.Time.Current.cur_unix_time () in
      match
        Daypack_lib.Sched_ver_history.Maybe_append_to_head.sched ~start:cur_time
          ~end_exc:
            (Daypack_lib.Time.Add.add_days_unix_time
               ~days:Config.sched_day_count cur_time)
          ~include_sched_reqs_starting_within_time_slot:true
          ~include_sched_reqs_ending_within_time_slot:true
          ~up_to_sched_req_id_inc:None context.sched_ver_history
      with
      | Ok (), ar ->
        print_endline "Scheduling was successful";
        Context.save context |> Result.get_ok;
        Dialog.report_action_record ar
      | Error (), ar ->
        print_endline "Failed to schedule";
        Dialog.report_action_record ar )

let cmd = (Term.(const run $ const ()), Term.info "sched")
