open Cmdliner

let run () : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    let cur_time =Daypack_lib.Time.Current.cur_unix_time () in
    match Daypack_lib.Sched_ver_history.Maybe_append_to_head.sched
      ~start:cur_time
      ~end_exc:(Daypack_lib.Time.Add.add_days_unix_time ~days:7 cur_time)
      ~include_sched_reqs_partially_within_time_period:true
      ~up_to_sched_req_id_inc:None
      context.sched_ver_history
    with
    | Ok () -> ()
    | Error () ->
      print_endline "Failed to schedule"

let cmd = (Term.(const run $ (const ())), Term.info "sched")
