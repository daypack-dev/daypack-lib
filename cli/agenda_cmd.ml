open Cmdliner

let free_time_slots_arg = Arg.(value & flag & info [ "free" ])

let run (list_free_time_slots : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    if list_free_time_slots then
      let hd =
        Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
      in
      let start = Daypack_lib.Time.Current.cur_unix_time () in
      let end_exc =
        Daypack_lib.Time.Add.add_days_unix_time ~days:3 start
      in
      Daypack_lib.Sched.Agenda.Time_slot.get_free_time_slots ~start ~end_exc
        hd
      |> Seq.iter (fun (start, end_exc) ->
          let start_str =
            Daypack_lib.Time.Print.time_to_date_string
              ~display_in_time_zone:`Local start
          in
          let end_exc_str =
            Daypack_lib.Time.Print.time_to_date_string
              ~display_in_time_zone:`Local end_exc
          in
          Printf.printf "| %s - %s | %Ld min\n" start_str end_exc_str
            (Int64.sub end_exc start))

let cmd = (Term.(const run $ free_time_slots_arg), Term.info "agenda")
