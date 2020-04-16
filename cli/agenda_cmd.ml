open Cmdliner

let free_time_slots_arg = Arg.(value & flag & info [ "free" ])

let run (list_free_time_slots : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Notification.display context;
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    let start = Daypack_lib.Time.Current.cur_unix_time () in
    let end_exc = Daypack_lib.Time.Add.add_days_unix_time ~days:Config.agenda_free_time_slot_search_day_count start in
    if list_free_time_slots then (
      let l =
        Daypack_lib.Sched.Agenda.Time_slot.get_free_time_slots ~start ~end_exc
          hd
        |> List.of_seq
      in
      Printf.printf "Free time slots in next %d days: %d\n" Config.agenda_free_time_slot_search_day_count (List.length l);
      List.iter
        (fun (start, end_exc) ->
           let start_str =
             Daypack_lib.Time.To_string.date_time_string_of_time
               ~display_in_time_zone:`Local start
           in
           let end_exc_str =
             Daypack_lib.Time.To_string.date_time_string_of_time
               ~display_in_time_zone:`Local end_exc
           in
           Printf.printf "| %s - %s | %s\n" start_str end_exc_str
             (Daypack_lib.Duration.To_string.human_readable_string_of_duration
                (Int64.sub end_exc start)))
        l )
    else
      let places_within_period =
        Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start
          ~end_exc ~include_task_seg_place_partially_within_time_period:true
          hd
        |> OSeq.take Config.agenda_display_task_seg_place_count
        |> List.of_seq
      in
      let places =
        if
          List.length places_within_period
          < Config.agenda_display_task_seg_place_count
        then
          Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start
            ~include_task_seg_place_partially_within_time_period:true hd
          |> OSeq.take Config.agenda_display_task_seg_place_count
          |> List.of_seq
        else places_within_period
      in
      Printf.printf "Task segment placements displayed: %d\n" (List.length places);
      List.iter
        (fun (task_seg_id, place_start, place_end_exc) ->
           let start_str =
             Daypack_lib.Time.To_string.date_time_string_of_time
               ~display_in_time_zone:`Local place_start
           in
           let end_exc_str =
             Daypack_lib.Time.To_string.date_time_string_of_time
               ~display_in_time_zone:`Local place_end_exc
           in
           let task_id =
             Daypack_lib.Task_ds.Id.task_id_of_task_seg_id task_seg_id
           in
           let task =
             Daypack_lib.Sched.Task.Find.find_task_any_opt task_id hd
             |> Option.get
           in
           Printf.printf "| %s - %s | %s | %s\n" start_str end_exc_str
             (Daypack_lib.Task_ds.Id.string_of_task_seg_id task_seg_id)
             task.name)
        places

let cmd = (Term.(const run $ free_time_slots_arg), Term.info "agenda")
