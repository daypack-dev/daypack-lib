open Cmdliner

let free_time_slots_arg = Arg.(value & flag & info [ "free" ])

let show_all_arg = Arg.(value & flag & info [ "all"])

let display_place ~cur_time sched ((task_seg_id, place_start, place_end_exc) : Daypack_lib.Task_ds.task_seg_place) : unit =
  let start_str =
    Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
      ~display_in_time_zone:`Local place_start
  in
  let end_exc_str =
    Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
      ~display_in_time_zone:`Local place_end_exc
  in
  let time_from_start_str =
    Int64.sub place_start cur_time
    |> Daypack_lib.Duration.of_seconds
    |> Daypack_lib.Duration.To_string.human_readable_string_of_duration
  in
  let task_id =
    Daypack_lib.Task_ds.Id.task_id_of_task_seg_id task_seg_id
  in
  let task =
    Daypack_lib.Sched.Task.Find.find_task_any_opt task_id sched
    |> Option.get
  in
  Printf.printf "| %s - %s | %s | %s \n" start_str end_exc_str
    (Daypack_lib.Task_ds.Id.string_of_task_seg_id task_seg_id)
    task.name;
  Printf.printf "  - Time from start: %s\n" time_from_start_str

let display_places ~title ~show_all ~cur_time ~end_exc ~max_count (sched : Daypack_lib.Sched.sched) : unit =
  let places =
    Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start:cur_time
      ~end_exc ~include_task_seg_place_starting_within_time_slot:true
      sched
    |> (fun s ->
        if show_all then s else
          OSeq.take max_count s)
    |> List.of_seq
  in
  print_endline title;
  List.iter (display_place ~cur_time sched) places

let display_places_active_or_soon ~show_all ~cur_time sched : unit =
  let end_exc =
    Int64.mul (Int64.of_int Config.agenda_search_minute_count_soon) Daypack_lib.Time.minute_to_second_multiplier
    |> Int64.add cur_time
  in
  let title =
    Printf.sprintf
      "Active or starting within next %d minutes%s:" Config.agenda_search_minute_count_soon
      (if show_all then "" else Printf.sprintf "(displaying up to %d task segments)" Config.agenda_display_task_seg_place_soon_max_count);
  in
  display_places ~title ~show_all ~cur_time ~end_exc ~max_count:Config.agenda_display_task_seg_place_soon_max_count
    sched

let display_places_close ~show_all ~cur_time sched : unit =
  let end_exc =
    Int64.mul (Int64.of_int Config.agenda_search_minute_count_soon) Daypack_lib.Time.minute_to_second_multiplier
    |> Int64.add cur_time
  in
  let title =
    Printf.sprintf
      "Starting within next %d minutes%s:" Config.agenda_search_minute_count_soon
      (if show_all then "" else Printf.sprintf "(displaying up to %d task segments)" Config.agenda_display_task_seg_place_soon_max_count);
  in
  display_places ~title ~show_all ~cur_time ~end_exc ~max_count:Config.agenda_display_task_seg_place_soon_max_count
    sched

let run (list_free_time_slots : bool) (show_all : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Header.display context;
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    let cur_time = Daypack_lib.Time.Current.cur_unix_time () in
    let end_exc =
      Daypack_lib.Time.Add.add_days_unix_time
        ~days:Config.agenda_search_day_count cur_time
    in
    if list_free_time_slots then (
      let l =
        Daypack_lib.Sched.Agenda.Time_slot.get_free_time_slots ~start:cur_time ~end_exc
          hd
        |> List.of_seq
      in
      Printf.printf "Free time slots in next %d days: %d\n"
        Config.agenda_search_day_count (List.length l);
      List.iter
        (fun (start, end_exc) ->
           let start_str =
             Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_time
               ~display_in_time_zone:`Local start
           in
           let end_exc_str =
             Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_time
               ~display_in_time_zone:`Local end_exc
           in
           let duration_str =
             Int64.sub end_exc start
             |> Daypack_lib.Duration.of_seconds
             |> Daypack_lib.Duration.To_string.human_readable_string_of_duration
           in
           Printf.printf "| %s - %s | %s\n" start_str end_exc_str
             duration_str
        )
        l )
    else (
      display_places_active_or_soon ~show_all ~cur_time hd;
    )
    let places_within_period =
        Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start:cur_time
          ~end_exc ~include_task_seg_place_partially_within_time_slot:true hd
        |> OSeq.take Config.agenda_display_task_seg_place_max_count
        |> List.of_seq
      in
      let places =
        if
          List.length places_within_period
          < Config.agenda_display_task_seg_place_max_count
        then
          Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start:cur_time
            ~include_task_seg_place_partially_within_time_slot:true hd
          |> OSeq.take Config.agenda_display_task_seg_place_max_count
          |> List.of_seq
        else places_within_period
      in
      Printf.printf
        "Agenda of next %d days (displaying up to %d task segments):\n"
        Config.agenda_search_day_count
        Config.agenda_display_task_seg_place_max_count;
      List.iter
        (fun (task_seg_id, place_start, place_end_exc) ->
           let start_str =
             Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
               ~display_in_time_zone:`Local place_start
           in
           let end_exc_str =
             Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
               ~display_in_time_zone:`Local place_end_exc
           in
           let time_from_start_str =
             Int64.sub place_start cur_time
             |> Daypack_lib.Duration.of_seconds
             |> Daypack_lib.Duration.To_string.human_readable_string_of_duration
           in
           let task_id =
             Daypack_lib.Task_ds.Id.task_id_of_task_seg_id task_seg_id
           in
           let task =
             Daypack_lib.Sched.Task.Find.find_task_any_opt task_id hd
             |> Option.get
           in
           Printf.printf "| %s - %s | %s | %s \n" start_str end_exc_str
             (Daypack_lib.Task_ds.Id.string_of_task_seg_id task_seg_id)
             task.name;
           Printf.printf "  - Time from start: %s\n" time_from_start_str
        )
        places

let cmd = (Term.(const run $ free_time_slots_arg $ show_all_arg), Term.info "agenda")
