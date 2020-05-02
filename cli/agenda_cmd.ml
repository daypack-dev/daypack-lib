open Cmdliner

let free_time_slots_arg = Arg.(value & flag & info [ "free" ])

let show_all_arg = Arg.(value & flag & info [ "all" ])

let display_place ~start sched
    ((task_seg_id, place_start, place_end_exc) :
       Daypack_lib.Task.task_seg_place) : unit =
  let start_str =
    Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
      ~display_in_time_zone:`Local place_start
  in
  let end_exc_str =
    Daypack_lib.Time.To_string.yyyymmdd_hhmm_string_of_unix_time
      ~display_in_time_zone:`Local place_end_exc
  in
  let time_from_start_str =
    Int64.sub place_start start
    |> Daypack_lib.Duration.of_seconds
    |> Daypack_lib.Duration.To_string.human_readable_string_of_duration
  in
  let task_id = Daypack_lib.Task.Id.task_id_of_task_seg_id task_seg_id in
  let task_seg_id_str = Daypack_lib.Task.Id.string_of_task_seg_id task_seg_id in
  let task =
    Daypack_lib.Sched.Task.Find.find_task_any_opt task_id sched |> Option.get
  in
  let time_str = Printf.sprintf "| %s - %s " start_str end_exc_str in
  let time_str_space = String.make (String.length time_str) ' ' in
  Printf.printf "%s| %s \n" time_str task.name;
  Printf.printf "%s| ID: %s\n" time_str_space task_seg_id_str;
  Printf.printf "%s| From start: %s\n" time_str_space time_from_start_str

let display_places ~title ~show_all ~start ~end_exc ~max_count
    (sched : Daypack_lib.Sched.sched) : unit =
  let places_all =
    Daypack_lib.Sched.Agenda.To_seq.task_seg_place_uncompleted ~start ~end_exc
      ~include_task_seg_place_starting_within_time_slot:true sched
  in
  let total_count = OSeq.length places_all in
  let title =
    title
    ^
    if show_all then ""
    else if total_count <= max_count || show_all then
      Printf.sprintf " (displaying %d/%d)" total_count total_count
    else Printf.sprintf " (displaying %d/%d)" max_count total_count
  in
  print_string title;
  print_newline ();
  let title_len = String.length title in
  for _ = 0 to title_len - 1 do
    print_char '='
  done;
  print_newline ();
  places_all
  |> (fun s -> if show_all then s else OSeq.take max_count s)
  |> Seq.iter (display_place ~start sched)

let display_places_active_or_soon ~show_all ~cur_time sched : unit =
  let end_exc =
    Int64.mul
      (Int64.of_int Config.agenda_search_minute_count_soon)
      Daypack_lib.Time.minute_to_second_multiplier
    |> Int64.add cur_time
  in
  let title =
    Printf.sprintf "Active or starting within next %d minutes"
      Config.agenda_search_minute_count_soon
  in
  display_places ~title ~show_all ~start:cur_time ~end_exc
    ~max_count:Config.agenda_display_task_seg_place_soon_max_count sched

let display_places_close ~show_all ~cur_time sched : unit =
  let start =
    Int64.mul
      (Int64.of_int Config.agenda_search_minute_count_soon)
      Daypack_lib.Time.minute_to_second_multiplier
    |> Int64.add cur_time
  in
  let end_exc =
    Int64.mul
      (Int64.of_int Config.agenda_search_day_count_close)
      Daypack_lib.Time.day_to_second_multiplier
    |> Int64.add cur_time
  in
  let title =
    Printf.sprintf "Next batch starting within next %d days"
      Config.agenda_search_day_count_close
  in
  display_places ~title ~show_all ~start ~end_exc
    ~max_count:Config.agenda_display_task_seg_place_close_max_count sched

let display_places_far ~show_all ~cur_time sched : unit =
  let start =
    Int64.mul
      (Int64.of_int Config.agenda_search_day_count_close)
      Daypack_lib.Time.day_to_second_multiplier
    |> Int64.add cur_time
  in
  let end_exc =
    Int64.mul
      (Int64.of_int Config.agenda_search_day_count_far)
      Daypack_lib.Time.day_to_second_multiplier
    |> Int64.add cur_time
  in
  let title =
    Printf.sprintf "Next batch starting within next %d days"
      Config.agenda_search_day_count_far
  in
  display_places ~title ~show_all ~start ~end_exc
    ~max_count:Config.agenda_display_task_seg_place_far_max_count sched

let run (list_free_time_slots : bool) (show_all : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    Header.display context;
    let hd =
      Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
    in
    let cur_time = Daypack_lib.Time.Current.cur_unix_time () in
    if list_free_time_slots then (
      let end_exc =
        Daypack_lib.Time.Add.add_days_unix_time
          ~days:Config.agenda_search_day_count_far cur_time
      in
      let l =
        Daypack_lib.Sched.Agenda.Time_slot.get_free_time_slots ~start:cur_time
          ~end_exc hd
        |> List.of_seq
      in
      Printf.printf "Free time slots in next %d days: %d\n"
        Config.agenda_search_day_count_far (List.length l);
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
             |> Daypack_lib.Duration.To_string
                .human_readable_string_of_duration
           in
           Printf.printf "| %s - %s | %s\n" start_str end_exc_str duration_str)
        l )
    else (
      display_places_active_or_soon ~show_all ~cur_time hd;
      print_newline ();
      display_places_close ~show_all ~cur_time hd;
      print_newline ();
      display_places_far ~show_all ~cur_time hd )

let cmd =
  (Term.(const run $ free_time_slots_arg $ show_all_arg), Term.info "agenda")
