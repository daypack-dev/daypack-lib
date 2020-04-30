type yn =
  [ `Yes
  | `No
  ]

let loop_until_success (type a) (f : unit -> (a, string) result) : a =
  let rec aux () =
    match f () with
    | Ok x -> x
    | Error msg ->
      Printf.printf "Error: %s\n" msg;
      aux ()
  in
  aux ()

let indent_str ~indent_level = String.make (indent_level * 2) ' '

let ask (type a) ?(skip_colon : bool = false)
    ~(f_until : (a -> (a, string) result) option) ~indent_level
    ~(prompt : string) (f : string -> (a, string) result) : a =
  loop_until_success (fun () ->
      let indent_str = String.make (indent_level * 2) ' ' in
      if skip_colon then Printf.printf "%s%s " indent_str prompt
      else Printf.printf "%s%s: " indent_str prompt;
      let s = read_line () in
      match f s with
      | Error msg -> Error msg
      | Ok x -> (
          match f_until with None -> Ok x | Some f_until -> f_until x ))

let ask_yn ~indent_level ~prompt : yn =
  ask ~indent_level ~prompt:(prompt ^ " (yes/no)") ~f_until:None (fun s ->
      let matching_choices =
        Daypack_lib.Misc_utils.prefix_string_match
          [ ("yes", `Yes); ("no", `No) ]
          s
      in
      match matching_choices with
      | [] -> Error "Input doesn't match yes or no"
      | [ (_k, v) ] -> Ok v
      | _ -> Error "Input is too ambiguous")

let ask_multiple (type a) ~indent_level ~(prompt : string)
    (f : string -> (a, string) result) : a list =
  let rec aux acc =
    print_string (indent_str ~indent_level ^ "- ");
    match read_line () with
    | "" -> List.rev acc
    | s -> (
        match f s with
        | Ok x -> aux (x :: acc)
        | Error msg ->
          Printf.printf "Error: %s\n" msg;
          aux acc )
  in
  Printf.printf "%s (empty to end loop) :\n" prompt;
  aux []

let ask_id (type a) ~indent_level ~(name : string) ~f_until
    (f : string -> (a, unit) result) : a =
  ask ~indent_level ~prompt:("Please enter " ^ name) ~f_until (fun s ->
      match f s with
      | Ok x -> Ok x
      | Error () -> Error (Printf.sprintf "Failed to parse %s string" name))

let ask_ids (type a) ~indent_level ~(name : string)
    (f : string -> (a, unit) result) : a list =
  ask_multiple ~indent_level ~prompt:("Please enter " ^ name) (fun s ->
      match f s with
      | Ok x -> Ok x
      | Error () -> Error (Printf.sprintf "Failed to parse %s string" name))

let ask_task_id ~indent_level
    ~(exists_in_sched : Daypack_lib.Sched.sched option) :
  Daypack_lib.Task.task_id =
  let f_until =
    match exists_in_sched with
    | None -> None
    | Some sched ->
      Some
        (fun id ->
           match Daypack_lib.Sched.Task.Find.find_task_any_opt id sched with
           | None ->
             Error
               (Printf.sprintf "Failed to find task with ID: %s"
                  (Daypack_lib.Task.Id.string_of_task_id id))
           | Some _ -> Ok id)
  in
  ask_id ~indent_level ~name:"task ID" ~f_until
    Daypack_lib.Task.Id.task_id_of_string

let ask_task_inst_id ~indent_level
    ~(exists_in_sched : Daypack_lib.Sched.sched option) :
  Daypack_lib.Task.task_inst_id =
  let f_until =
    match exists_in_sched with
    | None -> None
    | Some sched ->
      Some
        (fun id ->
           match
             Daypack_lib.Sched.Task_inst.Find.find_task_inst_any_opt id sched
           with
           | None ->
             Error
               (Printf.sprintf "Failed to find task instance with ID: %s"
                  (Daypack_lib.Task.Id.string_of_task_inst_id id))
           | Some _ -> Ok id)
  in
  ask_id ~indent_level ~name:"task inst ID" ~f_until
    Daypack_lib.Task.Id.task_inst_id_of_string

let ask_task_inst_ids ~indent_level : Daypack_lib.Task.task_inst_id list =
  ask_ids ~indent_level ~name:"task inst IDs"
    Daypack_lib.Task.Id.task_inst_id_of_string

let ask_task_seg_id ~indent_level
    ~(exists_in_sched : Daypack_lib.Sched.sched option) :
  Daypack_lib.Task.task_seg_id =
  let f_until =
    match exists_in_sched with
    | None -> None
    | Some sched ->
      Some
        (fun id ->
           match
             Daypack_lib.Sched.Task_seg.Find.find_task_seg_any_opt id sched
           with
           | None ->
             Error
               (Printf.sprintf "Failed to find task segment with ID: %s"
                  (Daypack_lib.Task.Id.string_of_task_seg_id id))
           | Some _ -> Ok id)
  in
  ask_id ~indent_level ~name:"task seg ID" ~f_until
    Daypack_lib.Task.Id.task_seg_id_of_string

let ask_pending_sched_req_id ~indent_level
    ~(exists_in_sched : Daypack_lib.Sched.sched option) :
  Daypack_lib.Sched_req.sched_req_id =
  let f_until =
    match exists_in_sched with
    | None -> None
    | Some sched ->
      Some
        (fun id ->
           match
             Daypack_lib.Sched.Sched_req.Find.Pending.find_pending_sched_req id
               sched
           with
           | None ->
             Error
               (Printf.sprintf
                  "Failed to find pending schedule request with ID: %Ld" id)
           | Some _ -> Ok id)
  in
  ask_id ~indent_level ~name:"sched request ID" ~f_until (fun s ->
      try
        let x = Int64.of_string s in
        if x >= 0L then Ok x else Error ()
      with Failure _ -> Error ())

let ask_sched_req_record_id ~indent_level
    ~(exists_in_sched : Daypack_lib.Sched.sched option) :
  Daypack_lib.Sched_req.sched_req_id =
  let f_until =
    match exists_in_sched with
    | None -> None
    | Some sched ->
      Some
        (fun id ->
           match
             Daypack_lib.Sched.Sched_req.Find.Record.find_sched_req_record id
               sched
           with
           | None ->
             Error
               (Printf.sprintf
                  "Failed to find schedule request record with ID: %Ld" id)
           | Some _ -> Ok id)
  in
  ask_id ~indent_level ~name:"sched request ID" ~f_until (fun s ->
      try
        let x = Int64.of_string s in
        if x >= 0L then Ok x else Error ()
      with Failure _ -> Error ())

let ask_pick_choice (type a) ~indent_level ~(prompt : string)
    (choices : (string * a) list) : a =
  Printf.printf "%s%s :\n" (indent_str ~indent_level) prompt;
  List.iter (fun (s, _) -> Printf.printf "  %s\n" s) choices;
  ask ~indent_level
    ~prompt:
      "Please enter choice (case insensitive full/prefix string of the choice)"
    ~f_until:None (fun s ->
        let matching_choices =
          Daypack_lib.Misc_utils.prefix_string_match choices s
        in
        match matching_choices with
        | [] -> Error "Input does not match any choice"
        | [ (_k, v) ] -> Ok v
        | _ -> Error "Input is too ambiguous and matches multiple choices")

let ask_uint ~indent_level ~(prompt : string) : int =
  ask ~indent_level ~prompt ~f_until:None (fun s ->
      try
        let x = int_of_string s in
        if x >= 0 then Ok x else Error "Input is negative"
      with Failure msg -> Error msg)

let ask_uint64 ~indent_level ~(prompt : string) : int64 =
  ask ~indent_level ~prompt ~f_until:None (fun s ->
      try
        let x = Int64.of_string s in
        if x >= 0L then Ok x else Error "Input is negative"
      with Failure msg -> Error msg)

let ask_uint64_multi ~indent_level ~(prompt : string) : int64 list =
  ask_multiple ~indent_level ~prompt (fun s ->
      try
        let x = Int64.of_string s in
        if x >= 0L then Ok x else Error "Input is negative"
      with Failure msg -> Error msg)

let process_time_string (s : string) : (int64, string) result =
  match Daypack_lib.Time_expr.Interpret_string.time_points_expr_of_string s with
  | Error msg -> Error msg
  | Ok expr -> (
      match
        Daypack_lib.Time_expr.Time_points_expr.next_match_unix_time
          (Years_ahead_start_unix_time
             {
               search_in_time_zone = `Local;
               start = Daypack_lib.Time.Current.cur_unix_time ();
               search_years_ahead = Config.time_pattern_search_years_ahead;
             })
          expr
      with
      | Error msg -> Error msg
      | Ok None -> Error "Failed to find a matching time"
      | Ok (Some x) -> Ok x )

let process_time_slots_string (s : string) :
  ((int64 * int64) list, string) result =
  let cur_time = Daypack_lib.Time.Current.cur_unix_time () in
  match Daypack_lib.Time_expr.Interpret_string.time_slots_expr_of_string s with
  | Error msg -> Error msg
  | Ok e -> (
      match
        Daypack_lib.Time_expr.Time_slots_expr.matching_time_slots
          (Years_ahead_start_unix_time
             {
               search_in_time_zone = `Local;
               start = cur_time;
               search_years_ahead = Config.time_pattern_search_years_ahead;
             })
          e
      with
      | Error msg -> Error msg
      | Ok s -> (
          match List.of_seq s with
          | [] -> Error "Failed to find matching time slots"
          | l -> Ok l ) )

let process_time_slot_string (s : string) : (int64 * int64, string) result =
  match process_time_slots_string s with
  | Error msg -> Error msg
  | Ok [] -> Error "Failed to find matching time slots"
  | Ok [ x ] -> Ok x
  | Ok _ -> Error "Too many time slots"

let ask_time ~indent_level ~(prompt : string) : int64 =
  ask ~indent_level
    ~prompt:
      (prompt ^ " (see `daypc grammar --time-point-expr` for grammar guide)")
    ~f_until:None process_time_string

let ask_time_slot ~indent_level ~(prompt : string) : int64 * int64 =
  ask ~indent_level
    ~prompt:
      (prompt ^ " (see `daypc grammar --time-slots-expr` for grammar guide)")
    ~f_until:None process_time_slot_string

let ask_time_slots ~indent_level ~(prompt : string) : (int64 * int64) list =
  ask_multiple ~indent_level
    ~prompt:
      (prompt ^ " (see `daypc grammar --time-slots-expr` for grammar guide)")
    process_time_slots_string
  |> List.to_seq
  |> Seq.flat_map List.to_seq
  |> Daypack_lib.Time_slots.Normalize.normalize
  |> List.of_seq

let process_task_inst_alloc_req_string (s : string) :
  (Daypack_lib.Task.task_seg_alloc_req, string) result =
  try
    Scanf.sscanf s "%[^,],%Ld" (fun maybe_task_inst_id task_seg_size ->
        match Daypack_lib.Task.Id.task_inst_id_of_string maybe_task_inst_id with
        | Error () -> Error "Failed to parse task inst id string"
        | Ok task_inst_id -> Ok (task_inst_id, task_seg_size))
  with _ -> Error "Failed to parse task inst alloc req"

let ask_task_inst_alloc_req ~indent_level ~task_inst_id :
  Daypack_lib.Task.task_seg_alloc_req =
  match task_inst_id with
  | None ->
    ask ~indent_level
      ~prompt:
        "Please enter task inst alloc req (format = \
         task_inst_id,task_seg_size)"
      ~f_until:None process_task_inst_alloc_req_string
  | Some task_inst_id ->
    let task_seg_size =
      ask_uint64 ~indent_level
        ~prompt:"Please enter task seg size to allocate"
    in
    (task_inst_id, task_seg_size)

let ask_task_inst_alloc_reqs ~indent_level ~task_inst_id :
  Daypack_lib.Task.task_seg_alloc_req list =
  match task_inst_id with
  | None ->
    ask_multiple ~indent_level
      ~prompt:
        "Please enter task inst alloc reqs (format = \
         task_inst_id,task_seg_size)"
      process_task_inst_alloc_req_string
  | Some task_inst_id ->
    ask_uint64_multi ~indent_level
      ~prompt:"Please enter task seg sizes to allocate"
    |> List.map (fun task_seg_size -> (task_inst_id, task_seg_size))

let ask_sched_req_data_unit ~indent_level
    ?(task_inst_id : Daypack_lib.Task.task_inst_id option) () :
  (Daypack_lib.Sched_req.sched_req_data_unit, string) result =
  let sched_req_choice =
    ask_pick_choice ~indent_level ~prompt:"Pick scheduling request type"
      [
        ("fixed", `Fixed);
        ("shift", `Shift);
        ("split_and_shift", `Split_and_shift);
        ("split_even", `Split_even);
        ("time_share", `Time_share);
        ("push_toward", `Push_toward);
      ]
  in
  match sched_req_choice with
  | `Fixed ->
    let task_inst_id =
      match task_inst_id with
      | None -> ask_task_inst_id ~indent_level ~exists_in_sched:None
      | Some x -> x
    in
    let start = ask_time ~indent_level ~prompt:"Enter start time" in
    let duration =
      ask ~indent_level ~prompt:"Enter duration" ~f_until:None
        Daypack_lib.Duration.Interpret_string.of_string
      |> Daypack_lib.Duration.to_seconds
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Fixed
         { task_seg_related_data = (task_inst_id, duration); start })
  | `Shift ->
    let task_inst_alloc_reqs =
      ask_task_inst_alloc_reqs ~indent_level ~task_inst_id
    in
    let time_slots =
      ask_time_slots ~indent_level ~prompt:"Please enter usable time slots"
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Shift
         {
           task_seg_related_data_list = task_inst_alloc_reqs;
           time_slots;
           incre = 1L;
         })
  | `Split_and_shift ->
    let task_inst_alloc_req =
      ask_task_inst_alloc_req ~indent_level ~task_inst_id
    in
    let time_slots =
      ask_time_slots ~indent_level ~prompt:"Please enter usable time slots"
    in
    let min_seg_size =
      ask_uint64 ~indent_level
        ~prompt:"Please enter the minimum size of each split"
    in
    let max_seg_size =
      match
        ask_yn ~indent_level
          ~prompt:"Do you want to specify a maximum size for each split?"
      with
      | `Yes ->
        Some
          (ask_uint64 ~indent_level
             ~prompt:"Please enter the maximum size of each split")
      | `No -> None
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Split_and_shift
         {
           task_seg_related_data = task_inst_alloc_req;
           time_slots;
           incre = 1L;
           split_count = Max_split 10L;
           min_seg_size;
           max_seg_size;
         })
  | `Split_even ->
    let task_inst_alloc_req =
      ask_task_inst_alloc_req ~indent_level ~task_inst_id
    in
    let time_slots =
      ask_time_slots ~indent_level ~prompt:"Please enter usable time slots"
    in
    let buckets =
      ask_time_slots ~indent_level ~prompt:"Please enter buckets/boundaries"
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Split_even
         {
           task_seg_related_data = task_inst_alloc_req;
           time_slots;
           buckets;
           incre = 1L;
         })
  | `Time_share ->
    let task_inst_alloc_reqs =
      ask_task_inst_alloc_reqs ~indent_level ~task_inst_id
    in
    let time_slots =
      ask_time_slots ~indent_level ~prompt:"Please enter usable time slots"
    in
    let interval_size =
      ask_uint64 ~indent_level ~prompt:"Please enter interval size"
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Time_share
         {
           task_seg_related_data_list = task_inst_alloc_reqs;
           time_slots;
           interval_size;
         })
  | `Push_toward ->
    let task_inst_alloc_req =
      ask_task_inst_alloc_req ~indent_level ~task_inst_id
    in
    let target =
      ask_time ~indent_level
        ~prompt:"Please enter the target time to push toward"
    in
    let time_slots =
      ask_time_slots ~indent_level ~prompt:"Please enter usable time slots"
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Push_toward
         {
           task_seg_related_data = task_inst_alloc_req;
           target;
           time_slots;
           incre = 1L;
         })

let report_action_record (r : Daypack_lib.Sched_ver_history.action_record) :
  unit =
  match r with
  | Updated_head id -> Printf.printf "Updated head schedule #%d in history\n" id
  | Added_new_head id ->
    Printf.printf "Added new head schedule #%d in history\n" id
  | Did_nothing -> print_endline "No changes were made to history"
