let loop_until_success (type a) (f : unit -> (a, string) result) : a =
  let rec aux () =
    match f () with
    | Ok x -> x
    | Error msg ->
      Printf.printf "Error : %s\n" msg;
      aux ()
  in
  aux ()

let indent_str ~indent = String.make (indent * 2) ' '

let ask (type a) ?(skip_colon : bool = false) ~indent ~(prompt : string)
    (f : string -> (a, string) result) : a =
  loop_until_success (fun () ->
      let indent_str = String.make (indent * 2) ' ' in
      if skip_colon then Printf.printf "%s%s " indent_str prompt
      else Printf.printf "%s%s : " indent_str prompt;
      let s = read_line () in
      f s)

let ask_multiple (type a) ~indent ~(prompt : string)
    (f : string -> (a, string) result) : a list =
  let rec aux acc =
    print_string (indent_str ~indent ^ "- ");
    match read_line () with
    | "" -> List.rev acc
    | s -> (
        match f s with
        | Ok x -> aux (x :: acc)
        | Error msg ->
          Printf.printf "Error : %s\n" msg;
          aux acc )
  in
  Printf.printf "%s (empty to end loop) :\n" prompt;
  aux []

let ask_id (type a) ~indent ~(name : string) (f : string -> (a, unit) result) :
  a =
  ask ~indent ~prompt:("Please enter " ^ name) (fun s ->
      match f s with
      | Ok x -> Ok x
      | Error () -> Error (Printf.sprintf "Failed to parse %s string" name))

let ask_ids (type a) ~indent ~(name : string) (f : string -> (a, unit) result) :
  a list =
  ask_multiple ~indent ~prompt:("Please enter " ^ name) (fun s ->
      match f s with
      | Ok x -> Ok x
      | Error () -> Error (Printf.sprintf "Failed to parse %s string" name))

let ask_task_id ~indent : Daypack_lib.Task_ds.task_id =
  ask_id ~indent ~name:"task ID" Daypack_lib.Task_ds.string_to_task_id

let ask_task_inst_id ~indent : Daypack_lib.Task_ds.task_inst_id =
  ask_id ~indent ~name:"task inst ID" Daypack_lib.Task_ds.string_to_task_inst_id

let ask_task_inst_ids ~indent : Daypack_lib.Task_ds.task_inst_id list =
  ask_ids ~indent ~name:"task inst IDs"
    Daypack_lib.Task_ds.string_to_task_inst_id

let ask_task_seg_id ~indent : Daypack_lib.Task_ds.task_seg_id =
  ask_id ~indent ~name:"task seg ID" Daypack_lib.Task_ds.string_to_task_seg_id

let ask_pick_choice (type a) ~indent ~(prompt : string)
    (choices : (string * a) list) : a =
  Printf.printf "%s%s :\n" (indent_str ~indent) prompt;
  List.iter (fun (s, _) -> Printf.printf "  %s\n" s) choices;
  ask ~indent:(indent + 1)
    ~prompt:
      "Please enter choice (case insensitive full/partial string of the choice)"
    (fun s ->
       let regexp = Str.regexp_case_fold s in
       let matching_choices =
         List.filter
           (fun (k, _v) ->
              try
                Str.search_forward regexp k 0 |> ignore;
                true
              with Not_found -> false)
           choices
       in
       match matching_choices with
       | [] -> Error "Input does not match any choice"
       | [ (_k, v) ] -> Ok v
       | _ -> Error "Input is too ambiguous and matches multiple choices")

let ask_int ~indent ~(prompt : string) : int =
  ask ~indent ~prompt (fun s ->
      try int_of_string s |> Result.ok with Failure msg -> Error msg)

let ask_int64 ~indent ~(prompt : string) : int64 =
  ask ~indent ~prompt (fun s ->
      try Int64.of_string s |> Result.ok with Failure msg -> Error msg)

let process_time_string (s : string) : (int64, string) result =
  match Daypack_lib.Time_pattern.Interpret_string.of_string s with
  | Error msg -> Error msg
  | Ok pat -> (
      match
        Daypack_lib.Time_pattern.next_match_int64
          ~search_years_ahead:Config.time_pattern_search_years_ahead
          ~start:(Daypack_lib.Time.cur_unix_time_min ())
          pat
      with
      | None -> Error "Failed to find matching time"
      | Some time -> Ok time )

let process_time_slot_string (s : string) : (int64 * int64, string) result =
  let cur_time = Daypack_lib.Time.cur_unix_time_min () in
  try
    Scanf.sscanf s "%[^,],%[^,]" (fun start_str end_exc_str ->
        match Daypack_lib.Time_pattern.Interpret_string.of_string start_str with
        | Error _ -> Error "Failed to process start time pattern string"
        | Ok start_pat -> (
            match
              Daypack_lib.Time_pattern.Interpret_string.of_string end_exc_str
            with
            | Error _ -> Error "Failed to process end exc time pattern string"
            | Ok end_exc_pat -> (
                match
                  Daypack_lib.Time_pattern.next_match_time_slot
                    ~search_years_ahead:Config.time_pattern_search_years_ahead
                    ~start:cur_time start_pat
                with
                | None -> Error "Failed to find match for start pattern"
                | Some (start, _) -> (
                    match
                      Daypack_lib.Time_pattern.next_match_time_slot
                        ~search_years_ahead:
                          Config.time_pattern_search_years_ahead ~start:cur_time
                        end_exc_pat
                    with
                    | None -> Error "Failed to find match for end exc pattern"
                    | Some (_, end_exc) -> Ok (start, end_exc) ) ) ))
  with _ -> (
      match Daypack_lib.Time_pattern.Interpret_string.of_string s with
      | Error msg -> Error msg
      | Ok pat -> (
          match
            Daypack_lib.Time_pattern.next_match_time_slot
              ~search_years_ahead:Config.time_pattern_search_years_ahead
              ~start:cur_time pat
          with
          | None -> Error "Failed to find match for pattern"
          | Some x -> Ok x ) )

let ask_time ~indent ~(prompt : string) : int64 =
  ask ~indent ~prompt process_time_string

let ask_time_slot ~indent ~(prompt : string) : int64 * int64 =
  ask ~indent
    ~prompt:(prompt ^ " (single or pair of time pattern)")
    process_time_slot_string

let ask_time_slots ~indent ~(prompt : string) : (int64 * int64) list =
  ask_multiple ~indent ~prompt:(prompt ^ " (format = start,end_exc) : ")
    (fun s ->
       try
         Scanf.sscanf s "%[^,],%[^,]" (fun start_string end_exc_string ->
             match process_time_string start_string with
             | Error msg -> Error msg
             | Ok start -> (
                 match process_time_string end_exc_string with
                 | Error msg -> Error msg
                 | Ok end_exc -> Ok (start, end_exc) ))
       with _ -> Error "Failed to parse time slot string")

let process_task_inst_alloc_req_string (s : string) :
  (Daypack_lib.Task_ds.task_seg_alloc_req, string) result =
  try
    Scanf.sscanf s "%[^,],%Ld" (fun maybe_task_inst_id task_seg_size ->
        match Daypack_lib.Task_ds.string_to_task_inst_id maybe_task_inst_id with
        | Error () -> Error "Failed to parse task inst id string"
        | Ok task_inst_id -> Ok (task_inst_id, task_seg_size))
  with _ -> Error "Failed to parse task inst alloc req"

let ask_task_inst_alloc_req ~indent : Daypack_lib.Task_ds.task_seg_alloc_req =
  ask ~indent
    ~prompt:
      "Please enter task inst alloc req (format = task_inst_id,task_seg_size)"
    process_task_inst_alloc_req_string

let ask_task_inst_alloc_reqs ~indent :
  Daypack_lib.Task_ds.task_seg_alloc_req list =
  ask_multiple ~indent
    ~prompt:
      "Please enter task inst alloc reqs (format = task_inst_id,task_seg_size)"
    process_task_inst_alloc_req_string

let ask_sched_req_data_unit ~indent
    ?(task_inst_id : Daypack_lib.Task_ds.task_inst_id option) () :
  (Daypack_lib.Sched_req_ds.sched_req_data_unit, string) result =
  let sched_req_choice =
    ask_pick_choice ~indent ~prompt:"Pick scheduling request type"
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
      match task_inst_id with None -> ask_task_inst_id ~indent | Some x -> x
    in
    let start = ask_time ~indent ~prompt:"Enter start time" in
    let duration = ask_int64 ~indent ~prompt:"Enter duration (minutes)" in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Fixed
         { task_seg_related_data = (task_inst_id, duration); start })
  | `Shift ->
    let task_inst_alloc_reqs = ask_task_inst_alloc_reqs ~indent in
    let time_slots =
      ask_time_slots ~indent ~prompt:"Please enter usable time slots"
    in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Shift
         {
           task_seg_related_data_list = task_inst_alloc_reqs;
           time_slots;
           incre = 1L;
         })
  | `Split_and_shift | `Split_even | `Time_share | `Push_toward | _ ->
    failwith "Not implemented"
