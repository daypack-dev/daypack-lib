let loop_until_success (type a) (f : unit -> (a, string) result) : a =
  let rec aux () =
    match f () with
    | Ok x -> x
    | Error msg ->
      Printf.printf "Error : %s\n" msg;
      aux ()
  in
  aux ()

let ask (type a) ~(prompt : string) (f : string -> (a, string) result) : a =
  loop_until_success (fun () ->
      Printf.printf "%s : " prompt;
      let s = read_line () in
      f s)

let ask_id (type a) ~(name : string) (f : string -> (a, unit) result) : a =
  ask ~prompt:("Please enter " ^ name) (fun s ->
      match f s with
      | Ok x -> Ok x
      | Error () -> Error (Printf.sprintf "Failed to parse %s string" name))

let ask_task_id () : Daypack_lib.Task_ds.task_id =
  ask_id ~name:"task ID" Daypack_lib.Task_ds.string_to_task_id

let ask_task_inst_id () : Daypack_lib.Task_ds.task_inst_id =
  ask_id ~name:"task inst ID" Daypack_lib.Task_ds.string_to_task_inst_id

let ask_task_seg_id () : Daypack_lib.Task_ds.task_seg_id =
  ask_id ~name:"task seg ID" Daypack_lib.Task_ds.string_to_task_seg_id

let ask_pick_choice (type a) ~(prompt : string) (choices : (string * a) list) :
  a =
  Printf.printf "%s :\n" prompt;
  List.iter (fun (s, _) -> Printf.printf "  %s\n" s) choices;
  ask
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

let ask_int ~(prompt : string) : int =
  ask ~prompt (fun s ->
      try int_of_string s |> Result.ok with Failure msg -> Error msg)

let ask_int64 ~(prompt : string) : int64 =
  ask ~prompt (fun s ->
      try Int64.of_string s |> Result.ok with Failure msg -> Error msg)

let ask_time ~(prompt : string) : int64 =
  loop_until_success (fun () ->
      let pat =
        ask ~prompt Daypack_lib.Time_pattern.Interpret_string.of_string
      in
      match
        Daypack_lib.Time_pattern.next_match_int64
          ~search_years_ahead:Config.time_pattern_search_years_ahead
          ~start:(Daypack_lib.Time.cur_unix_time_min ())
          pat
      with
      | None -> Error "Failed to find matching start time"
      | Some time -> Ok time)

let ask_time_slot ~(prompt : string) : int64 * int64 =
  let start = ask_time ~prompt:(prompt ^ " (start)") in
  let end_exc = ask_time ~prompt:(prompt ^ " (end exc)") in
  (start, end_exc)

let ask_sched_req_data_unit () :
  (Daypack_lib.Sched_req_ds.sched_req_data_unit, string) result =
  let sched_req_choice =
    ask_pick_choice ~prompt:"Pick scheduling request type"
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
    let task_inst_id = ask_task_inst_id () in
    let start = ask_time ~prompt:"Enter start time" in
    let duration = ask_int64 ~prompt:"Enter duration (minutes)" in
    Ok
      (Daypack_lib.Sched_req_data_unit_skeleton.Fixed
         { task_seg_related_data = (task_inst_id, duration); start })
  | `Shift | `Split_and_shift | `Split_even | `Time_share | `Push_toward | _ ->
    failwith "Not implemented"
