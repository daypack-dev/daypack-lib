open Int64_utils

type time_pattern = unit

type arith_seq = {
  start : int64;
  end_exc : int64;
  diff : int64;
}

type user_id = int64

and task_id = user_id * int64

and task_inst_id = int64 * int64 * int64

and task_seg_id = int64 * int64 * int64 * int64 * int64 option

type task = task_id * task_data

and task_data = {
  splittable : bool;
  parallelizable : bool;
  task_type : task_type;
}

and task_type =
  | One_off
  | Recurring of recur

and recur =
  | Arithemtic_seq of arith_seq * recur_data
  | Time_pattern_match of time_pattern * recur_data

and sched_req_template = task_seg_size Sched_req_data_skeleton.t

and recur_data = {
  task_inst_data : task_inst_data;
  sched_req_templates : sched_req_template list;
}

and task_inst = task_inst_id * task_inst_data

and task_inst_data = { task_inst_type : task_inst_type }

and task_inst_type =
  | Reminder
  | Reminder_quota_counting of { quota : int64 }
  | Passing

and task_seg = task_seg_id * task_seg_size

and task_seg_alloc_req = task_inst_id * task_seg_size

and task_seg_size = int64

and task_seg_place = task_seg_id * int64 * int64

let task_seg_id_w_first_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) :
  task_seg_id =
  (id1, id2, id3, id4, match id5 with None -> Some 0L | Some x -> Some x)

let init_task_seg_sub_id ((id, len) : task_seg) : task_seg =
  (task_seg_id_w_first_sub_id id, len)

let succ_task_seg_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) =
  (id1, id2, id3, id4, Option.map (fun x -> x +^ 1L) id5)

let task_id_to_string ((id1, id2) : task_id) = Printf.sprintf "%Ld_%Ld" id1 id2

let task_inst_id_to_string ((id1, id2, id3) : task_inst_id) =
  Printf.sprintf "%Ld_%Ld_%Ld" id1 id2 id3

let task_seg_id_to_string ((id1, id2, id3, id4, id5) : task_seg_id) =
  Printf.sprintf "%Ld_%Ld_%Ld_%Ld_%s" id1 id2 id3 id4
    (match id5 with None -> "X" | Some x -> Int64.to_string x)

let task_seg_alloc_req_sum_length reqs =
  List.fold_left (fun acc (_, size) -> acc +^ size) 0L reqs

module Print = struct
  let debug_print_task ?(indent_level = 0) (id, data) =
    Debug_print.printf ~indent_level "task id : %s\n" (task_id_to_string id);
    Debug_print.printf ~indent_level:(indent_level + 1) "splittable : %b\n"
      data.splittable;
    Debug_print.printf ~indent_level:(indent_level + 1) "parallelizable : %b\n"
      data.parallelizable;
    match data.task_type with
    | One_off ->
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task type : one-off\n"
    | Recurring recur -> (
        Debug_print.printf ~indent_level:(indent_level + 1)
          "task type : recurring\n";
        match recur with
        | Arithemtic_seq
            ( { start; end_exc; diff },
              { task_inst_data = _; sched_req_templates } ) ->
          Debug_print.printf ~indent_level:(indent_level + 2)
            "recur type : arithmetic sequence\n";
          Debug_print.printf ~indent_level:(indent_level + 2) "start : %Ld\n"
            start;
          Debug_print.printf ~indent_level:(indent_level + 2)
            "end_exc : %Ld\n" end_exc;
          Debug_print.printf ~indent_level:(indent_level + 2) "diff : %Ld\n"
            diff;
          Debug_print.printf ~indent_level:(indent_level + 2)
            "sched req templates :\n";
          List.iter
            (fun sched_req_template ->
               match sched_req_template with
               | Sched_req_data_skeleton.Fixed { task_seg_related_data; start }
                 ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "fixed : size : %Ld, start : %Ld\n" task_seg_related_data
                   start
               | Shift (l, time_slots) ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "shift :\n";
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "sizes :\n";
                 List.iter
                   (fun size ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "%Ld\n" size)
                   l;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "time slots :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   time_slots
               | Split_and_shift (size, time_slots) ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "split and shift :";
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "size : %Ld\n" size;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "time slots :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   time_slots
               | Split_even { task_seg_related_data; time_slots; buckets } ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "split even :";
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "size : %Ld\n" task_seg_related_data;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "time slots :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   time_slots;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "buckets :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   buckets
               | Time_share (sizes, time_slots) ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "time share :";
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "sizes :";
                 List.iter
                   (fun size ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "%Ld\n" size)
                   sizes;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "time slots :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   time_slots
               | Push_to (dir, size, time_slots) ->
                 Debug_print.printf ~indent_level:(indent_level + 3)
                   "push to :";
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "direction : %s\n"
                   (match dir with `Front -> "front" | `Back -> "right");
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "size : %Ld\n" size;
                 Debug_print.printf ~indent_level:(indent_level + 4)
                   "time slots :";
                 List.iter
                   (fun (start, end_exc) ->
                      Debug_print.printf ~indent_level:(indent_level + 5)
                        "[%Ld, %Ld)\n" start end_exc)
                   time_slots)
            sched_req_templates
        | Time_pattern_match _ -> failwith "Unimplemented" )

  let debug_print_task_inst ?(indent_level = 0) (id, data) =
    Debug_print.printf ~indent_level "task inst id : %s\n"
      (task_inst_id_to_string id);
    Debug_print.printf ~indent_level:(indent_level + 1) "type : %s\n"
      ( match data.task_inst_type with
        | Reminder -> "reminder"
        | Reminder_quota_counting { quota } ->
          Printf.sprintf "reminder with quota : %Ld" quota
        | Passing -> "passing" )

  let debug_print_task_seg ?(indent_level = 0) (id, size) =
    Debug_print.printf ~indent_level "task seg id : %s\n"
      (task_seg_id_to_string id);
    Debug_print.printf ~indent_level:(indent_level + 1) "size : %Ld\n" size
end
