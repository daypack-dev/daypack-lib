open Int64_utils

type time_pattern = unit

type arith_seq = {
  start : int64;
  end_exc : int64;
  diff : int64;
}

type user_id = int64

and task_id = user_id * int64

and task_inst_id = user_id * int64 * int64

and task_seg_id = user_id * int64 * int64 * int64 * int64 option

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

module Serialize = struct
  let pack_arith_seq (arith_seq : arith_seq) : Task_t.arith_seq =
    {
      start = arith_seq.start;
      end_exc = arith_seq.end_exc;
      diff = arith_seq.diff;
    }

  let rec pack_task ((id, data) : task) : Task_t.task = (id, pack_task_data data)

  and pack_task_data (task_data : task_data) : Task_t.task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = pack_task_type task_data.task_type;
    }

  and pack_task_type (task_type : task_type) : Task_t.task_type =
    match task_type with
    | One_off -> `One_off
    | Recurring recur -> `Recurring (pack_recur recur)

  and pack_recur (recur : recur) : Task_t.recur =
    match recur with
    | Arithemtic_seq (arith_seq, recur_data) ->
      `Arithmetic_seq (pack_arith_seq arith_seq, pack_recur_data recur_data)
    | Time_pattern_match _ -> failwith "Unimplemented"

  and pack_sched_req_template (sched_req_template : sched_req_template) :
    Task_t.sched_req_template =
    Sched_req_data_skeleton.Serialize.pack sched_req_template

  and pack_recur_data (recur_data : recur_data) : Task_t.recur_data =
    {
      task_inst_data = pack_task_inst_data recur_data.task_inst_data;
      sched_req_templates =
        List.map pack_sched_req_template recur_data.sched_req_templates;
    }

  and pack_task_inst ((id, data) : task_inst) : Task_t.task_inst =
    (id, pack_task_inst_data data)

  and pack_task_inst_data (task_inst_data : task_inst_data) :
    Task_t.task_inst_data =
    { task_inst_type = pack_task_inst_type task_inst_data.task_inst_type }

  and pack_task_inst_type (task_inst_type : task_inst_type) :
    Task_t.task_inst_type =
    match task_inst_type with
    | Reminder -> `Reminder
    | Reminder_quota_counting { quota } -> `Reminder_quota_counting quota
    | Passing -> `Passing

  and pack_task_seg x = x

  and pack_task_seg_alloc_req x = x

  and pack_task_seg_size x = x

  and pack_task_seg_place x = x
end

module Deserialize = struct
  let unpack_arith_seq (arith_seq : Task_t.arith_seq) : arith_seq =
    {
      start = arith_seq.start;
      end_exc = arith_seq.end_exc;
      diff = arith_seq.diff;
    }

  let rec unpack_task ((id, data) : Task_t.task) : task =
    (id, unpack_task_data data)

  and unpack_task_data (task_data : Task_t.task_data) : task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = unpack_task_type task_data.task_type;
    }

  and unpack_task_type (task_type : Task_t.task_type) : task_type =
    match task_type with
    | `One_off -> One_off
    | `Recurring recur -> Recurring (unpack_recur recur)

  and unpack_recur (recur : Task_t.recur) : recur =
    match recur with
    | `Arithmetic_seq (arith_seq, recur_data) ->
      Arithemtic_seq (unpack_arith_seq arith_seq, unpack_recur_data recur_data)

  and unpack_sched_req_template (sched_req_template : Task_t.sched_req_template)
    : sched_req_template =
    Sched_req_data_skeleton.Deserialize.unpack sched_req_template

  and unpack_recur_data (recur_data : Task_t.recur_data) : recur_data =
    {
      task_inst_data = unpack_task_inst_data recur_data.task_inst_data;
      sched_req_templates =
        List.map unpack_sched_req_template recur_data.sched_req_templates;
    }

  and unpack_task_inst ((id, data) : Task_t.task_inst) : task_inst =
    (id, unpack_task_inst_data data)

  and unpack_task_inst_data (task_inst_data : Task_t.task_inst_data) :
    task_inst_data =
    { task_inst_type = unpack_task_inst_type task_inst_data.task_inst_type }

  and unpack_task_inst_type (task_inst_type : Task_t.task_inst_type) :
    task_inst_type =
    match task_inst_type with
    | `Reminder -> Reminder
    | `Reminder_quota_counting quota -> Reminder_quota_counting { quota }
    | `Passing -> Passing

  and unpack_task_seg x = x

  and unpack_task_seg_alloc_req x = x

  and unpack_task_seg_size x = x

  and unpack_task_seg_place x = x
end

module Print = struct
  let debug_string_of_arith_seq ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) arith_seq =
    Debug_print.bprintf ~indent_level buffer "{";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "start = %Ld"
      arith_seq.start;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "end_exc = %Ld"
      arith_seq.end_exc;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "diff = %Ld"
      arith_seq.diff;
    Debug_print.bprintf ~indent_level buffer "}";
    Buffer.contents buffer

  let debug_print_arith_seq ?(indent_level = 0) arith_seq =
    print_string (debug_string_of_arith_seq ~indent_level arith_seq)

  let debug_string_of_task ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (id, data) =
    Debug_print.bprintf ~indent_level buffer "task id : %s\n"
      (task_id_to_string id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "splittable : %b\n" data.splittable;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "parallelizable : %b\n" data.parallelizable;
    ( match data.task_type with
      | One_off ->
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "task type : one-off\n"
      | Recurring recur -> (
          Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
            "task type : recurring\n";
          match recur with
          | Arithemtic_seq
              ( { start; end_exc; diff },
                { task_inst_data = _; sched_req_templates } ) ->
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "recur type : arithmetic sequence\n";
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "start : %Ld\n" start;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "end_exc : %Ld\n" end_exc;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "diff : %Ld\n" diff;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "sched req templates :\n";
            List.iter
              (fun sched_req_template ->
                 match sched_req_template with
                 | Sched_req_data_skeleton.Fixed { task_seg_related_data; start }
                   ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "fixed : size : %Ld, start : %Ld\n" task_seg_related_data
                     start
                 | Shift (l, time_slots) ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "shift :\n";
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "sizes :\n";
                   List.iter
                     (fun size ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "%Ld\n" size)
                     l;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "time slots :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     time_slots
                 | Split_and_shift (size, time_slots) ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "split and shift :";
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "size : %Ld\n" size;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "time slots :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     time_slots
                 | Split_even { task_seg_related_data; time_slots; buckets } ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "split even :";
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "size : %Ld\n" task_seg_related_data;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "time slots :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     time_slots;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "buckets :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     buckets
                 | Time_share (sizes, time_slots) ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "time share :";
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "sizes :";
                   List.iter
                     (fun size ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "%Ld\n" size)
                     sizes;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "time slots :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     time_slots
                 | Push_to (dir, size, time_slots) ->
                   Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
                     "push to :";
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "direction : %s\n"
                     (match dir with `Front -> "front" | `Back -> "right");
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "size : %Ld\n" size;
                   Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
                     "time slots :";
                   List.iter
                     (fun (start, end_exc) ->
                        Debug_print.bprintf ~indent_level:(indent_level + 5)
                          buffer "[%Ld, %Ld)\n" start end_exc)
                     time_slots)
              sched_req_templates
          | Time_pattern_match _ -> failwith "Unimplemented" ) );
    Buffer.contents buffer

  let debug_string_of_task_inst ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, data) =
    Debug_print.bprintf ~indent_level buffer "task inst id : %s\n"
      (task_inst_id_to_string id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "type : %s\n"
      ( match data.task_inst_type with
        | Reminder -> "reminder"
        | Reminder_quota_counting { quota } ->
          Printf.sprintf "reminder with quota : %Ld" quota
        | Passing -> "passing" );
    Buffer.contents buffer

  let debug_string_of_task_seg ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, size) =
    Debug_print.bprintf ~indent_level buffer "task seg id : %s\n"
      (task_seg_id_to_string id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "size : %Ld\n"
      size;
    Buffer.contents buffer

  let debug_print_task ?(indent_level = 0) task =
    print_string (debug_string_of_task ~indent_level task)

  let debug_print_task_inst ?(indent_level = 0) task_inst =
    print_string (debug_string_of_task_inst ~indent_level task_inst)

  let debug_print_task_seg ?(indent_level = 0) task_seg =
    print_string (debug_string_of_task_seg ~indent_level task_seg)
end
