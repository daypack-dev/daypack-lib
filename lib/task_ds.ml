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

and recur_type =
  | Arithemtic_seq of arith_seq * recur_data
  | Time_pattern_match of time_pattern * recur_data

and recur = {
  excluded_time_slots : Time_slot_ds.t list;
  recur_type : recur_type;
}

and sched_req_template_data_unit =
  (task_seg_size, int64, Time_slot_ds.t) Sched_req_data_unit_skeleton.t

and sched_req_template = sched_req_template_data_unit list

and recur_data = {
  task_inst_data : task_inst_data;
  sched_req_template : sched_req_template;
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

and progress = {
  completed : bool;
  chunks : (int64 * int64) list;
}

let task_seg_id_w_first_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) :
  task_seg_id =
  (id1, id2, id3, id4, match id5 with None -> Some 0L | Some x -> Some x)

let init_task_seg_sub_id ((id, len) : task_seg) : task_seg =
  (task_seg_id_w_first_sub_id id, len)

let succ_task_seg_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) =
  (id1, id2, id3, id4, Option.map (fun x -> x +^ 1L) id5)

let user_id_to_string (id : user_id) = Printf.sprintf "%Ld" id

let task_id_to_string ((id1, id2) : task_id) = Printf.sprintf "%Ld_%Ld" id1 id2

let task_inst_id_to_string ((id1, id2, id3) : task_inst_id) =
  Printf.sprintf "%Ld_%Ld_%Ld" id1 id2 id3

let task_seg_id_to_string ((id1, id2, id3, id4, id5) : task_seg_id) =
  Printf.sprintf "%Ld_%Ld_%Ld_%Ld_%s" id1 id2 id3 id4
    (match id5 with None -> "X" | Some x -> Int64.to_string x)

let task_seg_alloc_req_sum_length reqs =
  List.fold_left (fun acc (_, size) -> acc +^ size) 0L reqs

let sched_req_template_bound_on_start_and_end_exc
    (sched_req_template : sched_req_template) : (int64 * int64) option =
  List.fold_left
    (fun acc req_template_data_unit ->
       let cur =
         match req_template_data_unit with
         | Sched_req_data_unit_skeleton.Fixed
             { task_seg_related_data = task_seg_size; start } ->
           Some (start, start +^ task_seg_size)
         | Shift { time_slots; _ }
         | Split_and_shift { time_slots }
         | Split_even { time_slots; _ }
         | Time_share { time_slots; _ }
         | Push_toward { time_slots; _ } ->
           Time_slot_ds.min_start_and_max_end_exc_list time_slots
       in
       match acc with
       | None -> cur
       | Some (start, end_exc) -> (
           match cur with
           | None -> acc
           | Some (cur_start, cur_end_exc) ->
             Some (min start cur_start, max end_exc cur_end_exc) ))
    None sched_req_template

module Serialize = struct
  let pack_arith_seq (arith_seq : arith_seq) : Task_ds_t.arith_seq =
    {
      start = arith_seq.start;
      end_exc = arith_seq.end_exc;
      diff = arith_seq.diff;
    }

  let rec pack_task ((id, data) : task) : Task_ds_t.task = (id, pack_task_data data)

  and pack_task_data (task_data : task_data) : Task_ds_t.task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = pack_task_type task_data.task_type;
    }

  and pack_task_type (task_type : task_type) : Task_ds_t.task_type =
    match task_type with
    | One_off -> `One_off
    | Recurring recur -> `Recurring (pack_recur recur)

  and pack_recur_type (recur_type : recur_type) : Task_ds_t.recur_type =
    match recur_type with
    | Arithemtic_seq (arith_seq, recur_data) ->
      `Arithmetic_seq (pack_arith_seq arith_seq, pack_recur_data recur_data)
    | Time_pattern_match _ -> failwith "Unimplemented"

  and pack_recur (recur : recur) : Task_ds_t.recur =
    {
      excluded_time_slots = recur.excluded_time_slots;
      recur_type = pack_recur_type recur.recur_type;
    }

  and pack_sched_req_template_data_unit
      (sched_req_template_data_unit : sched_req_template_data_unit) :
    Task_ds_t.sched_req_template_data_unit =
    Sched_req_data_unit_skeleton.Serialize.pack
      ~pack_data:(fun x -> x)
      ~pack_time:(fun x -> x)
      ~pack_time_slot:(fun x -> x)
      sched_req_template_data_unit

  and pack_sched_req_template (sched_req_template : sched_req_template) :
    Task_ds_t.sched_req_template =
    List.map pack_sched_req_template_data_unit sched_req_template

  and pack_recur_data (recur_data : recur_data) : Task_ds_t.recur_data =
    {
      task_inst_data = pack_task_inst_data recur_data.task_inst_data;
      sched_req_template = pack_sched_req_template recur_data.sched_req_template;
    }

  and pack_task_inst ((id, data) : task_inst) : Task_ds_t.task_inst =
    (id, pack_task_inst_data data)

  and pack_task_inst_data (task_inst_data : task_inst_data) :
    Task_ds_t.task_inst_data =
    { task_inst_type = pack_task_inst_type task_inst_data.task_inst_type }

  and pack_task_inst_type (task_inst_type : task_inst_type) :
    Task_ds_t.task_inst_type =
    match task_inst_type with
    | Reminder -> `Reminder
    | Reminder_quota_counting { quota } -> `Reminder_quota_counting quota
    | Passing -> `Passing

  and pack_task_seg x = x

  and pack_task_seg_alloc_req x = x

  and pack_task_seg_size x = x

  and pack_task_seg_place x = x

  and pack_progress (x : progress) : Task_ds_t.progress =
    { completed = x.completed; chunks = x.chunks }
end

module Deserialize = struct
  let unpack_arith_seq (arith_seq : Task_ds_t.arith_seq) : arith_seq =
    {
      start = arith_seq.start;
      end_exc = arith_seq.end_exc;
      diff = arith_seq.diff;
    }

  let rec unpack_task ((id, data) : Task_ds_t.task) : task =
    (id, unpack_task_data data)

  and unpack_task_data (task_data : Task_ds_t.task_data) : task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = unpack_task_type task_data.task_type;
    }

  and unpack_task_type (task_type : Task_ds_t.task_type) : task_type =
    match task_type with
    | `One_off -> One_off
    | `Recurring recur -> Recurring (unpack_recur recur)

  and unpack_recur_type (recur_type : Task_ds_t.recur_type) : recur_type =
    match recur_type with
    | `Arithmetic_seq (arith_seq, recur_data) ->
      Arithemtic_seq (unpack_arith_seq arith_seq, unpack_recur_data recur_data)

  and unpack_recur (recur : Task_ds_t.recur) : recur =
    {
      excluded_time_slots = recur.excluded_time_slots;
      recur_type = unpack_recur_type recur.recur_type;
    }

  and unpack_sched_req_template_data_unit
      (sched_req_template_data_unit : Task_ds_t.sched_req_template_data_unit) :
    sched_req_template_data_unit =
    Sched_req_data_unit_skeleton.Deserialize.unpack
      ~unpack_data:(fun x -> x)
      ~unpack_time:(fun x -> x)
      ~unpack_time_slot:(fun x -> x)
      sched_req_template_data_unit

  and unpack_sched_req_template (sched_req_template : Task_ds_t.sched_req_template)
    : sched_req_template =
    List.map unpack_sched_req_template_data_unit sched_req_template

  and unpack_recur_data (recur_data : Task_ds_t.recur_data) : recur_data =
    {
      task_inst_data = unpack_task_inst_data recur_data.task_inst_data;
      sched_req_template =
        unpack_sched_req_template recur_data.sched_req_template;
    }

  and unpack_task_inst ((id, data) : Task_ds_t.task_inst) : task_inst =
    (id, unpack_task_inst_data data)

  and unpack_task_inst_data (task_inst_data : Task_ds_t.task_inst_data) :
    task_inst_data =
    { task_inst_type = unpack_task_inst_type task_inst_data.task_inst_type }

  and unpack_task_inst_type (task_inst_type : Task_ds_t.task_inst_type) :
    task_inst_type =
    match task_inst_type with
    | `Reminder -> Reminder
    | `Reminder_quota_counting quota -> Reminder_quota_counting { quota }
    | `Passing -> Passing

  and unpack_task_seg x = x

  and unpack_task_seg_alloc_req x = x

  and unpack_task_seg_size x = x

  and unpack_task_seg_place x = x

  and unpack_progress (x : Task_ds_t.progress) : progress =
    { completed = x.completed; chunks = x.chunks }
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
          Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
            "recur excluded time slots :\n";
          List.iter
            (fun time_slot ->
               Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
                 (Time_slot_ds.to_string time_slot))
            recur.excluded_time_slots;
          match recur.recur_type with
          | Arithemtic_seq
              ( { start; end_exc; diff },
                { task_inst_data = _; sched_req_template } ) ->
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "recur type : arithmetic sequence\n";
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "start : %Ld\n" start;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "end_exc : %Ld\n" end_exc;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "diff : %Ld\n" diff;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "sched req template :\n";
            List.iter
              (* (fun sched_req_template ->
               *    match sched_req_template with
               *    | Sched_req_data_unit_skeleton.Fixed
               *        { task_seg_related_data; start } ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "fixed : size : %Ld, start : %Ld\n" task_seg_related_data
               *        start
               *    | Shift (l, time_slots) ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "shift :\n";
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "sizes :\n";
               *      List.iter
               *        (fun size ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "%Ld\n" size)
               *        l;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "time slots :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        time_slots
               *    | Split_and_shift (size, time_slots) ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "split and shift :";
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "size : %Ld\n" size;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "time slots :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        time_slots
               *    | Split_even { task_seg_related_data; time_slots; buckets } ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "split even :";
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "size : %Ld\n" task_seg_related_data;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "time slots :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        time_slots;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "buckets :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        buckets
               *    | Time_share (sizes, time_slots) ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "time share :";
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "sizes :";
               *      List.iter
               *        (fun size ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "%Ld\n" size)
               *        sizes;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "time slots :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        time_slots
               *    | Push_toward (size, target, time_slots) ->
               *      Debug_print.bprintf ~indent_level:(indent_level + 3) buffer
               *        "push to :";
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "direction : %s\n"
               *        (match dir with `Front -> "front" | `Back -> "right");
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "size : %Ld\n" size;
               *      Debug_print.bprintf ~indent_level:(indent_level + 4) buffer
               *        "time slots :";
               *      List.iter
               *        (fun (start, end_exc) ->
               *           Debug_print.bprintf ~indent_level:(indent_level + 5)
               *             buffer "[%Ld, %Ld)\n" start end_exc)
               *        time_slots) *)
              (fun x ->
                 Sched_req_data_unit_skeleton.Print
                 .debug_string_of_sched_req_data_unit_skeleton ~buffer
                   ~indent_level:(indent_level + 3)
                   ~string_of_data:Int64.to_string
                   ~string_of_time:Int64.to_string
                   ~string_of_time_slot:Time_slot_ds.to_string x
                 |> ignore)
              sched_req_template
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

  let debug_string_of_progress ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) progress =
    Debug_print.bprintf ~indent_level buffer "completed : %b\n"
      progress.completed;
    Debug_print.bprintf ~indent_level buffer "chunks :\n";
    List.iter
      (fun (start, end_exc) ->
         Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
           "[%Ld, %Ld)\n" start end_exc)
      progress.chunks;
    Buffer.contents buffer

  let debug_print_task ?(indent_level = 0) task =
    print_string (debug_string_of_task ~indent_level task)

  let debug_print_task_inst ?(indent_level = 0) task_inst =
    print_string (debug_string_of_task_inst ~indent_level task_inst)

  let debug_print_task_seg ?(indent_level = 0) task_seg =
    print_string (debug_string_of_task_seg ~indent_level task_seg)

  let debug_print_progress ?(indent_level = 0) progress =
    print_string (debug_string_of_progress ~indent_level progress)
end
