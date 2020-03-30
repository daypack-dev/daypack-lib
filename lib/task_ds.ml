open Int64_utils

type arith_seq = {
  start : int64;
  end_exc : int64 option;
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
  name : string;
}

and task_type =
  | One_off
  | Recurring of recur

and recur_type =
  | Arithemtic_seq of arith_seq * recur_data
  | Time_pattern_match of Time_pattern.t * recur_data

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

and progress = { chunks : Int64_int64_set.t }

let task_seg_id_w_first_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) :
  task_seg_id =
  (id1, id2, id3, id4, match id5 with None -> Some 0L | Some x -> Some x)

let init_task_seg_sub_id ((id, len) : task_seg) : task_seg =
  (task_seg_id_w_first_sub_id id, len)

let succ_task_seg_sub_id ((id1, id2, id3, id4, id5) : task_seg_id) =
  (id1, id2, id3, id4, Option.map (fun x -> x +^ 1L) id5)

let user_id_to_string (id : user_id) = Printf.sprintf "%Ld" id

let string_to_user_id (s : string) : (user_id, unit) result =
  try Ok (Int64.of_string s) with _ -> Error ()

let string_of_task_id ((id1, id2) : task_id) = Printf.sprintf "%Ld.%Ld" id1 id2

let string_to_task_id (s : string) : (task_id, unit) result =
  try Scanf.sscanf s "%Ld.%Ld" (fun id1 id2 -> Ok (id1, id2))
  with _ -> Error ()

let string_of_task_inst_id ((id1, id2, id3) : task_inst_id) =
  Printf.sprintf "%Ld.%Ld.%Ld" id1 id2 id3

let string_to_task_inst_id (s : string) : (task_inst_id, unit) result =
  try Scanf.sscanf s "%Ld.%Ld.%Ld" (fun id1 id2 id3 -> Ok (id1, id2, id3))
  with _ -> Error ()

let string_of_task_seg_id ((id1, id2, id3, id4, id5) : task_seg_id) =
  Printf.sprintf "%Ld.%Ld.%Ld.%Ld.%s" id1 id2 id3 id4
    (match id5 with None -> "X" | Some x -> Int64.to_string x)

let string_to_task_seg_id (s : string) : (task_seg_id, unit) result =
  try
    Scanf.sscanf s "%Ld.%Ld.%Ld.%Ld.X" (fun id1 id2 id3 id4 ->
        Ok (id1, id2, id3, id4, None))
  with _ -> (
      try
        Scanf.sscanf s "%Ld.%Ld.%Ld.%Ld.%Ld" (fun id1 id2 id3 id4 id5 ->
            Ok (id1, id2, id3, id4, Some id5))
      with _ -> Error () )

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

module Check = struct
  let check_user_id (id : user_id) = id >= 0L

  let check_task_id ((id1, id2) : task_id) = id1 >= 0L && id2 >= 0L

  let check_task_inst_id ((id1, id2, id3) : task_inst_id) =
    id1 >= 0L && id2 >= 0L && id3 >= 0L

  let check_task_seg_id ((id1, id2, id3, id4, id5) : task_seg_id) =
    id1 >= 0L
    && id2 >= 0L
    && id3 >= 0L
    && id4 >= 0L
    && match id5 with None -> true | Some x -> x >= 0L

  let check_task_seg_size (size : task_seg_size) : bool = size > 0L

  let check_task_seg_alloc_req ((id, size) : task_seg_alloc_req) : bool =
    check_task_inst_id id && check_task_seg_size size

  let check_task_seg ((id, size) : task_seg) : bool =
    check_task_seg_id id && check_task_seg_size size
end

module Serialize = struct
  let pack_arith_seq (arith_seq : arith_seq) : Task_ds_t.arith_seq =
    {
      start = Misc_utils.int32_int32_of_int64 arith_seq.start;
      end_exc = Option.map Misc_utils.int32_int32_of_int64 arith_seq.end_exc;
      diff = Misc_utils.int32_int32_of_int64 arith_seq.diff;
    }

  let pack_user_id = Misc_utils.int32_int32_of_int64

  let pack_task_id (id1, id2) =
    (Misc_utils.int32_int32_of_int64 id1, Misc_utils.int32_int32_of_int64 id2)

  let pack_task_inst_id (id1, id2, id3) =
    ( Misc_utils.int32_int32_of_int64 id1,
      Misc_utils.int32_int32_of_int64 id2,
      Misc_utils.int32_int32_of_int64 id3 )

  let pack_task_seg_id (id1, id2, id3, id4, id5) =
    ( Misc_utils.int32_int32_of_int64 id1,
      Misc_utils.int32_int32_of_int64 id2,
      Misc_utils.int32_int32_of_int64 id3,
      Misc_utils.int32_int32_of_int64 id4,
      Option.map Misc_utils.int32_int32_of_int64 id5 )

  let rec pack_task ((id, data) : task) : Task_ds_t.task =
    (pack_task_id id, pack_task_data data)

  and pack_task_data (task_data : task_data) : Task_ds_t.task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = pack_task_type task_data.task_type;
      name = task_data.name;
    }

  and pack_task_type (task_type : task_type) : Task_ds_t.task_type =
    match task_type with
    | One_off -> `One_off
    | Recurring recur -> `Recurring (pack_recur recur)

  and pack_recur_type (recur_type : recur_type) : Task_ds_t.recur_type =
    match recur_type with
    | Arithemtic_seq (arith_seq, recur_data) ->
      `Arithmetic_seq (pack_arith_seq arith_seq, pack_recur_data recur_data)
    | Time_pattern_match (pattern, recur_data) ->
      `Time_pattern_match
        ( Time_pattern.Serialize.pack_pattern pattern,
          pack_recur_data recur_data )

  and pack_recur (recur : recur) : Task_ds_t.recur =
    {
      excluded_time_slots =
        Time_slot_ds.Serialize.pack_time_slots recur.excluded_time_slots;
      recur_type = pack_recur_type recur.recur_type;
    }

  and pack_sched_req_template_data_unit
      (sched_req_template_data_unit : sched_req_template_data_unit) :
    Task_ds_t.sched_req_template_data_unit =
    Sched_req_data_unit_skeleton.Serialize.pack
      ~pack_data:Misc_utils.int32_int32_of_int64
      ~pack_time:Misc_utils.int32_int32_of_int64
      ~pack_time_slot:Time_slot_ds.Serialize.pack_time_slot
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
    (pack_task_inst_id id, pack_task_inst_data data)

  and pack_task_inst_data (task_inst_data : task_inst_data) :
    Task_ds_t.task_inst_data =
    { task_inst_type = pack_task_inst_type task_inst_data.task_inst_type }

  and pack_task_inst_type (task_inst_type : task_inst_type) :
    Task_ds_t.task_inst_type =
    match task_inst_type with
    | Reminder -> `Reminder
    | Reminder_quota_counting { quota } ->
      `Reminder_quota_counting (Misc_utils.int32_int32_of_int64 quota)
    | Passing -> `Passing

  and pack_task_seg (id, size) =
    (pack_task_seg_id id, Misc_utils.int32_int32_of_int64 size)

  and pack_task_seg_alloc_req (id, size) =
    (pack_task_inst_id id, Misc_utils.int32_int32_of_int64 size)

  and pack_task_seg_size x = x

  and pack_task_seg_place x = x

  and pack_progress (x : progress) : Task_ds_t.progress =
    { chunks = Int64_int64_set.Serialize.pack x.chunks }
end

module Deserialize = struct
  let unpack_arith_seq (arith_seq : Task_ds_t.arith_seq) : arith_seq =
    {
      start = Misc_utils.int64_of_int32_int32 arith_seq.start;
      end_exc = Option.map Misc_utils.int64_of_int32_int32 arith_seq.end_exc;
      diff = Misc_utils.int64_of_int32_int32 arith_seq.diff;
    }

  let unpack_user_id = Misc_utils.int64_of_int32_int32

  let unpack_task_id (id1, id2) =
    (Misc_utils.int64_of_int32_int32 id1, Misc_utils.int64_of_int32_int32 id2)

  let unpack_task_inst_id (id1, id2, id3) =
    ( Misc_utils.int64_of_int32_int32 id1,
      Misc_utils.int64_of_int32_int32 id2,
      Misc_utils.int64_of_int32_int32 id3 )

  let unpack_task_seg_id (id1, id2, id3, id4, id5) =
    ( Misc_utils.int64_of_int32_int32 id1,
      Misc_utils.int64_of_int32_int32 id2,
      Misc_utils.int64_of_int32_int32 id3,
      Misc_utils.int64_of_int32_int32 id4,
      Option.map Misc_utils.int64_of_int32_int32 id5 )

  let rec unpack_task ((id, data) : Task_ds_t.task) : task =
    (unpack_task_id id, unpack_task_data data)

  and unpack_task_data (task_data : Task_ds_t.task_data) : task_data =
    {
      splittable = task_data.splittable;
      parallelizable = task_data.parallelizable;
      task_type = unpack_task_type task_data.task_type;
      name = task_data.name;
    }

  and unpack_task_type (task_type : Task_ds_t.task_type) : task_type =
    match task_type with
    | `One_off -> One_off
    | `Recurring recur -> Recurring (unpack_recur recur)

  and unpack_recur_type (recur_type : Task_ds_t.recur_type) : recur_type =
    match recur_type with
    | `Arithmetic_seq (arith_seq, recur_data) ->
      Arithemtic_seq (unpack_arith_seq arith_seq, unpack_recur_data recur_data)
    | `Time_pattern_match (pattern, recur_data) ->
      Time_pattern_match
        ( Time_pattern.Deserialize.unpack_pattern pattern,
          unpack_recur_data recur_data )

  and unpack_recur (recur : Task_ds_t.recur) : recur =
    {
      excluded_time_slots =
        Time_slot_ds.Deserialize.unpack_time_slots recur.excluded_time_slots;
      recur_type = unpack_recur_type recur.recur_type;
    }

  and unpack_sched_req_template_data_unit
      (sched_req_template_data_unit : Task_ds_t.sched_req_template_data_unit) :
    sched_req_template_data_unit =
    Sched_req_data_unit_skeleton.Deserialize.unpack
      ~unpack_data:Misc_utils.int64_of_int32_int32
      ~unpack_time:Misc_utils.int64_of_int32_int32
      ~unpack_time_slot:Time_slot_ds.Deserialize.unpack_time_slot
      sched_req_template_data_unit

  and unpack_sched_req_template
      (sched_req_template : Task_ds_t.sched_req_template) : sched_req_template =
    List.map unpack_sched_req_template_data_unit sched_req_template

  and unpack_recur_data (recur_data : Task_ds_t.recur_data) : recur_data =
    {
      task_inst_data = unpack_task_inst_data recur_data.task_inst_data;
      sched_req_template =
        unpack_sched_req_template recur_data.sched_req_template;
    }

  and unpack_task_inst ((id, data) : Task_ds_t.task_inst) : task_inst =
    (unpack_task_inst_id id, unpack_task_inst_data data)

  and unpack_task_inst_data (task_inst_data : Task_ds_t.task_inst_data) :
    task_inst_data =
    { task_inst_type = unpack_task_inst_type task_inst_data.task_inst_type }

  and unpack_task_inst_type (task_inst_type : Task_ds_t.task_inst_type) :
    task_inst_type =
    match task_inst_type with
    | `Reminder -> Reminder
    | `Reminder_quota_counting quota ->
      Reminder_quota_counting
        { quota = Misc_utils.int64_of_int32_int32 quota }
    | `Passing -> Passing

  and unpack_task_seg (id, size) =
    (unpack_task_seg_id id, Misc_utils.int64_of_int32_int32 size)

  and unpack_task_seg_alloc_req (id, size) =
    (unpack_task_inst_id id, Misc_utils.int64_of_int32_int32 size)

  and unpack_task_seg_size x = x

  and unpack_task_seg_place x = x

  and unpack_progress (x : Task_ds_t.progress) : progress =
    { chunks = Int64_int64_set.Deserialize.unpack x.chunks }
end

module Print = struct
  let debug_string_of_sched_req_template ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (sched_req_template : sched_req_template) :
    string =
    List.iter
      (fun x ->
         Sched_req_data_unit_skeleton.Print
         .debug_string_of_sched_req_data_unit_skeleton ~buffer ~indent_level
           ~string_of_data:Int64.to_string ~string_of_time:Int64.to_string
           ~string_of_time_slot:Time_slot_ds.to_string x
         |> ignore)
      sched_req_template;
    Buffer.contents buffer

  let debug_string_of_arith_seq ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) arith_seq =
    Debug_print.bprintf ~indent_level buffer "{";
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "start = %Ld"
      arith_seq.start;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "end_exc = %s"
      (Option.fold ~none:"None" ~some:Int64.to_string arith_seq.end_exc);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "diff = %Ld"
      arith_seq.diff;
    Debug_print.bprintf ~indent_level buffer "}";
    Buffer.contents buffer

  let debug_print_arith_seq ?(indent_level = 0) arith_seq =
    print_string (debug_string_of_arith_seq ~indent_level arith_seq)

  let debug_string_of_task ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (id, data) =
    Debug_print.bprintf ~indent_level buffer "task id : %s\n"
      (string_of_task_id id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "name : %s\n"
      data.name;
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
              "end_exc : %s\n"
              (Option.fold ~none:"None" ~some:Int64.to_string end_exc);
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "diff : %Ld\n" diff;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "sched req template :\n";
            debug_string_of_sched_req_template ~indent_level:(indent_level + 3)
              ~buffer sched_req_template
            |> ignore
          | Time_pattern_match
              (pattern, { task_inst_data = _; sched_req_template }) ->
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "recur type : time pattern\n";
            Time_pattern.Print.debug_string_of_pattern
              ~indent_level:(indent_level + 2) ~buffer pattern
            |> ignore;
            Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
              "sched req template :\n";
            debug_string_of_sched_req_template ~indent_level:(indent_level + 3)
              ~buffer sched_req_template
            |> ignore ) );
    Buffer.contents buffer

  let debug_string_of_task_inst ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, data) =
    Debug_print.bprintf ~indent_level buffer "task inst id : %s\n"
      (string_of_task_inst_id id);
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
      (string_of_task_seg_id id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "size : %Ld\n"
      size;
    Buffer.contents buffer

  let debug_string_of_task_seg_place ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, start, end_exc) =
    Debug_print.bprintf ~indent_level buffer "task seg id : %s\n"
      (string_of_task_seg_id id);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "[%Ld, %Ld)\n"
      start end_exc;
    Buffer.contents buffer

  let debug_string_of_progress ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) progress =
    Debug_print.bprintf ~indent_level buffer "chunks :\n";
    Int64_int64_set.iter
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

  let debug_print_task_seg_place ?(indent_level = 0) task_seg_place =
    print_string (debug_string_of_task_seg_place ~indent_level task_seg_place)

  let debug_print_progress ?(indent_level = 0) progress =
    print_string (debug_string_of_progress ~indent_level progress)
end
