open Int64_utils

type sched_req_id = int64

type sched_req = sched_req_id * sched_req_data

and sched_req_data = Task.task_seg_alloc_req Sched_req_data_skeleton.t

type sched_req_record = sched_req_id * sched_req_record_data

and sched_req_record_data = Task.task_seg Sched_req_data_skeleton.t

let flexibility_score_of_sched_req_record
    ((_id, req_record_data) : sched_req_record) : float =
  match req_record_data with
  | Sched_req_data_skeleton.Fixed _ -> 0.0
  | Shift (task_seg_alloc_reqs, time_slots) ->
    let task_seg_alloc_req_sum_len =
      Task.task_seg_alloc_req_sum_length task_seg_alloc_reqs |> Int64.to_float
    in
    let time_slot_sum_len =
      Time_slot.sum_length_list time_slots |> Int64.to_float
    in
    1. -. (task_seg_alloc_req_sum_len /. time_slot_sum_len)
  | Split_and_shift ((_, size), time_slots) ->
    let time_slot_sum_len =
      Time_slot.sum_length_list time_slots |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)
  | Split_even { task_seg_related_data = _, size; time_slots; buckets } ->
    let time_slot_sum_len =
      Time_slot.intersect (time_slots |> List.to_seq) (buckets |> List.to_seq)
      |> List.of_seq |> Time_slot.sum_length_list |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)
  | Time_share (task_seg_alloc_reqs, time_slots) ->
    let task_seg_alloc_req_sum_len =
      Task.task_seg_alloc_req_sum_length task_seg_alloc_reqs |> Int64.to_float
    in
    let time_slot_sum_len =
      Time_slot.sum_length_list time_slots |> Int64.to_float
    in
    1. -. (task_seg_alloc_req_sum_len /. time_slot_sum_len)
  | Push_to (_, (_, size), time_slots) ->
    let time_slot_sum_len =
      Time_slot.sum_length_list time_slots |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)

let sort_sched_req_record_list_by_flexibility_score
    (reqs : sched_req_record list) : sched_req_record list =
  List.sort
    (fun x y ->
       compare
         (flexibility_score_of_sched_req_record x)
         (flexibility_score_of_sched_req_record y))
    reqs

let sched_req_bound_on_start_and_end_exc ((_id, req_record_data) : sched_req) :
  (int64 * int64) option =
  match req_record_data with
  | Fixed { task_seg_related_data = _, task_seg_size; start } ->
    Some (start, start +^ task_seg_size)
  | Shift (_, time_slots)
  | Split_and_shift (_, time_slots)
  | Split_even { time_slots; _ }
  | Time_share (_, time_slots)
  | Push_to (_, _, time_slots) ->
    Time_slot.min_start_and_max_end_exc_list time_slots

let sched_req_fully_within_time_period ~start ~end_exc (sched_req : sched_req) :
  bool =
  match sched_req_bound_on_start_and_end_exc sched_req with
  | None -> false
  | Some (start', end_exc') -> start <= start' && end_exc' <= end_exc

let sched_req_partially_within_time_period ~start ~end_exc
    (sched_req : sched_req) : bool =
  match sched_req_bound_on_start_and_end_exc sched_req with
  | None -> false
  | Some (start', end_exc') ->
    (start' < start && start < end_exc')
    || (start' < end_exc && end_exc < end_exc')

module Serialize = struct
  let rec pack_sched_req (id, data) : Sched_req_t.sched_req =
    (id, pack_sched_req_data data)

  and pack_sched_req_data (sched_req_data : sched_req_data) :
    Sched_req_t.sched_req_data =
    Sched_req_data_skeleton.Serialize.pack sched_req_data
end

module Print = struct
  let debug_print_sched_req_data ?(indent_level = 0) req_data =
    match req_data with
    | Sched_req_data_skeleton.Fixed { task_seg_related_data = id, len; start }
      ->
      Debug_print.printf ~indent_level "fixed :\n";
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task segment allocation :\n";
      Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
        (Task.task_inst_id_to_string id)
        len;
      Debug_print.printf ~indent_level:(indent_level + 1) "start :\n";
      Debug_print.printf ~indent_level:(indent_level + 2) "%Ld\n" start
    | Shift (task_seg_allocs, time_slots) ->
      Debug_print.printf ~indent_level "shift :\n";
      Debug_print.printf ~indent_level:(indent_level + 1)
        "  task segment allocations :\n";
      List.iter
        (fun (id, len) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
             (Task.task_inst_id_to_string id)
             len)
        task_seg_allocs;
      Debug_print.printf ~indent_level:(indent_level + 1) "time slots :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        time_slots
    | Split_and_shift ((id, len), time_slots) ->
      Debug_print.printf ~indent_level "split and shift :\n";
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task segment allocation :\n";
      Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
        (Task.task_inst_id_to_string id)
        len;
      Debug_print.printf ~indent_level:(indent_level + 1) "time slots :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        time_slots
    | Split_even { task_seg_related_data = id, len; time_slots; buckets } ->
      Debug_print.printf ~indent_level "split even :\n";
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task segment allocation :\n";
      Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
        (Task.task_inst_id_to_string id)
        len;
      Debug_print.printf ~indent_level:(indent_level + 1) "time slots :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        time_slots;
      Debug_print.printf ~indent_level:(indent_level + 1) "buckets :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        buckets
    | Time_share (task_seg_allocs, time_slots) ->
      Debug_print.printf ~indent_level "time share :\n";
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task segment allocations :\n";
      List.iter
        (fun (id, len) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
             (Task.task_inst_id_to_string id)
             len)
        task_seg_allocs;
      Debug_print.printf ~indent_level:(indent_level + 1) "time slots :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        time_slots
    | Push_to (side, (id, len), time_slots) ->
      Debug_print.printf ~indent_level "push to %s:\n"
        (match side with `Front -> "front" | `Back -> "back");
      Debug_print.printf ~indent_level:(indent_level + 1)
        "task segment allocation :\n";
      Debug_print.printf ~indent_level:(indent_level + 2) "%s : %Ld\n"
        (Task.task_inst_id_to_string id)
        len;
      Debug_print.printf ~indent_level:(indent_level + 1) "time slots :\n";
      List.iter
        (fun (start, end_exc) ->
           Debug_print.printf ~indent_level:(indent_level + 2) "[%Ld, %Ld)\n"
             start end_exc)
        time_slots

  let debug_print_sched_req ?(indent_level = 0) (id, req_data) =
    Debug_print.printf ~indent_level "schedule request id : %Ld\n" id;
    debug_print_sched_req_data ~indent_level:(indent_level + 1) req_data
end
