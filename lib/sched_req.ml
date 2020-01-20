open Int64_utils

type sched_req_id = int64

type sched_req = sched_req_id * sched_req_data

and sched_req_data =
  (Task.task_seg_alloc_req, Time_slot.t) Sched_req_data_skeleton.t

type sched_req_record = sched_req_id * sched_req_record_data

and sched_req_record_data =
  (Task.task_seg, Time_slot.t) Sched_req_data_skeleton.t

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
    Sched_req_data_skeleton.Serialize.pack
      ~pack_time_slot:(fun x -> x)
      sched_req_data

  let rec pack_sched_req_record (id, data) : Sched_req_t.sched_req_record =
    (id, pack_sched_req_record_data data)

  and pack_sched_req_record_data (sched_req_record_data : sched_req_record_data)
    : Sched_req_t.sched_req_record_data =
    Sched_req_data_skeleton.Serialize.pack
      ~pack_time_slot:(fun x -> x)
      sched_req_record_data
end

module Deserialize = struct
  let rec unpack_sched_req (id, data) : sched_req =
    (id, unpack_sched_req_data data)

  and unpack_sched_req_data (sched_req_data : Sched_req_t.sched_req_data) :
    sched_req_data =
    Sched_req_data_skeleton.Deserialize.unpack
      ~unpack_time_slot:(fun x -> x)
      sched_req_data

  let rec unpack_sched_req_record (id, data) : sched_req_record =
    (id, unpack_sched_req_record_data data)

  and unpack_sched_req_record_data
      (sched_req_record_data : Sched_req_t.sched_req_record_data) :
    sched_req_record_data =
    Sched_req_data_skeleton.Deserialize.unpack
      ~unpack_time_slot:(fun x -> x)
      sched_req_record_data
end

module Print = struct
  let debug_string_of_sched_req_data ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_data =
    Sched_req_data_skeleton.Print.debug_string_of_sched_req_data_skeleton
      ~indent_level ~buffer
      ~string_of_data:(fun (id, len) ->
          Printf.sprintf "id : %s, len : %Ld\n"
            (Task.task_inst_id_to_string id)
            len)
      ~string_of_time_slot:Time_slot.to_string req_data

  let debug_string_of_sched_req ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, req_data) =
    Debug_print.bprintf ~indent_level buffer "schedule request id : %Ld\n" id;
    debug_string_of_sched_req_data ~indent_level:(indent_level + 1) ~buffer
      req_data
    |> ignore;
    Buffer.contents buffer

  let debug_string_of_sched_req_record_data ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_data =
    Sched_req_data_skeleton.Print.debug_string_of_sched_req_data_skeleton
      ~indent_level ~buffer
      ~string_of_data:(fun (id, len) ->
          Printf.sprintf "id : %s, len : %Ld\n"
            (Task.task_seg_id_to_string id)
            len)
      ~string_of_time_slot:Time_slot.to_string req_data

  let debug_string_of_sched_req_record ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, req_data) =
    Debug_print.bprintf ~indent_level buffer
      "schedule request record id : %Ld\n" id;
    debug_string_of_sched_req_record_data ~indent_level:(indent_level + 1)
      ~buffer req_data
    |> ignore;
    Buffer.contents buffer

  let debug_print_sched_req_data ?(indent_level = 0) sched_req_data =
    print_string (debug_string_of_sched_req_data ~indent_level sched_req_data)

  let debug_print_sched_req ?(indent_level = 0) sched_req =
    print_string (debug_string_of_sched_req ~indent_level sched_req)

  let debug_print_sched_req_record_data ?(indent_level = 0) sched_req_data =
    print_string
      (debug_string_of_sched_req_record_data ~indent_level sched_req_data)

  let debug_print_sched_req_record ?(indent_level = 0) sched_req =
    print_string (debug_string_of_sched_req_record ~indent_level sched_req)
end
