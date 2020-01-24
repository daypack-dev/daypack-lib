open Int64_utils

type ('task_seg_related_data, 'time) fixed = {
  task_seg_related_data : 'task_seg_related_data;
  start : 'time;
}

type ('task_seg_related_data, 'time_slot) split_even = {
  task_seg_related_data : 'task_seg_related_data;
  time_slots : 'time_slot list;
  buckets : 'time_slot list;
}

type ('task_seg_related_data, 'time, 'time_slot) t =
  | Fixed of ('task_seg_related_data, 'time) fixed
  | Shift of 'task_seg_related_data list * 'time_slot list
  | Split_and_shift of 'task_seg_related_data * 'time_slot list
  | Split_even of ('task_seg_related_data, 'time_slot) split_even
  | Time_share of 'task_seg_related_data list * 'time_slot list
  | Push_toward of 'task_seg_related_data * 'time * 'time_slot list

let shift_time ~offset (t : ('a, int64, Time_slot.t) t) : ('a, int64, Time_slot.t) t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed { task_seg_related_data; start = start +^ offset }
  | Shift (l, time_slots) -> Shift (l, Time_slot.shift_list ~offset time_slots)
  | Split_and_shift (x, time_slots) ->
    Split_and_shift (x, Time_slot.shift_list ~offset time_slots)
  | Split_even { task_seg_related_data; time_slots; buckets } ->
    Split_even
      {
        task_seg_related_data;
        time_slots = Time_slot.shift_list ~offset time_slots;
        buckets = Time_slot.shift_list ~offset buckets;
      }
  | Time_share (l, time_slots) ->
    Time_share (l, Time_slot.shift_list ~offset time_slots)
  | Push_toward (x, target, time_slots) ->
    Push_toward (x, target +^ offset, Time_slot.shift_list ~offset time_slots)

let shift_time_list ~offset (ts : ('a, int64, Time_slot.t) t list) :
  ('a, int64, Time_slot.t) t list =
  List.map (shift_time ~offset) ts

let map (type a b c d e f) ~(f_data : a -> d) ~(f_time : b -> e) ~(f_time_slot : c -> f) (t : (a, b, c) t)
  : (d, e, f) t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed { task_seg_related_data = f_data task_seg_related_data; start = f_time start }
  | Shift (l, time_slots) ->
    Shift (List.map f_data l, List.map f_time_slot time_slots)
  | Split_and_shift (x, time_slots) ->
    Split_and_shift (f_data x, List.map f_time_slot time_slots)
  | Split_even { task_seg_related_data; time_slots; buckets } ->
    Split_even
      {
        task_seg_related_data = f_data task_seg_related_data;
        time_slots = List.map f_time_slot time_slots;
        buckets = List.map f_time_slot buckets;
      }
  | Time_share (l, time_slots) ->
    Time_share (List.map f_data l, List.map f_time_slot time_slots)
  | Push_toward (x, target, time_slots) ->
    Push_toward (f_data x, f_time target, List.map f_time_slot time_slots)

let map_list ~f_data ~f_time ~f_time_slot ts = List.map (map ~f_data ~f_time ~f_time_slot) ts

module Print = struct
  let debug_string_of_sched_req_data_unit_skeleton (type a b c) ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) ~(string_of_data : a -> string)
      ~(string_of_time : b -> string)
      ~(string_of_time_slot : c -> string) (t : (a, b, c) t) =
    ( match t with
      | Fixed { task_seg_related_data; start } ->
        Debug_print.bprintf ~indent_level buffer "fixed\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "start = %s\n" (string_of_time start)
      | Shift (l, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_data x))
          l;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          time_slots
      | Split_and_shift (x, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "split and shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (string_of_data x);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          time_slots
      | Split_even { task_seg_related_data; time_slots; buckets } ->
        Debug_print.bprintf ~indent_level buffer "split even\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          time_slots;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "buckets\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          buckets
      | Time_share (l, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "time share\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_data x))
          l;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          time_slots
      | Push_toward (x, target, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "push toward\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (string_of_data x);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "target\n";
        Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
          (string_of_time target);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (string_of_time_slot x))
          time_slots );
    Buffer.contents buffer
end

module Serialize = struct
  let pack (type a b c d e f)
      ~(pack_data : a -> d)
      ~(pack_time : b -> e)
      ~(pack_time_slot : c -> f) (t : (a, b, c) t) :
    (d, e, f) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton =
    match t with
    | Fixed { task_seg_related_data; start } ->
      `Fixed { task_seg_related_data = pack_data task_seg_related_data; start = pack_time start}
    | Shift (l, time_slots) -> `Shift (List.map pack_data l, List.map pack_time_slot time_slots)
    | Split_and_shift (x, time_slots) ->
      `Split_and_shift (pack_data x, List.map pack_time_slot time_slots)
    | Split_even { task_seg_related_data; time_slots; buckets } ->
      `Split_even
        {
          task_seg_related_data = pack_data task_seg_related_data;
          time_slots = List.map pack_time_slot time_slots;
          buckets = List.map pack_time_slot buckets;
        }
    | Time_share (l, time_slots) ->
      `Time_share (List.map pack_data l, List.map pack_time_slot time_slots)
    | Push_toward (x, target, time_slots) ->
      `Push_toward (pack_data x, pack_time target, List.map pack_time_slot time_slots)
end

module Deserialize = struct
  let unpack (type a b c d e f)
      ~(unpack_data : d -> a)
      ~(unpack_time : e -> b)
      ~(unpack_time_slot : f -> c)
      (x : (d, e, f) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton)
    : (a, b, c) t =
    match x with
    | `Fixed { task_seg_related_data; start } ->
      Fixed { task_seg_related_data = unpack_data task_seg_related_data; start = unpack_time start}
    | `Shift (l, time_slots) -> Shift (List.map unpack_data l, List.map unpack_time_slot time_slots)
    | `Split_and_shift (x, time_slots) ->
      Split_and_shift (unpack_data x, List.map unpack_time_slot time_slots)
    | `Split_even { task_seg_related_data; time_slots; buckets } ->
      Split_even
        {
          task_seg_related_data = unpack_data task_seg_related_data;
          time_slots = List.map unpack_time_slot time_slots;
          buckets = List.map unpack_time_slot buckets;
        }
    | `Time_share (l, time_slots) ->
      Time_share (List.map unpack_data l, List.map unpack_time_slot time_slots)
    | `Push_toward (x, target, time_slots) ->
      Push_toward (unpack_data x, unpack_time target, List.map unpack_time_slot time_slots)
end
