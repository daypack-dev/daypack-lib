open Int64_utils

type 'a fixed = {
  task_seg_related_data : 'a;
  start : int64;
}

type ('a, 'b) split_even = {
  task_seg_related_data : 'a;
  time_slots : 'b list;
  buckets : 'b list;
}

type ('a, 'b) t =
  | Fixed of 'a fixed
  | Shift of 'a list * 'b list
  | Split_and_shift of 'a * 'b list
  | Split_even of ('a, 'b) split_even
  | Time_share of 'a list * 'b list
  | Push_to of [ `Front | `Back ] * 'a * 'b list

let shift_time ~offset (t : ('a, Time_slot.t) t) : ('a, Time_slot.t) t =
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
  | Push_to (dir, x, time_slots) ->
    Push_to (dir, x, Time_slot.shift_list ~offset time_slots)

let shift_time_list ~offset (ts : ('a, Time_slot.t) t list) :
  ('a, Time_slot.t) t list =
  List.map (shift_time ~offset) ts

let map (type a b c d) ~(f_data : a -> c) ~(f_time_slot : b -> d) (t : (a, b) t)
  : (c, d) t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed { task_seg_related_data = f_data task_seg_related_data; start }
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
  | Push_to (dir, x, time_slots) ->
    Push_to (dir, f_data x, List.map f_time_slot time_slots)

let map_list ~f_data ~f_time_slot ts = List.map (map ~f_data ~f_time_slot) ts

module Print = struct
  let debug_string_of_sched_req_data_unit_skeleton ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) ~(string_of_data : 'a -> string)
      ~(string_of_time_slot : 'b -> string) (t : ('a, 'b) t) =
    ( match t with
      | Fixed { task_seg_related_data; start } ->
        Debug_print.bprintf ~indent_level buffer "fixed\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "start = %Ld\n" start
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
      | Push_to (dir, x, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "push to\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "direction\n";
        Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
          (match dir with `Front -> "front" | `Back -> "back");
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (string_of_data x);
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
  let pack ~(pack_time_slot : 'b -> 'c) (t : ('a, 'b) t) :
    ('a, 'c) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton =
    match t with
    | Fixed { task_seg_related_data; start } ->
      `Fixed { task_seg_related_data; start }
    | Shift (l, time_slots) -> `Shift (l, List.map pack_time_slot time_slots)
    | Split_and_shift (x, time_slots) ->
      `Split_and_shift (x, List.map pack_time_slot time_slots)
    | Split_even { task_seg_related_data; time_slots; buckets } ->
      `Split_even
        {
          task_seg_related_data;
          time_slots = List.map pack_time_slot time_slots;
          buckets = List.map pack_time_slot buckets;
        }
    | Time_share (l, time_slots) ->
      `Time_share (l, List.map pack_time_slot time_slots)
    | Push_to (dir, x, time_slots) ->
      `Push_to (dir, x, List.map pack_time_slot time_slots)
end

module Deserialize = struct
  let unpack ~(unpack_time_slot : 'c -> 'b)
      (x : ('a, 'c) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton)
    : ('a, 'b) t =
    match x with
    | `Fixed { task_seg_related_data; start } ->
      Fixed { task_seg_related_data; start }
    | `Shift (l, time_slots) -> Shift (l, List.map unpack_time_slot time_slots)
    | `Split_and_shift (x, time_slots) ->
      Split_and_shift (x, List.map unpack_time_slot time_slots)
    | `Split_even { task_seg_related_data; time_slots; buckets } ->
      Split_even
        {
          task_seg_related_data;
          time_slots = List.map unpack_time_slot time_slots;
          buckets = List.map unpack_time_slot buckets;
        }
    | `Time_share (l, time_slots) ->
      Time_share (l, List.map unpack_time_slot time_slots)
    | `Push_to (dir, x, time_slots) ->
      Push_to (dir, x, List.map unpack_time_slot time_slots)
end
