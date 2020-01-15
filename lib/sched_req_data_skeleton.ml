open Int64_utils

type 'a fixed = {
  task_seg_related_data : 'a;
  start : int64;
}

type 'a split_even = {
  task_seg_related_data : 'a;
  time_slots : Time_slot.t list;
  buckets : Time_slot.t list;
}

type 'a t =
  | Fixed of 'a fixed
  | Shift of 'a list * Time_slot.t list
  | Split_and_shift of 'a * Time_slot.t list
  | Split_even of 'a split_even
  | Time_share of 'a list * Time_slot.t list
  | Push_to of [ `Front | `Back ] * 'a * Time_slot.t list

let shift_time ~offset (t : 'a t) : 'a t =
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

let shift_time_list ~offset (ts : 'a t list) : 'a t list =
  List.map (shift_time ~offset) ts

let map (f : 'a -> 'b) (t : 'a t) : 'b t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed { task_seg_related_data = f task_seg_related_data; start }
  | Shift (l, time_slots) -> Shift (List.map f l, time_slots)
  | Split_and_shift (x, time_slots) -> Split_and_shift (f x, time_slots)
  | Split_even { task_seg_related_data; time_slots; buckets } ->
    Split_even
      { task_seg_related_data = f task_seg_related_data; time_slots; buckets }
  | Time_share (l, time_slots) -> Time_share (List.map f l, time_slots)
  | Push_to (dir, x, time_slots) -> Push_to (dir, f x, time_slots)

let map_list f ts = List.map (map f) ts

module Print = struct
  let debug_string_of_sched_req_data_skeleton ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (f : 'a -> string) (t : 'a t) =
    ( match t with
      | Fixed { task_seg_related_data; start } ->
        Debug_print.bprintf ~indent_level buffer "fixed\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (f task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "start = %Ld\n" start
      | Shift (l, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (f x))
          l;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          time_slots
      | Split_and_shift (x, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "split and shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (f x);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          time_slots
      | Split_even { task_seg_related_data; time_slots; buckets } ->
        Debug_print.bprintf ~indent_level buffer "split even\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (f task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          time_slots;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "buckets\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          buckets
      | Time_share (l, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "time share\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s"
               (f x))
          l;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          time_slots
      | Push_to (dir, x, time_slots) ->
        Debug_print.bprintf ~indent_level buffer "push to\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "direction\n";
        Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
          (match dir with `Front -> "front" | `Back -> "back");
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n" (f x);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "[%Ld, %Ld)" start end_exc)
          time_slots );
    Buffer.contents buffer
end

module Serialize = struct
  let pack (t : 'a t) : 'a Sched_req_data_skeleton_t.sched_req_data_skeleton =
    match t with
    | Fixed { task_seg_related_data; start } ->
      `Fixed { task_seg_related_data; start }
    | Shift (l, time_slots) -> `Shift (l, time_slots)
    | Split_and_shift (x, time_slots) -> `Split_and_shift (x, time_slots)
    | Split_even { task_seg_related_data; time_slots; buckets } ->
      `Split_even { task_seg_related_data; time_slots; buckets }
    | Time_share (l, time_slots) -> `Time_share (l, time_slots)
    | Push_to (dir, x, time_slots) -> `Push_to (dir, x, time_slots)
end

module Deserialize = struct
  let unpack (x : 'a Sched_req_data_skeleton_t.sched_req_data_skeleton) : 'a t =
    match x with
    | `Fixed { task_seg_related_data; start } ->
      Fixed { task_seg_related_data; start }
    | `Shift (l, time_slots) -> Shift (l, time_slots)
    | `Split_and_shift (x, time_slots) -> Split_and_shift (x, time_slots)
    | `Split_even { task_seg_related_data; time_slots; buckets } ->
      Split_even { task_seg_related_data; time_slots; buckets }
    | `Time_share (l, time_slots) -> Time_share (l, time_slots)
    | `Push_to (dir, x, time_slots) -> Push_to (dir, x, time_slots)
end
