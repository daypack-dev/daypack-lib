open Int64_utils

type t = int64 * int64

module Normalize = struct
  let filter_invalid_or_empty (time_slots : t Seq.t) : t Seq.t =
    Seq.filter (fun (x, y) -> x < y) time_slots

  let filter_invalid_or_empty_list (time_slots : t list) : t list =
    List.filter (fun (x, y) -> x < y) time_slots

  let sort_uniq_time_slots_list (time_slots : t list) : t list =
    List.sort_uniq
      (fun (x1, y1) (x2, y2) ->
         (* lexicographic product *)
         if x1 < x2 || (x1 = x2 && y1 < y2) then -1
         else if x1 = x2 && y1 = y2 then 0
         else 1)
      time_slots

  let sort_uniq_time_slots (time_slots : t Seq.t) : t Seq.t =
    time_slots |> List.of_seq |> sort_uniq_time_slots_list |> List.to_seq

  let defrag_and_join_overlapping (time_slots : t Seq.t) : t Seq.t =
    let rec aux last_start_and_last_end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> (
          match last_start_and_last_end_exc with
          | None -> Seq.empty
          | Some (last_start, last_end_exc) ->
            Seq.return (last_start, last_end_exc) )
      | Seq.Cons ((start, end_exc), rest) -> (
          match last_start_and_last_end_exc with
          | None -> aux (Some (start, end_exc)) rest
          | Some (last_start, last_end_exc) ->
            if start <= last_end_exc then
              (* can be merged with the time slot currently carrying *)
              if last_end_exc <= end_exc then
                (* larger than the time slot currently carrying completely *)
                aux (Some (last_start, end_exc)) rest
              else
                (* falls within the time slot currently carrying completely *)
                aux (Some (last_start, last_end_exc)) rest
            else
              (* cannot be merged, add time slot being carried to the sequence *)
              fun () ->
                Seq.Cons
                  ((last_start, last_end_exc), aux (Some (start, end_exc)) rest)
        )
    in
    aux None time_slots
end

let normalize ?(skip_filter = false) ?(skip_sort = false) time_slots =
  time_slots
  |> (fun l -> if skip_filter then l else Normalize.filter_invalid_or_empty l)
  |> (fun l -> if skip_sort then l else Normalize.sort_uniq_time_slots l)
  |> Normalize.defrag_and_join_overlapping

let normalize_list_in_seq_out ?(skip_filter = false) ?(skip_sort = false)
    time_slots =
  time_slots
  |> (fun l ->
      if skip_filter then l else Normalize.filter_invalid_or_empty_list l)
  |> (fun l -> if skip_sort then l else Normalize.sort_uniq_time_slots_list l)
  |> List.to_seq
  |> Normalize.defrag_and_join_overlapping

module Slice = struct
  let slice_start ~start (time_slots : t Seq.t) : t Seq.t =
    let rec aux start time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), rest) ->
        if start <= ts_start then
          (* entire time slot is after start, do nothing *)
          time_slots
        else if ts_start < start && start < ts_end_exc then
          (* time slot spans across the start mark, split time slot *)
          fun () -> Seq.Cons ((start, ts_end_exc), rest)
        else
          (* time slot is before start mark, move to next time slot *)
          aux start rest
    in
    aux start time_slots

  let slice_end_exc ~end_exc (time_slots : t Seq.t) : t Seq.t =
    let rec aux end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), rest) ->
        if end_exc <= ts_start then
          (* entire time slot is after end_exc mark, drop everything *)
          aux end_exc Seq.empty
        else if ts_start < end_exc && end_exc < ts_end_exc then
          (* time slot spans across the end_exc mark, split time slot,
             skip remaining slots *)
          fun () -> Seq.Cons ((ts_start, end_exc), aux end_exc Seq.empty)
        else
          (* time slot is before end_exc, add to sequence and move to next time slot *)
          fun () -> Seq.Cons ((ts_start, ts_end_exc), aux end_exc rest)
    in
    aux end_exc time_slots
end

module Slice_rev = struct
  let slice_start ~start (time_slots : t Seq.t) : t Seq.t =
    let rec aux acc start time_slots =
      match time_slots () with
      | Seq.Nil -> List.rev acc |> List.to_seq
      | Seq.Cons ((ts_start, ts_end_exc), slots) ->
        if start <= ts_start then
          (* entire time slot is after start, add to acc *)
          aux ((ts_start, ts_end_exc) :: acc) start slots
        else if ts_start < start && start < ts_end_exc then
          (* time slot spans across the start mark, split time slot *)
          aux ((start, ts_end_exc) :: acc) start slots
        else
          (* time slot is before start mark, do nothing *)
          aux acc start Seq.empty
    in
    aux [] start time_slots

  let slice_end_exc ~end_exc (time_slots : t Seq.t) : t Seq.t =
    let rec aux end_exc time_slots =
      match time_slots () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons ((ts_start, ts_end_exc), slots) ->
        if ts_end_exc <= end_exc then
          (* entire time slot is before end_exc mark, do nothing *)
          time_slots
        else if ts_start < end_exc && end_exc < ts_end_exc then
          (* time slot spans across the end_exc mark, split time slot *)
          OSeq.cons (ts_start, end_exc) slots
        else
          (* time slot is after end_exc mark, move to next time slot *)
          aux end_exc slots
    in
    aux end_exc time_slots
end

let slice ?start ?end_exc time_slots =
  time_slots
  |> (fun l ->
      match start with None -> l | Some start -> Slice.slice_start ~start l)
  |> fun l ->
  match end_exc with
  | None -> l
  | Some end_exc -> Slice.slice_end_exc ~end_exc l

let slice_rev ?start ?end_exc time_slots =
  time_slots
  |> (fun l ->
      match start with
      | None -> l
      | Some start -> Slice_rev.slice_start ~start l)
  |> fun l ->
  match end_exc with
  | None -> l
  | Some end_exc -> Slice_rev.slice_end_exc ~end_exc l

let relative_complement ~(mem_of : t Seq.t) ~(not_mem_of : t Seq.t) : t Seq.t =
  let rec aux mem_of not_mem_of =
    match (mem_of (), not_mem_of ()) with
    | Seq.Nil, _ -> Seq.empty
    | _, Seq.Nil -> mem_of
    | ( Seq.Cons ((mem_of_start, mem_of_end_exc), mem_of_rest),
        Seq.Cons ((not_mem_of_start, not_mem_of_end_exc), not_mem_of_rest) )
      -> (
          if mem_of_end_exc < not_mem_of_start then
            (* mem_of is before not_mem_of entirely, output mem_of *)
            fun () ->
              Seq.Cons ((mem_of_start, mem_of_end_exc), aux mem_of_rest not_mem_of)
          else if not_mem_of_end_exc < mem_of_start then
            (* not_mem_of is before mem_of entirely, drop not_mem_of *)
            aux mem_of not_mem_of_rest
          else
            (* there is an overlap or touching *)
            let overlap_start = max mem_of_start not_mem_of_start in
            let overlap_end_exc = min mem_of_end_exc not_mem_of_end_exc in
            let mem_of_split1 =
              if mem_of_start < overlap_start then
                Some (mem_of_start, overlap_start)
              else None
            in
            let mem_of_split2 =
              if overlap_end_exc < mem_of_end_exc then
                Some (overlap_end_exc, mem_of_end_exc)
              else None
            in
            match (mem_of_split1, mem_of_split2) with
            | None, None -> aux mem_of_rest not_mem_of
            | Some s1, None -> fun () -> Seq.Cons (s1, aux mem_of_rest not_mem_of)
            | None, Some s2 ->
              aux (fun () -> Seq.Cons (s2, mem_of_rest)) not_mem_of_rest
            | Some s1, Some s2 ->
              fun () ->
                Seq.Cons
                  ( s1,
                    aux (fun () -> Seq.Cons (s2, mem_of_rest)) not_mem_of_rest
                  ) )
  in
  aux mem_of not_mem_of

let invert ~start ~end_exc (time_slots : t Seq.t) : t Seq.t =
  relative_complement
    ~mem_of:(Seq.return (start, end_exc))
    ~not_mem_of:time_slots

let intersect (time_slots1 : t Seq.t) (time_slots2 : t Seq.t) : t Seq.t =
  let rec aux time_slots1 time_slots2 : t Seq.t =
    match (time_slots1 (), time_slots2 ()) with
    | Seq.Nil, _ -> Seq.empty
    | _, Seq.Nil -> Seq.empty
    | Seq.Cons ((start1, end_exc1), rest1), Seq.Cons ((start2, end_exc2), rest2)
      ->
      if end_exc1 < start2 then
        (* 1 is before 2 entirely, drop 1, keep 2 *)
        aux rest1 time_slots2
      else if end_exc2 < start1 then
        (* 2 is before 1 entirely, keep 1, drop 2 *)
        aux time_slots1 rest2
      else
        (* there is an overlap or touching *)
        let overlap_start = max start1 start2 in
        let overlap_end_exc = min end_exc1 end_exc2 in
        let s1 = if end_exc1 <= overlap_end_exc then rest1 else time_slots1 in
        let s2 = if end_exc2 <= overlap_end_exc then rest2 else time_slots2 in
        if overlap_start < overlap_end_exc then
          (* there is an overlap *)
          fun () -> Seq.Cons ((overlap_start, overlap_end_exc), aux s1 s2)
        else aux s1 s2
  in
  aux time_slots1 time_slots2

let merge (time_slots1 : t Seq.t) (time_slots2 : t Seq.t) : t Seq.t =
  let rec aux time_slots1 time_slots2 =
    match (time_slots1 (), time_slots2 ()) with
    | Seq.Nil, s | s, Seq.Nil -> fun () -> s
    | (Seq.Cons ((start1, end_exc1), rest1) as ts1), (Seq.Cons ((start2, end_exc2), rest2) as ts2)
      ->
      if start1 <= start2 then fun () ->
        Seq.Cons ((start1, end_exc1), aux rest1 (fun () -> ts2))
      else fun () -> Seq.Cons ((start2, end_exc2), aux (fun () -> ts1) rest2)
  in
  aux time_slots1 time_slots2

let merge_multi_seq (time_slot_batches : t Seq.t Seq.t) : t Seq.t =
  Seq.fold_left
    (fun acc time_slots -> merge acc time_slots)
    Seq.empty time_slot_batches

let merge_multi_list (time_slot_batches : t Seq.t list) : t Seq.t =
  List.to_seq time_slot_batches |> merge_multi_seq

let collect_round_robin_non_decreasing (batches : t Seq.t list) :
  t option list Seq.t =
  let rec get_usable_part (cur_start : int64) (seq : t Seq.t) : t Seq.t =
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start, end_exc), rest) as s ->
      if cur_start <= start then fun () -> s
      else if end_exc <= cur_start then get_usable_part cur_start rest
      else fun () -> Seq.Cons ((cur_start, end_exc), rest)
  in
  let rec aux (cur_start : int64 option) (batches : t Seq.t list) :
    t option list Seq.t =
    let cur_start, acc, new_batches =
      List.fold_left
        (fun (cur_start, acc, new_batches) seq ->
           let usable =
             match cur_start with
             | None -> seq
             | Some cur_start -> get_usable_part cur_start seq
           in
           match usable () with
           | Seq.Nil -> (cur_start, None :: acc, new_batches)
           | Seq.Cons ((start, end_exc), rest) ->
             (Some start, Some (start, end_exc) :: acc, rest :: new_batches))
        (cur_start, [], []) batches
    in
    let acc = List.rev acc in
    let new_batches = List.rev new_batches in
    fun () -> Seq.Cons (acc, aux cur_start new_batches)
  in
  aux None batches

let merge_multi_list_round_robin_non_decreasing (batches : t Seq.t list) :
  t Seq.t =
  collect_round_robin_non_decreasing batches
  |> Seq.flat_map (fun l -> List.to_seq l |> Seq.filter_map (fun x -> x))

let merge_multi_seq_round_robin_non_decreasing (batches : t Seq.t Seq.t) :
  t Seq.t =
  batches |> List.of_seq |> merge_multi_list_round_robin_non_decreasing

let union time_slots1 time_slots2 =
  merge time_slots1 time_slots2 |> normalize ~skip_filter:true ~skip_sort:true

let union_multi_seq (time_slot_batches : t Seq.t Seq.t) : t Seq.t =
  Seq.fold_left
    (fun acc time_slots -> union acc time_slots)
    Seq.empty time_slot_batches

let union_multi_list (time_slot_batches : t Seq.t list) : t Seq.t =
  List.to_seq time_slot_batches |> union_multi_seq

let chunk ~chunk_size ?(drop_partial = false) (time_slots : t Seq.t) : t Seq.t =
  let rec aux time_slots =
    match time_slots () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start, end_exc), rest) ->
      let chunk_end_exc = min end_exc (start +^ chunk_size) in
      let size = chunk_end_exc -^ start in
      if size = 0L || (size < chunk_size && drop_partial) then aux rest
      else
        let rest () = Seq.Cons ((chunk_end_exc, end_exc), rest) in
        fun () -> Seq.Cons ((start, chunk_end_exc), aux rest)
  in
  aux time_slots

let sum_length (time_slots : t Seq.t) : int64 =
  Seq.fold_left
    (fun acc (start, end_exc) -> acc +^ (end_exc -^ start))
    0L time_slots

let sum_length_list (time_slots : t list) : int64 =
  time_slots |> List.to_seq |> sum_length

let min_start_and_max_end_exc (time_slots : t Seq.t) : (int64 * int64) option =
  Seq.fold_left
    (fun acc (start, end_exc) ->
       match acc with
       | None -> Some (start, end_exc)
       | Some (min_start, max_end_exc) ->
         Some (min min_start start, max max_end_exc end_exc))
    None time_slots

let min_start_and_max_end_exc_list (time_slots : t list) :
  (int64 * int64) option =
  time_slots |> List.to_seq |> min_start_and_max_end_exc

let shift_list ~offset (time_slots : t list) : t list =
  List.map
    (fun (start, end_exc) -> (start +^ offset, end_exc +^ offset))
    time_slots

let equal (time_slots1 : t list) (time_slots2 : t list) : bool =
  let time_slots1 = time_slots1 |> List.to_seq |> normalize |> List.of_seq in
  let time_slots2 = time_slots2 |> List.to_seq |> normalize |> List.of_seq in
  time_slots1 = time_slots2

let a_is_subset_of_b ~(a : t Seq.t) ~(b : t Seq.t) : bool =
  let inter = intersect a b |> List.of_seq in
  let a = List.of_seq a in
  a = inter

let to_string ((start, end_exc) : t) : string =
  Printf.sprintf "[%Ld, %Ld)" start end_exc

module Check = struct
  let check_time_slot ((start, end_exc) : t) : bool =
    0L <= start && start <= end_exc
end

module Serialize = struct
  let pack_time_slot (start, end_exc) =
    ( Misc_utils.int32_int32_of_int64 start,
      Misc_utils.int32_int32_of_int64 end_exc )

  let pack_time_slots time_slots = List.map pack_time_slot time_slots
end

module Deserialize = struct
  let unpack_time_slot (start, end_exc) =
    ( Misc_utils.int64_of_int32_int32 start,
      Misc_utils.int64_of_int32_int32 end_exc )

  let unpack_time_slots time_slots = List.map unpack_time_slot time_slots
end
