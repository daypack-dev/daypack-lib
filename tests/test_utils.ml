module Print_utils = struct
  let small_nat = QCheck.Print.int

  let int64 = Int64.to_string

  let time_slots = QCheck.Print.list (QCheck.Print.pair int64 int64)

  let task_seg_id = Daypack_lib.Task.task_seg_id_to_string

  let task_seg = QCheck.Print.(pair task_seg_id int64)

  let task_segs = QCheck.Print.list QCheck.Print.(pair task_seg_id int64)
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 0L |> min bound) ui64)

let nz_pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 1L |> min bound) ui64)

let pos_int64_gen = pos_int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64 = QCheck.make ~print:Print_utils.int64 pos_int64_gen

let small_pos_int64 =
  QCheck.make ~print:Print_utils.int64 (pos_int64_bound_gen 100L)

let nz_pos_int64_gen =
  QCheck.Gen.map (Int64.add 1L)
    (pos_int64_bound_gen (Int64.sub Int64.max_int 1L))

let nz_pos_int64 = QCheck.make ~print:Print_utils.int64 nz_pos_int64_gen

let tiny_sorted_time_slots =
  let open QCheck in
  make ~print:Print_utils.time_slots
    Gen.(
      map
        (fun (start, sizes_and_gaps) ->
           sizes_and_gaps
           |> List.fold_left
             (fun (last_end_exc, acc) (size, gap) ->
                let start =
                  match last_end_exc with
                  | None -> start
                  | Some x -> Int64.add x gap
                in
                let end_exc = Int64.add start size in
                (Some end_exc, (start, end_exc) :: acc))
             (None, [])
           |> fun (_, l) -> List.rev l)
        (pair
           (pos_int64_bound_gen 10_000L)
           (list_size (int_bound 5)
              (pair (pos_int64_bound_gen 20L) (pos_int64_bound_gen 20L)))))

let sorted_time_slots_maybe_gaps =
  let open QCheck in
  make ~print:Print_utils.time_slots
    Gen.(
      map
        (fun (start, sizes_and_gaps) ->
           sizes_and_gaps
           |> List.fold_left
             (fun (last_end_exc, acc) (size, gap) ->
                let start =
                  match last_end_exc with
                  | None -> start
                  | Some x -> Int64.add x (Int64.of_int gap)
                in
                let end_exc = Int64.add start (Int64.of_int size) in
                (Some end_exc, (start, end_exc) :: acc))
             (None, [])
           |> fun (_, l) -> List.rev l)
        (pair pos_int64_gen (list (pair nz_small_nat_gen small_nat))))

let sorted_time_slots_with_gaps =
  let open QCheck in
  make ~print:Print_utils.time_slots
    Gen.(
      map
        (fun (start, sizes_and_gaps) ->
           sizes_and_gaps
           |> List.fold_left
             (fun (last_end_exc, acc) (size, gap) ->
                let start =
                  match last_end_exc with
                  | None -> start
                  | Some x -> Int64.add x (Int64.of_int gap)
                in
                let end_exc = Int64.add start (Int64.of_int size) in
                (Some end_exc, (start, end_exc) :: acc))
             (None, [])
           |> fun (_, l) -> List.rev l)
        (pair pos_int64_gen (list (pair nz_small_nat_gen nz_small_nat_gen))))

let tiny_time_slots =
  let open QCheck in
  make ~print:Print_utils.time_slots
    Gen.(
      map
        (List.map (fun (start, size) -> (start, Int64.add start size)))
        (list_size (int_bound 5)
           (pair (pos_int64_bound_gen 10_000L) (pos_int64_bound_gen 20L))))

let time_slots =
  let open QCheck in
  make ~print:Print_utils.time_slots
    Gen.(
      map
        (List.map (fun (start, size) ->
             (start, Int64.add start (Int64.of_int size))))
        (list (pair ui64 small_nat)))

let task_seg_id_gen =
  let open QCheck.Gen in
  map
    (fun ((id1, id2, id3, id4), id5) -> (id1, id2, id3, id4, id5))
    (pair
       (quad pos_int64_gen pos_int64_gen pos_int64_gen pos_int64_gen)
       (opt pos_int64_gen))

let task_seg_id = QCheck.make task_seg_id_gen

let tiny_task_seg_gen =
  let open QCheck in
  Gen.(pair task_seg_id_gen (nz_pos_int64_bound_gen 20L))

let tiny_task_seg = QCheck.(make ~print:Print_utils.task_seg tiny_task_seg_gen)

let tiny_task_segs =
  let open QCheck in
  make ~print:Print_utils.task_segs
    Gen.(list_size (int_bound 5) tiny_task_seg_gen)
