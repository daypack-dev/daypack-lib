open Test_utils

module Qc = struct
  let slice_start =
    QCheck.Test.make ~count:10_000 ~name:"Qc.slice_start"
      QCheck.(pair pos_int64 sorted_time_slots_maybe_gaps)
      (fun (start, l) ->
         l
         |> List.to_seq
         |> Daypack_lib.Time_slots.Slice.slice ~start
         |> List.of_seq
         |> List.for_all (fun (x, _) -> start <= x))

  let slice_end_exc =
    QCheck.Test.make ~count:10_000 ~name:"Qc.slice_end_exc"
      QCheck.(pair pos_int64 sorted_time_slots_maybe_gaps)
      (fun (end_exc, l) ->
         l
         |> List.to_seq
         |> Daypack_lib.Time_slots.Slice.slice ~end_exc
         |> List.of_seq
         |> List.for_all (fun (_, y) -> y <= end_exc))

  let normalize_pairs_are_fine =
    QCheck.Test.make ~count:10_000 ~name:"Qc.normalize_pairs_are_fine"
      time_slots (fun l ->
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.Normalize.normalize
          |> List.of_seq
          |> List.for_all (fun (x, y) -> x <= y))

  let normalize_time_slots_are_sorted =
    QCheck.Test.make ~count:10_000 ~name:"Qc.normalize_time_slots_are_sorted"
      time_slots (fun l ->
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.Normalize.normalize
          |> List.of_seq
          |> List.fold_left
            (fun (res, last) (x, y) ->
               if res then
                 match last with
                 | None -> (true, Some (x, y))
                 | Some (last_start, last_end_exc) ->
                   (last_start <= x && last_end_exc <= x, Some (x, y))
               else (false, None))
            (true, None)
          |> fun (x, _) -> x)

  let normalize_time_slots_are_unique =
    QCheck.Test.make ~count:10_000 ~name:"Qc.normalize_time_slots_are_unique"
      time_slots (fun l ->
          let l =
            l
            |> List.to_seq
            |> Daypack_lib.Time_slots.Normalize.normalize
            |> List.of_seq
          in
          List.length (List.sort_uniq compare l) = List.length l)

  let normalize_time_slots_are_disjoint_with_gaps =
    QCheck.Test.make ~count:10_000
      ~name:"Qc.normalize_time_slots_are_disjoint_with_gaps" time_slots
      (fun l ->
         l
         |> List.to_seq
         |> Daypack_lib.Time_slots.Normalize.normalize
         |> Seq.fold_left
           (fun (res, last) (x, y) ->
              if res then
                match last with
                | None -> (true, Some (x, y))
                | Some (_, last_end_exc) -> (last_end_exc < x, Some (x, y))
              else (false, None))
           (true, None)
         |> fun (x, _) -> x)

  let normalize_idempotent_wrt_normalized_time_slots =
    QCheck.Test.make ~count:10_000
      ~name:"Qc.normalize_idempotent_wrt_normalized_time_slots"
      sorted_time_slots_with_gaps (fun l ->
          l
          |> List.to_seq
          |> Daypack_lib.Time_slots.Normalize.normalize
          |> List.of_seq
             = l)

  let invert_disjoint_from_original =
    QCheck.Test.make ~count:10_000 ~name:"Qc.invert_disjoint_from_original"
      QCheck.(triple pos_int64 pos_int64 sorted_time_slots_maybe_gaps)
      (fun (start, end_exc, l) ->
         QCheck.assume (start <= end_exc);
         let sliced =
           l
           |> List.to_seq
           |> Daypack_lib.Time_slots.Slice.slice ~start ~end_exc
           |> List.of_seq
         in
         let inverted =
           l
           |> List.to_seq
           |> Daypack_lib.Time_slots.invert ~start ~end_exc
           |> List.of_seq
         in
         let sliced_count = List.length sliced in
         let inverted_count = List.length inverted in
         List.length (List.sort_uniq compare (sliced @ inverted))
         = sliced_count + inverted_count)

  let invert_fit_gaps =
    QCheck.Test.make ~count:10_000 ~name:"Qc.invert_fit_gaps"
      QCheck.(triple pos_int64 pos_int64 sorted_time_slots_maybe_gaps)
      (fun (start, end_exc, l) ->
         QCheck.assume (start < end_exc);
         let res =
           l
           |> List.to_seq
           |> Daypack_lib.Time_slots.invert ~start ~end_exc
           |> List.of_seq
           |> (fun inverted ->
               ( Daypack_lib.Time_slots.Slice.slice ~start ~end_exc
                   (List.to_seq l)
                 |> List.of_seq )
               @ inverted)
           |> List.to_seq
           |> Daypack_lib.Time_slots.Normalize.normalize
           |> List.of_seq
         in
         (l <> [] && List.for_all (fun (x, y) -> y < start || end_exc < x) l)
         || [ (start, end_exc) ] = res)

  let relatvie_complement_result_disjoint_from_not_mem_of =
    QCheck.Test.make ~count:10_000
      ~name:"Qc.relative_complement_disjoint_from_not_mem_of"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (mem_of, not_mem_of) ->
         let res =
           Daypack_lib.Time_slots.relative_complement
             ~not_mem_of:(List.to_seq not_mem_of) (List.to_seq mem_of)
           |> List.of_seq
         in
         let not_mem_of_count = List.length not_mem_of in
         let res_count = List.length res in
         List.length (List.sort_uniq compare (not_mem_of @ res))
         = not_mem_of_count + res_count)

  let relatvie_complement_result_subset_of_mem_of =
    QCheck.Test.make ~count:10_000
      ~name:"Qc.relatvie_complement_result_subset_of_mem_of"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (mem_of, not_mem_of) ->
         let res_s =
           Daypack_lib.Time_slots.relative_complement
             ~not_mem_of:(List.to_seq not_mem_of) (List.to_seq mem_of)
         in
         let res = res_s |> List.of_seq in
         Daypack_lib.Time_slots.intersect (List.to_seq mem_of) res_s
         |> List.of_seq
            = res)

  let relatvie_complement_self =
    QCheck.Test.make ~count:10_000 ~name:"Qc.relatvie_complement_self"
      sorted_time_slots_maybe_gaps (fun l ->
          let s = List.to_seq l in
          Daypack_lib.Time_slots.relative_complement ~not_mem_of:s s
          |> List.of_seq
             = [])

  let intersect_with_self =
    QCheck.Test.make ~count:10_000 ~name:"Qc.intersect_with_self"
      sorted_time_slots_maybe_gaps (fun l ->
          let s = l |> List.to_seq in
          let res = Daypack_lib.Time_slots.intersect s s |> List.of_seq in
          l = res)

  let intersect_commutative =
    QCheck.Test.make ~count:10_000 ~name:"Qc.intersect_commutative"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (l1, l2) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let inter1 = Daypack_lib.Time_slots.intersect s1 s2 |> List.of_seq in
         let inter2 = Daypack_lib.Time_slots.intersect s2 s1 |> List.of_seq in
         inter1 = inter2)

  let intersect_associative =
    QCheck.Test.make ~count:10_000 ~name:"Qc.intersect_associative"
      QCheck.(
        triple sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let s3 = l3 |> List.to_seq in
         let inter1 =
           Daypack_lib.Time_slots.(intersect (intersect s1 s2) s3) |> List.of_seq
         in
         let inter2 =
           Daypack_lib.Time_slots.(intersect s1 (intersect s2 s3)) |> List.of_seq
         in
         inter1 = inter2)

  let union_with_self =
    QCheck.Test.make ~count:10_000 ~name:"Qc.union_with_self"
      sorted_time_slots_with_gaps (fun l ->
          let s = l |> List.to_seq in
          let res = Daypack_lib.Time_slots.Union.union s s |> List.of_seq in
          l = res)

  let union_commutative =
    QCheck.Test.make ~count:10_000 ~name:"Qc.union_commutative"
      QCheck.(pair sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps)
      (fun (l1, l2) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let inter1 = Daypack_lib.Time_slots.Union.union s1 s2 |> List.of_seq in
         let inter2 = Daypack_lib.Time_slots.Union.union s2 s1 |> List.of_seq in
         inter1 = inter2)

  let union_associative =
    QCheck.Test.make ~count:10_000 ~name:"Qc.union_associative"
      QCheck.(
        triple sorted_time_slots_with_gaps sorted_time_slots_with_gaps
          sorted_time_slots_with_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let s3 = l3 |> List.to_seq in
         let res1 =
           Daypack_lib.Time_slots.(Union.union (Union.union s1 s2) s3)
           |> List.of_seq
         in
         let res2 =
           Daypack_lib.Time_slots.(Union.union s1 (Union.union s2 s3))
           |> List.of_seq
         in
         res1 = res2)

  let intersect_union_distributive1 =
    QCheck.Test.make ~count:10_000 ~name:"Qc.intersect_union_distributive1"
      QCheck.(
        triple sorted_time_slots_maybe_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let s3 = l3 |> List.to_seq in
         let res1 =
           Daypack_lib.Time_slots.(Union.union s1 (intersect s2 s3))
           |> List.of_seq
         in
         let res2 =
           Daypack_lib.Time_slots.(
             intersect (Union.union s1 s2) (Union.union s1 s3))
           |> List.of_seq
         in
         res1 = res2)

  let intersect_union_distributive2 =
    QCheck.Test.make ~count:10_000 ~name:"Qc.intersect_union_distributive2"
      QCheck.(
        triple sorted_time_slots_with_gaps sorted_time_slots_maybe_gaps
          sorted_time_slots_maybe_gaps)
      (fun (l1, l2, l3) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let s3 = l3 |> List.to_seq in
         let res1 =
           Daypack_lib.Time_slots.(intersect s1 (Union.union s2 s3))
           |> List.of_seq
         in
         let res2 =
           Daypack_lib.Time_slots.(
             Union.union (intersect s1 s2) (intersect s1 s3))
           |> List.of_seq
         in
         res1 = res2)

  let merge =
    QCheck.Test.make ~count:10_000 ~name:"Qc.merge"
      QCheck.(
        pair sorted_time_slots_with_overlaps sorted_time_slots_with_overlaps)
      (fun (l1, l2) ->
         let s1 = l1 |> List.to_seq in
         let s2 = l2 |> List.to_seq in
         let res1 = Daypack_lib.Time_slots.Merge.merge s1 s2 |> List.of_seq in
         let res2 = Daypack_lib.Time_slots.Sort.sort_time_slots_list (l1 @ l2) in
         res1 = res2)

  let suite =
    [
      slice_start;
      slice_end_exc;
      normalize_pairs_are_fine;
      normalize_time_slots_are_sorted;
      normalize_time_slots_are_unique;
      normalize_time_slots_are_disjoint_with_gaps;
      normalize_idempotent_wrt_normalized_time_slots;
      invert_disjoint_from_original;
      invert_fit_gaps;
      relatvie_complement_result_disjoint_from_not_mem_of;
      relatvie_complement_result_subset_of_mem_of;
      relatvie_complement_self;
      intersect_with_self;
      intersect_commutative;
      intersect_associative;
      union_with_self;
      union_commutative;
      union_associative;
      intersect_union_distributive1;
      intersect_union_distributive2;
      merge;
    ]
end
