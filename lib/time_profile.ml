type period = Time_pattern.t * Time_pattern.t

type data = { periods : period list }

let matching_time_slots_of_periods ~start ~end_exc (periods : period list) :
  Time_slot_ds.t Seq.t =
  let time_slots = [ (start, end_exc) ] in
  periods
  |> List.to_seq
  |> Seq.map (fun (start_pat, end_exc_pat) ->
      ( let start_seq =
          Time_pattern.matching_time_slots start_pat time_slots
          |> Seq.map (fun (x, _) -> x)
        in
        let end_exc_seq =
          Time_pattern.matching_time_slots end_exc_pat time_slots
          |> Seq.map (fun (_, y) -> y)
        in
        OSeq.map2
          (fun start end_exc -> (start, end_exc))
          start_seq end_exc_seq
        : Time_slot_ds.t Seq.t ))
  |> OSeq.merge
