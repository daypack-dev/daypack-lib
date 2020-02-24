type data = { periods : (Time_pattern.t * Time_pattern.t) list }

let matching_time_slots_of_data ~start ~end_exc (data : data) :
  Time_slot_ds.t Seq.t =
  let time_slots = [ (start, end_exc) ] in
  List.to_seq data.periods
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
