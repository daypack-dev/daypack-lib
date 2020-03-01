type period = Time_pattern.t * Time_pattern.t

type data = { periods : period list }

type t = string * data

(* let matching_tm_seq_of_periods ~search_years_ahead (start : Unix.tm) :
 *   (Unix.tm Seq.t =
 *   periods
 *   |> List.to_seq
 *   |> Seq.map (fun (start_pat, end_exc_pat) ->
 *       let start_seq =
 *         Time_pattern.matching_tm_seq ~search_years_ahead start_pat start
 *       in
 *       let end_exc_seq =
 *         Time_pattern.matching_tm_seq ~search_years_ahead end_exc_pat start
 *       in
 *       OSeq.map2 (fun start end_exc -> (start, end_exc)) start_seq end_exc_seq
 *     )
 * |> OSeq.merge *)

let matching_time_slots_of_periods ~start ~end_exc (periods : period list) :
  Time_slot_ds.t Seq.t =
  let time_slots = [ (start, end_exc) ] in
  periods
  |> List.to_seq
  |> Seq.map (fun (start_pat, end_exc_pat) ->
      let start_seq =
        Time_pattern.matching_time_slots start_pat time_slots
        |> Seq.map (fun (x, _) -> x)
      in
      let end_exc_seq =
        Time_pattern.matching_time_slots end_exc_pat time_slots
        |> Seq.map (fun (_, y) -> y)
      in
      OSeq.map2 (fun start end_exc -> (start, end_exc)) start_seq end_exc_seq)
  |> OSeq.merge

let matching_time_slots_of_data ~start ~end_exc (data : data) :
  Time_slot_ds.t Seq.t =
  matching_time_slots_of_periods ~start ~end_exc data.periods

let matching_time_slots_of_profile ~start ~end_exc ((_id, data) : t) :
  Time_slot_ds.t Seq.t =
  matching_time_slots_of_periods ~start ~end_exc data.periods

module Serialize = struct
  let pack_period (start, end_exc) : Time_profile_t.period =
    ( Time_pattern.Serialize.pack_pattern start,
      Time_pattern.Serialize.pack_pattern end_exc )

  let pack_periods (periods : period list) : Time_profile_t.period list =
    List.map pack_period periods

  let pack_data (data : data) : Time_profile_t.data =
    { periods = pack_periods data.periods }

  let pack_profile ((id, data) : t) : Time_profile_t.t = (id, pack_data data)
end

module Deserialize = struct
  let unpack_period (start, end_exc) : period =
    ( Time_pattern.Deserialize.unpack_pattern start,
      Time_pattern.Deserialize.unpack_pattern end_exc )

  let unpack_periods (periods : Time_profile_t.period list) : period list =
    List.map unpack_period periods

  let unpack_data (data : Time_profile_t.data) : data =
    { periods = unpack_periods data.periods }

  let unpack_profile ((id, data) : Time_profile_t.t) : t = (id, unpack_data data)
end
