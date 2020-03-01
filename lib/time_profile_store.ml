type t = Time_profile.data String_map.t

let matching_time_slots_of_profile ~start ~end_exc ~(profile : string) (t : t) : Time_slot_ds.t Seq.t option =
  String_map.find_opt profile t
|>
  Option.map (fun profile ->
    Time_profile.matching_time_slots_of_data ~start ~end_exc profile
    )

module Serialize = struct
  let pack_store (t : t) : Time_profile_store_t.t =
    t |>
    String_map.to_seq
    |> Seq.map Time_profile.Serialize.pack_profile
    |> List.of_seq
end

module Deserialize = struct
  let unpack_store (t : Time_profile_store_t.t) : t =
    t
    |> List.to_seq
    |> Seq.map Time_profile.Deserialize.unpack_profile
    |> String_map.of_seq
end
