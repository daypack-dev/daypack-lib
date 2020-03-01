type t = { mutable profiles : Time_profile.data String_map.t }

let make_empty () : t = { profiles = String_map.empty }

let matching_time_slots_of_profile =
  let cache : (string, Time_slot_ds.t list) Hashtbl.t = Hashtbl.create 100 in
  fun ~start ~end_exc ~(profile : string) (t : t) ->
    ( match Hashtbl.find_opt cache profile with
      | None ->
        String_map.find_opt profile t.profiles
        |> Option.map (fun data ->
            let time_slots =
              Time_profile.matching_time_slots_of_data ~start ~end_exc data
              |> List.of_seq
            in
            Hashtbl.add cache profile time_slots;
            time_slots)
      | Some time_slots -> Some time_slots
                           : Time_slot_ds.t list option )

let add_profile ~(profile : string) (data : Time_profile.data) (t : t) : unit =
  t.profiles <- String_map.add profile data t.profiles

module Serialize = struct
  let pack_store (t : t) : Time_profile_store_t.t =
    t.profiles
    |> String_map.to_seq
    |> Seq.map Time_profile.Serialize.pack_profile
    |> List.of_seq
end

module Deserialize = struct
  let unpack_store (t : Time_profile_store_t.t) : t =
    let profiles =
      t
      |> List.to_seq
      |> Seq.map Time_profile.Deserialize.unpack_profile
      |> String_map.of_seq
    in
    { profiles }
end
