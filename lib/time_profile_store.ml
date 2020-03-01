type t = Time_profile.data String_map.t

module Serialize = struct
  let pack_store (t : t) : Time_profile_store_t.t =
    t |>
    String_map.to_seq
    |> Seq.map (fun (id, data) ->
        id, Time_profile.Serialize.pack_data data)
    |> List.of_seq
end
