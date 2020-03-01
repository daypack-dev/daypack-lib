type t = Time_profile.data String_map.t

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
