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

  let write_to_dir ~(dir : string) (t : t) : (unit, string) result =
    try
      if Sys.is_directory dir then Error "File is not a directory"
      else
        t.profiles
        |> String_map.to_seq
        |> Seq.map (fun (name, data) ->
            ( name,
              data
              |> Time_profile.Serialize.pack_data
              |> Time_profile_j.string_of_data ))
        |> Seq.iter (fun (name, data) ->
            let path = Filename.concat dir name in
            let oc = open_out path in
            Fun.protect
              ~finally:(fun () -> close_out oc)
              (fun () -> output_string oc data))
        |> Result.ok
    with Sys_error msg -> Error msg
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

  let read_from_dir ~(dir : string) : (t, string) result =
    try
      let profiles =
        Sys.readdir dir
        |> Array.to_list
        |> List.filter_map (fun s ->
            Filename.chop_suffix_opt ~suffix:".json" s
            |> Option.map (fun name -> (name, Filename.concat dir s)))
        |> List.map (fun (name, path) ->
            let ic = open_in path in
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () ->
                 let s = really_input_string ic (in_channel_length ic) in
                 (name, s)))
        |> List.map (fun (name, s) ->
            let data =
              s
              |> Time_profile_j.data_of_string
              |> Time_profile.Deserialize.unpack_data
            in
            (name, data))
        |> List.to_seq
        |> String_map.of_seq
      in
      Ok { profiles }
    with Sys_error msg -> Error msg
end
