type t =
  | Time_slots of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      time_slots : Time_slot.t list;
    }
  | Years_ahead_start_unix_second of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_date_time of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : Time.date_time;
      search_years_ahead : int;
    }

type error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead
  | Too_far_into_future

let search_using_tz_offset_s_of_search_param (param : t) :
  Time.tz_offset_s option =
  match param with
  | Time_slots { search_using_tz_offset_s; _ } -> search_using_tz_offset_s
  | Years_ahead_start_unix_second { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s
  | Years_ahead_start_date_time { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s

let push_search_param_to_later_start ~(start : int64)
    (search_param : t) : (t, unit) result =
  match search_param with
  | Time_slots { search_using_tz_offset_s; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> Ok search_param
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slots.inter (Seq.return (start, end_exc'))
          |> List.of_seq
        in
        Ok (Time_slots { search_using_tz_offset_s; time_slots }) )
  | Years_ahead_start_unix_second
      { search_using_tz_offset_s; start = start'; search_years_ahead } ->
    let start = max start' start in
    Ok
      (Years_ahead_start_unix_second
         { search_using_tz_offset_s; start; search_years_ahead })
  | Years_ahead_start_date_time
      { search_using_tz_offset_s; start = start'; search_years_ahead } -> (
      match Time.unix_second_of_date_time start' with
      | Error () -> Error ()
      | Ok start' ->
        let start = max start' start in
        Time.date_time_of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s start
        |> Result.map (fun start ->
            Years_ahead_start_date_time
              { search_using_tz_offset_s; start; search_years_ahead }) )
