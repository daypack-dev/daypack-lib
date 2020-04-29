let weekdays = [ `Mon; `Tue; `Wed; `Thu; `Fri ]

let weekend = [ `Sat; `Sun ]

let make_time_pattern_weekday ~hour ?(min = 0) weekday :
  Daypack_lib.Time_pattern.t =
  {
    years = [];
    months = [];
    weekdays = [ weekday ];
    month_days = [];
    hours = [ hour ];
    minutes = [ min ];
    seconds = [];
    unix_times = [];
  }

let make_time_profile_single_weekday ~start_hour ?(start_min = 0) ~end_exc_hour
    ?(end_exc_min = 0) weekday : Daypack_lib.Time_profile.t =
  ( Daypack_lib.Time.To_string.string_of_weekday weekday
    |> String.lowercase_ascii,
    {
      periods =
        [
          ( make_time_pattern_weekday ~hour:start_hour ~min:start_min weekday,
            make_time_pattern_weekday ~hour:end_exc_hour ~min:end_exc_min
              weekday );
        ];
    } )

let make_time_profiles_weekdays ~start_hour ?(start_min = 0) ~end_exc_hour
    ?(end_exc_min = 0) () : Daypack_lib.Time_profile.t list =
  List.map
    (fun weekday ->
       make_time_profile_single_weekday ~start_hour ~start_min ~end_exc_hour
         ~end_exc_min weekday)
    weekdays

let profiles_9to5 : Daypack_lib.Time_profile.t list =
  ( "weekday-9to5",
    {
      periods =
        List.map
          (fun day ->
             ( make_time_pattern_weekday day ~hour:9,
               make_time_pattern_weekday day ~hour:17 ))
          weekdays;
    } )
  :: ( "weekend-9to5",
       {
         periods =
           List.map
             (fun day ->
                ( make_time_pattern_weekday day ~hour:9,
                  make_time_pattern_weekday day ~hour:17 ))
             weekdays;
       } )
  :: make_time_profiles_weekdays ~start_hour:9 ~end_exc_hour:17 ()

let profiles : Daypack_lib.Time_profile.t list = []
