let weekdays = [
  `Mon; `Tue; `Wed; `Thu; `Fri
]

let weekend = [
  `Sat; `Sun
]

let make_time_pattern_weekday ~hour ?(min = 0) weekday : Daypack_lib.Time_pattern.t =
  {
    years = [];
    months = [];
    days = `Weekdays [weekday];
    hours = [hour];
    minutes = [min];
  }

(* let make_time_profile_weekday ~hour ?(min = 0) weekday : Daypack_lib.Time_profile.t =
 *   ()
 *   {
 *     periods = [
 *       ( make_time_pattern_weekday ~hour ~min weekday
 *       )
 *     ]
 *   } *)

let profiles_9to5 : Daypack_lib.Time_profile.t list = [
  ("weekday-9to5",
   {
     periods = List.map (fun day ->
       (make_time_pattern_weekday day ~hour:9,
        make_time_pattern_weekday day ~hour:17)
     ) weekdays
   }
  );
]

let profiles : Daypack_lib.Time_profile.t list = [
]
