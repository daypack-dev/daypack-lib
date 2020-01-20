type t = {
  year : int option;
  month : int option;
  day : int option;
  hour : int option;
  minute : int option;
}

let matching_time_slots  (t : t) (time_slots : Time_slot.t list) : time_slots =
