type t = int64 * int64

let to_string ((start, end_exc) : t) : string =
  Printf.sprintf "[%Ld, %Ld)" start end_exc

let join ((start1, end_exc1) : t) ((start2, end_exc2) : t) : t option =
  let aux (start1, end_exc1) (start2, end_exc2) =
    if start2 <= end_exc1 then
      Some (start1, max end_exc1 end_exc2)
    else
      None
  in
  if start1 <= start2 then
    aux (start1, end_exc1) (start2, end_exc2)
  else
    aux (start2, end_exc2) (start1, end_exc1)

module Check = struct
  let check_time_slot ((start, end_exc) : t) : bool =
    0L <= start && start <= end_exc
end

module Serialize = struct
  let pack_time_slot (start, end_exc) =
    ( Misc_utils.int32_int32_of_int64 start,
      Misc_utils.int32_int32_of_int64 end_exc )
end

module Deserialize = struct
  let unpack_time_slot (start, end_exc) =
    ( Misc_utils.int64_of_int32_int32 start,
      Misc_utils.int64_of_int32_int32 end_exc )
end
