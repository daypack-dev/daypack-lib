type t = int64 * int64

let to_string ((start, end_exc) : t) : string =
  Printf.sprintf "[%Ld, %Ld)" start end_exc

module Multi = struct end

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
