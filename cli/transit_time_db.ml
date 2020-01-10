type transit_time_record = int64 Int64_map.t

type t = (string * string, transit_time_record) Hashtbl.t
