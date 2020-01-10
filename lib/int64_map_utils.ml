include Map_utils.Make (Int64_map)
module Int64_bucketed = Map_utils.Make_bucketed (Int64_map) (Int64_set)
