let prefix_string_match (choices : (string * 'a) list) (s : string) :
  (string * 'a) list =
  let regexp = Str.regexp_case_fold s in
  choices
  |> List.filter (fun (k, _) ->
      try Str.search_forward regexp k 0 = 0 with Not_found -> false)
