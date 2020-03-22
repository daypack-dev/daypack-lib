let substring_match (choices : (string * 'a) list) (s : string) :
  ('a, unit) result =
  let regexp = Str.regexp_case_fold s in
  let matches =
    choices
    |> List.filter (fun (k, _) ->
        try
          Str.search_forward regexp k 0 |> ignore;
          true
        with Not_found -> false)
  in
  match matches with [ (_, v) ] -> Ok v | _ -> Error ()
