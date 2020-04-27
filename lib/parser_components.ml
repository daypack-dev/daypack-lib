open Angstrom

let alpha_string : string t =
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let skip_non_num_string ~delim : unit t =
  skip_while (function
      | '0' .. '9' -> false
      | c -> ( match delim with None -> true | Some x -> c <> x ))

let nat_zero : int t =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>= fun s ->
  try return (int_of_string s)
  with _ -> fail (Printf.sprintf "Integer %s out of range" s)

let space : unit t = skip_while (function ' ' | '\t' -> true | _ -> false)

let comma = char ','

let dot = char '.'

let hyphen = char '-'
