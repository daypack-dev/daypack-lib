open Angstrom

let alpha_string : string t =
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let ident_string ~(reserved_words : string list) : string t =
  let reserved_words = List.map String.lowercase_ascii reserved_words in
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  >>= fun s ->
  if List.mem (String.lowercase_ascii s) reserved_words then
    fail (Printf.sprintf "\"%s\" is a reserved word" s)
  else return s

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

let sep_by_comma1 (p : 'a t) : 'a list t = sep_by1 (space *> comma *> space) p
