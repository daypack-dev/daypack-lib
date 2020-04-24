open Angstrom

let alpha_string : string t =
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let integer : int t =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let space : unit t = skip_while (function ' ' | '\t' -> true | _ -> false)

let comma = char ','

let dot = char '.'

let hyphen = char '-'

