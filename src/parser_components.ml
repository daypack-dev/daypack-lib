open CCParse

let alpha_string : string t = chars1_if is_alpha

let any_string : string t = chars_if (fun _ -> true)

let take_space : string t = chars_if is_space

let ident_string ~(reserved_words : string list) : string t =
  let reserved_words = List.map String.lowercase_ascii reserved_words in
  chars1_if is_alpha
  >>= fun s ->
  if List.mem (String.lowercase_ascii s) reserved_words then
    failf "\"%s\" is a reserved word" s
  else return s

let skip_non_num_string ~end_markers : unit t =
  skip_chars (function
      | '0' .. '9' -> false
      | c -> ( match end_markers with None -> true | Some x -> not (String.contains x c) ))

let nat_zero : int t =
  chars1_if (function '0' .. '9' -> true | _ -> false)
  >>= fun s ->
  try return (int_of_string s)
  with _ -> fail (Printf.sprintf "Integer %s is out of range" s)

let comma = char ','

let dot = char '.'

let hyphen = char '-'

let non_square_bracket_string = chars_if (function ']' -> false | _ -> true)

let sep_by_comma (p : 'a t) : 'a list t =
  sep ~by:(skip_space *> comma *> skip_space) p

let sep_by_comma1 (p : 'a t) : 'a list t =
  sep1 ~by:(skip_space *> comma *> skip_space) p

let option (default : 'a) p : 'a t = try_ p <|> return default

let skip_space1 = char_if is_space *> skip_space

let get_first_line_error_msg (s : string) : string =
  List.hd (String.split_on_char '\n' s)

let map_first_line_error_msg (x : ('a, string) result) : ('a, string) result =
  Result.map_error get_first_line_error_msg x

let parse_string (p : 'a t) s : ('a, string) result =
  parse_string p s |> map_first_line_error_msg

let shift_pos_of_error_msg ~(incre : int) (s : string) : string =
  try
    Scanf.sscanf s "%[^,], pos: %d" (fun s pos ->
        Printf.sprintf "%s, pos: %d" s (pos + incre)
      )
  with
  | Scanf.Scan_failure _ -> s

let sep_res_seq ~by ~end_markers (p : 'a t) : ('a, string) result Seq.t t =
  sep ~by:(char by)
    (get_cnum >>= fun cnum -> chars1_if (fun c ->
         c <> by &&
         not (String.contains end_markers c)
       ) >>= fun s -> return (cnum, s)
    )
  >>= fun l ->
  return (l |> List.to_seq |> Seq.map (fun (cnum, s) ->
      parse_string p s
      |> Result.map_error (fun s ->
          shift_pos_of_error_msg ~incre:cnum s
        )
    ))

let sep_res ~by ~end_markers (p : 'a t) : ('a, string) result list t =
  sep_res_seq ~by ~end_markers p >>=
  fun s -> return (List.of_seq s)

let sep_fail_on_first_fail ~by ~end_markers (p : 'a t) : 'a list t =
  sep_res_seq ~by ~end_markers p >>=
  fun s -> match Seq_utils.find_first_error_or_return_whole_list s with
  | Ok l -> return l
  | Error s -> fail s
