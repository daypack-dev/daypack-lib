open Int64_utils

module Interpret_string = struct
  let parse lexbuf : Duration_expr_ast.t =
    Duration_expr_parser.parse Duration_expr_lexer.read lexbuf

  let lexbuf_to_pos_str lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 1)

  let of_string (s : string) : (int64, string) result =
    let lexbuf = Lexing.from_string s in
    try
      let Duration_expr_ast.{ days; hours; minutes } = parse lexbuf in
      if days < 0 then Error "Day count is negative"
      else if hours < 0 then Error "Hour count is negative"
      else if minutes < 0 then Error "Minute count is negative"
      else
        let days = Int64.of_int days in
        let hours = Int64.of_int hours in
        let minutes = Int64.of_int minutes in
        Ok
          ( (days *^ Time.day_to_second_multiplier)
            +^ (hours *^ Time.hour_to_second_multiplier)
            +^ (minutes *^ Time.hour_to_second_multiplier) )
    with
    | Duration_expr_lexer.Syntax_error msg ->
      Error (Printf.sprintf "%s: %s" (lexbuf_to_pos_str lexbuf) msg)
    | Duration_expr_parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (lexbuf_to_pos_str lexbuf))
end

module To_string = struct
  let human_readable_string_of_duration (duration : int64) : string =
    let minutes = Int64.div duration 60L in
    let hours = Int64.div minutes 60L in
    let days = Int64.div hours 24L in
    if days > 0L then
      Printf.sprintf "%Ld days %Ld hours %Ld mins" days (Int64.rem hours 24L)
        (Int64.rem minutes 60L)
    else if hours > 0L then
      Printf.sprintf "%Ld hours %Ld mins" hours (Int64.rem minutes 60L)
    else Printf.sprintf "%Ld mins" minutes
end
