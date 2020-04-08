open Time_expr_ast

module Interpret_string = struct
  let parse lexbuf : t = Time_expr_parser.parse Time_expr_lexer.read lexbuf

  let lexbuf_to_pos_str lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol - 1)

  let of_string (s : string) : (t, string) result =
    let lexbuf = Lexing.from_string s in
    try Ok (parse lexbuf)
    with
    | Time_expr_lexer.Syntax_error msg ->
      Error (Printf.sprintf "%s: %s" (lexbuf_to_pos_str lexbuf) msg)
    | Time_expr_parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (lexbuf_to_pos_str lexbuf))
end
