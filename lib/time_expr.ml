open Time_expr_ast

module Interpret_string = struct
  let parse lexbuf : t = Time_expr_parser.parse Time_expr_lexer.read lexbuf

  let of_string (s : string) : t =
    let lexbuf = Lexing.from_string s in
    parse lexbuf
end
