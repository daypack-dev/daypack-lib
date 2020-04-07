{
  open Lexing
  open Time_expr_parser

  exception Syntax_error of string
}

(* characters *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | "thursday"
    { THURSDAY }
