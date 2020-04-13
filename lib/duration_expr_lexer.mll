{
  open Duration_expr_parser

  exception Syntax_error of string

  let get = Lexing.lexeme
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let nat = ['0' - '9']+

rule read =
  parse
  | white { read lexbuf }

  (* data *)
  | nat { NAT (get lexbuf |> int_of_string) }

  (* separators *)
  | "," { COMMA }

  (* keywords *)
  | "min"
  | "mins"
  | "minute"
  | "minutes"
    { MINUTES }

  | "hr"
  | "hrs"
  | "hour"
  | "hours"
    { HOURS }

  | "day"
  | "days"
    { DAYS }

  | _
    { raise (Syntax_error ("Unexpected char: " ^ (get lexbuf))) }
  | eof { EOF }
