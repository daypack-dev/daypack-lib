{
  open Lexing
  open Time_expr_parser

  exception Syntax_error of string

  let get = Lexing.lexeme
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let nat = ['0' - '9']+

(* weekdays *)
let sunday    = ['S' 's'] "unday"    | ['S' 's'] "un"
let monday    = ['M' 't'] "onday"    | ['M' 'm'] "on"
let tuesday   = ['T' 't'] "uesday"   | ['T' 't'] "ue"
let wednesday = ['W' 'w'] "ednesday" | ['W' 'w'] "ed"
let thursday  = ['T' 't'] "hursday"  | ['T' 't'] "hu"
let friday    = ['F' 'f'] "riday"    | ['F' 'f'] "fri"
let saturday  = ['S' 's'] "aturday"  | ['S' 's'] "at"

(* months *)
let january   = ['J' 'j'] "anuary"   | ['J' 'j'] "an"
let february  = ['F' 'f'] "ebruary"  | ['J' 'j'] "eb"
let march     = ['M' 'm'] "arch"     | ['J' 'j'] "ar"
let april     = ['A' 'a'] "pril"     | ['J' 'j'] "pr"
let may       = ['M' 'm'] "ay"
let june      = ['J' 'j'] "une"      | ['J' 'j'] "un"
let july      = ['J' 'j'] "uly"      | ['J' 'j'] "ul"
let august    = ['A' 'a'] "ugust"    | ['A' 'a'] "ug"
let september = ['S' 's'] "eptember" | ['S' 's'] "ep"
let october   = ['O' 'o'] "ctober"   | ['O' 'o'] "ct"
let november  = ['N' 'n'] "ovember"  | ['N' 'n'] "ov"
let december  = ['D' 'd'] "ecember"  | ['D' 'd'] "ec"

rule read =
  parse
  | white { read lexbuf }

  (* data *)
  | nat { NAT (get lexbuf |> int_of_string) }

  (* separators *)
  | "-" { HYPHEN }
  | "," { COMMA }
  | ":" { COLON }

  (* weekdays *)
  | sunday
    { SUNDAY }
  | monday
    { MONDAY }
  | tuesday
    { TUESDAY }
  | wednesday
    { WEDNESDAY }
  | thursday
    { THURSDAY }
  | friday
    { FRIDAY }
  | saturday
    { SATURDAY }

  (* months *)
  | january
    { JANUARY }
  | february
    { FEBRUARY }
  | march
    { MARCH }
  | april
    { APRIL }
  | may
    { MAY }
  | june
    { JUNE }
  | july
    { JULY }
  | august
    { AUGUST }
  | september
    { SEPTEMBER }
  | october
    { OCTOBER }
  | november
    { NOVEMBER }
  | december
    { DECEMBER }

  | _
    { raise (Syntax_error ("Unexpected char: " ^ (get lexbuf))) }
  | eof { EOF }
