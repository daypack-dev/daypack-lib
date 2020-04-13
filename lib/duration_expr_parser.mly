%{
  open Duration_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT

(* keywords *)
%token MINUTES
%token HOURS
%token DAYS

(* separators *)
%token COMMA

%start <Duration_expr_ast.t> parse

%%

parse:
  | minutes = NAT; MINUTES; EOF;
    {
      {
        days = 0;
        hours = 0;
        minutes;
      }
    }
  | hours = NAT; HOURS; minutes = NAT; MINUTES; EOF;
  | hours = NAT; HOURS; COMMA; minutes = NAT; MINUTES; EOF;
    {
      {
        days = 0;
        hours;
        minutes;
      }
    }
  | days = NAT; DAYS; hours = NAT; HOURS; minutes = NAT; MINUTES; EOF;
  | days = NAT; DAYS; COMMA; hours = NAT; HOURS; COMMA; minutes = NAT; MINUTES; EOF;
    {
      {
        days;
        hours;
        minutes;
      }
    }
  | days = NAT; DAYS; EOF;
    {
      {
        days;
        hours = 0;
        minutes = 0;
      }
    }
  | hours = NAT; HOURS; EOF;
    {
      {
        days = 0;
        hours;
        minutes = 0;
      }
    }
  | days = NAT; DAYS; hours = NAT; HOURS; EOF;
  | days = NAT; DAYS; COMMA; hours = NAT; HOURS; EOF;
    {
      {
        days;
        hours;
        minutes = 0;
      }
    }
