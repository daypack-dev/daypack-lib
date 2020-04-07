%{
  open Time_expr_ast
%}

(* keywords *)
%token OF
%token TO

(* separators *)
%token HYPHEN
%token COLON
%token COMMA

(* weekdays *)
%token SUNDAY
%token MONDAY
%token TUESDAY
%token WEDNESDAY
%token THURSDAY
%token FRIDAY
%token SATURDAY

(* months *)
%token JANUARY

%start <Time_expr_ast.t> parse

%%

parse:
  | e = time_point_expr; EOF { Time_point_expr e }
  | e = time_slots_expr; EOF { Time_slots_expr e }

time_point_expr:
  | year = INTEGER; HYPHEN; month = INTEGER; day = INTEGER; CHARACTER;
    hour_minute = hour_minute_expr;
    {
      Year_month_day_hour_minute
        {
          year;
          month;
          day = Month_day day;
          hour_minute = { hour; minute };
        }
    }

time_slots_expr:
  | hour_minute = hour_minute_expr; OF; day = separated_list(COMMA, INTEGER

hour_minute_expr:
  | hour = INTEGER; COLON; minute = INTEGER
    {
      { hour; minute }
    }

month_expr:
  | x = INTEGER { Human_int_month x }
  | JANUARY { Direct_pick_month Jan }
