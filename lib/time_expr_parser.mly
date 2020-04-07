%{
  open Time_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT
%token <char> CHAR

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
  ;

time_point_expr:
  | year = NAT; HYPHEN; month = NAT; month_day = NAT; CHAR;
    hour_minute = hour_minute_expr;
    {
      match Time.month_of_human_int month with
      | Ok month ->
        Year_month_day_hour_minute
          {
            year;
            month = Direct_pick_month month;
            month_day;
            hour_minute;
          }
      | Error () ->
        failwith "Failed to interpret month int"
    }
  ;

time_slots_expr:
  | hour_minutes = hour_minutes_expr; OF; days = separated_list(COMMA, day_expr);
    {
      Hour_minutes_of_day_list { hour_minutes; days }
    }
  ;

hour_minutes_expr:
  | x = hour_minute_expr;
    {
      Single x
    }
  | start = hour_minute_expr; COMMA; end_exc = hour_minute_expr;
    {
      Ranged (start, end_exc)
    }
  ;

hour_minute_expr:
  | hour = NAT; COLON; minute = NAT
    {
      { hour; minute }
    }
  ;

day_expr:
  | SUNDAY  { Weekday `Sun }
  | x = NAT { Month_day x }

month_expr:
  | x = NAT { Human_int_month x }
  | JANUARY { Direct_pick_month Jan }
  ;
