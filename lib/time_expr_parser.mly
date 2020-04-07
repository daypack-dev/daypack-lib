%{
  open Time_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT

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
%token FEBRUARY
%token MARCH
%token APRIL
%token MAY
%token JUNE
%token JULY
%token AUGUST
%token SEPTEMBER
%token OCTOBER
%token NOVEMBER
%token DECEMBER

%start <Time_expr_ast.t> parse

%%

parse:
  | e = time_point_expr; EOF { Time_point_expr e }
  | e = time_slots_expr; EOF { Time_slots_expr e }
  ;

time_point_expr:
  | year = NAT; HYPHEN; month = NAT; HYPHEN; month_day = NAT;
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
  | JANUARY   { Direct_pick_month Jan }
  | FEBRUARY  { Direct_pick_month Feb }
  | MARCH     { Direct_pick_month Mar }
  | APRIL     { Direct_pick_month Apr }
  | MAY       { Direct_pick_month May }
  | JUNE      { Direct_pick_month Jun }
  | JULY      { Direct_pick_month Jul }
  | AUGUST    { Direct_pick_month Aug }
  | SEPTEMBER { Direct_pick_month Sep }
  | OCTOBER   { Direct_pick_month Oct }
  | NOVEMBER  { Direct_pick_month Nov }
  | DECEMBER  { Direct_pick_month Dec }
  ;
