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
  | year = NAT; HYPHEN; month = month_expr; HYPHEN; month_day = NAT; hour_minute = hour_minute_expr;
    {
      Year_month_day_hour_minute
        {
          year;
          month;
          month_day;
          hour_minute;
        }
    }
  | HYPHEN; month = month_expr; HYPHEN; month_day = NAT; hour_minute = hour_minute_expr;
    {
      Month_day_hour_minute
        {
          month;
          month_day;
          hour_minute;
        }
    }
  | HYPHEN; HYPHEN; day = day_expr; hour_minute = hour_minute_expr;
    {
      Day_hour_minute
        {
          day;
          hour_minute;
        }
    }
  | HYPHEN; HYPHEN; hour_minute = hour_minute_expr;
    {
      Hour_minute hour_minute
    }
  ;

time_slots_expr:
  | hour_minutes = hour_minutes_expr; OF; days = separated_list(COMMA, day_expr);
    {
      Hour_minutes_of_day_list { hour_minutes; days }
    }
  | hour_minutes = hour_minutes_expr; OF; day_start = weekday_expr; TO; day_end_inc = weekday_expr;
    {
      Hour_minutes_of_day_range { hour_minutes; days = Weekday_range (day_start, day_end_inc) }
    }
  | hour_minutes = hour_minutes_expr; OF; day_start = month_day_expr; TO; day_end_inc = month_day_expr;
    {
      Hour_minutes_of_day_range { hour_minutes; days = Month_day_range (day_start, day_end_inc) }
    }
  ;

hour_minutes_expr:
  | start = hour_minute_expr;
    {
      Range_inc (start, start)
    }
  | start = hour_minute_expr; TO; end_exc = hour_minute_expr;
    {
      Range_exc (start, end_exc)
    }
  ;

hour_minute_expr:
  | hour = NAT; COLON; minute = NAT
    {
      { hour; minute }
    }
  ;

month_day_expr:
  | x = NAT { x }

weekday_expr:
  | SUNDAY    { `Sun }
  | MONDAY    { `Mon }
  | TUESDAY   { `Tue }
  | WEDNESDAY { `Wed }
  | THURSDAY  { `Thu }
  | FRIDAY    { `Fri }
  | SATURDAY  { `Sat }

day_expr:
  | x = month_day_expr { Month_day x }
  | x = weekday_expr   { Weekday x }

month_expr:
  | x = NAT   { Human_int_month x }
  | JANUARY   { Direct_pick_month `Jan }
  | FEBRUARY  { Direct_pick_month `Feb }
  | MARCH     { Direct_pick_month `Mar }
  | APRIL     { Direct_pick_month `Apr }
  | MAY       { Direct_pick_month `May }
  | JUNE      { Direct_pick_month `Jun }
  | JULY      { Direct_pick_month `Jul }
  | AUGUST    { Direct_pick_month `Aug }
  | SEPTEMBER { Direct_pick_month `Sep }
  | OCTOBER   { Direct_pick_month `Oct }
  | NOVEMBER  { Direct_pick_month `Nov }
  | DECEMBER  { Direct_pick_month `Dec }
  ;
