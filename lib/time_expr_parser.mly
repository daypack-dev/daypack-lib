%{
  open Time_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT

(* keywords *)
%token OF
%token TO
%token EVERY
%token NEXT
%token AM
%token PM

(* separators *)
%token HYPHEN
%token COLON
%token COMMA
%token DOT

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
  | year = NAT; HYPHEN; month = month_expr; HYPHEN; month_day = month_day_expr; hour_minute = hour_minute_expr;
    {
      Year_month_day_hour_minute
        {
          year;
          month;
          month_day;
          hour_minute;
        }
    }
  | HYPHEN; month = human_int_month_expr; HYPHEN; month_day = month_day_expr; hour_minute = hour_minute_expr;
  | month = direct_pick_month_expr; HYPHEN; month_day = month_day_expr; hour_minute = hour_minute_expr;
    {
      Month_day_hour_minute
        {
          month;
          month_day;
          hour_minute;
        }
    }
  | HYPHEN; HYPHEN; month_day = month_day_expr; hour_minute = hour_minute_expr;
  | NEXT; month_day = month_day_expr; hour_minute = hour_minute_expr;
    {
      Day_hour_minute
        {
          day = Month_day month_day;
          hour_minute;
        }
    }
  | HYPHEN; HYPHEN; hour_minute = hour_minute_expr;
  | hour_minute = hour_minute_expr;
    {
      Hour_minute hour_minute
    }
  | weekday = weekday_expr; hour_minute = hour_minute_expr;
    {
      Day_hour_minute
        {
          day = Weekday weekday;
          hour_minute;
        }
    }
  ;

time_slots_expr:
  (* time point to time point *)
  | start = time_point_expr; TO; end_exc = time_point_expr;
    {
      Single_time_slot (start, end_exc)
    }

  (* day list + hour minutes *)
  | days = separated_nonempty_list(COMMA, day_expr);
    DOT; hour_minutes = hour_minutes_expr;
    {
      Day_list_and_hour_minutes { hour_minutes; days }
    }

  (* weekday to weekday + hour minutes *)
  | day_start = weekday_expr; TO; day_end_inc = weekday_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Day_range_and_hour_minutes { hour_minutes; days = Weekday_range (day_start, day_end_inc) }
    }

  (* month day to month day + hour minutes *)
  | day_start = month_day_expr; TO; day_end_inc = month_day_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Day_range_and_hour_minutes { hour_minutes; days = Month_day_range (day_start, day_end_inc) }
    }

  (* month list + month day list + hour minutes *)
  | months = separated_nonempty_list(COMMA, direct_pick_month_expr);
    DOT; month_days = separated_nonempty_list(COMMA, month_day_expr);
    DOT; hour_minutes = hour_minutes_expr;
    {
      Month_list_and_month_day_list_and_hour_minutes { hour_minutes; month_days; months }
    }

  (* month list + weekday list + hour minutes *)
  | months = separated_nonempty_list(COMMA, direct_pick_month_expr);
    DOT; weekdays = separated_nonempty_list(COMMA, weekday_expr);
    DOT; hour_minutes = hour_minutes_expr;
    {
      Month_list_and_weekday_list_and_hour_minutes { hour_minutes; weekdays; months }
    }
  ;

hour_minutes_expr:
  | l = separated_nonempty_list(COMMA, hour_minute_range_expr);
    {
      l
    }

hour_minute_range_expr:
  | start = hour_minute_expr;
    {
      `Range_inc (start, start)
    }
  | start = hour_minute_expr; TO; end_exc = hour_minute_expr;
    {
      `Range_exc (start, end_exc)
    }
  ;

hour_minute_expr:
  | hour = NAT; COLON; minute = NAT
    {
      { hour; minute; mode = Hour_in_24_hours }
    }
  | hour = NAT; COLON; minute = NAT; AM
    {
      { hour; minute; mode = Hour_in_AM }
    }
  | hour = NAT; COLON; minute = NAT; PM
    {
      { hour; minute; mode = Hour_in_PM }
    }
  | hour = NAT; AM
    {
      { hour; minute = 0; mode = Hour_in_AM }
    }
  | hour = NAT; PM
    {
      { hour; minute = 0; mode = Hour_in_PM }
    }
  ;

month_day_expr:
  | x = NAT { x }
  ;

weekday_expr:
  | SUNDAY    { `Sun }
  | MONDAY    { `Mon }
  | TUESDAY   { `Tue }
  | WEDNESDAY { `Wed }
  | THURSDAY  { `Thu }
  | FRIDAY    { `Fri }
  | SATURDAY  { `Sat }
  ;

day_expr:
  | x = month_day_expr { Month_day x }
  | x = weekday_expr   { Weekday x }
  ;

human_int_month_expr:
  | x = NAT   { Human_int_month x }
  ;

direct_pick_month_expr:
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

month_expr:
  | x = human_int_month_expr { x }
  | x = direct_pick_month_expr { x }
