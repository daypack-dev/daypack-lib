%{
  open Time_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT

(* keywords *)
(* %token OF *)
%token TO
%token EVERY
%token FIRST
%token LAST
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
          match_mode = `Next;
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
          match_mode = `Next;
        }
    }
  | HYPHEN; HYPHEN; month_day = month_day_expr; hour_minute = hour_minute_expr;
  | NEXT; month_day = month_day_expr; hour_minute = hour_minute_expr;
    {
      Day_hour_minute
        {
          day = Month_day month_day;
          hour_minute;
          match_mode = `Next;
        }
    }
  | HYPHEN; HYPHEN; hour_minute = hour_minute_expr;
  | hour_minute = hour_minute_expr;
    {
      Hour_minute
        {
          hour_minute;
          match_mode = `Next;
        }
    }
  | weekday = weekday_expr; hour_minute = hour_minute_expr;
    {
      Day_hour_minute
        {
          day = Weekday weekday;
          hour_minute;
          match_mode = `Next;
        }
    }
  ;

time_slots_expr:
  (* time point to time point *)
  | start = time_point_expr; TO; end_exc = time_point_expr;
    {
      Single_time_slot
        {
          start;
          end_exc;
          match_mode = `Next;
        }
    }

  (* month days + hour minutes *)
  | month_days = month_day_ranges_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Month_days_and_hour_minutes
        {
          month_days;
          hour_minutes;
          match_mode = `Next;
        }
    }

  (* weekdays + hour minutes *)
  | weekdays = weekday_ranges_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Weekdays_and_hour_minutes
        {
          weekdays;
          hour_minutes;
          match_mode = `Next;
        }
    }

  (* months + month days + hour minutes *)
  | months = month_ranges_expr;
    DOT; month_days = month_day_ranges_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Months_and_month_days_and_hour_minutes
        {
          hour_minutes;
          month_days;
          months;
          match_mode = `Next;
        }
    }

  (* months + weekdays + hour minutes *)
  | months = month_ranges_expr;
    DOT; weekdays = weekday_ranges_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Months_and_weekdays_and_hour_minutes
        {
          hour_minutes;
          weekdays;
          months;
          match_mode = `Next;
        }
    }

  (* months + weekday + hour minutes *)
  | months = month_ranges_expr;
    DOT; FIRST; n = NAT; weekday = weekday_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Months_and_weekday_and_hour_minutes
        {
          months;
          weekday;
          hour_minutes;
          match_mode = `Next;
          month_weekday_mode = Some (First_n n);
        }
    }

  | months = month_ranges_expr;
    DOT; LAST; n = NAT; weekday = weekday_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Months_and_weekday_and_hour_minutes
        {
          months;
          weekday;
          hour_minutes;
          month_weekday_mode = Some (Last_n n);
          match_mode = `Next;
        }
    }

  (* years + months + weekdays + hour minutes *)
  | years = year_ranges_expr;
    DOT; months = month_ranges_expr;
    DOT; month_days = month_day_ranges_expr;
    DOT; hour_minutes = hour_minutes_expr;
    {
      Years_and_months_and_month_days_and_hour_minutes
        {
          years;
          months;
          month_days;
          hour_minutes;
          match_mode = `Next;
        }
    }
  ;

(* hour minutes expressions *)
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

(* day expressions *)
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

weekday_range_expr:
  | x = weekday_expr;
    { `Range_inc (x, x) }
  | x = weekday_expr; TO; y = weekday_expr;
    { `Range_inc (x, y) }
  ;

weekday_ranges_expr:
  | l = separated_nonempty_list(COMMA, weekday_range_expr)
    { Range.compress_list ~to_int:Time.tm_int_of_weekday l }
  ;

month_day_range_expr:
  | x = month_day_expr;
    { `Range_inc (x, x) }
  | x = month_day_expr; TO; y = month_day_expr;
    { `Range_inc (x, y) }
  ;

month_day_ranges_expr:
  | l = separated_nonempty_list(COMMA, month_day_range_expr)
    { Range.compress_list ~to_int:(fun x -> x) l }
  ;

(* month expressions *)
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
  ;

month_range_expr:
  | x = month_expr
    { `Range_inc (x, x) }
  | x = month_expr; TO; y = month_expr;
    { `Range_exc (x, y) }
  ;

month_ranges_expr:
  | l = separated_nonempty_list(COMMA, month_range_expr);
    { l }

(* month expressions *)
year_expr:
  | x = NAT;
    { x }

year_range_expr:
  | x = year_expr;
    { `Range_inc(x, x) }
  | x = year_expr; TO; y = year_expr;
    { `Range_inc(x, y) }

year_ranges_expr:
  | l = separated_nonempty_list(COMMA, year_range_expr);
    { l }
