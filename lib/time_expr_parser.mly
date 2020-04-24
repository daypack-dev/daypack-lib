%{
  open Time_expr_ast
%}

%token EOF

(* data *)
%token <int> NAT

(* keywords *)
(* %token OF *)
%token FROM
%token TO
%token EVERY
%token FIRST
%token LAST
%token COMING
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

bound_expr:
  | COMING { `Next }
  | EVERY  { `Every }

unbounded_time_point_expr:
  | year = NAT; HYPHEN; month = month_expr; HYPHEN; month_day = month_day_expr; hms = hms_expr;
  | year = NAT; month = direct_pick_month_expr; month_day = month_day_expr; hms = hms_expr;
    {
      Year_month_day_hms
        {
          year;
          month;
          month_day;
          hms;
        }
    }
  | HYPHEN; month = month_expr; HYPHEN; month_day = month_day_expr; hms = hms_expr;
  | month = direct_pick_month_expr; month_day = month_day_expr; hms = hms_expr;
    {
      Month_day_hms
        {
          month;
          month_day;
          hms;
        }
    }
  | HYPHEN; HYPHEN; month_day = month_day_expr; hms = hms_expr;
    {
      Day_hms
        {
          day = Month_day month_day;
          hms;
        }
    }
  | HYPHEN; HYPHEN; hms = hms_expr;
  | hms = hms_expr;
    {
      Hms
        {
          hms;
        }
    }
  | weekday = weekday_expr; hms = hms_expr;
    {
      Day_hms
        {
          day = Weekday weekday;
          hms;
        }
    }
  ;

time_point_expr:
  | year = NAT; HYPHEN; month = month_expr; HYPHEN; month_day = month_day_expr; hms = hms_expr;
  | year = NAT; month = direct_pick_month_expr; month_day = month_day_expr; hms = hms_expr;
    {
      ( `Next,
        Year_month_day_hms
          {
            year;
            month;
            month_day;
            hms;
          }
      )
    }
  | bound = bound_expr; month = month_expr; HYPHEN; month_day = month_day_expr; hms = hms_expr;
  | bound = bound_expr; month = direct_pick_month_expr; month_day = month_day_expr; hms = hms_expr;
    {
      ( bound,
        Month_day_hms
          {
            month;
            month_day;
            hms;
          }
      )
    }
  | bound = bound_expr; month_day = month_day_expr; hms = hms_expr;
    {
      ( bound,
        Day_hms
          {
            day = Month_day month_day;
            hms;
          }
      )
    }
  | bound = bound_expr; hms = hms_expr;
    {
      Hms
        {
          hms;
        }
    }
  | bound = bound_expr; weekday = weekday_expr; hms = hms_expr;
    {
      Day_hms
        {
          day = Weekday weekday;
          hms;
        }
    }
  ;

time_slots_expr:
  (* time point to time point *)
  | bound = bound_expr;
    FROM; start = unbounded_time_point_expr; TO; end_exc = unbounded_time_point_expr;
    {
      ( bound,
        Single_time_slot
          {
            start;
            end_exc;
          }
      )
    }

  (* month days + hour minutes *)
  | COMING;
    DOT; month_days = month_day_ranges_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Month_days_and_hms_ranges
          {
            month_days;
            hms_ranges;
          }
      )
    }

  (* weekdays + hour minutes *)
  | COMING;
    DOT; weekdays = weekday_ranges_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Weekdays_and_hms_ranges
          {
            weekdays;
            hms_ranges;
          }
      )
    }

  (* months + month days + hour minutes *)
  | COMING;
    DOT; months = direct_pick_month_ranges_expr;
    DOT; month_days = month_day_ranges_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Months_and_month_days_and_hms_ranges
          {
            hms_ranges;
            month_days;
            months;
          }
      )
    }

  (* months + weekdays + hour minutes *)
  | COMING;
    DOT; months = direct_pick_month_ranges_expr;
    DOT; weekdays = weekday_ranges_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Months_and_weekdays_and_hms_ranges
          {
            hms_ranges;
            weekdays;
            months;
          }
      )
    }

  (* months + weekday + hour minutes *)
  | COMING;
    DOT; months = direct_pick_month_ranges_expr;
    DOT; FIRST; n = NAT; weekday = weekday_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Months_and_weekday_and_hms_ranges
          {
            months;
            weekday;
            hms_ranges;
            month_weekday_mode = Some (First_n n);
          }
      )
    }

  | COMING;
    DOT; months = direct_pick_month_ranges_expr;
    DOT; LAST; n = NAT; weekday = weekday_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Months_and_weekday_and_hms_ranges
          {
            months;
            weekday;
            hms_ranges;
            month_weekday_mode = Some (Last_n n);
          }
      )
    }

  (* years + months + weekdays + hour minutes *)
  | years = year_ranges_expr;
    DOT; months = month_ranges_expr;
    DOT; month_days = month_day_ranges_expr;
    DOT; hms_ranges = hms_ranges_expr;
    {
      ( `Next,
        Years_and_months_and_month_days_and_hms_ranges
          {
            years;
            months;
            month_days;
            hms_ranges;
          }
      )
    }
  ;

(* hms expressions *)
hms_ranges_expr:
  | l = separated_nonempty_list(COMMA, hms_range_expr);
    {
      l
    }

hms_range_expr:
  | start = hms_expr;
    {
      `Range_inc (start, start)
    }
  | start = hms_expr; TO; end_exc = hms_expr;
    {
      `Range_exc (start, end_exc)
    }
  ;

hms_expr:
  | hour = NAT; COLON; minute = NAT; COLON; second = NAT;
    {
      { hour; minute; second; mode = Hour_in_24_hours }
    }
  | hour = NAT; COLON; minute = NAT; COLON; second = NAT; AM
    {
      { hour; minute; second; mode = Hour_in_AM }
    }
  | hour = NAT; COLON; minute = NAT; COLON; second = NAT; PM
    {
      { hour; minute; second; mode = Hour_in_PM }
    }
  | hour = NAT; COLON; minute = NAT
    {
      { hour; minute; second = 0; mode = Hour_in_24_hours }
    }
  | hour = NAT; COLON; minute = NAT; AM
    {
      { hour; minute; second = 0; mode = Hour_in_AM }
    }
  | hour = NAT; COLON; minute = NAT; PM
    {
      { hour; minute; second = 0; mode = Hour_in_PM }
    }
  | hour = NAT; AM
    {
      { hour; minute = 0; second = 0; mode = Hour_in_AM }
    }
  | hour = NAT; PM
    {
      { hour; minute = 0; second = 0; mode = Hour_in_PM }
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

human_int_month_range_expr:
  | x = human_int_month_expr;
    { `Range_inc (x, x) }
  | x = human_int_month_expr; TO; y = human_int_month_expr;
    { `Range_exc (x, y) }
  ;

direct_pick_month_range_expr:
  | x = direct_pick_month_expr;
    { `Range_inc (x, x) }
  | x = direct_pick_month_expr; TO; y = direct_pick_month_expr;
    { `Range_exc (x, y) }
  ;

month_range_expr:
  | x = month_expr;
    { `Range_inc (x, x) }
  | x = month_expr; TO; y = month_expr;
    { `Range_exc (x, y) }
  ;

human_int_month_ranges_expr:
  | l = separated_nonempty_list(COMMA, human_int_month_range_expr);
    { l }

direct_pick_month_ranges_expr:
  | l = separated_nonempty_list(COMMA, direct_pick_month_range_expr);
    { l }

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
