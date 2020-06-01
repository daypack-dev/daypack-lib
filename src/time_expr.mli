type search_param = Time_pattern.search_param

type error =
  | Invalid_time_points_expr
  | Invalid_time_slots_expr

type f_resolve_tse_name =
  string -> Time_expr_ast.unbounded_time_slots_expr option

type f_resolve_tpe_name =
  string -> Time_expr_ast.unbounded_time_points_expr option

module Check : sig
  val check_unbounded_time_points_expr :
    Time_expr_ast.unbounded_time_points_expr -> (unit, unit) result
end

module Time_points_expr : sig
  val next_match_unix_second :
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    search_param ->
    Time_expr_ast.time_points_expr ->
    (int64 option, string) result

  val matching_unix_seconds :
    ?force_bound:Time_expr_ast.bound ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    search_param ->
    Time_expr_ast.time_points_expr ->
    (int64 Seq.t, string) result
end

module Time_slots_expr : sig
  val next_match_time_slot :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    search_param ->
    Time_expr_ast.time_slots_expr ->
    ((int64 * int64) option, string) result

  val matching_time_slots :
    ?force_bound:Time_expr_ast.bound ->
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    search_param ->
    Time_expr_ast.time_slots_expr ->
    ((int64 * int64) Seq.t, string) result
end

module Resolve : sig
  val resolve_unbounded_time_points_expr :
    f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.unbounded_time_points_expr ->
    (Time_expr_ast.unbounded_time_points_expr, string) result

  val resolve_unbounded_time_slots_expr :
    f_resolve_tse_name:f_resolve_tse_name ->
    f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.unbounded_time_slots_expr ->
    (Time_expr_ast.unbounded_time_slots_expr, string) result
end

module To_string : sig
  val debug_string_of_hour_minute_second_ranges :
    Time_expr_ast.hour_minute_second_expr -> string
end

module Of_string : sig
  val of_string : string -> (Time_expr_ast.t, string) result

  val time_points_expr_of_string :
    string -> (Time_expr_ast.time_points_expr, string) result

  val time_slots_expr_of_string :
    string -> (Time_expr_ast.time_slots_expr, string) result
end

module To_time_pattern_lossy : sig
  val time_pattern_of_time_points_expr :
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.time_points_expr ->
    (Time_pattern.time_pattern, string) result

  val time_range_patterns_of_time_slots_expr :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.time_slots_expr ->
    (Time_pattern.time_range_pattern list, string) result

  val single_or_ranges_of_time_expr :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.t ->
    (Time_pattern.single_or_ranges, string) result

  val time_pattern_of_time_expr :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.t ->
    (Time_pattern.time_pattern, string) result

  val time_range_pattern_of_time_expr :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.t ->
    (Time_pattern.time_range_pattern, string) result

  val time_range_patterns_of_time_expr :
    ?f_resolve_tse_name:f_resolve_tse_name ->
    ?f_resolve_tpe_name:f_resolve_tpe_name ->
    Time_expr_ast.t ->
    (Time_pattern.time_range_pattern list, string) result
end
