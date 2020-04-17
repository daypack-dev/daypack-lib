type search_param = Time_pattern.search_param

val next_match_time_slot :
  search_param ->
  Time_expr_normalized_ast.t ->
  ((int64 * int64) option, string) result

val matching_time_slots :
  search_param ->
  Time_expr_normalized_ast.t ->
  ((int64 * int64) Seq.t, string) result

module Time_point_expr : sig
  val next_match_unix_time :
    search_param ->
    Time_expr_normalized_ast.time_point_expr ->
    (int64 option, string) result

  val next_match_time_slot :
    search_param ->
    Time_expr_normalized_ast.time_point_expr ->
    ((int64 * int64) option, string) result

  val matching_time_slots :
    search_param ->
    Time_expr_normalized_ast.time_point_expr ->
    ((int64 * int64) Seq.t, string) result
end

module Time_slots_expr : sig
  val next_match_time_slot :
    search_param ->
    Time_expr_normalized_ast.time_slots_expr ->
    ((int64 * int64) option, string) result

  val matching_time_slots :
    ?force_match_mode:Time_expr_ast.match_mode ->
    search_param ->
    Time_expr_normalized_ast.time_slots_expr ->
    ((int64 * int64) Seq.t, string) result
end

module Interpret_string : sig
  val of_string : string -> (Time_expr_normalized_ast.t, string) result

  val time_point_expr_of_string :
    string -> (Time_expr_normalized_ast.time_point_expr, string) result

  val time_slots_expr_of_string :
    string -> (Time_expr_normalized_ast.time_slots_expr, string) result
end

module To_time_pattern_lossy : sig
  val time_pattern_of_time_point_expr :
    Time_expr_normalized_ast.time_point_expr -> (Time_pattern.t, string) result

  val time_range_patterns_of_time_slots_expr :
    Time_expr_normalized_ast.time_slots_expr ->
    (Time_pattern.time_range_pattern list, string) result

  val single_or_ranges_of_time_expr :
    Time_expr_normalized_ast.t -> (Time_pattern.single_or_ranges, string) result

  val time_pattern_of_time_expr :
    Time_expr_normalized_ast.t -> (Time_pattern.t, string) result

  val time_range_pattern_of_time_expr :
    Time_expr_normalized_ast.t ->
    (Time_pattern.time_range_pattern, string) result

  val time_range_patterns_of_time_expr :
    Time_expr_normalized_ast.t ->
    (Time_pattern.time_range_pattern list, string) result
end
