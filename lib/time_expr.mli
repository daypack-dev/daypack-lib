type search_param = Time_pattern.search_param

val next_match_unix_time_time_point_expr :
  search_in_time_zone:Time.time_zone ->
  search_param ->
  Time_expr_ast.time_point_expr ->
  (int64 option, string) result

val next_match_time_slot :
  search_in_time_zone:Time.time_zone ->
  search_param ->
  Time_expr_ast.t ->
  ((int64 * int64) option, string) result

val matching_time_slots :
  search_in_time_zone:Time.time_zone ->
  search_param ->
  Time_expr_ast.t ->
  ((int64 * int64) Seq.t, string) result

module Interpret_string : sig
  val of_string : string -> (Time_expr_ast.t, string) result

  val time_point_expr_of_string :
    string -> (Time_expr_ast.time_point_expr, string) result

  val time_slots_expr_of_string :
    string -> (Time_expr_ast.time_slots_expr, string) result
end

module To_time_pattern_lossy : sig
  val time_pattern_of_time_point_expr :
    Time_expr_ast.time_point_expr -> (Time_pattern.t, string) result

  val time_pattern_pairs_of_time_slots_expr :
    Time_expr_ast.time_slots_expr ->
    ((Time_pattern.t * Time_pattern.t) list, string) result

  val single_or_pairs_of_time_expr :
    Time_expr_ast.t -> (Time_pattern.single_or_pairs, string) result

  val time_pattern_of_time_expr :
    Time_expr_ast.t -> (Time_pattern.t, string) result

  val time_pattern_pair_of_time_expr :
    Time_expr_ast.t -> (Time_pattern.t * Time_pattern.t, string) result

  val time_pattern_pairs_of_time_expr :
    Time_expr_ast.t -> ((Time_pattern.t * Time_pattern.t) list, string) result
end
