val time_pattern_of_time_point_expr :
  Time_expr_ast.time_point_expr -> (Time_pattern.t, unit) result

val paired_time_patterns_of_time_slots_expr :
  Time_expr_ast.time_slots_expr ->
  ((Time_pattern.t * Time_pattern.t) list, unit) result

module Interpret_string : sig
  val of_string : string -> Time_expr_ast.t
end
