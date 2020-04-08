val paired_time_patterns_of_time_slots_expr : Time_expr_ast.time_slots_expr ->
  ((Time_pattern.t * Time_pattern.t) list, unit) result

module Interpret_string : sig
  val of_string : string -> Time_expr_ast.t
end
