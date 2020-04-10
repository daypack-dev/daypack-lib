module Interpret_string : sig
  val of_string : string -> (Time_expr_ast.t, string) result
end

module To_time_pattern : sig
  val time_pattern_of_time_point_expr : Time_expr_ast.time_point_expr -> (Time_pattern.t, string) result

  val paired_time_patterns_of_time_slots_expr :
    Time_expr_ast.time_slots_expr -> ((Time_pattern.t * Time_pattern.t) list, string) result

  val single_or_multi_paired_time_patterns_of_time_expr :
    Time_expr_ast.t -> (Time_pattern.single_or_multi_paired, string) result
end
