type error =
  | Invalid_time_point_expr
  | Invalid_time_slot_expr

type f_resolve_tse_name = string -> Time_expr_ast.time_slot_expr option

type f_resolve_tpe_name = string -> Time_expr_ast.time_point_expr option

type lang_fragment =
  [ `Time_point_expr
  | `Time_slot_expr
  (* | `Branching_time_point_expr *)
  | `Branching_time_slot_expr
  | `Time_pattern
  ]

(* module Check : sig *)
(* val check_unbounded_time_point_expr :
 *   Time_expr_ast.unbounded_time_point_expr -> (unit, unit) result
 * 
 * val check_unbounded_time_slot_expr :
 *   Time_expr_ast.unbounded_time_slot_expr -> (unit, unit) result *)
(* end *)

val check_time_expr : Time_expr_ast.t -> (unit, unit) result

(* module Time_point_expr : sig
 *   val next_match_unix_second :
 *     ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *     search_param ->
 *     Time_expr_ast.time_point_expr ->
 *     (int64 option, string) result
 * 
 *   val matching_unix_seconds :
 *     ?force_bound:Time_expr_ast.bound ->
 *     ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *     search_param ->
 *     Time_expr_ast.time_point_expr ->
 *     (int64 Seq.t, string) result
 * end
 * 
 * module Time_slot_expr : sig
 *   val next_match_time_slot :
 *     ?f_resolve_tse_name:f_resolve_tse_name ->
 *     ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *     search_param ->
 *     Time_expr_ast.time_slot_expr ->
 *     ((int64 * int64) option, string) result
 * 
 *   val matching_time_slots :
 *     ?force_bound:Time_expr_ast.bound ->
 *     ?f_resolve_tse_name:f_resolve_tse_name ->
 *     ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *     search_param ->
 *     Time_expr_ast.time_slot_expr ->
 *     ((int64 * int64) Seq.t, string) result
 * end *)

(* module Resolve : sig
 *   val resolve_unbounded_time_point_expr :
 *     f_resolve_tpe_name:f_resolve_tpe_name ->
 *     Time_expr_ast.unbounded_time_point_expr ->
 *     (Time_expr_ast.unbounded_time_point_expr, string) result
 * 
 *   val resolve_unbounded_time_slot_expr :
 *     f_resolve_tse_name:f_resolve_tse_name ->
 *     f_resolve_tpe_name:f_resolve_tpe_name ->
 *     Time_expr_ast.unbounded_time_slot_expr ->
 *     (Time_expr_ast.unbounded_time_slot_expr, string) result
 * end *)

module To_string : sig
  val debug_string_of_hms_ranges : Time_expr_ast.hms_expr -> string
end

val time_expr_parser :
  ?enabled_fragments:lang_fragment list -> Time_expr_ast.t CCParse.t

val of_string :
  ?enabled_fragments:lang_fragment list ->
  string ->
  (Time_expr_ast.t, string) result

(* module To_time_pattern_lossy : sig *)
(* val time_pattern_of_time_point_expr :
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.time_point_expr ->
 *   (Time_pattern.time_pattern, string) result
 * 
 * val time_range_patterns_of_time_slot_expr :
 *   ?f_resolve_tse_name:f_resolve_tse_name ->
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.time_slot_expr ->
 *   (Time_pattern.time_range_pattern list, string) result *)

(* val single_or_ranges_of_time_expr :
 *   ?f_resolve_tse_name:f_resolve_tse_name ->
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.t ->
 *   (Time_pattern.single_or_ranges, string) result
 * 
 * val time_pattern_of_time_expr :
 *   ?f_resolve_tse_name:f_resolve_tse_name ->
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.t ->
 *   (Time_pattern.time_pattern, string) result
 * 
 * val time_range_pattern_of_time_expr :
 *   ?f_resolve_tse_name:f_resolve_tse_name ->
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.t ->
 *   (Time_pattern.time_range_pattern, string) result
 * 
 * val time_range_patterns_of_time_expr :
 *   ?f_resolve_tse_name:f_resolve_tse_name ->
 *   ?f_resolve_tpe_name:f_resolve_tpe_name ->
 *   Time_expr_ast.t ->
 *   (Time_pattern.time_range_pattern list, string) result *)
(* end *)

val matching_time_slots :
  ?f_resolve_tpe_name:f_resolve_tpe_name ->
  ?f_resolve_tse_name:f_resolve_tse_name ->
  Search_param.t ->
  Time_expr_ast.t ->
  (Time_slot.t Seq.t, string) result

val next_match_time_slot :
  ?f_resolve_tpe_name:f_resolve_tpe_name ->
  ?f_resolve_tse_name:f_resolve_tse_name ->
  Search_param.t ->
  Time_expr_ast.t ->
  (Time_slot.t option, string) result
