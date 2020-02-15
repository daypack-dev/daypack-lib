type t

val make_empty : unit -> t

val of_sched_list : Sched.sched list -> t

module In_place_head : sig
  val add_task :
    parent_user_id:int64 ->
    Task.task_data ->
    Task.task_inst_data list ->
    t ->
    Task.task * Task.task_inst list

  val add_task_inst :
    parent_task_id:Task.task_id -> Task.task_inst_data -> t -> Task.task_inst

  val queue_sched_req : Sched_req.sched_req_data -> t -> Sched_req.sched_req

  val instantiate : start:int64 -> end_exc:int64 -> t -> unit

  val mark_task_seg_completed : Task.task_seg_id -> t -> unit

  val mark_task_seg_uncompleted : Task.task_seg_id -> t -> unit

  val mark_task_inst_completed : Task.task_inst_id -> t -> unit

  val mark_task_inst_uncompleted : Task.task_inst_id -> t -> unit
end

module Maybe_append_to_head : sig
  val remove_task : Task.task_id -> t -> unit

  val remove_task_inst : Task.task_inst_id -> t -> unit

  val sched :
    start:int64 ->
    end_exc:int64 ->
    include_sched_reqs_partially_within_time_period:bool ->
    up_to_sched_req_id_inc:Sched_req.sched_req_id option ->
    t ->
    (unit, unit) result
end

module Equal : sig
  val equal : t -> t -> bool
end

module Serialize : sig
  val list_to_base_and_diffs :
    Sched.sched list -> (Sched.sched * Sched.sched_diff list) option

  val to_base_and_diffs : t -> (Sched.sched * Sched.sched_diff list) option
end

module Deserialize : sig
  val list_of_base_and_diffs :
    Sched.sched -> Sched.sched_diff list -> Sched.sched list

  val of_base_and_diffs : Sched.sched -> Sched.sched_diff list -> t
end

module Print : sig
  val debug_string_of_sched_ver_history :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_print_sched_ver_history : ?indent_level:int -> t -> unit
end
