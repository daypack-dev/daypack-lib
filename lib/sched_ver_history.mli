type t

val make_empty : unit -> t

val of_sched_list : Sched.sched list -> t

module Read : sig
  val get_head : t -> Sched.sched
end

module In_place_head : sig
  module Task : sig
    module Add : sig
      val add_task :
        parent_user_id:int64 ->
        Task_ds.task_data ->
        Task_ds.task_inst_data list ->
        t ->
        Task_ds.task * Task_ds.task_inst list
    end
  end

  module Task_inst : sig
    module Add : sig
      val add_task_inst :
        parent_task_id:Task_ds.task_id ->
        Task_ds.task_inst_data ->
        t ->
        Task_ds.task_inst
    end
  end

  module Sched_req : sig
    module Enqueue : sig
      val enqueue_sched_req :
        Sched_req_ds.sched_req_data ->
        t ->
        (Sched_req_ds.sched_req, unit) result
    end
  end

  module Recur : sig
    val instantiate : start:int64 -> end_exc:int64 -> t -> unit
  end

  module Progress : sig
    module Move : sig
      val move_task_seg_to_completed : Task_ds.task_seg_id -> t -> unit

      val move_task_seg_to_uncompleted : Task_ds.task_seg_id -> t -> unit

      val move_task_seg_to_discarded : Task_ds.task_seg_id -> t -> unit

      val move_task_inst_to_completed : Task_ds.task_inst_id -> t -> unit

      val move_task_inst_to_uncompleted : Task_ds.task_inst_id -> t -> unit

      val move_task_inst_to_discarded : Task_ds.task_inst_id -> t -> unit
    end

    module Add : sig
      val add_task_seg_progress_chunk :
        Task_ds.task_seg_id -> int64 * int64 -> t -> unit

      val add_task_inst_progress_chunk :
        Task_ds.task_inst_id -> int64 * int64 -> t -> unit
    end
  end
end

module Maybe_append_to_head : sig
  val remove_task : Task_ds.task_id -> t -> unit

  val remove_task_inst : Task_ds.task_inst_id -> t -> unit

  val remove_task_seg_progress_chunk :
    Task_ds.task_seg_id -> int64 * int64 -> t -> unit

  val remove_task_inst_progress_chunk :
    Task_ds.task_inst_id -> int64 * int64 -> t -> unit

  val sched :
    start:int64 ->
    end_exc:int64 ->
    include_sched_reqs_partially_within_time_period:bool ->
    up_to_sched_req_id_inc:Sched_req_ds.sched_req_id option ->
    t ->
    (unit, unit) result
end

module Append_to_head : sig
  val snapshot : t -> unit
end

module Equal : sig
  val equal : t -> t -> bool
end

module Serialize : sig
  val base_and_diffs_of_list :
    Sched.sched list -> (Sched.sched * Sched.sched_diff list) option

  val to_base_and_diffs : t -> (Sched.sched * Sched.sched_diff list) option

  val write_to_dir : dir:string -> t -> (unit, string) result
end

module Deserialize : sig
  val list_of_base_and_diffs :
    Sched.sched -> Sched.sched_diff list -> Sched.sched list

  val of_base_and_diffs : Sched.sched -> Sched.sched_diff list -> t

  val read_from_dir : dir:string -> (t, string) result
end

module Print : sig
  val debug_string_of_sched_ver_history :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_print_sched_ver_history : ?indent_level:int -> t -> unit
end
