type sched_id = int

type task_store = Task_ds.task_data Task_id_map.t

type task_store_diff = Task_ds.task_data Task_id_map_utils.diff

type task_inst_store = Task_ds.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task_ds.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task_ds.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task_ds.task_seg_size Task_seg_id_map_utils.diff

type sched_req_store = Sched_req_ds.sched_req_data Sched_req_id_map.t

type sched_req_store_diff =
  Sched_req_ds.sched_req_data Sched_req_id_map_utils.diff

type sched_req_record_store =
  Sched_req_ds.sched_req_record_data Sched_req_id_map.t

type sched_req_record_store_diff =
  Sched_req_ds.sched_req_record_data Sched_req_id_map_utils.diff

type task_seg_place_map = Task_seg_place_set.t Int64_map.t

type task_seg_place_map_diff =
  Int64_map_utils.Task_seg_place_bucketed.diff_bucketed

type task_related_status =
  [ `Uncompleted
  | `Completed
  | `Discarded
  ]

type sched_req_status =
  [ `Pending
  | `Discarded
  | `Recorded
  ]

type store = {
  task_uncompleted_store : task_store;
  task_completed_store : task_store;
  task_discarded_store : task_store;
  task_inst_uncompleted_store : task_inst_store;
  task_inst_completed_store : task_inst_store;
  task_inst_discarded_store : task_inst_store;
  task_seg_uncompleted_store : task_seg_store;
  task_seg_completed_store : task_seg_store;
  task_seg_discarded_store : task_seg_store;
  user_id_to_task_ids : Int64_set.t User_id_map.t;
  task_id_to_task_inst_ids : Int64_set.t Task_id_map.t;
  task_inst_id_to_task_seg_ids : Int64_int64_option_set.t Task_inst_id_map.t;
  sched_req_ids : Int64_set.t;
  sched_req_pending_store : sched_req_store;
  sched_req_discarded_store : sched_req_store;
  sched_req_record_store : sched_req_record_store;
  quota : int64 Task_inst_id_map.t;
  task_seg_id_to_progress : Task_ds.progress Task_seg_id_map.t;
  task_inst_id_to_progress : Task_ds.progress Task_inst_id_map.t;
}

type store_diff = {
  task_uncompleted_store_diff : task_store_diff;
  task_completed_store_diff : task_store_diff;
  task_discarded_store_diff : task_store_diff;
  task_inst_uncompleted_store_diff : task_inst_store_diff;
  task_inst_completed_store_diff : task_inst_store_diff;
  task_inst_discarded_store_diff : task_inst_store_diff;
  task_seg_uncompleted_store_diff : task_seg_store_diff;
  task_seg_completed_store_diff : task_seg_store_diff;
  task_seg_discarded_store_diff : task_seg_store_diff;
  user_id_to_task_ids_diff : User_id_map_utils.Int64_bucketed.diff_bucketed;
  task_id_to_task_inst_ids_diff :
    Task_id_map_utils.Int64_bucketed.diff_bucketed;
  task_inst_id_to_task_seg_ids_diff :
    Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed;
  sched_req_ids_diff : Int64_set_utils.diff;
  sched_req_pending_store_diff : sched_req_store_diff;
  sched_req_discarded_store_diff : sched_req_store_diff;
  sched_req_record_store_diff : sched_req_record_store_diff;
  quota_diff : int64 Task_inst_id_map_utils.diff;
  task_seg_id_to_progress_diff : Task_ds.progress Task_seg_id_map_utils.diff;
  task_inst_id_to_progress_diff : Task_ds.progress Task_inst_id_map_utils.diff;
}

type agenda = { indexed_by_start : task_seg_place_map }

type agenda_diff = { indexed_by_start_diff : task_seg_place_map_diff }

type sched_data = {
  store : store;
  agenda : agenda;
}

type sched_data_diff = {
  store_diff : store_diff;
  agenda_diff : agenda_diff;
}

type sched = sched_id * sched_data

type sched_diff = sched_id * sched_id * sched_data_diff

val sched_data_empty : sched_data

val empty : sched

module Time_slot : sig
  val get_occupied_time_slots :
    ?start:int64 -> ?end_exc:int64 -> sched -> (int64 * int64) Seq.t

  val get_free_time_slots :
    start:int64 -> end_exc:int64 -> sched -> (int64 * int64) Seq.t
end

module Quota : sig
  val update_quota : int64 Task_inst_id_map.t -> sched -> sched

  val add_quota : int64 Task_inst_id_map.t -> sched -> sched
end

module Task_seg : sig
  module Add : sig
    val add_task_seg :
      parent_task_inst_id:Task_ds.task_inst_id ->
      Task_ds.task_seg_size ->
      sched ->
      Task_ds.task_seg * sched

    val add_task_seg_via_task_seg_alloc_req :
      Task_ds.task_seg_alloc_req -> sched -> Task_ds.task_seg * sched

    val add_task_segs_via_task_seg_alloc_req_list :
      Task_ds.task_seg_alloc_req list -> sched -> Task_ds.task_seg list * sched

    val add_task_seg_via_task_seg_place :
      Task_ds.task_seg_place -> sched -> sched

    val add_task_segs_via_task_seg_place_list :
      Task_ds.task_seg_place list -> sched -> sched

    val add_task_segs_via_task_seg_place_seq :
      Task_ds.task_seg_place Seq.t -> sched -> sched
  end

  module To_seq : sig
    val task_seg_seq_uncompleted : sched -> Task_ds.task_seg Seq.t

    val task_seg_seq_completed : sched -> Task_ds.task_seg Seq.t

    val task_seg_seq_discarded : sched -> Task_ds.task_seg Seq.t

    val task_seg_seq_all : sched -> Task_ds.task_seg Seq.t
  end

  module Find : sig
    val find_task_seg_uncompleted_opt :
      Task_ds.task_seg_id -> sched -> Task_ds.task_seg_size option

    val find_task_seg_completed_opt :
      Task_ds.task_seg_id -> sched -> Task_ds.task_seg_size option

    val find_task_seg_discarded_opt :
      Task_ds.task_seg_id -> sched -> Task_ds.task_seg_size option

    val find_task_seg_ids_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg_id Seq.t

    val find_task_seg_seq_uncompleted_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_completed_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_discarded_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_any_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_any_w_status_by_task_inst_id :
      Task_ds.task_inst_id ->
      sched ->
      (task_related_status * Task_ds.task_seg) Seq.t

    val find_task_seg_ids_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg_id Seq.t

    val find_task_seg_seq_uncompleted_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_completed_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_discarded_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_any_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg Seq.t

    val find_task_seg_seq_any_w_status_by_task_id :
      Task_ds.task_id -> sched -> (task_related_status * Task_ds.task_seg) Seq.t
  end

  module Remove : sig
    val remove_task_seg_uncompleted : Task_ds.task_seg_id -> sched -> sched

    val remove_task_seg_completed : Task_ds.task_seg_id -> sched -> sched

    val remove_task_seg_discarded : Task_ds.task_seg_id -> sched -> sched

    val remove_task_seg_all : Task_ds.task_seg_id -> sched -> sched

    val remove_task_seg_uncompleted_strict :
      Task_ds.task_seg_id -> sched -> (sched, unit) result

    val remove_task_seg_completed_strict :
      Task_ds.task_seg_id -> sched -> (sched, unit) result

    val remove_task_seg_discarded_strict :
      Task_ds.task_seg_id -> sched -> (sched, unit) result

    val remove_task_seg_uncompleted_seq :
      Task_ds.task_seg_id Seq.t -> sched -> sched

    val remove_task_seg_completed_seq :
      Task_ds.task_seg_id Seq.t -> sched -> sched

    val remove_task_seg_discarded_seq :
      Task_ds.task_seg_id Seq.t -> sched -> sched
  end
end

module Task_inst : sig
  module Add : sig
    val add_task_inst :
      parent_task_id:Task_ds.task_id ->
      Task_ds.task_inst_data ->
      sched ->
      Task_ds.task_inst * sched

    val add_task_inst_list :
      parent_task_id:Task_ds.task_id ->
      Task_ds.task_inst_data list ->
      sched ->
      Task_ds.task_inst list * sched
  end

  module To_seq : sig
    val task_inst_seq_uncompleted : sched -> Task_ds.task_inst Seq.t

    val task_inst_seq_completed : sched -> Task_ds.task_inst Seq.t

    val task_inst_seq_discarded : sched -> Task_ds.task_inst Seq.t

    val task_inst_seq_all : sched -> Task_ds.task_inst Seq.t
  end

  module Find : sig
    val find_task_inst_uncompleted_opt :
      Task_ds.task_inst_id -> sched -> Task_ds.task_inst_data option

    val find_task_inst_completed_opt :
      Task_ds.task_inst_id -> sched -> Task_ds.task_inst_data option

    val find_task_inst_discarded_opt :
      Task_ds.task_inst_id -> sched -> Task_ds.task_inst_data option

    val find_task_inst_any_opt :
      Task_ds.task_inst_id -> sched -> Task_ds.task_inst_data option

    val find_task_inst_any_w_status_opt :
      Task_ds.task_inst_id ->
      sched ->
      (task_related_status * Task_ds.task_inst_data) option

    val find_task_inst_ids_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_inst_id Seq.t

    val find_task_inst_seq_uncompleted_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_inst Seq.t

    val find_task_inst_seq_completed_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_inst Seq.t

    val find_task_inst_seq_discarded_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_inst Seq.t

    val find_task_inst_seq_any_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_inst Seq.t

    val find_task_inst_seq_any_w_status_by_task_id :
      Task_ds.task_id ->
      sched ->
      (task_related_status * Task_ds.task_inst) Seq.t
  end

  module Remove : sig
    val remove_task_inst_uncompleted :
      ?remove_children_task_segs:bool -> Task_ds.task_inst_id -> sched -> sched

    val remove_task_inst_completed :
      ?remove_children_task_segs:bool -> Task_ds.task_inst_id -> sched -> sched

    val remove_task_inst_discarded :
      ?remove_children_task_segs:bool -> Task_ds.task_inst_id -> sched -> sched

    val remove_task_inst_all :
      ?remove_children_task_segs:bool -> Task_ds.task_inst_id -> sched -> sched

    val remove_task_inst_uncompleted_strict :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id ->
      sched ->
      (sched, unit) result

    val remove_task_inst_completed_strict :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id ->
      sched ->
      (sched, unit) result

    val remove_task_inst_discarded_strict :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id ->
      sched ->
      (sched, unit) result

    val remove_task_inst_uncompleted_seq :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id Seq.t ->
      sched ->
      sched

    val remove_task_inst_completed_seq :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id Seq.t ->
      sched ->
      sched

    val remove_task_inst_discarded_seq :
      ?remove_children_task_segs:bool ->
      Task_ds.task_inst_id Seq.t ->
      sched ->
      sched
  end
end

module Task : sig
  module Add : sig
    val add_task :
      parent_user_id:Task_ds.user_id ->
      Task_ds.task_data ->
      Task_ds.task_inst_data list ->
      sched ->
      Task_ds.task * Task_ds.task_inst list * sched
  end

  module To_seq : sig
    val task_seq_uncompleted : sched -> Task_ds.task Seq.t

    val task_seq_completed : sched -> Task_ds.task Seq.t

    val task_seq_discarded : sched -> Task_ds.task Seq.t

    val task_seq_all : sched -> Task_ds.task Seq.t
  end

  module Find : sig
    val find_task_uncompleted_opt :
      Task_ds.task_id -> sched -> Task_ds.task_data option

    val find_task_completed_opt :
      Task_ds.task_id -> sched -> Task_ds.task_data option

    val find_task_discarded_opt :
      Task_ds.task_id -> sched -> Task_ds.task_data option

    val find_task_any_opt : Task_ds.task_id -> sched -> Task_ds.task_data option

    val find_task_any_w_status_opt :
      Task_ds.task_id ->
      sched ->
      (task_related_status * Task_ds.task_data) option
  end

  module Remove : sig
    val remove_task_uncompleted :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      sched

    val remove_task_completed :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      sched

    val remove_task_discarded :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      sched

    val remove_task_all :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      sched

    val remove_task_uncompleted_strict :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      (sched, unit) result

    val remove_task_completed_strict :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      (sched, unit) result

    val remove_task_discarded_strict :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_ds.task_id ->
      sched ->
      (sched, unit) result
  end
end

module Progress : sig
  module Status : sig
    val get_task_status : Task_ds.task_id -> sched -> task_related_status option

    val get_task_inst_status :
      Task_ds.task_inst_id -> sched -> task_related_status option

    val get_task_seg_status :
      Task_ds.task_seg_id -> sched -> task_related_status option
  end

  module Move : sig
    val move_task_seg_to_completed : Task_ds.task_seg_id -> sched -> sched

    val move_task_seg_to_uncompleted : Task_ds.task_seg_id -> sched -> sched

    val move_task_seg_to_discarded : Task_ds.task_seg_id -> sched -> sched

    val move_task_inst_to_completed : Task_ds.task_inst_id -> sched -> sched

    val move_task_inst_to_uncompleted : Task_ds.task_inst_id -> sched -> sched

    val move_task_inst_to_discarded : Task_ds.task_inst_id -> sched -> sched

    val move_task_to_completed : Task_ds.task_id -> sched -> sched

    val move_task_to_uncompleted : Task_ds.task_id -> sched -> sched

    val move_task_to_discarded : Task_ds.task_id -> sched -> sched
  end

  module Add : sig
    val add_task_seg_progress_chunk :
      Task_ds.task_seg_id -> int64 * int64 -> sched -> sched

    val add_task_seg_progress_chunk :
      Task_ds.task_seg_id -> int64 * int64 -> sched -> sched

    val add_task_inst_progress_chunk :
      Task_ds.task_inst_id -> int64 * int64 -> sched -> sched
  end

  module Find : sig
    val find_task_seg_progress :
      Task_ds.task_seg_id -> sched -> Task_ds.progress option

    val find_task_seg_progress_chunk_set :
      Task_ds.task_seg_id -> sched -> Int64_int64_set.t

    val find_task_seg_progress_chunk_seq :
      Task_ds.task_seg_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress :
      Task_ds.task_seg_id -> sched -> Task_ds.progress option

    val find_task_seg_progress_seq_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.progress Seq.t

    val find_task_seg_progress_seq_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.progress Seq.t

    val find_task_seg_progress_chunk_set :
      Task_ds.task_seg_id -> sched -> Int64_int64_set.t

    val find_task_seg_progress_chunk_seq :
      Task_ds.task_seg_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress_chunk_seq_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress_chunk_seq_by_task_id :
      Task_ds.task_id -> sched -> (int64 * int64) Seq.t

    val find_task_inst_progress :
      Task_ds.task_inst_id -> sched -> Task_ds.progress option

    val find_task_inst_progress_seq_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.progress Seq.t

    val find_task_inst_progress_chunk_set :
      Task_ds.task_inst_id -> sched -> Int64_int64_set.t

    val find_task_inst_progress_chunk_seq :
      Task_ds.task_inst_id -> sched -> (int64 * int64) Seq.t

    val find_task_inst_progress_chunk_seq_by_task_id :
      Task_ds.task_id -> sched -> (int64 * int64) Seq.t
  end

  module Remove : sig
    val remove_task_seg_progress_chunk :
      Task_ds.task_seg_id -> int64 * int64 -> sched -> sched

    val remove_task_seg_progress_chunk :
      Task_ds.task_seg_id -> int64 * int64 -> sched -> sched

    val remove_task_inst_progress_chunk :
      Task_ds.task_inst_id -> int64 * int64 -> sched -> sched
  end
end

module Agenda : sig
  module Add : sig
    val add_task_seg_place : Task_ds.task_seg_place -> sched -> sched

    val add_task_seg_place_list : Task_ds.task_seg_place list -> sched -> sched

    val add_task_seg_place_seq : Task_ds.task_seg_place Seq.t -> sched -> sched
  end

  module Filter : sig
    val filter_task_seg_place_seq :
      (Task_ds.task_seg_place -> bool) -> sched -> Task_ds.task_seg_place Seq.t
  end

  module Find : sig
    val find_task_seg_place_seq_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> Task_ds.task_seg_place Seq.t

    val find_task_seg_place_seq_by_task_id :
      Task_ds.task_id -> sched -> Task_ds.task_seg_place Seq.t

    val find_task_seg_place_seq_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Task_ds.task_seg_place Seq.t

    val find_task_seg_place_seq_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> Task_ds.task_seg_place Seq.t
  end

  module Remove : sig
    val remove_task_seg_place : Task_ds.task_seg_place -> sched -> sched

    val remove_task_seg_place_seq :
      Task_ds.task_seg_place Seq.t -> sched -> sched

    val remove_task_seg_place_by_task_id : Task_ds.task_id -> sched -> sched

    val remove_task_seg_place_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> sched

    val remove_task_seg_place_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> sched
  end
end

module Sched_req : sig
  module Status : sig
    val get_sched_req_status :
      Sched_req_ds.sched_req_id -> sched -> sched_req_status option
  end

  module Enqueue : sig
    val enqueue_sched_req_data :
      Sched_req_ds.sched_req_data -> sched -> Sched_req_ds.sched_req * sched

    val enqueue_sched_req_data_list :
      Sched_req_ds.sched_req_data list ->
      sched ->
      Sched_req_ds.sched_req list * sched
  end

  module Dequeue : sig
    val dequeue_sched_req : Sched_req_ds.sched_req_id -> sched -> sched
  end

  module To_seq : sig
    val pending_sched_req_seq : sched -> Sched_req_ds.sched_req Seq.t

    val sched_req_record_seq : sched -> Sched_req_ds.sched_req_record Seq.t
  end

  module Filter : sig
    val filter_sched_req_record_seq :
      (Sched_req_ds.sched_req_record -> bool) ->
      sched ->
      Sched_req_ds.sched_req_record Seq.t
  end

  module Find : sig
    val find_sched_req_record_by_task_id :
      Task_ds.task_id -> sched -> Sched_req_ds.sched_req_record Seq.t

    val find_sched_req_record_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> Sched_req_ds.sched_req_record Seq.t

    val find_sched_req_record_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> Sched_req_ds.sched_req_record Seq.t
  end

  module Remove : sig
    val remove_pending_sched_req_if_contains_matching_task_seg_alloc_req :
      (Task_ds.task_seg_alloc_req -> bool) -> sched -> sched

    val remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req :
      (Task_ds.task_seg_alloc_req -> bool) -> sched -> sched

    val remove_sched_req_record_if_contains_matching_task_seg :
      (Task_ds.task_seg -> bool) -> sched -> sched

    val remove_sched_req_record_data_unit_if_contains_matching_task_seg :
      (Task_ds.task_seg -> bool) -> sched -> sched

    val remove_pending_sched_req_by_task_id : Task_ds.task_id -> sched -> sched

    val remove_pending_sched_req_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> sched

    val remove_pending_sched_req_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> sched

    val remove_pending_sched_req_data_unit_by_task_id :
      Task_ds.task_id -> sched -> sched

    val remove_pending_sched_req_data_unit_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> sched

    val remove_pending_sched_req_data_unit_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> sched

    val remove_sched_req_record_by_task_id : Task_ds.task_id -> sched -> sched

    val remove_sched_req_record_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> sched

    val remove_sched_req_record_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> sched

    val remove_sched_req_record_data_unit_by_task_id :
      Task_ds.task_id -> sched -> sched

    val remove_sched_req_record_data_unit_by_task_inst_id :
      Task_ds.task_inst_id -> sched -> sched

    val remove_sched_req_record_data_unit_by_task_seg_id :
      Task_ds.task_seg_id -> sched -> sched
  end

  module Discard : sig
    val discard_pending_sched_req : Sched_req_ds.sched_req_id -> sched -> sched
  end

  module Allocate_task_segs : sig
    val allocate_task_segs_for_pending_sched_reqs :
      start:int64 ->
      end_exc:int64 ->
      include_sched_reqs_partially_within_time_period:bool ->
      up_to_sched_req_id_inc:Sched_req_ds.sched_req_id option ->
      sched ->
      Sched_req_ds.sched_req_record list * sched
  end
end

module Recur : sig
  val instantiate : start:int64 -> end_exc:int64 -> sched -> sched
end

module Leftover : sig
  val get_leftover_task_segs : start:int64 -> sched -> Task_ds.task_seg Seq.t

  val sched_for_leftover_task_segs :
    start:int64 -> end_exc:int64 -> sched -> sched
end

module Serialize : sig
  val pack_task_uncompleted_store : task_store -> Sched_t.task list

  val pack_task_completed_store : task_store -> Sched_t.task list

  val pack_task_discarded_store : task_store -> Sched_t.task list

  val pack_task_inst_uncompleted_store :
    task_inst_store -> Sched_t.task_inst list

  val pack_task_inst_completed_store : task_inst_store -> Sched_t.task_inst list

  val pack_task_inst_discarded_store : task_inst_store -> Sched_t.task_inst list

  val pack_task_seg_uncompleted_store : task_seg_store -> Sched_t.task_seg list

  val pack_task_seg_completed_store : task_seg_store -> Sched_t.task_seg list

  val pack_task_seg_discarded_store : task_seg_store -> Sched_t.task_seg list

  val pack_sched_req_pending_store :
    sched_req_store -> Sched_req_ds_t.sched_req list

  val pack_sched_req_record_store :
    sched_req_record_store -> Sched_req_ds_t.sched_req_record list

  val pack_quota :
    int64 Task_inst_id_map.t -> (Task_ds.task_inst_id * int64) list

  val pack_user_id_to_task_ids :
    Int64_set.t User_id_map.t -> (Task_ds_t.user_id * int64 list) list

  val pack_task_id_to_task_inst_ids :
    Int64_set.t Task_id_map.t -> (Task_ds_t.task_id * int64 list) list

  val pack_task_inst_id_to_task_seg_ids :
    Int64_int64_option_set.t Task_inst_id_map.t ->
    (Task_ds_t.task_inst_id * (int64 * int64 option) list) list

  val pack_task_seg_id_to_progress :
    Task_ds.progress Task_seg_id_map.t ->
    (Task_ds_t.task_seg_id * Task_ds_t.progress) list

  val pack_task_inst_id_to_progress :
    Task_ds.progress Task_inst_id_map.t ->
    (Task_ds_t.task_inst_id * Task_ds_t.progress) list

  val pack_indexed_by_start :
    task_seg_place_map -> (int64 * Task_ds_t.task_seg_place list) list

  val pack_sched_req_ids : Int64_set.t -> int64 list

  val pack_sched : sched -> Sched_t.sched

  val pack_sched_diff : sched_diff -> Sched_t.sched_diff

  val json_string_of_sched : sched -> string

  val json_string_of_sched_diff : sched_diff -> string
end

module Deserialize : sig
  val unpack_task_uncompleted_list : Sched_t.task list -> task_store

  val unpack_task_completed_list : Sched_t.task list -> task_store

  val unpack_task_discarded_list : Sched_t.task list -> task_store

  val unpack_task_inst_uncompleted_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_inst_completed_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_inst_discarded_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_seg_uncompleted_list : Sched_t.task_seg list -> task_seg_store

  val unpack_task_seg_completed_list : Sched_t.task_seg list -> task_seg_store

  val unpack_task_seg_discarded_list : Sched_t.task_seg list -> task_seg_store

  val unpack_sched_req_pending_list :
    Sched_req_ds_t.sched_req list -> sched_req_store

  val unpack_sched_req_record_list :
    Sched_req_ds_t.sched_req_record list -> sched_req_record_store

  val unpack_quota :
    (Task_ds.task_inst_id * int64) list -> int64 Task_inst_id_map.t

  val unpack_user_id_to_task_ids :
    (Task_ds_t.user_id * int64 list) list -> Int64_set.t User_id_map.t

  val unpack_task_id_to_task_inst_ids :
    (Task_ds_t.task_id * int64 list) list -> Int64_set.t Task_id_map.t

  val unpack_task_inst_id_to_task_seg_ids :
    (Task_ds_t.task_inst_id * (int64 * int64 option) list) list ->
    Int64_int64_option_set.t Task_inst_id_map.t

  val unpack_task_seg_id_to_progress :
    (Task_ds_t.task_seg_id * Task_ds_t.progress) list ->
    Task_ds.progress Task_seg_id_map.t

  val unpack_task_inst_id_to_progress :
    (Task_ds_t.task_inst_id * Task_ds_t.progress) list ->
    Task_ds.progress Task_inst_id_map.t

  val unpack_indexed_by_start :
    (int64 * Task_ds_t.task_seg_place list) list -> task_seg_place_map

  val unpack_sched_req_ids : int64 list -> Int64_set.t

  val unpack_sched : Sched_t.sched -> sched

  val unpack_sched_diff : Sched_t.sched_diff -> sched_diff

  val sched_of_json_string : string -> sched

  val sched_diff_of_json_string : string -> sched_diff
end

module Equal : sig
  val sched_data_equal : sched_data -> sched_data -> bool

  val sched_equal : sched -> sched -> bool
end

module Diff : sig
  val diff_sched_data : old:sched_data -> sched_data -> sched_data_diff

  val diff_sched : old:sched -> sched -> sched_diff

  val add_diff_sched_data : sched_data_diff -> sched_data -> sched_data

  val add_diff_sched : sched_diff -> sched -> sched

  val sub_diff_sched_data : sched_data_diff -> sched_data -> sched_data

  val sub_diff_sched : sched_diff -> sched -> sched
end

module Print : sig
  val debug_string_of_sched :
    ?indent_level:int -> ?buffer:Buffer.t -> sched -> string

  val debug_print_sched : ?indent_level:int -> sched -> unit
end
