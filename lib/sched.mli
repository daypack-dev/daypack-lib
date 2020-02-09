type sched_id = int

type task_store = Task.task_data Task_id_map.t

type task_store_diff = Task.task_data Task_id_map_utils.diff

type task_inst_store = Task.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task.task_seg_size Task_seg_id_map_utils.diff

type sched_req_store = Sched_req.sched_req_data Sched_req_id_map.t

type sched_req_store_diff = Sched_req.sched_req_data Sched_req_id_map_utils.diff

type sched_req_record_store = Sched_req.sched_req_record_data Sched_req_id_map.t

type sched_req_record_store_diff =
  Sched_req.sched_req_record_data Sched_req_id_map_utils.diff

type task_seg_place_map = Task_seg_place_set.t Int64_map.t

type task_seg_place_map_diff =
  Int64_map_utils.Task_seg_place_bucketed.diff_bucketed

type store = {
  task_store : task_store;
  task_inst_store : task_inst_store;
  task_seg_store : task_seg_store;
  user_id_to_task_ids : Int64_set.t User_id_map.t;
  task_id_to_task_inst_ids : Int64_set.t Task_id_map.t;
  task_inst_id_to_task_seg_ids : Int64_set.t Task_inst_id_map.t;
  sched_req_ids : Int64_set.t;
  sched_req_pending_store : sched_req_store;
  sched_req_record_store : sched_req_record_store;
  quota : int64 Task_inst_id_map.t;
}

type store_diff = {
  task_store_diff : task_store_diff;
  task_inst_store_diff : task_inst_store_diff;
  task_seg_store_diff : task_seg_store_diff;
  user_id_to_task_ids_diff : User_id_map_utils.Int64_bucketed.diff_bucketed;
  task_id_to_task_inst_ids_diff :
    Task_id_map_utils.Int64_bucketed.diff_bucketed;
  task_inst_id_to_task_seg_ids_diff :
    Task_inst_id_map_utils.Int64_bucketed.diff_bucketed;
  sched_req_ids_diff : Int64_set_utils.diff;
  sched_req_pending_store_diff : sched_req_store_diff;
  sched_req_record_store_diff : sched_req_record_store_diff;
  quota_diff : int64 Task_inst_id_map_utils.diff;
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

module Quota_store : sig
  val update_quota : int64 Task_inst_id_map.t -> sched -> sched

  val add_quota : int64 Task_inst_id_map.t -> sched -> sched
end

module Task_seg_store : sig
  val add_task_seg :
    parent_task_inst_id:Task.task_inst_id ->
    Task.task_seg_size ->
    sched ->
    Task.task_seg * sched

  val add_task_seg_via_task_seg_alloc_req :
    Task.task_seg_alloc_req -> sched -> Task.task_seg * sched

  val add_task_segs_via_task_seg_alloc_req_list :
    Task.task_seg_alloc_req list -> sched -> Task.task_seg list * sched

  val find_task_seg_opt : Task.task_seg_id -> sched -> Task.task_seg_size option

  val remove_task_seg : Task.task_seg_id -> sched -> sched

  val remove_task_seg_strict : Task.task_seg_id -> sched -> (sched, unit) result

  val remove_task_seg_seq : Task.task_seg_id Seq.t -> sched -> sched
end

module Task_inst_store : sig
  val add_task_inst :
    parent_task_id:Task.task_id ->
    Task.task_inst_data ->
    sched ->
    Task.task_inst * sched

  val add_task_inst_list :
    parent_task_id:Task.task_id ->
    Task.task_inst_data list ->
    sched ->
    Task.task_inst list * sched

  val find_task_inst_opt :
    Task.task_inst_id -> sched -> Task.task_inst_data option

  val remove_task_inst : Task.task_inst_id -> sched -> sched

  val remove_task_inst_strict :
    Task.task_inst_id -> sched -> (sched, unit) result

  val remove_task_inst_seq : Task.task_inst_id Seq.t -> sched -> sched
end

module Task_store : sig
  val add_task :
    parent_user_id:Task.user_id ->
    Task.task_data ->
    Task.task_inst_data list ->
    sched ->
    Task.task * Task.task_inst list * sched

  val find_task_opt : Task.task_id -> sched -> Task.task_data option

  val remove_task : Task.task_id -> sched -> sched

  val remove_task_strict : Task.task_id -> sched -> (sched, unit) result
end

module Task_seg_place_map : sig
  val add_task_seg_place : Task.task_seg_place -> sched -> sched

  val add_task_seg_place_list : Task.task_seg_place list -> sched -> sched

  val add_task_seg_place_seq : Task.task_seg_place Seq.t -> sched -> sched

  val find_task_seg_place_seq_by_task_seg_id :
    Task.task_seg_id -> sched -> Task.task_seg_place Seq.t

  val filter_task_seg_place_seq :
    (Task.task_seg_place -> bool) -> sched -> Task.task_seg_place Seq.t

  val find_task_seg_place_seq_by_task_id :
    Task.task_id -> sched -> Task.task_seg_place Seq.t

  val find_task_seg_place_seq_by_task_inst_id :
    Task.task_inst_id -> sched -> Task.task_seg_place Seq.t

  val find_task_seg_place_seq_by_task_seg_id :
    Task.task_seg_id -> sched -> Task.task_seg_place Seq.t

  val remove_task_seg_place : Task.task_seg_place -> sched -> sched

  val remove_task_seg_place_seq : Task.task_seg_place Seq.t -> sched -> sched

  val remove_task_seg_place_by_task_id : Task.task_id -> sched -> sched

  val remove_task_seg_place_by_task_inst_id :
    Task.task_inst_id -> sched -> sched

  val remove_task_seg_place_by_task_seg_id : Task.task_seg_id -> sched -> sched
end

module Sched_req_store : sig
  val queue_sched_req_data :
    Sched_req.sched_req_data -> sched -> Sched_req.sched_req * sched

  val queue_sched_req_data_list :
    Sched_req.sched_req_data list -> sched -> Sched_req.sched_req list * sched

  val unqueue_sched_req : Sched_req.sched_req_id -> sched -> sched

  val filter_sched_req_record_seq : (Sched_req.sched_req_record -> bool) -> sched -> Sched_req.sched_req_record Seq.t

  val find_sched_req_record_by_task_id : Task.task_id -> sched -> Sched_req.sched_req_record Seq.t
  val find_sched_req_record_by_task_inst_id : Task.task_inst_id -> sched -> Sched_req.sched_req_record Seq.t
  val find_sched_req_record_by_task_seg_id : Task.task_seg_id -> sched -> Sched_req.sched_req_record Seq.t
  val remove_sched_req_record_if_contains_matching_task_seg : (Task.task_seg -> bool) -> sched -> sched
  val remove_sched_req_record_by_task_id : Task.task_id -> sched -> sched
  val remove_sched_req_record_by_task_inst_id : Task.task_inst_id -> sched -> sched
  val remove_sched_req_record_by_task_seg_id : Task.task_seg_id -> sched -> sched

  val allocate_task_segs_for_pending_sched_reqs :
    start:int64 ->
    end_exc:int64 ->
    include_sched_reqs_partially_within_time_period:bool ->
    up_to_sched_req_id_inc:Sched_req.sched_req_id option ->
    sched ->
    Sched_req.sched_req_record list * sched
end

module Recur : sig
  val instantiate : start:int64 -> end_exc:int64 -> sched -> sched
end

module Serialize : sig
  val pack_task_store : task_store -> Sched_t.task list

  val pack_task_inst_store : task_inst_store -> Sched_t.task_inst list

  val pack_task_seg_store : task_seg_store -> Sched_t.task_seg list

  val pack_sched_req_pending_store :
    sched_req_store -> Sched_req_t.sched_req list

  val pack_sched_req_record_store :
    sched_req_record_store -> Sched_req_t.sched_req_record list

  val pack_quota : int64 Task_inst_id_map.t -> (Task.task_inst_id * int64) list

  val pack_user_id_to_task_ids :
    Int64_set.t User_id_map.t -> (Task_t.user_id * int64 list) list

  val pack_task_id_to_task_inst_ids :
    Int64_set.t Task_id_map.t -> (Task_t.task_id * int64 list) list

  val pack_task_inst_id_to_task_seg_ids :
    Int64_set.t Task_inst_id_map.t -> (Task_t.task_inst_id * int64 list) list

  val pack_indexed_by_start :
    task_seg_place_map -> (int64 * Task_t.task_seg_place list) list

  val pack_sched_req_ids : Int64_set.t -> int64 list

  val json_string_of_sched : sched -> string

  val json_string_of_sched_diff : sched_diff -> string
end

module Deserialize : sig
  val unpack_task_list : Sched_t.task list -> task_store

  val unpack_task_inst_list : Sched_t.task_inst list -> task_inst_store

  val unpack_task_seg_list : Sched_t.task_seg list -> task_seg_store

  val unpack_sched_req_pending_list :
    Sched_req_t.sched_req list -> sched_req_store

  val unpack_sched_req_record_list :
    Sched_req_t.sched_req_record list -> sched_req_record_store

  val unpack_quota :
    (Task.task_inst_id * int64) list -> int64 Task_inst_id_map.t

  val unpack_user_id_to_task_ids :
    (Task_t.user_id * int64 list) list -> Int64_set.t User_id_map.t

  val unpack_task_id_to_task_inst_ids :
    (Task_t.task_id * int64 list) list -> Int64_set.t Task_id_map.t

  val unpack_task_inst_id_to_task_seg_ids :
    (Task_t.task_inst_id * int64 list) list -> Int64_set.t Task_inst_id_map.t

  val unpack_indexed_by_start :
    (int64 * Task_t.task_seg_place list) list -> task_seg_place_map

  val unpack_sched_req_ids : int64 list -> Int64_set.t

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
