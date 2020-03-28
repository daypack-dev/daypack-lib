open Cmdliner

let free_time_slots_arg = Arg.(value & flag & info [ "free"])

(* let run (list_free_time_slots : bool) : unit =
 *   match Context.load () with
 *   | Error msg -> print_endline msg
 *   | Ok context ->
 *     if list_free_time_slots then
 *       let hd = Daypack_lib.Sched_ver_history.Read.get_head context in
 *       Daypack_lib.Sched.Agenda.Time_slot.get_free_time_slots *)
