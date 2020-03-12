open Cmdliner

let task_arg =
  Arg.(value & flag & info ["task"])

(* let run (add_task : bool) : unit =
 *   if add_task then (
 *     let name = Dialog.ask ~prompt:"Enter task name" (fun s ->
 *         if s <> "" then Error "Task name cannot be empty" else Ok s
 *       )
 *     in
 *     let task_type = Dialog.ask_pick_choice ~prompt:"Pick task type" [("one-off", `One_off); ("recurring", `Recurring)] in
 *     let data = Daypack_lib.Task_ds.{
 *       splittable = false;
 *       parallelizable = false;
 *       task_type;
 *       name
 *     }
 *     in
 * 
 *   );
 *   () *)

let cmd =
  Term.(const ()),
  Term.info "add"
