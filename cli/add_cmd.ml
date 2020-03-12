open Cmdliner

type task_type_choice = [
  `One_off | `Recurring
]

let task_arg =
  Arg.(value & flag & info ["task"])

let run (add_task : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    if add_task then (
      let name = Dialog.ask ~prompt:"Enter task name" (fun s ->
          if s = "" then Error "Task name cannot be empty" else Ok s
        )
      in
      let task_type_choice = Dialog.ask_pick_choice ~prompt:"Pick task type" [("one-off", `One_off); ("recurring", `Recurring)] in
      let task_type =
        match task_type_choice with
        | `One_off ->
          Daypack_lib.Task_ds.One_off
        | `Recurring -> One_off
      in
      let data = Daypack_lib.Task_ds.{
          splittable = false;
          parallelizable = false;
          task_type;
          name;
        }
      in
      Daypack_lib.Sched_ver_history.In_place_head.Task.Add.add_task ~parent_user_id:0L data []
        context.sched_ver_history
      |> ignore;
    );
    Context.save context |> Result.get_ok;
    ()

let cmd =
  Term.(const run $ task_arg),
  Term.info "add"
