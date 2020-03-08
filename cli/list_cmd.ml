open Cmdliner

let task_arg =
  Arg.(value & flag & info ["task"])

let run (list_task : bool) : unit =
  match Context.load () with
  | Error msg -> print_endline msg
  | Ok context ->
    let hd = Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history in
    ()

let cmd =
  Term.(const run $ task_arg),
  Term.info "list"
