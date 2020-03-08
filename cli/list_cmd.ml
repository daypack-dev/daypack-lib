open Cmdliner

let task_arg =
  Arg.(value & flag & info ["task"])

let run (list_task : bool) =
  Printf.printf "task : %b\n" list_task

let cmd =
  Term.(const run $ task_arg),
  Term.info "list"
