open Cmdliner

let cmds =
  [
    Add_cmd.cmd;
    Complete_cmd.cmd;
    List_cmd.cmd;
    Agenda_cmd.cmd;
    Sched_cmd.cmd;
    Remove_cmd.cmd;
  ]

let default_cmd = (Term.(ret (const (`Help (`Pager, None)))), Term.info "daypc")

let () = Term.(exit @@ Term.eval_choice default_cmd cmds)
