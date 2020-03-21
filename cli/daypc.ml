open Cmdliner

let cmds = [ Add_cmd.cmd; List_cmd.cmd ]

let default_cmd = (Term.(ret (const (`Help (`Pager, None)))), Term.info "daypc")

let () = Term.(exit @@ Term.eval_choice default_cmd cmds)
