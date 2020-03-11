open Cmdliner

let cmds = [
  Add_cmd.cmd;
  List_cmd.cmd;
]

let () =
  Term.(exit @@ Term.eval_choice List_cmd.cmd [])
