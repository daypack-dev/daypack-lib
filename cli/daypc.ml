open Cmdliner

let () =
  Term.(exit @@ Term.eval_choice Task_cmd.list_cmd [])
