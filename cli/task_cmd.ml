open Cmdliner

let list_cmd =
  Term.(const ()),
  Term.info "list"

let cmd =
  Term.eval_choice list_cmd []
