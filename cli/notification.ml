let display_pending_sched_reqs (context : Context.t) : unit =
  let hd =
    Daypack_lib.Sched_ver_history.Read.get_head context.sched_ver_history
  in
  let pending_sched_reqs =
    Daypack_lib.Sched.Sched_req.To_seq.pending_sched_req_seq hd |> List.of_seq
  in
  match pending_sched_reqs with
  | [] ->
    print_endline "No pending scheduling requests"
  | l ->
    Printf.printf "Pending scheduling requests: %d\n" (List.length l)

let display (context : Context.t) : unit =
  display_pending_sched_reqs context;
  print_newline ()
