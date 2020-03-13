let ask (type a) ~(prompt : string) (f : string -> (a, string) result) : a =
  let rec aux prompt f =
    Printf.printf "%s : " prompt;
    let s = read_line () in
    match f s with
    | Ok x -> x
    | Error msg -> Printf.printf "Error : %s\n" msg;
      aux prompt f
  in
  aux prompt f

let ask_pick_choice (type a) ~(prompt : string) (choices : (string * a) list) : a =
  Printf.printf "%s :\n" prompt;
  List.iter
    (fun (s, _) ->
       Printf.printf "  %s\n" s;
    ) choices;
  ask ~prompt:"Please enter choice (full string or a uniquely matching substring)" (fun s ->
      Ok (List.assoc s choices)
    )

(* let ask_sched_req () : sched_req_data =
 *   let sched_req_choice =
 *     ask_pick_choice ~prompt:"Pick scheduling request type" [
 *     ("fixed", `Fixed); ("shift", `Shift); ("split_and_shift", `Split_and_shift); ("split_even", `Split_even);
 *     ("time_share", `Time_share); ("push_toward", `Push_toward)
 *   ] in
 *   match sched_req_choice with
 *   | `Fixed -> *)
