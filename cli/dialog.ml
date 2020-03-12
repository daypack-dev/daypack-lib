let ask (type a b) ~(prompt : string) (f : string -> (a, b) result) : (a, b) result =
  Printf.printf "%s : " prompt;
  let s = read_line () in
  f s

let ask_pick_choice (type a) ~(prompt : string) (choices : (string * a) list) : a =
  Printf.printf "%s :\n" prompt;
  List.iter
    (fun (s, _) ->
       Printf.printf "  %s\n" s;
    ) choices;
  ask ~prompt:"Please enter choice" (fun s ->
      Ok (List.assoc s choices)
    )
  |> Result.get_ok
