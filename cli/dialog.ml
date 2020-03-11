let ask (type a b) ~(prompt : string) (f : string -> (a, b) result) : (a, b) result =
  Printf.printf "%s : " prompt;
  let s = read_line () in
  f s
