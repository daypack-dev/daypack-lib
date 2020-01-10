let indent_single = "  "

let printf ?(indent_level : int = 0) fmt =
  for _ = 0 to indent_level - 1 do
    print_string indent_single
  done;
  Printf.printf fmt
