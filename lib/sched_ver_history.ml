module Serialize = struct
let to_base_and_diffs (l : Sched.sched list) :
  (Sched.sched * Sched.sched_diff list) option =
  let rec aux
      (base_and_last_and_diffs :
         (Sched.sched * Sched.sched * Sched.sched_diff list) option)
      (l : Sched.sched list) =
    match l with
    | [] -> (
        match base_and_last_and_diffs with
        | None -> None
        | Some (base, _, diffs) -> Some (base, List.rev diffs) )
    | sched :: rest -> (
        match base_and_last_and_diffs with
        | None -> aux (Some (sched, sched, [])) rest
        | Some (base, last, diffs) ->
          let diff = Sched.Diff.diff_sched ~old:last sched in
          aux (Some (base, sched, diff :: diffs)) rest )
  in
  aux None (List.rev l)
end

module Deserialize = struct
let of_base_and_diffs (base : Sched.sched) (diffs : Sched.sched_diff list) :
  Sched.sched list =
  let rec aux (acc : Sched.sched list) (cur : Sched.sched)
      (diffs : Sched.sched_diff list) : Sched.sched list =
    match diffs with
    | [] -> acc
    | diff :: diffs ->
      let next = Sched.Diff.add_diff_sched diff cur in
      aux (next :: acc) next diffs
  in
  aux [ base ] base diffs
end

