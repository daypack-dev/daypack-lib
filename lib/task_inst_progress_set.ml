include Set.Make (struct
    type t = Task.task_inst_progress

    let compare = compare
  end)

module Serialize = struct
  let pack (t : t) : Task.task_inst_progress list = t |> to_seq |> List.of_seq
end

module Deserialize = struct
  let unpack (l : Task.task_inst_progress list) : t = l |> List.to_seq |> of_seq
end
