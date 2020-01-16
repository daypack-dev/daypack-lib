exception Invalid_diff

module type S = sig
  type 'a t

  type 'a diff = {
    common : 'a t;
    updated : ('a * 'a) t;
    added : 'a t;
    removed : 'a t;
  }

  val diff : old:'a t -> 'a t -> 'a diff
end

module type S_bucketed = sig
  type 'a map

  type set

  type diff_bucketed = {
    common : set map;
    added : set map;
    removed : set map;
  }

  val diff_bucketed : old:set map -> set map -> diff_bucketed
end

module Make (M : Map.S) : S with type 'a t := 'a M.t = struct
  type 'a t = 'a M.t

  type 'a diff = {
    common : 'a t;
    updated : ('a * 'a) t;
    added : 'a t;
    removed : 'a t;
  }

  let get_common (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, None -> None
         | Some _, None -> None
         | None, Some _ -> None
         | Some x1, Some x2 -> if x1 = x2 then Some x1 else None)
      m1 m2

  let get_updated (m1 : 'a t) (m2 : 'a t) : ('a * 'a) t =
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, None -> None
         | Some _, None -> None
         | None, Some _ -> None
         | Some x1, Some x2 -> if x1 <> x2 then Some (x1, x2) else None)
      m1 m2

  let get_added (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.filter (fun key2 _ ->
        not (M.mem key2 m1)
      ) m2

  let get_removed (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.filter (fun key1 _ ->
        not (M.mem key1 m2)
      ) m1

  let diff ~(old : 'a t) (m : 'a t) : 'a diff =
    {
      common = get_common old m;
      updated = get_updated old m;
      added = get_added old m;
      removed = get_removed old m;
    }

  let add (diff : 'a diff) (m : 'a t) : 'a t =
    m
    (* apply updates *)
    |> M.mapi (fun key x ->
        match M.find_opt key diff.updated with
        | None -> x
        | Some (x1, x2) -> if x1 = x then x2 else raise Invalid_diff
      )
    (* add *)
    |> M.union (fun _key _ _ ->
        raise Invalid_diff
      ) diff.added
    (* remove *)
    |> M.merge (fun _key to_be_removed x ->
        match to_be_removed, x with
        | None, _
        | _, None -> x
        | Some to_be_removed, Some x -> if x = to_be_removed then None else raise Invalid_diff
      ) diff.removed

  let sub (diff : 'a diff) (m : 'a t) : 'a t =
    m
      (* revert updates *)
    |> M.mapi (fun key x ->
        match M.find_opt key diff.updated with
        | None -> x
        | Some (x1, x2) -> if x2 = x then x1 else Invalid_diff
      )
      (* revert add *)
    |> M.merge (fun _key to_be_removed x ->
        match to_be_removed, x with
        | None, _
        | _, None -> x
        | Some to_be_removed, Some x -> if x = to_be_removed then None else raise Invalid_diff
      ) diff.added
    (* revert remove *)
    |> M.union (fun _key _ _ -> raise Invalid_diff)
      diff.removed
end

module Make_bucketed (Map : Map.S) (Set : Set.S) :
  S_bucketed with type 'a map := 'a Map.t and type set := Set.t = struct
  type 'a map = 'a Map.t

  type set = Set.t

  type diff_bucketed = {
    common : set map;
    added : set map;
    removed : set map;
  }

  let get_common (m1 : set map) (m2 : set map) : set map =
    Map.merge
      (fun _key s1 s2 ->
         match (s1, s2) with
         | None, None -> None
         | Some _, None -> None
         | None, Some _ -> None
         | Some s1, Some s2 -> Some (Set.inter s1 s2))
      m1 m2

  let get_added (m1 : set map) (m2 : set map) : set map =
    Map.merge
      (fun _key s1 s2 ->
         match (s1, s2) with
         | None, None -> None
         | Some _, None -> None
         | None, Some _ -> s2
         | Some s1, Some s2 -> Some (Set.diff s2 s1))
      m1 m2

  let get_removed (m1 : set map) (m2 : set map) : set map =
    Map.merge
      (fun _key s1 s2 ->
         match (s1, s2) with
         | None, None -> None
         | Some _, None -> s1
         | None, Some _ -> None
         | Some s1, Some s2 -> Some (Set.diff s1 s2))
      m1 m2

  let diff_bucketed ~(old : set map) (m : set map) : diff_bucketed =
    {
      common = get_common old m;
      added = get_added old m;
      removed = get_removed old m;
    }
end
