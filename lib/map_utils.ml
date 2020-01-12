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
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, None -> None
         | Some _, None -> None
         | None, Some _ -> x2
         | Some _, Some _ -> None)
      m1 m2

  let get_removed (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, None -> None
         | Some _, None -> x1
         | None, Some _ -> None
         | Some _, Some _ -> None)
      m1 m2

  let diff ~(old : 'a t) (m : 'a t) : 'a diff =
    {
      common = get_common old m;
      updated = get_updated old m;
      added = get_added old m;
      removed = get_removed old m;
    }
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
