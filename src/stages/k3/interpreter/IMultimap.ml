open Util
open K3.AST

(* ------ Multimap functions ------ *)

module type S = sig
  type elt
  type t
  module InnerBag : IBag.S
  val init : IndexSet.t -> t
  val from_mmap : t -> t
  val get_idxs : t -> IndexSet.t
  val singleton : IndexSet.t -> elt -> t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val slice : index_t -> comp_t -> elt -> t -> InnerBag.t
  val combine : t -> t -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> InnerBag.t
  val iter : (elt -> unit) -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val update : elt -> elt -> t -> t
  val peek : t -> elt option
  val to_list : t -> elt list
  val compare : t -> t -> int
end

module Make(OrdKey: ICommon.OrderedKeyType) = struct

  module MMap = NearMap.Make(OrdKey)
  module InnerBag : (IBag.S
    with type elt = OrdKey.t
     and type t = int HashMap.Make(OrdKey).t) = IBag.Make(OrdKey)

  type elt = OrdKey.t

  type content = InnerBag.t

  (* map from index_t to bags of elements *)
  type t = (content MMap.t) IndexMap.t

  let init idxs = IndexSet.fold (fun idx acc -> IndexMap.add idx MMap.empty acc) idxs IndexMap.empty

  let get_idxs mm = IndexMap.fold (fun idx _ acc -> IndexSet.add idx acc) mm IndexSet.empty

  let from_mmap mm = IndexMap.map (fun _ -> MMap.empty) mm

  (* if even one map is empty, we're empty *)
  let is_empty mm = MMap.is_empty @@ snd @@ IndexMap.choose mm

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert (xs:elt) (mm:t) =
    IndexMap.mapi (fun idx map ->
      let key = OrdKey.filter_idxs idx xs in
      try
        let b = MMap.find key map in
        MMap.add key (InnerBag.insert xs b) map
      with Not_found ->
        MMap.add key (InnerBag.singleton xs) map
    ) mm

  let singleton idxs (x:elt) =
    let m = init idxs in
    insert x m

  let delete xs mm =
    IndexMap.mapi (fun idx map ->
      let key = OrdKey.filter_idxs idx xs in
      try
        let b =  MMap.find key map
              |> InnerBag.delete xs in
        if InnerBag.is_empty b then MMap.remove key map
        else MMap.add key b map
      with Not_found -> map
    ) mm

  (* @eqset: set of key members that need to remain equal even in LT/GT *)
  (* Assumes that max vid is the last member *)
  let slice (idx:index_t) comp (xs:elt) (mm:t) : InnerBag.t =
    let error x = failwith @@ "(slice):"^x in
    try
      let mmap = IndexMap.find idx mm in
      let key  = OrdKey.filter_idxs idx xs in
      begin try
        begin match idx with
          | OrdIdx(_, eq_set') ->
              let eq_set = HashIdx(eq_set') in
              let eq_key = OrdKey.filter_idxs eq_set xs in
              let find_all minmax key m =
                List.fold_left InnerBag.union InnerBag.empty @@
                    MMap.find_range key (OrdKey.set_to_minmax minmax eq_set' key) m
              in
              let find_fn = match comp with
                | GT  -> MMap.find_gt
                | EQ  -> MMap.find
                | LT  -> MMap.find_lt
                | LTA -> find_all `Min
                | GTA -> find_all `Max
              in
              let res = find_fn key mmap in

              (* check that the equality constraint holds *)
              if not (IntSet.is_empty eq_set') && List.mem comp [GT; LT] then
                begin match InnerBag.peek res with
                | None   -> res
                | Some x ->
                    let eq_key2 = OrdKey.filter_idxs eq_set x in
                    if OrdKey.compare eq_key eq_key2 = 0 then res
                    else InnerBag.empty
                end
              else res
          | HashIdx _ ->
              begin match comp with
              | EQ  -> MMap.find key mmap
              | _  -> failwith "Unsupported comparison for hash idx"
              end
        end
      with Not_found -> InnerBag.empty end
    with Not_found -> error "no corresponding index found"

  let combine l r =
    IndexMap.merge (fun idx lmap rmap ->
      match lmap, rmap with
      | Some lmap, Some rmap -> some @@ MMap.merge (fun _ b b' -> match b, b' with
          | Some b, Some b'  -> some @@ InnerBag.union b b'
          | Some b, _
          | _, Some b        -> Some b
          | _                -> None
        ) lmap rmap
      | _ -> failwith "(combine): mismatch in indexes"
    ) l r

  let fold f zero mm =
    (* doesn't matter which index we take *)
    try
      let map = snd @@ IndexMap.choose mm in
      MMap.fold (fun _ x acc -> InnerBag.fold f acc x) map zero
    with Not_found -> zero

  (* map can convert to a completely different elt type *)
  let map f mm =
    fold (fun acc x -> InnerBag.insert (f x) acc) InnerBag.empty mm

  let iter f mm =
    (* doesn't matter which index we take *)
    try
      let _, map = IndexMap.choose mm in
      MMap.iter (fun _ b -> InnerBag.iter f b) map
    with Not_found -> ()

  let filter f mm =
    fold (fun acc x ->
      if f x then insert x acc else acc)
      (from_mmap mm) mm

  let update old_val new_val mm =
    let mm' = delete old_val mm in
    insert new_val mm'

  let rec peek mm =
    try
      let map = snd @@ IndexMap.choose mm in
      let b = snd @@ MMap.choose map in
      InnerBag.peek b
    with Not_found -> None

  let to_list mm = fold (fun acc x -> x::acc) [] mm

  (* compare on any index *)
  let compare m m' =
    let idx, map  = IndexMap.choose m in
    try
      let map' = IndexMap.find idx m' in
      MMap.compare InnerBag.compare map map'
    with Not_found -> failwith "(compare): mismatching indexes"

end
