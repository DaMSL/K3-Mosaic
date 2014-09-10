open Util
open K3.AST

(* ------ Multimap functions ------ *)

module type S = sig
  type elt
  type t
  module InnerBag : IBag.S
  val init : index_t list -> t
  val from_mmap : t -> t
  val get_idxs : t -> index_t list
  val singleton : index_t list -> elt -> t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val slice : IntSet.t list -> [< `EQ | `GT | `LT ] list -> elt -> t -> InnerBag.t
  val combine : t -> t -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> InnerBag.t
  val iter : (elt -> unit) -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val update : elt -> elt -> t -> t
  val peek : t -> elt option
  val to_list : t -> elt list
  val compare_m : t -> t -> int
end

module Make(OrdKey: ICommon.OrderedKeyType) = struct

  module MMap = NearMap.Make(OrdKey)
  module InnerBag : (IBag.S 
    with type elt = OrdKey.t
     and type t = int HashMap.Make(OrdKey).t) = IBag.Make(OrdKey)

  type elt = OrdKey.t

  type vindex_t = index_t * content MMap.t

  (* top level type *)
  and t = vindex_t list

  and content = CMap of t | CBag of InnerBag.t

  let init idxs = List.map (fun idx -> idx, MMap.empty) idxs

  let get_idxs mm = List.map fst mm

  let from_mmap m = List.map (fun (idx, _) -> idx, MMap.empty) m

  let is_empty mm = List.for_all (fun (_, map) -> MMap.is_empty map) mm

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert (xs:elt) (mm:t) =
  List.map (fun (idx, map) -> idx,
    let key = OrdKey.filter_idxs idx.mm_indices xs in
    try begin
      match MMap.find key map with
      | CMap m -> MMap.add key (CMap(insert xs m)) map
      | CBag b -> MMap.add key (CBag(InnerBag.insert xs b)) map
      end
    with
      Not_found ->
        if null idx.mm_submaps then
          MMap.add key (CBag(InnerBag.singleton xs)) map
        else
          let v  = init idx.mm_submaps in
          MMap.add key (CMap(insert xs v)) map
  ) mm

  let singleton (idxs:index_t list) (x:elt) =
    let m = init idxs in
    insert x m

  let rec delete xs mm =
    List.map (fun (idx, map) -> idx,
      let key = OrdKey.filter_idxs idx.mm_indices xs in
      try begin
        match MMap.find key map with
        | CMap v ->
            let v' = delete xs v in
            if is_empty v' then MMap.remove key map
            else
              (* add back the reduced value *)
              MMap.add key (CMap v') map
        | CBag b ->
            let b' = InnerBag.delete xs b in
            if InnerBag.is_empty b' then MMap.remove key map
            else MMap.add key (CBag b') map
        end
      with Not_found -> map
    ) mm

  let rec slice (idx_ids:IntSet.t list) comps (xs:elt) (mm:t) : InnerBag.t =
    let error x = failwith @: "(slice):"^x in
    match idx_ids, comps with
    | idx_id::rem_ids, comp::rem_comps ->
        begin try
          (* TODO: change to map here to find faster? *)
          let idx, (map:content MMap.t) = List.find (fun (i,_) -> IntSet.equal i.mm_indices idx_id) mm in
          let key = OrdKey.filter_idxs idx.mm_indices xs in
          let (find_fn:MMap.key -> content MMap.t -> content) = match comp with
            | `GT -> MMap.find_gt
            | `EQ -> MMap.find
            | `LT -> MMap.find_lt
          in
          begin try
            match find_fn (key:elt) (map:content MMap.t) with
            | CMap v -> slice rem_ids rem_comps xs v
            | CBag b -> b
          with
            Not_found -> InnerBag.empty
          end
        with
          Not_found    -> error @: "no corresponding index found"
        end
    | [], _       -> error @: "no index provided"
    | _, []       -> error @: "no comps provided"

  let rec combine l r =
    List.map2 (fun (idx, map) (idx', map') -> idx,
      MMap.merge
        (fun key v v' -> match v, v' with
          | Some _, None    -> v
          | None, Some _    -> v'
          | Some(CMap s), Some(CMap s') -> Some(CMap(combine s s'))
          | Some(CBag s), Some(CBag s') -> Some(CBag(InnerBag.combine s s'))
          | _ -> failwith "(combine): mismatch in bags/maps")
        map map'
    ) l r

  let rec fold f zero mm =
    let error x = failwith @: "(fold): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | (_,map)::_ ->
      MMap.fold (fun _ x acc -> match x with
        | CMap m -> fold f acc m
        | CBag b -> InnerBag.fold f acc b
      ) map zero
    | [] -> error @: "malformed multimap"

  let map f mm =
    fold (fun acc x -> InnerBag.insert (f x) acc) InnerBag.empty mm

  let rec iter f = function
    (* doesn't matter which index we take *)
    | ((_,map)::_) ->
      MMap.iter (fun _ x -> match x with
        | CMap m -> iter f m
        | CBag b -> InnerBag.iter f b
      ) map
    | [] -> failwith @: "(iter): malformed multimap"

  (* very hard to implement filter efficiently *)
  let rec filter f mm =
    fold (fun acc x ->
        if f x then insert x acc
        else acc) (from_mmap mm) mm

  let update old_val new_val mm =
    let mm' = delete old_val mm in
    insert new_val mm'

  let rec peek mm =
    let error x = failwith @: "(peek): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | ((_, map)::_) ->
      begin try
        match snd @: MMap.choose map with
        | CMap m -> peek m
        | CBag b -> InnerBag.peek b
      with Not_found -> None end
    | [] -> error @: "malformed multimap"

  let to_list mm = fold (fun acc x -> x::acc) [] mm

  (* compare on any index *)
  let rec compare_m m m' = match m, m' with
    | (_, map)::_, (_, map')::_ ->
        MMap.compare (fun a b -> match a, b with
          | CMap m, CMap m' -> compare_m m m'
          | CBag b, CBag b' -> InnerBag.compare b b'
          | x, y            -> compare x y)
          map map'
    | x, y -> compare x y

end
