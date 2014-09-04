open Util
open K3.AST

(* ------ Multimap functions ------ *)

module type S = sig
  type elt
  type t
  type innerbag
  val init : index_t list -> t
  val from_mmap : t -> t
  val get_idxs : t -> index_t list
  val singleton : index_t list -> elt -> t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val slice : IntSet.t list -> [< `EQ | `GT | `LT ] list -> elt -> t -> innerbag
  val combine : t -> t -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> innerbag
  val iter : (elt -> unit) -> t -> unit
  val iter2 : (elt -> elt -> unit) -> t -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val update : elt -> elt -> t -> t
  val peek : t -> elt option
  val to_list : t -> elt list
end

module type OrderedKeyType =
  sig
    type t
    val compare: t -> t -> int
    val filter_idxs : IntSet.t -> t -> t
  end

module Make(OrdKey: ICommon.OrderedKeyType) = struct

  module InnerMap = NearMap.Make(OrdKey)
  module InnerBag = IBag.Make(OrdKey)

  type innerbag = InnerBag.t

  type elt = OrdKey.t

  type vindex_t = index_t * content InnerMap.t

  (* top level type *)
  and t = vindex_t list

  and content = CMap of t | CBag of innerbag

  let init idxs = List.map (fun idx -> idx, InnerMap.empty) idxs

  let get_idxs mm = List.map fst mm

  let from_mmap m = List.map (fun (idx, _) -> idx, InnerMap.empty) m

  let is_empty mm = List.for_all (fun (_, map) -> InnerMap.is_empty map) mm

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert (xs:elt) (mm:t) =
  List.map (fun (idx, map) -> idx,
    let key = OrdKey.filter_idxs idx.mm_indices xs in
    try begin
      match InnerMap.find key map with
      | CMap m -> InnerMap.add key (CMap(insert xs m)) map
      | CBag b -> InnerMap.add key (CBag(InnerBag.insert xs b)) map
      end
    with
      Not_found ->
        if null idx.mm_submaps then
          InnerMap.add key (CBag(InnerBag.singleton xs)) map
        else
          let v  = init idx.mm_submaps in
          InnerMap.add key (CMap(insert xs v)) map
  ) mm

  let singleton (idxs:index_t list) (x:elt) =
    let m = init idxs in
    insert x m

  let rec delete xs mm =
    List.map (fun (idx, map) -> idx,
      let key = OrdKey.filter_idxs idx.mm_indices xs in
      try begin
        match InnerMap.find key map with
        | CMap v ->
            let v' = delete xs v in
            if is_empty v' then InnerMap.remove key map
            else
              (* add back the reduced value *)
              InnerMap.add key (CMap v') map
        | CBag b ->
            let b' = InnerBag.delete xs b in
            if InnerBag.is_empty b' then InnerMap.remove key map
            else InnerMap.add key (CBag b') map
        end
      with Not_found -> map
    ) mm

  let rec slice idx_ids comps xs mm =
    let error x = failwith @: "(slice):"^x in
    match idx_ids, comps with
    | idx_id::rem_ids, comp::rem_comps ->
        begin try
          (* TODO: change to map here to find faster? *)
          let idx, map = List.find (fun (i,_) -> IntSet.equal i.mm_indices idx_id) mm in
          let key = OrdKey.filter_idxs idx.mm_indices xs in
          let find_fn = match comp with
            | `GT -> InnerMap.find_gt
            | `EQ -> InnerMap.find
            | `LT -> InnerMap.find_lt
          in
          begin try
            match find_fn key map with
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
      InnerMap.merge
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
      InnerMap.fold (fun _ x acc -> match x with
        | CMap m -> fold f acc m
        | CBag b -> InnerBag.fold f acc b
      ) map zero
    | [] -> error @: "malformed multimap"

  let map f mm =
    fold (fun acc x -> InnerBag.insert (f x) acc) InnerBag.empty mm

  let rec iter f = function
    (* doesn't matter which index we take *)
    | ((_,map)::_) ->
      InnerMap.iter (fun _ x -> match x with
        | CMap m -> iter f m
        | CBag b -> InnerBag.iter f b
      ) map
    | [] -> failwith @: "(iter): malformed multimap"

  let rec iter2 f mm mm' = match mm, mm' with
    (* doesn't matter which index we take *)
    | ((_,map)::_), ((_,map')::_) ->
      InnerMap.iter2 (fun _ x x' -> match x, x' with
        | CMap m, CMap m' -> iter2 f m m'
        | CBag b, CBag b' -> InnerBag.iter2 f b b'
        | _ -> failwith "(iter2): mismatch between multimaps"
      ) map map'
    | _ -> failwith @: "(iter): malformed multimap"

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
        match snd @: InnerMap.choose map with
        | CMap m -> peek m
        | CBag b -> InnerBag.peek b
      with Not_found -> None end
    | [] -> error @: "malformed multimap"

  let to_list mm = fold (fun acc x -> x::acc) [] mm

end
