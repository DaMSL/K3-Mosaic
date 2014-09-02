open Util
open K3.AST

(* ------ Multimap functions ------ *)

module type S = sig
  type elt
  type row = elt list
  type t
  val init : index_t list -> t
  val from_mmap : t -> t
  val get_idxs : t -> index_t list
  val singleton : index_t list -> row -> t
  val is_empty : t -> bool
  val insert : row -> t -> t
  val delete : row -> t -> t
  val slice : IntSet.t list -> [< `EQ | `GT | `LT ] list -> row -> t -> row IBag.t
  val combine : t -> t -> t
  val fold : ('a -> row -> 'a) -> 'a -> t -> 'a
  val map : (row -> 'a) -> t -> 'a IBag.t
  val iter : (row -> unit) -> t -> unit
  val iter2 : (row -> row -> unit) -> t -> t -> unit
  val filter : (row -> bool) -> t -> t
  val update : row -> row -> t -> t
  val peek : t -> row option
  val to_list : t -> row list
end

module Make(Ord: NearMap.OrderedType) = struct

  module OrdList = struct
    type t = Ord.t list
    let compare = compare
  end

  module InnerMap = NearMap.Make(OrdList)

  type elt = Ord.t

  type row = elt list

  type vindex_t = index_t * content InnerMap.t

  (* top level type *)
  and t = vindex_t list

  and content = CMap of t | CBag of row IBag.t

  let init idxs = List.map (fun idx -> idx, InnerMap.empty) idxs

  let get_idxs mm = List.map fst mm

  let from_mmap m = List.map (fun (idx, _) -> idx, InnerMap.empty) m

  let is_empty mm = List.for_all (fun (_, map) -> InnerMap.is_empty map) mm

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert (xs:row) (mm:t) =
  List.map (fun (idx, map) -> idx,
    let key = list_filter_idxs idx.mm_indices xs in
    try begin
      match InnerMap.find key map with
      | CMap m -> InnerMap.add key (CMap(insert xs m)) map
      | CBag b -> InnerMap.add key (CBag(IBag.insert xs b)) map
      end
    with
      Not_found ->
        if null idx.mm_submaps then
          InnerMap.add key (CBag(IBag.singleton xs)) map
        else
          let v  = init idx.mm_submaps in
          InnerMap.add key (CMap(insert xs v)) map
  ) mm

  let singleton (idxs:index_t list) (x:row) =
    let m = init idxs in
    insert x m

  let rec delete xs mm =
    List.map (fun (idx, map) -> idx,
      let key = list_filter_idxs idx.mm_indices xs in
      try begin
        match InnerMap.find key map with
        | CMap v ->
            let v' = delete xs v in
            if is_empty v' then InnerMap.remove key map
            else
              (* add back the reduced value *)
              InnerMap.add key (CMap v') map
        | CBag b ->
            let b' = IBag.delete xs b in
            if IBag.is_empty b' then InnerMap.remove key map
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
          let key = list_filter_idxs idx.mm_indices xs in
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
            Not_found -> IBag.empty
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
          | Some(CBag s), Some(CBag s') -> Some(CBag(IBag.combine s s'))
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
        | CBag b -> IBag.fold f acc b
      ) map zero
    | [] -> error @: "malformed multimap"

  let map f mm =
    fold (fun acc x -> IBag.insert (f x) acc) IBag.empty mm

  let rec iter f = function
    (* doesn't matter which index we take *)
    | ((_,map)::_) ->
      InnerMap.iter (fun _ x -> match x with
        | CMap m -> iter f m
        | CBag b -> IBag.iter f b
      ) map
    | [] -> failwith @: "(iter): malformed multimap"

  let rec iter2 f mm mm' = match mm, mm' with
    (* doesn't matter which index we take *)
    | ((_,map)::_), ((_,map')::_) ->
      InnerMap.iter2 (fun _ x x' -> match x, x' with
        | CMap m, CMap m' -> iter2 f m m'
        | CBag b, CBag b' -> IBag.iter2 f b b'
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
        | CBag b -> IBag.peek b
      with Not_found -> None end
    | [] -> error @: "malformed multimap"

  let to_list mm = fold (fun acc x -> x::acc) [] mm

end
