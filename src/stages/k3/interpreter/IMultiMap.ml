open Util
open IBag
open K3.AST

(* ------ Multimap functions ------ *)

module Make(Ord: NearMap.OrderedType) = struct

  module OrdList = struct
    type t = Ord.t list
    let compare = compare
  end

  module InnerMap = NearMap.Make(OrdList)

  type key = Ord.t list

  type 'a vindex_t = index_t * 'a InnerMap.t

  (* top level type *)
  type 'a t = 'a content vindex_t list

  and  'a content = CMap of 'a t | CBag of 'a IBag.t

  let init (idxs : index_t list) = List.map (fun idx -> idx, InnerMap.empty) idxs

  let from_mmap (m : 'a t) = List.map (fun (idx, _) -> idx, InnerMap.empty) m

  let singleton idxs x =
    let m = init idxs in
    insert x m

  let is_empty (mm: 'a t) = List.for_all (fun (_, map) -> InnerMap.is_empty map) mm

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert xs (mm: 'a t) =
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

  let rec delete xs (mm:'a t) =
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

  let rec combine (l:'a t) (r:'a t) =
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

  let rec fold f zero (mm:'a t) =
    let error x = failwith @: "(fold): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | (_,map)::_ ->
      InnerMap.fold (fun _ x acc -> match x with
        | CMap m -> fold f acc m
        | CBag b -> IBag.fold f acc b
      ) map zero
    | [] -> error @: "malformed map"

  let map f (mm:'a t) =
    fold (fun acc x -> IBag.insert (f x) acc) IBag.empty mm

  let rec iter f (mm:'a t) =
    match mm with
    (* doesn't matter which index we take *)
    | ((_,map)::_) ->
      InnerMap.iter (fun _ x -> match x with
        | CMap m -> iter f m
        | CBag b -> IBag.iter f b
      ) map
    | [] -> failwith @: "(iter): malformed map"

  (* very hard to implement filter efficiently *)
  let rec filter f (mm : 'a t) =
    fold (fun acc x ->
        if f x then insert x acc
        else acc) (from_mmap mm) mm

  let update old_val new_val (mm : 'a t) =
    let mm' = delete old_val mm in
    insert new_val mm'

  let rec peek (mm : 'a t) =
    let error x = failwith @: "(peek): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | ((_, map)::_) ->
      begin try
        match snd @: InnerMap.choose map with
        | CMap (m:'a t) -> peek m
        | CBag b        -> IBag.peek b
      with Not_found -> None end
    | [] -> error @: "malformed multimap"

  let to_list (mm : 'a t) = fold (fun acc x -> x::acc) [] mm

end
