open Util
open IBag

(* ------ Multimap functions ------ *)

module Make(Ord: OrderedType) = struct

  module OrdList = struct
    type t = Ord.t list
    let compare = compare
  end

  module InnerMap = NearMap.Make(OrdList)

  type key = Ord.t list

  type 'a vindex_t = index_t * 'a InnerMap.t

  type content = CMap of content vindex_t | CBag of Bag.t

  (* top level type *)
  type t = content vindex_t

  let init (idxs : index_t) = List.map (fun idx -> idx, InnerMap.empty) idxs

  let from_mmap (m : t) = init @: fst_many idxs

  let singleton idxs x =
    let m = init idxs in
    insert x m

  let is_empty (mm:t) = List.for_all (fun (_, map) -> InnerMap.is_empty map) idxs

  (* insert a value (list representing tuple) into a multimap *)
  let rec insert xs (mm:t) =
  List.map (fun (idx, map) -> idx,
    let key = list_filter_idxs idx.mm_indices xs in
    try begin
      match InnerMap.find key map with
      | CMap v -> InnerMap.add key (CMap(insert x v)) map
      | CBag b -> InnerMap.add key (CBag(Bag.insert x b)) map
      end
    with
      Not_found ->
        if null idx.mm_submaps then
          InnerMap.add key (CBag(Bag.singleton x)) map
        else
          let v  = init idx.mm_submaps in
          InnerMap.add key (CMap(insert x v)) map
  ) idxs

  let rec delete xs (mm:t) =
    List.map (fun (idx, map) -> idx,
      let key = list_filter_idxs idx.mm_indices xs in
      try begin
        match InnerMap.find key map with
        | Cmap v ->
            let v' = delete x v in
            if is_empty v' then InnerMap.remove key map
            else
              (* add back the reduced value *)
              InnerMap.add key (Cmap v') map
        | CBag b ->
            let b' = Bag.delete x b in
            if Bag.is_empty b' then InnerMap.remove key map
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
          let idx, map = List.find (fun (i,_) -> IntSet.equal i.mm_indices idx_id) idxs in
          let key = list_filter_idxs idx.mm_indices xs in
          let find_fn = match comp with
            | `GT -> InnerMap.find_gt
            | `EQ -> InnerMap.find
            | `LT -> InnerMap.find_lt
          in
          begin try
            match find_fn key map with
            | CMap v -> slice rem_ids rem_comps x v
            | CBag b -> b
          with
            Not_found -> Bag.empty
          end
        with
          Not_found    -> error @: "no corresponding index found"
        end
    | [], _       -> error @: "no index provided"
    | _, []       -> error @: "no comps provided"

  let rec combine (l:t) (r:t) =
    List.map2 (fun (idx, map) (idx', map') -> idx,
      InnerMap.merge
        (fun key v v' -> match v, v' with
          | Some _, None    -> v
          | None, Some _    -> v'
          | Some(CMap s), Some(CMap s') -> Some(CMap(combine s s'))
          | Some(CBag s), Some(CBag s') -> Some(CBag(Bag.combine s s'))
          | _ -> failwith "(combine): mismatch in bags/maps")
        map map'
    ) idxs idxs'

  let rec fold f zero (mm:t) =
    let error x = failwith @: "(fold): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | (_,map)::_ ->
      InnerMap.fold (fun _ x acc -> match x with
        | CMap m -> fold f acc m
        | CBag b -> Bag.fold f acc b
      ) map zero
    | [] -> error @: "malformed map"

  let map f (mm:t) =
    fold (fun acc x -> Bag.insert (f x) acc) Bag.empty mm

  let rec iterate f (mm:t) =
    match mm with
    (* doesn't matter which index we take *)
    | ((_,map)::_) ->
      InnerMap.iterate (fun _ x -> match x with
        | CMap m -> iterate f m
        | CBag b -> Bag.iterate f b
      ) map

  (* very hard to implement filter efficiently *)
  let rec filter f mm =
    let map = ref mm in
    iterate (fun x ->
      if not (f x) then map := delete x !mm
    ) !mm;
    !map

  let update old_val new_val mm =
    let mm' = delete old_val mm in
    insert new_val mm'

  let rec peek (mm:t) =
    let error x = failwith @: "(peek): "^x in
    match mm with
    (* doesn't matter which index we take *)
    | ((_, map)::_) ->
      begin try
        match InnerMap.choose map with
        | CMap m -> peek m
        | CBag b -> Bag.peek b
      with Not_found -> None
      end
    | [] -> error @: "malformed multimap"

end
