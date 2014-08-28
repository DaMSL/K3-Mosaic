open Util
open K3.AST
open K3Values
open K3Values.Value

(* ------ Multimap functions ------ *)

let init idxs =
  VMultimap(List.map (fun idx -> idx, ValueMap.empty) idxs)

let is_empty mm = match mm with
  | VMultimap idxs ->
      List.for_all (fun (_, map) -> ValueMap.is_empty map) idxs
  | x -> failwith @: "(is_empty): not a multimap: " ^ repr_of_value x

(* insert a value into a multimap *)
let rec insert x mm =
  let error x = failwith @: "(insert):"^x in
  match mm, x with
  | VMultimap idxs, VTuple xs ->
      VMultimap(
        List.map (fun (idx, map) -> idx,
          let key = VTuple(list_filter_idxs idx.mm_indices xs) in
          try begin
            match ValueMap.find key map with
            | VMultimap _ as v ->
                let v' = insert x v in
                ValueMap.add key v' map
            | VBag b -> ValueMap.add key (VBag(Bag.insert x b)) map
            | x -> error @: "bad value in map ("^repr_of_value x^")"
            end
          with
            Not_found ->
              if null idx.mm_submaps then
                ValueMap.add key (VBag (Bag.singleton x)) map
              else
                let v = init idx.mm_submaps in
                let v' = insert x v in
                ValueMap.add key v' map
        ) idxs
      )
  | VMultimap _, x -> error @: "not a tuple: " ^ repr_of_value x
  | x, _           -> error @: "not a multimap: " ^ repr_of_value x

let rec delete x mm =
  let error x = failwith @: "(delete):"^x in
  match mm, x with
  | VMultimap idxs, VTuple xs ->
      VMultimap(
        List.map (fun (idx, map) -> idx,
          let key = VTuple(list_filter_idxs idx.mm_indices xs) in
          try begin
            match ValueMap.find key map with
            | VMultimap _ as v ->
                let v' = delete x v in
                if is_empty v' then
                  ValueMap.remove key map
                else
                  (* add back the reduced value *)
                  ValueMap.add key v' map
            | VBag b ->
                let b' = Bag.delete x b in
                if Bag.is_empty b' then ValueMap.remove key map
                else ValueMap.add key (VBag b') map
            | x -> error @: "unexpected value: " ^ repr_of_value x
            end
          with Not_found -> map
        ) idxs
      )
  | VMultimap _, x -> error @: "not a tuple: " ^ repr_of_value x
  | x, _           -> error @: "not a valuemap" ^ repr_of_value x

let rec slice idx_ids comps x mm =
  let error x = failwith @: "(slice):"^x in
  match mm, x, idx_ids, comps with
  | VMultimap idxs, VTuple xs, idx_id::rem_ids, comp::rem_comps ->
      begin try
        (* TODO: change to map here to find faster? *)
        let idx, map = List.find (fun (i,_) -> IntSet.equal i.mm_indices idx_id) idxs in
        let key = VTuple(list_filter_idxs idx.mm_indices xs) in
        let find_fn = match comp with
          | VInt 1    -> ValueMap.find_gt
          | VInt 0    -> ValueMap.find
          | VInt (-1) -> ValueMap.find_lt
          | _ -> failwith "(slice): unexpected comp value"
        in
        begin try
          match find_fn key map with
          | VMultimap _ as v -> slice rem_ids rem_comps x v
          | VBag b as v      -> v
          | _ -> failwith "(slice): unexpected value in map"
        with
          Not_found -> VBag Bag.empty
        end
      with
        Not_found    -> error @: "no corresponding index found"
      end
  | _, _, [], _       -> error @: "no index provided"
  | _, _, _, []       -> error @: "no comps provided"
  | VMultimap _,x,_,_ -> error @: "not a tuple:" ^ repr_of_value x
  | x, _, _, _        -> error @: "not a valuemap" ^ repr_of_value x

let rec merge l r = match l, r with
  | VMultimap idxs, VMultimap idxs' ->
      VMultimap(
        List.map2 (fun (idx, map) (idx', map') -> idx,
          ValueMap.merge
            (fun key v v' -> match v, v' with
               | Some _, None    -> v
               | None, Some _    -> v'
               | Some(VMultimap _ as submaps),
                 Some(VMultimap _ as submaps') ->
                  Some(merge submaps submaps')
               | Some(VBag x), Some(VBag x') -> Some(VBag(Bag.merge x x'))
               | _ -> None)
            map map'
        ) idxs idxs'
      )

  | VMultimap _, x
  | x, _           -> failwith @: "(merge): not a multimap:" ^ repr_of_value x

let rec fold f zero mm =
  let error x = failwith @: "(fold): "^x in
  match mm with
  (* doesn't matter which index we take *)
  | VMultimap ((_,map)::_) ->
    ValueMap.fold (fun _ x acc -> match x with
      | VMultimap _ -> fold f acc x
      | VBag b      -> Bag.fold f acc b
      | x           -> error @: "unexpected value in multimap: " ^ repr_of_value x
    ) map zero
  | x -> error @: "not a multimap:" ^ repr_of_value x

let map f mm =
  fold (fun acc x -> Bag.insert (f x) acc) Bag.empty mm

let rec iterate f mm =
  let error x = failwith @: "(iterate): "^x in
  match mm with
  (* doesn't matter which index we take *)
  | VMultimap ((_,map)::_) ->
    ValueMap.iterate (fun _ x -> match x with
      | VMultimap _ -> iterate f x
      | VBag b      -> Bag.iterate f b
      | x           -> error @: "unexpected value in multimap: " ^ repr_of_value x
    ) map
  | x -> error @: "not a multimap:" ^ repr_of_value x

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

let rec peek mm =
  let error x = failwith @: "(peek): "^x in
  match mm with
  (* doesn't matter which index we take *)
  | VMultimap ((_, map)::_) ->
    begin try
      match ValueMap.choose map with
      | VMultimap _ as v -> peek v
      | VBag b           -> Bag.peek b
    with Not_found -> VOption None
    end
  | x -> error @: "not a multimap:" ^ repr_of_value x

