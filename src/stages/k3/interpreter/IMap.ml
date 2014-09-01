open Util
open K3Values

(* ----- Map functions ----- *)


let singleton = ValueMap.singleton

let empty = ValueMap.empty

let is_empty = ValueMap.is_empty

let insert = ValueMap.add

let delete = ValueMap.remove

let fold f zero m =
  ValueMap.fold (fun k v acc -> f acc k v) m zero

let to_list m = fold (fun acc k v -> (k, v)::acc) [] m

let from_list l = List.fold_left (fun acc (k,v) -> insert k v acc) empty l

let map = ValueMap.mapi

let filter = ValueMap.filter

let iter = ValueMap.iter

let iter2 = ValueMap.iter2

let combine x y = ValueMap.merge (fun k mv mv' ->
    match mv, mv' with
    | Some x, _  -> Some x
    | _, Some y  -> Some y
    | None, None -> None
  ) x y

let peek m =
  try
    let k, v = ValueMap.choose m in
    Some (k, v)
  with Not_found -> None

let update k v k' v' m =
  let m' = delete k m in
  insert k' v' m'
