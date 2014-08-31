open Util
open K3Values

(* ----- Map functions ----- *)

let kv_of_tuple = function
  | VTuple [k'; v'] -> k', v'
  | _ -> failwith "Not a key-value tuple"

let tuple_of_kv k v = VTuple [k; v]


let of_list l = List.fold_left (fun acc x ->
                  let k, v = extract_kv x in ValueMap.add key v acc)
                ValueMap.empty l

let to_list m = ValueMap.fold (fun key v acc -> (VTuple [key; v])::acc) vm []

let singleton x = ValueMap.singleton x

let empty = ValueMap.empty

let is_empty = ValueMap.is_empty

let insert x m =
  let k, v = extract_kv x in
  ValueMap.add k v m

let delete x m = 
  let k, _ = extract_kv x in
  ValueMap.remove k m

let fold f zero m =
  ValueMap.fold (fun k v acc ->
      let x = tuple_of_kv k v in
      f acc x
    ) m zero

let map f m = ValueMap.mapi (fun k v ->
    let x = tuple_of_kv k v in
    f x
  ) m

let filter f m = ValueMap.filter (fun k v ->
  match f @: tuple_of_kv k v with
  | VBool b -> b
  | _       -> failwith "not a bool"
  ) m


let iterate f m = ValueMap.iter (fun k v ->
  match f @: tuple_of_kv v with
  | VUnit -> ()
  | _     -> failwith "not a unit"
  ) m

let combine x y = ValueMap.merge (fun k mv mv' ->
    match mv, mv' with
    | Some x, _  -> Some x
    | _, Some y  -> Some y
    | None, None -> None
  ) x y

let peek m = 
  try
    let k, v = choose m in
    VOption(Some(tuple_of_kv k v))
  with Not_found -> VOption None
  
let update old_val new_val m = 
  let m' = delete old_val m in
  insert new_val m'
