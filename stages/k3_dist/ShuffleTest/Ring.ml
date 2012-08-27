open Util

exception EmptyRing

(* would be much faster as array or binary tree *)
type t_node = string * int (* node * hash *)
type t_ring = t_node list 

let node_ring : t_ring ref = ref []
let replicas = ref 1

let set_replicas num = replicas := num

let add_node name = 
  let range = create_range 1 !replicas in
  let new_elems = List.map 
    (fun i -> let n = name^string_of_int i in (name, Hashtbl.hash n))
    range in
  node_ring := List.sort (fun x y -> snd x - snd y) (new_elems@ (!node_ring))

let remove_node name =
  node_ring := List.remove_assoc name !node_ring

(* do we need to hash here? Probably not. We just want our linearization *)
let get_node_for data =
  try
    fst @: List.find (fun (_,n_hash) -> n_hash >= data) !node_ring
  with Not_found -> match !node_ring with h::t -> fst h 
    | [] -> raise EmptyRing

