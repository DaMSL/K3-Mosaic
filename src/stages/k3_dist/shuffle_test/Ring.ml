open Util
open Unix

exception EmptyRing

type hash_t = int
type name_t = string
type node_t = inet_addr * name_t option * hash_t
type ring_t = t_node list 

let node_ring : ring_t ref = ref []
let replicas = ref 1

let set_replicas num = replicas := num

let add_node (addr, m_name) = 
  let range = create_range 1 !replicas in
  let new_elems = List.map 
    (fun i -> let h = Hashtbl.hash @: string_of_inet_addr addr^string_of_int i
        in (addr, m_name, h)
    ) range in
  node_ring := List.sort (fun (_,_,x) (_,_,y) -> x - y) (new_elems @ !node_ring)

let remove_node (addr, m_name) =
  node_ring := List.filter (fun (a,n,_) -> a = addr && n = m_name) !node_ring

let get_ring_node value max_val =
  let scaled = int_of_float @: 
    ((float_of_int (2 * value) /. (float_of_int) max_val) -. 1) *. 
      float_of_int max_int in
  try
    let (a, _, _) = 
      List.find (fun (_,_,hash) -> hash >= scaled) !node_ring
    in a
  with Not_found ->  (* return first node *)
    match !node_ring with 
    | (a,_,_)::_ -> a
    | [] -> raise EmptyRing

