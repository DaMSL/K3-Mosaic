open Util
open K3.AST
open K3Helpers

exception Bad_data of string;;

(* data structure needed after extraction from the k3 program *)
type stmt_id_t = int
type trig_id_t = int
type trig_name_t = string
type map_id_t = int
(* id, position in map's args *)
type map_var_binding_t = id_t * int 

type stmt_data_t
  (* stmt_id, trig_id, lhs_map, lhs_map_binding, rhs_maps_with_bindings *)
  = stmt_id_t * trig_id_t * map_id_t * map_var_binding_t list * 
    (map_id_t * map_var_binding_t list) list 

type trig_data_t
  (* trig_id, name, bound_args, stmts *)
  = trig_id_t * string * (id_t * value_type_t) list * stmt_id_t list

  (* map_id, map_name, parameters *)
type map_data_t = map_id_t * string * value_type_t list

type prog_data_t = trig_data_t list * stmt_data_t list * map_data_t list



(* --- helper functions to get information from the data structure --- *)
let get_trig_data (p:prog_data_t) = match p with
  (trig, _, _) -> trig

let get_stmt_data (p:prog_data_t) = match p with
  (_, stmt, _) -> stmt 

let get_map_data (p:prog_data_t) = match p with
  (_, _, map) -> map
 
let get_trig_list (p:prog_data_t) = 
  List.map
  (fun (_, name, _, _) -> name)
  (get_trig_data p)

let map_all_trigs (p:prog_data_t) f =
  List.map (fun t -> f t) @: get_trig_list p

let get_stmt_list (p:prog_data_t) =
    List.map
    (fun (id,_,_,_,_) -> id)
    (get_stmt_data p)

let get_map_list (p:prog_data_t) =
    List.map
    (fun (id, _, _) -> id)
    (get_map_data p)
 
let find_trigger (p:prog_data_t) (tname:string) = 
  try
    List.find
    (fun (_, name, _, _) -> name = tname)
    (get_trig_data p)
  with
    Not_found -> raise (Bad_data ("No "^tname^" trigger found"))

let find_map (p:prog_data_t) (map_id:map_id_t) =
  try
    List.find
      (fun (id, _, _) -> id = map_id)
      (get_map_data p)
  with
    Not_found -> raise (Bad_data ("No "^(string_of_int map_id)^" map_id found"))
    

let find_stmt (p:prog_data_t) (stmt_id:stmt_id_t) =
  try
    List.find
      (fun (id, _, _, _, _) -> id = stmt_id)
      (get_stmt_data p)
  with
    Not_found -> raise (Bad_data ("No "^(string_of_int stmt_id)^" stmt_id found"))
  
let trigger_id_for_name p trig_name = 
  let (id, _, _, _) = find_trigger p trig_name in
  id

let args_of_t (p:prog_data_t) (trig_name:string) =
  let (_, _, args, _) = find_trigger p trig_name in
  args

let stmts_of_t (p:prog_data_t) trig_name =
  let (_, _, _, stmts) = find_trigger p trig_name in
  stmts

(* map a function over stmts in a trigger in a specific way *)
let map_over_stmts_in_t (p:prog_data_t) func map_func trig_name = 
  let stmts = stmts_of_t p trig_name in
  List.flatten
    (List.map 
      (fun stmt -> 
        List.map (map_func trig_name stmt)
        (func p stmt)
      )
      stmts
    )

(* output (stmt_id, x) list. Guaranteed to be in stmt order *)
let s_and_over_stmts_in_t (p:prog_data_t) func trig_name = 
    map_over_stmts_in_t p func (fun _ stmt x -> (stmt, x)) trig_name

let rhs_maps_of_stmt (p:prog_data_t) (stmt_id:stmt_id_t) = 
  let (_, _, _, _, maplist) = find_stmt p stmt_id in
  List.map
    (fun (map_id, _) -> map_id)
    maplist

let stmt_has_rhs_map p stmt_id rhs_map_id =
  let (_, _, _, _, maplist) = find_stmt p stmt_id in
  List.exists
    (fun (map_id, _) -> map_id = rhs_map_id)
    maplist

let stmts_without_rhs_maps_in_t (p:prog_data_t) trigger_name = 
  let stmt_data = get_stmt_data p in
  let (_, _, _, trig_stmt_ids) = find_trigger p trigger_name in
  List.map
    (fun (id, _, _, _, _) -> id)
    (List.filter
      (fun (stmt_id, _, _, _, maplist) -> 
        (maplist = []) &&
        (List.exists
          (fun id -> id = stmt_id)
          trig_stmt_ids)
      )
      stmt_data
    )

let lhs_map_of_stmt p stmt_id =
  let (_, _, lhs_map, _, _) = find_stmt p stmt_id in
  lhs_map

let rhs_lhs_of_stmt (p:prog_data_t) stmt_id =
  let (_, _, lhs_map, _, rhs_maps) = find_stmt p stmt_id in
  List.map
    (fun (rhs_map, _) -> (rhs_map, lhs_map))
    rhs_maps

let find_lmap_bindings_in_stmt (p:prog_data_t) (stmt:stmt_id_t) (map:map_id_t)
=
  let (_, _, lmap, lmapbind, _) = find_stmt p stmt in
  if lmap = map then lmapbind 
  else raise (Bad_data ("find_lhs_map_binding_in_stmt: No "^
          (string_of_int map)^" lhs map_id found"))

let find_rmap_bindings_in_stmt (p:prog_data_t) (stmt:stmt_id_t) (map:map_id_t)
=
  let (_, _, _, _, rmaps) = find_stmt p stmt in
  let (_, binding) = 
    try
      List.find 
        (fun (rmap, _) -> map = rmap)
        rmaps
    with
      Not_found -> raise (Bad_data ("find_rhs_map_bindings_in_stmt: No "^
        (string_of_int map)^" map_id found"))
  in
  binding

let find_map_bindings_in_stmt (p:prog_data_t) (stmt_id:stmt_id_t) (map_id:map_id_t) =
  let (_, _, lmap, lmapbind, rmaps) = find_stmt p stmt_id in
  if lmap = map_id then lmapbind
  else 
    begin let (_, binding) = 
      try
        List.find 
          (fun (rmap, _) -> map_id = rmap)
          rmaps
      with
        Not_found -> raise (Bad_data ("find_map_bindings_in_stmt: No "^
          (string_of_int map_id)^" map_id found"))
    in
    binding end

let map_name_of p map_id = 
  let (_, name, _) = find_map p map_id in name

let trigger_of_stmt p stmt_id =
  let (_, trig, _, _, _) = find_stmt p stmt_id in trig

let map_types_for p map_id = 
  let (_, _, vars) = find_map p map_id in vars

(* useful for adding vid to maps *)
let map_types_with_v_for p map_id = t_vid::map_types_for p map_id

(* because we add the vid first, we need to modify the numbering of arguments in
* the key. It's easier to control this in one place *)
let adjust_key_id_for_v i = i + 1

(* returns a k3 list of maybes that has the relevant map pattern *)
let var_list_from_bound (p:prog_data_t) (stmt_id:stmt_id_t) (map_id:map_id_t) = 
  let map_binds = find_map_bindings_in_stmt p stmt_id map_id in
  let (_, _, map_params) = find_map p map_id in
  let range = create_range 0 @: List.length map_params in
  List.map
    (fun x -> 
      try 
        let (name, _) = List.find
          (fun (id, loc) -> loc = x)
          map_binds
        in name
      with Not_found -> "_"
    )
    range

(* returns a k3 list of maybes that has the relevant map pattern *)
let partial_key_from_bound (p:prog_data_t) stmt_id map_id = 
  List.map 
  (fun x -> if x = "_" then mk_const CNothing else mk_just @: mk_var x) 
  (list_drop_end 1 @: var_list_from_bound p stmt_id map_id)
                  
(* returns a k3 list of variables or CUnknown. Can't use same types as
 * partial_key *)
let slice_key_from_bound (p:prog_data_t) (stmt_id:stmt_id_t) (map_id:map_id_t) = 
  List.map 
  (fun x -> if x = "_" then mk_const CUnknown else mk_var x) 
  (var_list_from_bound p stmt_id map_id)

(* return a binding pattern for a stmt of (left_index, right_index) list
 * showing how a lhs map variable corresponds to a rhs variable
 * starting at 0 index *)
let get_map_bindings_in_stmt (p:prog_data_t) (stmt_id:stmt_id_t) 
  (rmap:map_id_t) (lmap:map_id_t) =
  let lmap_bindings = find_lmap_bindings_in_stmt p stmt_id lmap in
  let rmap_bindings = find_rmap_bindings_in_stmt p stmt_id rmap in
  List.fold_left
    (fun acc (id, index) -> 
      try acc@[index, List.assoc id rmap_bindings]
      with Not_found -> acc
    )
    []
    lmap_bindings


