open Utils
open K3
open K3Helpers

(* data structure needed after extraction from the k3 program *)
type stmt_id_t = int
type trig_id_t = int
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
type map_data_t = map_id_t * string * (id_t * value_type_t) list

type prog_data_t = trig_data_t list * stmt_data_t list * map_data_t list



(* --- helper functions to get information from the data structure --- *)
let get_trig_data p = match p with
  (trig, _, _) -> trig

let get_map_data p = match p with
  (_, stmt, _) -> stmt 

let get_stmt p = match p with
  (_, _, map) -> map
 
let find_trigger p tname = 
  List.find
  (fun (_, name, _, _) -> name = tname)
  (get_trig_data p)

let find_map p map_id =
  List.find
  (fun (id, _, _) -> id = map_id)
  (get_map_data p)

let find_stmt p stmt_id =
  List.find
  (fun (id, _, _, _, _) -> id = stmt_id)
  (get_stmt_data p)
  
let trigger_id_for_name p trig_name = 
  let (id, _, _, _) = find_trigger trig_name p in
  id

let args_of_t p trig_name =
  let (_, _, args, _) = find_trigger trig_name p in
  args

let over_stmts_in_t p func trig_name = (* output (stmt_id, x) list *)
  let (_, _, _, stmts) = find_trigger p trig_name
  List.map
  (fun id -> (id, func p id))
  stmts

let read_maps_of_stmt p stmt_id = 
  let (_, _, _, _, maplist) = find_stmt stmt_id p in
  List.map
    (fun (map_id, _) -> map_id)
    maplist

let stmt_has_rhs_map p rhs_map_id =
  let (_, _, _, _, maplist) = find_stmt stmt_id p in
  List.exists
    (fun (map_id, _) -> map_id = rhs_map_id)
    maplist

let stmts_without_rhs_maps_of_t p trig_name = 
  let stmts = get_stmt_data p in
  List.filter
    (fun (_, _, _, _, maplist) -> maplist = [])
    stmts

let lhs_map_of_stmt p stmt_id =
  let (_, _, lhs_map, _, _) = find_stmt stmt_id p in
  lhs_map

let read_maps_of_stmt p stmt_id =
  let (_, _, _, _, rhs_maps) = find_stmt stmt_id p in
  List.map
    (fun (name, _) -> name)
    rhs_maps

let lhs_rhs_of_stmt p stmt_id =
  let (_, _, lhs_map, _, rhs_maps) = find_stmt stmt_id p in
  List.map
    (fun (rhs_map, _) -> (lhs_map, rhs_map))
    rhs_maps

let find_map_bindings_in_stmt p map_id stmt_id =
  let (_, _, lmap, lmapbind, rmaps) = find_stmt p stmt_id in
  if lmap = map_id then lmapbind
  else 
    begin let (_, binding) = 
      List.find 
        (fun (rmap, _) -> map_id = rmap)
        rmaps
      in
      bindings
    end

let map_name_of p map_id = 
  let (_, name, _, _) = find_map p map_id in
  name

let stmts_of_trigger p trig_name = 
  let (_, _, _, stmts) = find_trigger p trig_name in
  stmts

let trigger_of_stmt p stmt_id =
  let (_, trig, _, _, _) = find_stmt p stmt_id in
  trig

let map_types_for p map_id = 
  let (_, _, types) = find_map p map_id in
  extract_arg_types types

(* returns a k3 tuple of maybes that has the relevant map pattern *)
let partial_key_from_bound p stmt_id map_id = 
  let map_binds = find_map_bindings_in_stmt p stmt_id map_id in
  let (_, _, map_params) = find_map p map_id in
  let range = create_range 0 (List.length map_params) in
  List.map
    (fun x -> 
      try 
        let (name, _) = List.find
          (fun (id, loc) -> loc = x)
          map_binds
        in
        Some name
      with
        Not_found -> None
    )
    range
                  
