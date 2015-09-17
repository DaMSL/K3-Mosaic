open Util
open K3.AST
open K3Helpers
open K3Printing

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
  = trig_id_t * string * (id_t * type_t) list * stmt_id_t list

  (* map_id, map_name, parameters *)
type map_data_t = map_id_t * string * type_t list

type prog_data_t = trig_data_t list * stmt_data_t list * map_data_t list

let string_of_var (vn, vt) = vn^":"^(string_of_type vt)

let string_of_binding (vn, idx) = vn^":"^(string_of_int idx)
let string_of_bindings bl =
  (String.concat ", " (List.map string_of_binding bl))
let string_of_map (map, bl) =
  (string_of_int map) ^ "[" ^ (string_of_bindings bl) ^ "]"

let string_of_map_data ((map_id, map_name, tl):map_data_t):string =
  map_name ^ "(" ^ (string_of_int map_id) ^ ")[" ^
  (String.concat ", " (List.map string_of_type tl))^"]"


let string_of_stmt_data ((stmt_id, trig_id, lhs_map, lhs_map_binding,
                         rhs_maps_with_bindings):stmt_data_t):string =
  ( (string_of_int trig_id)^"::"^(string_of_int stmt_id)^" : "^
    (string_of_map (lhs_map, lhs_map_binding))^" <<== "^
    (String.concat "; " (List.map string_of_map rhs_maps_with_bindings))
  )

let string_of_trig_data ((trig_id, name, bound_args, stmts):trig_data_t):string=
  ( name^"("^(string_of_int trig_id)^") ["^
    (String.concat ", " (List.map string_of_var bound_args))^
    "] : "^
    (String.concat ", " (List.map string_of_int stmts))
  )

let string_of_prog_data ((trig_data, stmt_data, map_data):prog_data_t): string =
  ( "--- Triggers ---\n"^
    (String.concat "\n" (List.map string_of_trig_data trig_data))^
    "\n\n--- Statements ---\n"^
    (String.concat "\n" (List.map string_of_stmt_data stmt_data))^
    "\n\n--- Maps ---\n"^
    (String.concat "\n" (List.map string_of_map_data map_data))^
    "\n" )


(* --- helper functions to get information from the data structure --- *)
let get_trig_data (p:prog_data_t) = match p with
  (trig, _, _) -> trig

let get_stmt_data (p:prog_data_t) = match p with
  (_, stmt, _) -> stmt

let get_map_data (p:prog_data_t) = match p with
  (_, _, map) -> map

(* function to check if a trigger is a delete/insert trigger *)
let check_prefix name prefix =
  let len = String.length prefix in
  if String.sub name 0 len = prefix then true else false

let is_correct_t t = check_prefix t "correct_"
let is_delete_t t = check_prefix t "delete_"
let is_insert_t t = check_prefix t "insert_"
let is_sys_init_t t = t = "system_ready_event"
let remove_trig_prefix t = str_drop (String.length "delete_") t

let relevant_trig ?(corrective=false) ?(sys_init=true) ?(delete=true) t =
  is_insert_t t || (delete && is_delete_t t) ||
  (corrective && is_correct_t t) || (sys_init && is_sys_init_t t)

(* only non-corrective triggers *)
let get_trig_list ?corrective ?sys_init ?delete (p:prog_data_t) =
  let l = List.map (fun (_, name, _, _) -> name) @@ get_trig_data p in
  List.filter (relevant_trig ?corrective ?sys_init ?delete) l

let for_all_trigs ?(corrective=false) ?(sys_init=false) ?(delete=true) (p:prog_data_t) f =
  List.map f @@ get_trig_list ~corrective ~sys_init ~delete p

let find_trigger (p:prog_data_t) (tname:string) =
  try List.find (fun (_, name, _, _) -> name = tname) @@ get_trig_data p
  with
    Not_found -> raise (Bad_data ("No "^tname^" trigger found"))

let trigger_id_for_name p (trig_name:trig_name_t) =
  let (id, _, _, _) = find_trigger p trig_name in
  id

let trigger_name_for_id p (trig_id:trig_id_t) =
  try let (_, name, _, _) =
        List.find (fun (id, _, _, _) -> id = trig_id) @@ get_trig_data p
      in name
  with
    Not_found -> raise (Bad_data ("No trigger "^string_of_int trig_id^" found"))


(* only non-corrective triggers *)
let get_stmt_list (p:prog_data_t) =
  let l = List.map (fun (s,t,_,_,_) -> (s,trigger_name_for_id p t)) @@ get_stmt_data p in
  fst @@ List.split @@ List.filter (relevant_trig |- snd) l

let for_all_stmts (p:prog_data_t) f =
  List.map (fun s -> f s) @@ get_stmt_list p

let get_map_list (p:prog_data_t) =
  List.map (fun (id, _, _) -> id) @@ get_map_data p

let for_all_maps (p:prog_data_t) f =
  List.map (fun m -> f m) @@ get_map_list p

let find_map (p:prog_data_t) (map_id:map_id_t) =
  try List.find (fun (id, _, _) -> id = map_id) @@ get_map_data p
  with
    Not_found -> raise (Bad_data ("No "^(string_of_int map_id)^" map_id found"))

let find_map_by_name (p:prog_data_t) str =
  try List.find (fun (_, s, _) -> s = str) @@ get_map_data p
  with
    Not_found -> raise (Bad_data ("No "^str^" map name found"))

let find_stmt (p:prog_data_t) (stmt_id:stmt_id_t) =
  try List.find (fun (id, _, _, _, _) -> id = stmt_id) @@ get_stmt_data p
  with
    Not_found -> raise (Bad_data ("No "^(string_of_int stmt_id)^" stmt_id found"))


let args_of_t (p:prog_data_t) (trig_name:trig_name_t) =
  let (_, _, args, _) = find_trigger p trig_name in
  args

let stmts_of_t (p:prog_data_t) trig_name =
  let (_, _, _, stmts) = find_trigger p trig_name in
  stmts

(* map a function over stmts in a trigger in a specific way *)
let map_over_stmts_in_t (p:prog_data_t) func map_func trig_name =
  let stmts = stmts_of_t p trig_name in
  List.flatten @@ List.map
    (fun stmt -> List.map (map_func trig_name stmt) @@ func p stmt)
    stmts

(* output (stmt_id, x) list. Guaranteed to be in stmt order *)
let s_and_over_stmts_in_t (p:prog_data_t) func trig_name =
    map_over_stmts_in_t p func (fun _ stmt x -> (stmt, x)) trig_name

let rhs_maps_of_stmt (p:prog_data_t) (stmt_id:stmt_id_t) =
  let (_, _, _, _, maplist) = find_stmt p stmt_id in
  nub @@ fst_many maplist

let stmts_rhs_maps (p:prog_data_t) =
  List.flatten @@
  List.map (fun (stmt, _, _, _, rmaplist) ->
    let maplist = nub @@ fst_many rmaplist in
    List.map (fun map -> stmt, map) maplist
  )
  (get_stmt_data p)

let stmts_lhs_maps (p:prog_data_t) =
  List.map (fun (stmt, _, lmap, _, _) -> stmt, lmap) @@ get_stmt_data p

let for_all_stmts_rhs_maps p f = List.map f @@ stmts_rhs_maps p

let stmt_has_rhs_map p stmt_id rhs_map_id =
  let (_, _, _, _, maplist) = find_stmt p stmt_id in
  List.exists (fun (map_id, _) -> map_id = rhs_map_id) maplist

let stmts_rhs_map_inner (p:prog_data_t) (t:trig_name_t) ~op =
  let stmt_data = get_stmt_data p in
  let _, _, _, trig_stmt_ids = find_trigger p t in
  filter_map
    (fun (stmt_id, _, _, _, maplist) ->
      if op maplist [] &&
        List.exists ((=) stmt_id) trig_stmt_ids
      then Some stmt_id
      else None
    ) stmt_data

let stmts_without_rhs_maps_in_t p t = stmts_rhs_map_inner ~op:(=) p t

let stmts_with_rhs_maps_in_t p t = stmts_rhs_map_inner ~op:(<>) p t

let lhs_map_of_stmt p stmt_id =
  let (_, _, lhs_map, _, _) = find_stmt p stmt_id in
  lhs_map

let rhs_lhs_of_stmt (p:prog_data_t) stmt_id =
  let (_, _, lhs_map, _, rhs_maps) = find_stmt p stmt_id in
  nub @@ List.map
    (fun (rhs_map, _) -> (rhs_map, lhs_map))
    rhs_maps

let for_all_rhs_lhs_maps (p:prog_data_t) f =
  let rhs_lhs_l =
     nub @@ List.flatten @@ for_all_stmts p @@ rhs_lhs_of_stmt p in
  List.map f rhs_lhs_l

let map_name_of p map_id =
  let (_, name, _) = find_map p map_id in
  name

let map_id_of_name p str =
  let (id, _, _) = find_map_by_name p str in
  id

(* get the buffer version name of a map *)
let buf_of_stmt_map stmt map = Printf.sprintf "map_%s_s%d_buf" map stmt

let buf_of_stmt_map_id p stmt map_id =
  let map = map_name_of p map_id in
  buf_of_stmt_map stmt map

let trigger_of_stmt p stmt_id : trig_name_t =
  let (_, trig, _, _, _) = find_stmt p stmt_id in
  trigger_name_for_id p trig

let map_types_for p map_id =
  let (_, _, vars) = find_map p map_id in
  vars

let map_types_no_val_for p map = list_drop_end 1 @@ map_types_for p map
let map_types_add_v ts = t_vid::ts
let map_types_with_v_for p map = map_types_add_v @@ map_types_for p map

let def_map = "map_"
let def_vid = "vid"

(* create ids to reference the map vars *)
(* ["map_0",type; "map_1",type; ... "map_val",type ] *)
let map_ids_types_for ?(prefix=def_map) p map_id =
  let ts = map_types_for p map_id in
  let id_ts = types_to_ids_types prefix ts in
  (list_drop_end 1 id_ts)@[prefix^"val", hd @@ list_take_end 1 ts]

(* ids to reference the map vars, with vid *)
let map_ids_types_no_val_for ?(prefix=def_map) p map_id = list_drop_end 1 @@
  map_ids_types_for ~prefix:prefix p map_id
let map_add_v v xs = v::xs
let map_ids_add_v ?(vid=def_vid) xs = map_add_v vid xs
let map_ids_types_add_v ?(vid=def_vid) ts = (vid, t_vid)::ts
let map_ids_types_with_v_for ?(prefix=def_map) ?(vid=def_vid) p map_id =
   map_ids_types_add_v ~vid:vid @@ map_ids_types_for ~prefix:prefix p map_id

(* because we add the vid first, we need to modify the numbering of arguments in
* the key. It's easier to control this in one place *)
let adjust_key_id_for_v i = i + 1

let map_names_ids_of_stmt p stmt =
  let lmap = lhs_map_of_stmt p stmt in
  let rmaps = rhs_maps_of_stmt p stmt in
  let maps = ListAsSet.uniq @@ lmap::rmaps in
  List.map (fun m -> (map_name_of p m, m)) maps

let reduce_l_to_map_size p map l =
  let map_size = (List.length @@ map_types_for p map) - 1 in
  list_take map_size l

let find_lmap_bindings_in_stmt (p:prog_data_t) (stmt:stmt_id_t) (map:map_id_t) =
  let _, _, lmap, lmapbind, _ = find_stmt p stmt in
  (* make sure bindings are only as big as the map size minus the value *)
  if lmap = map then reduce_l_to_map_size p map lmapbind
  else raise @@ Bad_data ("find_lhs_map_binding_in_stmt: No "^
          string_of_int map^" lhs map_id found")

let find_rmap_bindings_in_stmt (p:prog_data_t) (stmt:stmt_id_t) (map:map_id_t) =
  let (_, _, _, _, rmaps) = find_stmt p stmt in
  let (_, binding) =
    try List.find (fun (rmap, _) -> map = rmap) rmaps
    with
      Not_found -> raise (Bad_data ("find_rhs_map_bindings_in_stmt: No "^
        (string_of_int map)^" map_id found in stmt "^string_of_int stmt))
  in
  (* make sure bindings are only as big as the map size - the value *)
  reduce_l_to_map_size p map binding

let find_map_bindings_in_stmt (p:prog_data_t) (stmt:stmt_id_t) (map:map_id_t) =
  try find_lmap_bindings_in_stmt p stmt map
  with Bad_data(_) ->
      find_rmap_bindings_in_stmt p stmt map

(* returns a k3 list of maybes that has the relevant map pattern *)
let var_list_from_bound (p:prog_data_t) (stmt_id:stmt_id_t) (map_id:map_id_t) =
  let map_binds = reduce_l_to_map_size p map_id @@
    find_map_bindings_in_stmt p stmt_id map_id in
  let trig_args = args_of_t p @@ trigger_of_stmt p stmt_id in
  let map_types = map_types_for p map_id in
  let idx_types = insert_index_fst map_types in
  List.map
    (fun (i,typ) -> try
        (* Look for var name with this binding, then check it's part of trig
         * args. If not, it's a loop var, and shouldn't be used *)
        let id = fst @@ List.find (fun (_, loc) -> loc = i) map_binds in
        let id2  = fst @@ List.find (fun (n, _) -> n = id) trig_args in
        id2, typ
      with Not_found -> "_", typ
    )
    idx_types

(* returns a k3 list of maybes that has the relevant map pattern *)
let partial_key_from_bound (p:prog_data_t) stmt_id map_id =
  let result = List.map (fun (x, typ) ->
      if x = "_" then mk_tup_nothing typ
      else mk_tup_just @@ mk_var x) @@
    list_drop_end 1 @@ var_list_from_bound p stmt_id map_id
  in match result with [] -> [mk_const CUnit] | _ -> result

(* returns a k3 list of variables or CUnknown. Can't use same types as
 * partial_key *)
let slice_key_from_bound (p:prog_data_t) (stmt_id:stmt_id_t) (map_id:map_id_t) =
  let result = List.map
    (fun (x,_) -> if x = "_" then mk_const CUnknown else mk_var x) @@
    var_list_from_bound p stmt_id map_id
  in match result with [] -> [mk_const CUnit] | _ -> result

(* return a binding pattern for a stmt of (left_index, right_index) list
 * showing how a lhs map variable corresponds to a rhs variable
 * starting at 0 index.
   This pattern is only for shuffles. For shuffles, only free (ie. loop) variables
   matter. All bound vars are the same as no connection between the lhs and rhs.
   For maps, however, we're still sharding by the rhs pattern on the rhs, and then
   caching that as a lhs shard pattern on the lhs.
   returns: rmap, lmap index set *)
let get_map_bindings_in_stmt p stmt_id rmap lmap =
  let trig_args = fst_many @@ args_of_t p @@ trigger_of_stmt p stmt_id in
  (* make sure we only take bindings not including the value, and not bound args *)
  let not_in_trig = List.filter (fun (s,_) -> not @@ List.mem s trig_args) in
  let lmap_bindings = not_in_trig @@ find_lmap_bindings_in_stmt p stmt_id lmap in
  let rmap_bindings = not_in_trig @@ find_rmap_bindings_in_stmt p stmt_id rmap in
  IntIntSet.of_list @@ List.flatten @@ List.map
    (fun (id, index) ->
      try [index, List.assoc id lmap_bindings]
      with Not_found -> [])
    rmap_bindings

(* check if a statement has map bindings that cause conservative routing:
   2 or more separate loop variables *)
let has_many_loops_map_bindings p s =
  let rmaps = rhs_maps_of_stmt p s in
  let lmap  = lhs_map_of_stmt p s in
  let binds = List.map (fun r -> get_map_bindings_in_stmt p s r lmap) rmaps in
  (* our condition is that the lmap has >1 loop var, and that >1 rmaps connect to
     the lmap *)
  let lmap_vars, rmap_count = first IntSet.cardinal @@
    List.fold_left (fun ((acc, count) as a) set ->
      if IntIntSet.is_empty set then a
      else
        let acc = IntIntSet.fold (fun (_,l) acc -> IntSet.add l acc) set acc in
        acc, count + 1)
      (IntSet.empty, 0) binds
  in
  lmap_vars > 1 && rmap_count > 1


module IntSetSetMap = Map.Make(struct type t = IntSetSet.t let compare = IntSetSet.compare end)

let map_access_patterns (p:prog_data_t) =
  let h = Hashtbl.create 40 in
  let insert_from_bind t_args map binds =
    (* iterate over lmap binds and get index *)
    let map_ts = map_types_for p map in
    let idx =
      List.flatten @@ List.map (fun (nm, i) ->
        (* only count bound variabls (trig args) *)
        if List.mem nm t_args then [i] else []) binds in
    if idx <> [] &&
      (* prune out indices that have entire key *)
      List.length idx < List.length map_ts - 1 then
      (* insert index into hashtable *)
      let idx = IntSet.of_list idx in
      hashtbl_replace h map (function
        | None   -> IntSetSet.singleton idx
        | Some x -> IntSetSet.add idx x)
  in
  ignore(for_all_trigs ~sys_init:true ~corrective:true ~delete:true p @@ fun trig ->
    let t_args = fst_many @@ args_of_t p trig in
    let ss = List.map (find_stmt p) @@ stmts_of_t p trig in
    List.iter (fun (_,_,lmap,lbinds,rbinds) ->
      insert_from_bind t_args lmap lbinds;
      List.iter (fun (rmap, rbind) ->
        insert_from_bind t_args rmap rbind) rbinds
    ) ss);
  (* enumerate all patterns and number them *)
  let pats = snd @@
    Hashtbl.fold (fun nm pat (i,acc) ->
      try ignore(IntSetSetMap.find pat acc); i, acc
      with Not_found ->
        i+1, IntSetSetMap.add pat i acc)
    h (1, IntSetSetMap.empty)
  in
  let h2 = Hashtbl.create 40 in
  (* create new hashtbl with number for the pattern *)
  Hashtbl.iter (fun nm idx ->
    let num = IntSetSetMap.find idx pats in
    Hashtbl.add h2 nm (num, idx)) h;
  h2

