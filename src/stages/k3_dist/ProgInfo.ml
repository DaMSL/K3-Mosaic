open Util
open K3.AST
open K3Helpers
open K3Printing

exception Bad_data of string;;

(* Make the types distinct so we don't mix them up *)
module type DATA = sig
  type t
  val to_s : t -> string
  val eq : t -> t -> bool
end

module type SDATA = sig
  include DATA
  val of_s : string -> t
end

module type IDATA = sig
  include DATA
  val of_i : int -> t
end

module StringBased = struct
  type t = string
  let to_s s = s
  let eq x y = String.compare x y = 0
  let of_s s = s
end

module IntBased = struct
  type t = int
  let to_s = soi
  let eq x y  = x - y = 0
  let of_i x = x
end

module Stmt : IDATA = struct
  include IntBased
end

module Map : IDATA = struct
  include IntBased
end

module Trig : IDATA = struct
  include IntBased
end

module TrigN : SDATA = struct
  include StringBased
end

module MapN : SDATA = struct
  include StringBased
end

(* data structure needed after extraction from the k3 program *)

(* id, position in map's args *)
type map_var_binding_t = string * int

type stmt_data_t
  (* stmt_id, trig_id, lhs_map, lhs_map_binding, rhs_maps_with_bindings *)
  = {
    sid : Stmt.t;
    trig : Trig.t;
    lmap : Map.t;
    lbind : map_var_binding_t list;
    rbinds : (Map.t * map_var_binding_t list) list;
  }

type trig_data_t
  (* trig_id, name, bound_args, stmts *)
  = {
    tid : Trig.t;
    tname : TrigN.t;
    args : (id_t * type_t) list;
    stmts : Stmt.t list;
  }

  (* map_id, map_name, parameters *)
type map_data_t = {
  mid : Map.t;
  mname : MapN.t;
  params : type_t list;
  is_table : bool;
}

type prog_data_t =
  {
    trigs: trig_data_t list;
    stmts: stmt_data_t list;
    maps: map_data_t list;
    tables : Map.t list;
  }

let sp = Printf.sprintf

let string_of_var (vn, vt)      = sp "%s:%s" vn (string_of_type vt)
let string_of_binding (vn, idx) = sp "%s:%d" vn idx
let string_of_bindings bl       = strcatmap string_of_binding bl
let string_of_map (map, bl)     = sp "%s[%s]" (Map.to_s map) (string_of_bindings bl)
let string_of_map_data {mid; mname; params; is_table} :string =
  sp "%s(%s%s)[%s]" (MapN.to_s mname) (Map.to_s mid) (if is_table then ", table" else "")
    (strcatmap string_of_type params)

let string_of_stmt_data {sid; trig; lmap; lbind; rbinds} : string =
  sp "%s::%s : %s <- %s" (Trig.to_s trig) (Stmt.to_s sid)
    (string_of_map (lmap, lbind)) (strcatmap string_of_map rbinds)

let string_of_trig_data {tid; tname; args; stmts} : string =
  sp "%s(%s)[%s] : %s" (TrigN.to_s tname) (Trig.to_s tid) (strcatmap string_of_var args) (strcatmap Stmt.to_s stmts)

let string_of_prog_data {trigs; stmts; maps}: string =
  sp "--- Triggers ---\n%s\n\n--- Statements ---\n%s\n\n--- Maps ---\n%s\n"
    (strcatmap ~sep:"\n" string_of_trig_data trigs)
    (strcatmap ~sep:"\n" string_of_stmt_data stmts)
    (strcatmap ~sep:"\n" string_of_map_data maps)

(* function to check if a trigger is a delete/insert trigger *)
let check_prefix name prefix =
  let len = String.length prefix in
  if String.sub name 0 len = prefix then true else false

type trig_kinds = AllTrigs | InsertTrigs | DeleteTrigs

let is_delete_t t = check_prefix t "delete_"
let is_insert_t t = check_prefix t "insert_"

let relevant_trig ?(kind=AllTrigs) t =
  let t = TrigN.to_s t in
  match kind with
  | AllTrigs    -> is_delete_t t || is_insert_t t || t = "system_ready_event"
  | InsertTrigs -> is_insert_t t
  | DeleteTrigs -> is_delete_t t

(* only non-corrective triggers *)
let get_trig_list ?(kind=AllTrigs) (p:prog_data_t) =
  let l = List.map (fun x -> x.tname) p.trigs in
  List.filter (relevant_trig ~kind) l

let for_all_trigs ?(deletes=true) (p:prog_data_t) f =
  let l_delete = String.length "delete" in
  let filter = if deletes then id_fn
               else List.filter ((<>) "delete" |- str_take l_delete |- TrigN.to_s) in
  List.map f @@ filter @@ get_trig_list p

let find_trigger (p:prog_data_t) tname =
  try List.find (fun t -> TrigN.eq t.tname tname) p.trigs
  with
    Not_found -> raise (Bad_data ("No "^TrigN.to_s tname^" trigger found"))

let find_trigger_by_id (p:prog_data_t) tid =
  try List.find (fun t -> Trig.eq t.tid tid) p.trigs
  with
    Not_found -> raise (Bad_data ("No trigger "^Trig.to_s tid^" found"))

let trigger_id_for_name p trig_name = (find_trigger p trig_name).tid

let trigger_name_for_id p trig_id = (find_trigger_by_id p trig_id).tname

(* only non-corrective triggers *)
let get_stmt_list ?kind (p:prog_data_t) =
  let l = List.map (fun s -> s.sid, trigger_name_for_id p s.trig) p.stmts in
  fst @@ List.split @@ List.filter (relevant_trig ?kind |- snd) l

let for_all_stmts p f = List.map f @@ get_stmt_list p

(* by default, we exclude tables (static maps that don't change) *)
let get_map_list ?(tables=false) p = List.map (fun m -> m.mid) @@ p.maps

let for_all_maps ?tables p f = List.map f @@ get_map_list ?tables p

let find_map p map_id =
  try List.find (fun m -> Map.eq m.mid map_id) p.maps
  with
    Not_found -> raise (Bad_data ("No "^Map.to_s map_id^" map_id found"))

let find_map_by_name p nm =
  try List.find (fun m -> MapN.eq m.mname nm) p.maps
  with
    Not_found -> raise (Bad_data ("No "^MapN.to_s nm^" map name found"))

let find_stmt p stmt_id =
  try List.find (fun s -> Stmt.eq s.sid stmt_id) p.stmts
  with
    Not_found -> raise (Bad_data ("No "^Stmt.to_s stmt_id^" stmt_id found"))

let args_of_t p trig_name = (find_trigger p trig_name).args

let stmts_of_t p trig_name = (find_trigger p trig_name).stmts

(* map a function over stmts in a trigger in a specific way *)
let map_over_stmts_in_t p func map_func trig_name =
  let stmts = stmts_of_t p trig_name in
  List.flatten @@ List.map
    (fun stmt -> List.map (map_func trig_name stmt) @@ func p stmt)
    stmts

(* output (stmt_id, x) list. Guaranteed to be in stmt order *)
let s_and_over_stmts_in_t p func trig_name =
  map_over_stmts_in_t p func (fun _ stmt x -> stmt, x) trig_name

let remove_table_maps p l =
    List.filter (fun m -> not @@ List.exists (Map.eq m) p.tables) l

let rhs_maps_of_stmt ?(tables=false) p stmt_id =
  let l = (find_stmt p stmt_id).rbinds in
  let l = nub @@ fst_many l in
  if tables then l
  else remove_table_maps p l

let stmts_rhs_maps ?(tables=false) p =
  List.flatten @@
    List.map (fun s ->
      let maplist = nub @@ fst_many s.rbinds in
      let maplist = if tables then maplist else remove_table_maps p maplist in
      List.map (fun map -> s.sid, map) maplist
    )
    p.stmts

let stmts_lhs_maps p = List.map (fun s -> s.sid, s.lmap) @@ p.stmts

let for_all_stmts_rhs_maps p f = List.map f @@ stmts_rhs_maps p

let stmt_has_rhs_map ?tables p stmt_id rhs_map_id =
  let rmaps = rhs_maps_of_stmt ?tables p stmt_id in
  List.exists (fun m -> Map.eq m rhs_map_id) rmaps

(* TODO: -- GOT UP TO HERE -- *)

let stmts_rhs_map_inner p (t:trig_name_t) ~op =
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
      if x = "_" then mk_nothing (wrap_tmaybe typ)
      else mk_just @@ mk_var x) @@
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
 * starting at 0 index *)
let get_map_bindings_in_stmt (p:prog_data_t) (stmt_id:stmt_id_t)
  (rmap:map_id_t) (lmap:map_id_t) =
  (* make sure we only take bindings not including the value *)
  let lmap_bindings = find_lmap_bindings_in_stmt p stmt_id lmap in
  let rmap_bindings = find_rmap_bindings_in_stmt p stmt_id rmap in
  IntIntSet.of_list @@ List.flatten @@ List.map
    (fun (id, index) ->
      try [index, List.assoc id rmap_bindings]
      with Not_found -> [])
    lmap_bindings

(* get a list of unique types for maps (no vid) the map *)
(* type_fn allows one to modify the types used in the hashtable *)
let uniq_types_and_maps ?(type_fn=map_types_for) (p:prog_data_t)  =
  let hash = Hashtbl.create 50 in
  ignore (for_all_maps p @@
    fun map_id ->
      hashtbl_replace hash (type_fn p map_id) @@
        function None -> [map_id] | Some l -> map_id::l);
  let fns = ref [] in
  Hashtbl.iter (fun t maps -> fns := (t, maps) :: !fns) hash;
  !fns

