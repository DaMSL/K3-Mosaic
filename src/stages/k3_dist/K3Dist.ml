(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
open K3.AST
module U = K3Util
module G = K3Global
module P = ProgInfo

module IdMap = Map.Make(struct type t = id_t let compare = String.compare end)

let str_of_date_t t = match t.typ with
  | TDate -> {t with typ = TString}
  | _ -> t

let sys_init = "system_ready_event"

(* an agenda mapping type *)
type mapping_t = type_t list * int list StrMap.t

let default_mapping = [], StrMap.empty

let string_of_mapping ((l, m):mapping_t) =
  Printf.sprintf "type:\n %s,\n map:\n %s"
    (String.concat ", " @@ List.map K3PrintSyntax.string_of_type l)
    (String.concat "\n" @@ StrMap.fold (fun k v acc -> (Printf.sprintf "%s => %s" k @@ string_of_int_list v)::acc) m [])

(* load a mapping file *)
let load_mapping_file file : mapping_t =
  let open Str in
  let open Printf in
  let sp = "\\([ \n\t]\\)" in
  let any = "\\(.\\|\n\\)" in
  let r_agenda = regexp @@ sprintf
    "^CREATE STREAM AGENDA (\\(%s+\\))%s+FROM\\(%s+\\)LINE DELIMITED AGENDA ([^)]+mapping[^']+'\\([^']+\\)'" any sp any in
  let r_semi, r_comma = regexp (sprintf ";%s*" sp), regexp (sprintf ",%s*" sp) in
  let r_colon = regexp @@ sprintf ":%s*" sp in
  let file_s = read_file file in
  let tup_s, map_s =
    try
      ignore(search_forward r_agenda file_s 0);
      String.trim @@ matched_group 1 file_s, String.trim @@ matched_group 6 file_s
    with Not_found -> failwith "Couldn't find mapping in agenda file" in
  let map = List.fold_left (fun acc s -> match split r_colon s with
    | [nm; nums] ->
          let nums = split r_comma nums in
          StrMap.add nm (List.map ios nums) acc
    | _ -> failwith "Bad format for agenda map") StrMap.empty @@
    split r_semi map_s in
  (* now get the tuple types *)
  let r_sp = regexp @@ sprintf "%s+" sp in
  let tup_types = List.map (fun s -> match split r_sp @@ String.trim s with
    | [k;v] -> v
    | l     -> failwith @@ "Bad agenda format: " ^ String.concat "/" l) @@
    Str.split r_comma tup_s in
  (* check for prefix *)
  let prefix s v = str_take (String.length v) (String.uppercase s) = v in
  let tup_types = List.map (fun s ->
    if prefix s "VARCHAR" || prefix s "CHAR" then t_string else
    if prefix s "INT" then t_int else
    if prefix s "DATE" then t_date else
    if prefix s "DECIMAL" then t_float else
    failwith @@ "unrecognized mapping type "^s)
    tup_types in
  tup_types, map


type shuffle_fn_entry = {
  stmts : IntSet.t;
  rmap : map_id_t;
  lmap : map_id_t;
  binding : int IntMap.t; (* rmap idx, lmap idx *)
  name : string;
}

type map_type =
  | MapVMap
  | MapMultiVMap

type has_ds = bool
type unique = bool
type sub_handler = string

type tag_type =
  | Trig      of has_ds (* a top-level trigger *)
  | SubTrig   of has_ds * sub_handler list (* a subtrigger handled by a high-level trigger *)
  | Ds        of unique (* a data structure *)
  | Event     (* incoming event *)

type tag_info = {
  tag: id_t;
  itag: int;
  fn: id_t;
  tag_typ: tag_type;
  args: (id_t * type_t) list;
  const_args: expr_t list;
  batch_id: bool;
  trig_args: bool;
}

type config = {
  p : P.prog_data_t;
  shuffle_meta : shuffle_fn_entry list;
  map_type : map_type;
  gen_deletes : bool;
  gen_correctives : bool;
  gen_single_vid : bool;
  (* optimize figuring out corrective map possiblities *)
  corr_maps : map_id_t list;
  (* whether there's a sys_ready_event trigger *)
  sys_init : bool;
  (* a file to use as the stream to switches *)
  stream_file : string;
  (* a mapping for agenda: how the relations map to agenda indices *)
  reduced_agenda_map : mapping_t;
  agenda_map : mapping_t;
  (* unused trig args, calculated once *)
  unused_trig_args : StrSet.t StrMap.t;
  (* map slice indices for multi-index maps *)
  map_indices: (map_id_t, int * IntSetSet.t) Hashtbl.t;
  (* map bind indices for route memoization *)
  route_indices: (map_id_t, int IntSetMap.t) Hashtbl.t;
  (* poly tag list for batching: int_tag * (tag * tag_type * types) *)
  poly_tags : tag_info list;
  (* poly tag for incoming events *)
  event_tags: (int * (string * type_t list)) list;
  (* freevar info for the program, per stmt *)
  freevar_info : freevar_info IntMap.t;
  use_opt_route : bool;
}

let default_config = {
  p = [], [], [];
  shuffle_meta = [];
  map_type = MapVMap;
  gen_deletes = true;
  gen_correctives = true;
  gen_single_vid = true;
  corr_maps = [];
  sys_init = false;
  stream_file = "";
  agenda_map = [], StrMap.empty;
  reduced_agenda_map = [], StrMap.empty;
  unused_trig_args = StrMap.empty;
  map_indices = Hashtbl.create 10;
  route_indices = Hashtbl.create 10;
  poly_tags = [];
  event_tags = [];
  freevar_info = IntMap.empty;
  use_opt_route = true;
}

let get_map_indices c map_id =
  try some @@ Hashtbl.find c.map_indices map_id
  with Not_found -> None

(* what the generic type of the global maps is *)
let wrap_t_of_map c map_id t = match c.map_type, get_map_indices c map_id with
  | MapVMap, _
  | MapMultiVMap, None -> mut @@ wrap_tvmap t
  | MapMultiVMap, Some(_, idx) -> mut @@ wrap_tvmap ~idx t

let wrap_string_map s = "[<"^s^">]"

let wrap_t_of_map' c map_id t = wrap_t_of_map c map_id (wrap_ttuple t)

(* split a map's types into key, value. For vmaps, remove the vid *)
let map_t_split' ts = list_split (-1) ts

(* get a list of unique types for maps (no vid) *)
(* type_fn allows one to modify the types used in the hashtable, eg. just keys *)
let uniq_types_and_maps ?(uniq_indices=true) ?(type_fn=P.map_types_for) c =
  let h   = Hashtbl.create 50 in
  ignore (P.for_all_maps c.p @@ fun map_id ->
    let t_elem = type_fn c.p map_id in
    let index : int =
      (* if we don't care about unique indices, use a zero value *)
      if uniq_indices then maybe 0 fst @@ get_map_indices c map_id
      else 0 in
    (* get unique entries by indices and types *)
    hashtbl_replace h (t_elem, index) @@
      function None -> [map_id] | Some l -> map_id::l);
  Hashtbl.fold (fun (t,i) maps acc -> (i, (t, maps))::acc) h []

let read_e ~vid ~global e =
  if vid && not global then ("vid", t_vid)::e
  else if global && List.length e = 1 then
      (* add a unit key for the case of no key *)
      ("_", t_unit)::e
  else e

let ds_e ds = read_e ~vid:ds.vid ~global:ds.global ds.e

let t_of_e e = wrap_ttuple @@ snd_many e
let tup_of_e e = mk_tuple @@ ids_to_vars @@ fst_many e

let string_of_pat pat =
  let open K3PrintSyntax in
  strcatmap (fun (e,t) -> "("^string_of_expr e^", "^string_of_type t^")") pat

let string_of_pat_e pat = strcatmap K3PrintSyntax.string_of_expr pat

let string_of_ds ds =
  Printf.sprintf "{id:%s; e:%s; ee:%s; t:%s; global:%b; vid:%b}"
  ds.id
  (String.concat ", " @@ List.map (fun (s,t) -> s^", "^K3PrintSyntax.string_of_type t) ds.e)
  (String.concat ", " @@ List.map (fun stl -> ("["^String.concat ", " (List.map (fun (s,t) -> s^", "^K3PrintSyntax.string_of_type t) stl) ^"]")) ds.ee)
  (K3PrintSyntax.string_of_type ds.t)
  ds.global
  ds.vid


(* get a ds representing a map *)
(* @calc: have the type of inner calculation *)
(* @vid: keep the vid *)
(* @global: always has vid (indirectly) *)
let map_ds_of_id ?(bag=false) ?(suffix="") ?(vid=true) ?name ~global c map_id =
  let vid = if global then true else vid in
  let nm = unwrap_option (map_name_of c.p map_id) name in
  let e = map_ids_types_for c.p map_id in
  let wrap =
    if global then wrap_t_of_map' c map_id
    else
      if bag then wrap_tbag'
      else wrap_t_calc' in
  (* suffix added only to last value *)
  let add_suffix l =
    let k, v = list_split (-1) l in
    let v' = List.map (first @@ fun x -> x^suffix) v in
    k @ v'
  in
  let e = add_suffix e in
  let e, ee, t, init =
    (* real external map *)
    if global then
      let k, v = map_t_split' e in
      let e, ee =
        if k = [] then
          ["value"^suffix, wrap_ttuple @@ snd_many v], [v]
        else
          ["key", wrap_ttuple @@ snd_many k;
           "value"^suffix, wrap_ttuple @@ snd_many v],
          [k; v]
      in
      let t = wrap @@ snd_many @@ read_e ~vid ~global e in
      let init = mk_ind @@ mk_empty t in
      e, ee, t, init
    else
      let t = wrap @@ snd_many @@ read_e ~vid ~global e in
      e, [], t, mk_empty t
  in
  create_ds nm t ~e ~ee ~init ~global ~map_id ~vid

(* create a map structure: used for both maps and buffers *)
let make_map_decl c name map_id =
  map_ds_of_id ~name ~vid:true ~global:true c map_id

(* turn a flat pattern into a potentially deep pattern *)
let pat_of_flat typ ~has_vid wrap_tup vid_fn unit_pat unit_ds ds flat =
  if ds.ee = [] then flat
  else
    let flat = if has_vid then tl flat else flat in
    let l_flat = List.length flat in
    let l_ee = list_sum List.length ds.ee in
    if l_flat <> l_ee then failwith @@
      Printf.sprintf "%s: flat[%d], ee[%d], has_vid[%b]. Length mismatch for map %s. ds=%s"
        typ l_flat l_ee has_vid ds.id (string_of_ds ds);
    let flat, ds_ee =
      (* add unit for pure value maps *)
      if l_ee = 1 && ds.global then unit_pat :: flat, [unit_ds]::ds.ee
      else flat, ds.ee in
    let lengths = List.map List.length ds_ee in
    let clumped = clump lengths flat in
    vid_fn @@ List.map wrap_tup clumped

let pat_of_flat_e ?(vid_nm="vid") ~add_vid ?(has_vid=false) ds flat =
  let vid_fn l = match add_vid, has_vid with
    | true, true -> hd flat :: l
    | true, _    -> mk_var vid_nm :: l
    | _          -> l
  in
  try
    pat_of_flat "e" ~has_vid mk_tuple vid_fn mk_cunit ("_", t_unit) ds flat
  with Failure s -> raise @@ Failure(s^", "^string_of_pat_e flat)

let pat_of_flat_t ~add_vid ?(has_vid=false) ds flat =
  let vid_fn l = if add_vid then t_vid::l else l in
  pat_of_flat "t" ~has_vid wrap_ttuple vid_fn t_unit ("_", t_unit) ds flat

(* create a list of access expressions, types even for deep data structures *)
(* we can either assume that we're in a loop named after ds.e or work off of
 * an expression *)
let pat_of_ds ?(flatten=false) ?(vid_nm="vid") ?expr ?(drop_vid=false) ds =
  let add_vid l =
    if ds.vid && not drop_vid then (mk_var vid_nm, t_vid)::l
    else l
  in
  if List.length ds.e = 1 then
  (* value but no key *)
    match expr with
    | None when ds.global && not flatten -> add_vid [mk_cunit, t_unit; first mk_var @@ hd ds.e]
    | None   -> add_vid [first mk_var @@ hd ds.e]
    | Some x when ds.global && flatten -> add_vid [mk_subscript 2 x, snd @@ hd @@ ds.e]
    | Some x when ds.global -> add_vid [mk_cunit, t_unit; mk_subscript 2 x, snd @@ hd @@ ds.e]
    | Some x -> add_vid [x, snd @@ hd @@ ds.e]
  else if ds.ee = [] then
  (* one layered ds *)
    let e = if ds.vid then (vid_nm, t_vid)::ds.e else ds.e in
    let e = insert_index_fst e in
    let e = List.map (fun (i, (x,y)) -> match expr with
      | Some e -> mk_subscript (i+1) e, y
      | _      -> mk_var x, y) e
    in
    if drop_vid then tl e else e
  else
    let e = insert_index_fst ds.e in
    let l =
      List.flatten @@
        List.map2 (fun (j, (id,t)) idts ->
          match idts with
          | [_] ->
            (* either direct or subscript access *)
            let access = maybe (mk_var id) (fun e -> mk_subscript (j+1) e) expr in
            [access, t]
          | _   ->
            List.mapi (fun i (_, t) ->
              (* either direct or subscript access *)
              let access = maybe (mk_var id) (fun e -> mk_subscript (j+1) e) expr in
              mk_subscript (i+1) @@ access, t
            ) idts
        ) e ds.ee
    in
    if flatten then add_vid l
    else
      let e, t = list_unzip l in
      let e = pat_of_flat_e ~vid_nm ~add_vid:(not drop_vid) ds e in
      let t = pat_of_flat_t ~add_vid:(not drop_vid) ds t in
      list_zip e t

let is_unknown e =
  match U.tag_of_expr e with Const CUnknown -> true | _ -> false

(* check whether a pattern matches the criteria for being a lookup:
 * no unknown except for the value *)
let is_lookup_pat pat =
  let pat = mk_tuple @@ list_drop_end 1 @@ U.unwrap_tuple pat in
  not @@ Tree.fold_tree_th_bu (fun acc e -> is_unknown e || acc) false pat

let drop_val l = list_drop_end 1 l
let drop_val' l = fst_many @@ list_drop_end 1 l
let get_val  l = hd @@ list_take_end 1 l
let get_val' l = fst @@ hd @@ list_take_end 1 l
let get_vid' l = fst @@ hd l
let drop_vid l = tl l
let drop_vid' l = fst_many @@ tl l
let unknown_val l = drop_val l @ [mk_cunknown]
let unknown_val' l = drop_val' l @ [mk_cunknown]
let vid_and_unknowns l = List.mapi (fun i (x,y) -> if i > 0 then mk_cunknown,y else x,y) l
let vid_and_unknowns' l = fst_many @@ vid_and_unknowns l
let new_val l x = drop_val l @ [x]
let new_val' l x = drop_val' l @ [x]
let new_vid' s l = (mk_var s) :: drop_vid' l

(* convert a global map to a bag type for calculation *)
let calc_of_map_t c ?bag ~keep_vid map_id col =
  let map_ds  = map_ds_of_id ~global:true c map_id in
  let calc_ds = map_ds_of_id ?bag ~global:false ~vid:keep_vid c map_id in
  let map_pat = pat_of_ds ~drop_vid:true map_ds in
  let map_flat =
    pat_of_ds ~drop_vid:(not keep_vid) ~flatten:true ~expr:(mk_var "vals") map_ds in
  mk_aggv
    (mk_lambda''
      (["acc", calc_ds.t; "vid", t_vid; "vals", wrap_ttuple @@ snd_many map_pat]) @@
      mk_insert_block "acc" @@ fst_many map_flat)
    (mk_empty calc_ds.t) @@
    col

(* location of vid in tuples *)
let vid_idx = 0
let vid_shift = (+) 1
let add_vid_idx l = l @ [vid_idx]

let t_vid_list = wrap_tvector t_vid
let t_vid_sortedset = wrap_tsortedset t_vid

(* global declaration of default vid to put into every map *)
let g_init_vid =
                      (* counter=0 *)
  let init = mk_tuple [mk_cint 0] in
  create_ds "g_init_vid" t_vid ~init

let g_max_int = create_ds "g_max_int" t_int ~init:(mk_apply' "get_max_int" [mk_cunit])
let g_min_vid = create_ds "g_min_vid" t_vid ~init:min_vid_k3
let g_max_vid =
  (* get rid of lowest bit *)
  let init = mk_mult (mk_divi (mk_apply' "get_max_int" [mk_cunit]) @@ mk_cint 2) @@ mk_cint 2 in
  create_ds "g_max_vid" t_vid ~init
let g_start_vid = create_ds "g_start_vid" t_vid ~init:start_vid_k3

let mk_vid_add curr add = mk_add curr add

(* whether a vid is a tuple or a non-tuple value *)
let is_vid_tuple = false

(* reduce trig arguments to those that are actually used by the code *)
let filter_t_args c trig args =
  let set =
    try StrMap.find (P.remove_trig_prefix trig) c.unused_trig_args
    with Not_found -> StrSet.empty
  in
  List.filter (fun (id,_) -> not @@ StrSet.mem id set) args

(* function that filters out unused arguments in trigger *)
let args_of_t c trig = filter_t_args c trig @@ P.args_of_t c.p trig

(* trigger argument manipulation convenience functions *)
let arg_types_of_t c trig_nm = snd_many @@ args_of_t c trig_nm
let arg_names_of_t c trig_nm = fst_many @@ args_of_t c trig_nm
let args_of_t_as_vars c trig_nm = ids_to_vars (arg_names_of_t c trig_nm)

let args_of_t_with_v ?(vid="vid") c trig_nm = (vid, t_vid)::args_of_t c trig_nm
let arg_types_of_t_with_v c trig_nm = t_vid::arg_types_of_t c trig_nm
let args_of_t_as_vars_with_v ?(vid="vid") c trig_nm =
  mk_var vid::args_of_t_as_vars c trig_nm


(**** global data structures ****)

let mk_sort_vector ds new_t =
  mk_convert_col (wrap_tlist t_addr) new_t @@
  mk_sort (mk_lambda'' ["a1", t_addr; "a2", t_addr] @@
            mk_lt (mk_var "a1") @@ mk_var "a2") @@
  mk_convert_col ds.t (wrap_tlist t_addr) @@
  mk_var ds.id

(* sorted version of peers *)
let my_peers =
  let e = ["addr", t_addr] in
  let t = wrap_tvector' @@ snd_many e in
  let init = mk_sort_vector G.peers t in
  create_ds "my_peers" ~init t ~e

(* sorted locally, and converted to integers *)
let my_local_peers =
  let e = ["addr", t_int] in
  let init =
    mk_map (mk_lambda' ["addr", t_addr] @@
        mk_apply' "int_of_addr" [mk_var "addr"]) @@
      mk_sort_vector G.local_peers (wrap_tvector t_addr)
  in
  create_ds "my_local_peers" t_int_vector ~init ~e

let me_int =
  let init = mk_apply' "int_of_addr" [G.me_var] in
  create_ds "me_int" t_int ~init

(* the first peer in the local peers is the local master *)
let local_master =
  let init =
    mk_if (mk_gt (mk_size @@ mk_var my_local_peers.id) @@ mk_cint 0)
      (mk_at (mk_var my_local_peers.id) @@ mk_cint 0)
      (mk_cint (-1))
  in
  create_ds "local_master" t_int ~init

(* whether we're running in the interpreter *)
let interpreter_mode = create_ds "interpreter_mode" t_bool

let num_peers =
  let init = mk_size @@ mk_var my_peers.id in
  create_ds "num_peers" (mut t_int) ~init

(* specifies the job of a node: master/switch/node *)
let job_master_v = 0
let job_switch_v = 1
(* local_master is determined from the local peers, NOT from the role alone *)
let job_local_master_v = 2
let job_node_v   = 3
let job_timer_v  = 4
let job_none_v   = 5

let job_master = create_ds "job_master" t_int ~init:(mk_cint job_master_v)
let job_switch = create_ds "job_switch" t_int ~init:(mk_cint job_switch_v)
let job_local_master = create_ds "job_local_master" t_int ~init:(mk_cint job_local_master_v)
let job_node   = create_ds "job_node"   t_int ~init:(mk_cint job_node_v)
let job_timer  = create_ds "job_timer"  t_int ~init:(mk_cint job_timer_v)

let job =
  let do_if regex e_if e_ifnot =
    mk_if
      (mk_eq (mk_apply' "regex_match_int" [mk_cstring regex; mk_var "role2"]) @@ mk_cint 1)
      e_if
      e_ifnot
  in
  let init =
    mk_let ["role2"] (mk_peek_or_error "bad role" @@ mk_var "role") @@
      do_if "master.*" (mk_var job_master.id) @@
      do_if "switch.*" (mk_var job_switch.id) @@
      do_if "node.*"
        (mk_if_eq (mk_var me_int.id) (mk_var local_master.id)
          (mk_var job_local_master.id) @@
          mk_var job_node.id) @@
      do_if "timer.*" (mk_var job_timer.id) @@
      mk_error "failed to find proper role"
  in
  create_ds "job" (mut t_int) ~init

let job_of_str = function
  | "master" -> job_master_v
  | "switch" -> job_switch_v
  | "local_master" -> job_local_master_v
  | "node"   -> job_node_v
  | "timer"  -> job_timer_v
  | _        -> job_none_v

(* this must be created for the specific run *)
(* we fill it dynamically in the interpreter *)
let jobs =
  let e = ["job", t_int] in
  let t = mut @@ wrap_tvector' @@ snd_many e in
  create_ds "jobs" t ~e

(* address of master node *)
let master_addr = create_ds "master_addr" (mut t_addr)

(* address of timer peer *)
let timer_addr =
  let d_init =
    mk_case_sn
      (mk_peek @@ mk_filter_cnt
        (mk_eq (mk_var "job") @@ mk_var job_timer.id) jobs)
      "timer"
      (mk_apply' "addr_of_int" [mk_var "timer"]) @@
      mk_error "no timer peer found" in
  create_ds "timer_addr" (mut t_addr) ~d_init

let nodes =
  let d_init =
    mk_filter_cnt
      (mk_eq (mk_var job.id) @@ mk_var job_node.id) jobs in
  let e = ["addr", t_int] in
  create_ds "nodes" (mut @@ wrap_tbag' @@ snd_many e) ~e ~d_init

let local_masters =
  let d_init =
    mk_filter_cnt
      (mk_eq (mk_var job.id) @@ mk_var job_local_master.id) jobs in
  let e = ["addr", t_int] in
  create_ds "local_masters" (mut @@ wrap_tbag' @@ snd_many e) ~e ~d_init

let switches =
  let d_init =
    mk_filter_cnt
      (mk_eq (mk_var job.id) @@ mk_var job_switch.id) jobs in
  let e = ["addr", t_int] in
  create_ds "switches" (mut @@ wrap_tbag' @@ snd_many e) ~e ~d_init

(* peer_addr -> master_addr *)
let peer_master_map =
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_cint 0) @@ mk_var my_peers.id in
  create_ds "peer_master_map" (mut t_int_vector) ~init

let num_switches =
  let d_init = mk_size @@ mk_var switches.id in
  create_ds "num_switches" (mut t_int) ~d_init

let num_nodes =
  let d_init = mk_size @@ mk_var nodes.id in
  create_ds "num_nodes" (mut t_int) ~d_init

let nd_send_next_batch_if_available_nm = "nd_send_next_batch_if_available"

let sw_rcv_token_trig_nm = "sw_rcv_token_trig"
let sw_rcv_vector_clock_nm = "sw_rcv_vector_clock"

(* timing data structures *)
let ms_start_time = create_ds "ms_start_time" @@ mut t_int
let ms_end_time = create_ds "ms_end_time" @@ mut t_int

(* number of stashed batches *)
let nd_num_stashed = create_ds "num_stashed" @@ mut t_int

(* for debugging, driver pause *)
let sw_event_driver_sleep = create_ds "sw_event_driver_sleep" @@ mut t_int

(**** No corrective mode ****)

(* whether we're operating with correctives on *)
let corrective_mode = create_ds "corrective_mode" t_bool

(* map stmt_id to lmap *)
let nd_lmap_of_stmt_id_id = "nd_lmap_of_stmt_id"
let nd_lmap_of_stmt_id c =
  let e = ["map_id", t_int] in
  let t = t_int_vector in
  let init = k3_container_of_list t @@
    List.map mk_cint @@ snd_many @@ P.stmts_lhs_maps c.p in
  create_ds "nd_lmap_of_stmt_id" t ~e ~init

(* for buffered fetches in no-corrective mode *)
let nd_fetch_buffer_inner2 =
  let e = ["stmt_map_id", t_int] in
  create_ds "inner2" (wrap_tset @@ t_of_e e) ~e

let nd_fetch_buffer_inner =
  let e = ["vid", t_vid; "stmt_map_ids", wrap_tset' [t_int]] in
  create_ds "inner" (wrap_tsortedmap' @@ snd_many e) ~e

(* map_id -> vid -> stmt_map_id set *)
let nd_fetch_buffer_id = "nd_fetch_buffer"
let nd_fetch_buffer_e = ["inner", nd_fetch_buffer_inner.t]
let nd_fetch_buffer c =
  let e = nd_fetch_buffer_e in
  let init =
    k3_container_of_list (wrap_tvector nd_fetch_buffer_inner.t) @@
    List.map (const @@ mk_empty nd_fetch_buffer_inner.t) @@ 0::P.get_map_list c.p in
  create_ds ~init ~e nd_fetch_buffer_id @@ wrap_tvector @@ t_of_e e

(**** Protocol Init code ****)

(* addr to int translation is inefficient *)
let int_of_addr = mk_global_fn "int_of_addr" ["addr", t_addr] [t_int] @@
  mk_fst @@
  mk_agg (mk_lambda2' ["acc", t_int; "count", t_int] ["addr2", t_addr] @@
          mk_if (mk_eq (mk_var "addr") @@ mk_var "addr2")
            (mk_tuple [mk_var "count"; mk_add (mk_var "count") @@ mk_cint 1]) @@
             mk_tuple [mk_var "acc";   mk_add (mk_var "count") @@ mk_cint 1])
    (mk_tuple [mk_cint @@ -1; mk_cint 0]) @@
    mk_var my_peers.id

let addr_of_int = mk_global_fn "addr_of_int" ["i", t_int] [t_addr] @@
  mk_at_with (mk_var my_peers.id) (mk_var "i") @@ mk_id_fn' [t_addr]

let local_master_of_peer = mk_global_fn "local_master_of_peer" ["i", t_int] [t_int] @@
  mk_at_with (mk_var peer_master_map.id) (mk_var "i") @@ mk_id_fn' [t_int]

let mk_sendi trig addr args = mk_send trig (mk_apply' "addr_of_int" [addr]) args

(* global containing mapping of map_id to map_name and dimensionality *)
let map_ids_id = "map_ids"
let map_ids c =
  let e = ["map_idx", t_int; "map_name", t_string; "map_dim", t_int] in
  let t = wrap_tbag' @@ snd_many e in
  let init =
    k3_container_of_list t @@
      P.for_all_maps c.p @@ fun i ->
        mk_tuple [mk_cint i;
                  mk_cstring @@ P.map_name_of c.p i;
                  mk_cint @@ List.length @@ P.map_types_for c.p i] in
  create_ds "map_ids" t ~e ~init

(* adds a string for trigger selector and an int for deletes (1=insert) *)
let combine_trig_args c =
  let suffix = str_drop (String.length "delete_") in
  (* get only the relevant parts *)
  let trigs = StrSet.of_list @@ List.map suffix @@ P.get_trig_list c.p in
  second (StrMap.filter @@ fun k _ -> StrSet.mem k trigs) c.reduced_agenda_map

let nd_stmt_cntrs_per_map_inner =
  let e = ["vid", t_vid; "stmt_id", t_int] in
  create_ds "inner" (wrap_tsortedmap' @@ snd_many e) ~e

(* indexed by map_id *)
let nd_stmt_cntrs_per_map_id = "nd_stmt_cntrs_per_map"
let nd_stmt_cntrs_per_map_e = ["inner", nd_stmt_cntrs_per_map_inner.t]
let nd_stmt_cntrs_per_map c =
  let e = nd_stmt_cntrs_per_map_e in
  let init =
    k3_container_of_list (wrap_tvector nd_stmt_cntrs_per_map_inner.t) @@
    List.map (const @@ mk_empty nd_stmt_cntrs_per_map_inner.t) @@ 0::P.get_map_list c.p in
  create_ds ~e ~init nd_stmt_cntrs_per_map_id @@ wrap_tvector @@ t_of_e e

(* not a real ds. only inside stmt_cntrs *)
let nd_corr_map_inner =
  let e = ["hop2", t_int; "count2", t_int] in
  create_ds "sc_corr_map" (wrap_tmap' @@ snd_many e) ~e

let nd_corr_map =
  let ee = [["vid", t_vid; "stmt_id", t_stmt_id]; ["tree", nd_corr_map_inner.t]] in
  let e = ["vid_stmt", wrap_ttuple [t_vid; t_stmt_id]; "tree", nd_corr_map_inner.t] in
  create_ds "nd_corr_map" ~e ~ee @@ wrap_tmap @@ t_of_e e

let nd_stmt_cntr_size =
  create_ds ~init:(mk_cint 0) "nd_stmt_cntr_size" @@ mut t_int

(* inner part of stmt counters *)
let nd_stmt_cntrs_inner =
  (* indexed by stmt_id *)
  let ee =
    [["vid", t_int];
     ["counter", t_int;
      "no_info", t_bool]] in
  let e = ["vid", t_vid; "stmt_cntr_info", wrap_ttuple @@ snd_many @@ at ee 1] in
  create_ds "nd_stmt_cntrs_inner" ~e ~ee @@ wrap_tmap' @@ snd_many e

(* stmt_cntrs - (vid, stmt_id, counter) *)
(* 1st counter: count messages received until do_complete *)
(* 2nd counter: map from hop to counter *)
let nd_stmt_cntrs_id = "nd_stmt_cntrs"
let nd_stmt_cntrs_e = [nd_stmt_cntrs_inner.id, nd_stmt_cntrs_inner.t]
let nd_stmt_cntrs c =
  let ss = List.map (const @@ mk_empty nd_stmt_cntrs_inner.t) @@ P.get_stmt_list c.p in
  (* indexed by stmt_id *)
  let e = nd_stmt_cntrs_e in
  let t = wrap_tvector' @@ snd_many e in
  let init = k3_container_of_list t ss in
  create_ds nd_stmt_cntrs_id ~init ~e t

let find_nd_stmt_cntrs_min_vid =
  mk_agg (mk_lambda2' ["min_vid", t_vid] nd_stmt_cntrs_e @@
          mk_agg (mk_lambda2' ["min_vid2", t_vid] nd_stmt_cntrs_inner.e @@
                  mk_if (mk_lt (mk_var "vid") @@ mk_var "min_vid2")
                    (mk_var "vid") @@
                    mk_var "min_vid2")
            (mk_var "min_vid") @@
            mk_var nd_stmt_cntrs_inner.id)
    (mk_var g_max_vid.id) @@
    mk_var nd_stmt_cntrs_id

(* master log *)
(* the master log shows which statements we pushed data for
 * filter_corrective_list calls nd_log_read_geq to figure out which
 * correctives should be sent *)
(* This is coarse-grain corrective control. *)
let nd_log_master =
  let e = ["stmt_id", t_stmt_id; "vid_set", wrap_tsortedset @@ t_vid] in
  create_ds "nd_log_master" (mut @@ wrap_tmap' @@ snd_many e) ~e

(* names for log *)
let lm_log_for_t t = "lm_log_"^t
let lm_log_buffer_for_t t = "lm_log_buffer_"^t
let nd_log_buffer_ind_for_t t = "nd_log_buffer_ind_"^t

let log_buffer_for c t =
  let e' = args_of_t c t in
  let e  = ["vid", t_vid; "args", wrap_ttuple @@ snd_many e'] in
  create_ds (lm_log_buffer_for_t t) (wrap_tmap' @@ snd_many e) ~e

(* log buffers: hold vid-based args for fast shared access *)
(* unfortunately this means we need to use a vector to hold a single element so we can move it out globally *)
let lm_log_buffer_ds c : data_struct list =
  let log_vec_for trig =
    let sub_ds = log_buffer_for c trig in
    let e = ["inner", sub_ds.t] in
    let t = wrap_tvector' @@ snd_many e in
    let init = mk_singleton t [mk_empty sub_ds.t] in
    create_ds (lm_log_buffer_for_t trig) t ~init ~e
  in
  P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p log_vec_for

(* ds to hold the pointer to the local master's block of args *)
let nd_log_buffer_ind_ds c : data_struct list =
  let log_ptr_for t =
    create_ds (nd_log_buffer_ind_for_t t) (mut @@ wrap_tind (log_buffer_for c t).t)
  in
  P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p log_ptr_for

(* bitmap to tell us which triggers have already been loaded into buffers *)
(* trig_id -> bool *)
let nd_log_buffer_bitmap = create_ds "nd_log_buffer_bitmap" t_bitset

(* log data structures: contains indirect log_buffers per batch_id *)
let lm_log_ds c : data_struct list =
  let log_struct_for trig =
    let inner_ds = log_buffer_for c trig in
    let e  = ["batch_id", t_vid; "inner", wrap_tind inner_ds.t] in
    (* must be shared across peers *)
    let t = mut @@ wrap_tmap' @@ snd_many e in
    create_ds (lm_log_for_t trig) t ~shared:true ~e ~gc_vid_nm:"batch_id"
  in
  P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p log_struct_for

(* ds to track whether arguments arrived *)
(* when the count hits 2, we execute the batch_id from the switch *)
let nd_trig_arg_notifications =
  let e = ["batch_id", t_vid] in
  create_ds "nd_trig_arg_notifications" (wrap_tset' @@ snd_many e)

(* Buffer versions of maps per statement (to prevent mixing values) *)
(* NOTE: doesn't contain special inits from AST *)
let map_buffers c =
  (* for all rhs, lhs map pairs *)
  let do_map (stmt, map_id) =
    let name = P.buf_of_stmt_map_id c.p stmt map_id in
    make_map_decl c name map_id
  in
  P.for_all_stmts_rhs_maps c.p do_map

(* Regular maps *)
(* NOTE: doesn't contain special inits from AST *)
let maps c =
  (* for all rhs, lhs map pairs *)
  let do_map map_id =
    let name = P.map_name_of c.p map_id in
    make_map_decl c name map_id
  in
  P.for_all_maps c.p do_map

let sw_seen_sentinel    = create_ds "sw_seen_sentinel" (mut t_bool) ~init:mk_cfalse
let sw_init           = create_ds "sw_init" (mut t_bool) ~init:mk_cfalse

(*** trigger names ***)

(* main dispatcher for whole protocol *)
let trig_dispatcher_trig_nm = "trig_dispatcher_trig"
let trig_dispatcher_trig_unique_nm = "trig_dispatcher_unique_trig"

(* dispatcher for sw -> node with corrective mode *)
let nd_from_sw_trig_dispatcher_trig_nm = "nd_from_sw_trig_dispatcher_trig"

(* handler for many other triggers *)
let m_nm = P.map_name_of
let trig_save_arg_name_of_t t = "lm_trig_save_args_"^t
let trig_load_arg_sub_handler_name_of_t t = "nd_trig_load_arg_sub_handler_"^t
let trig_no_arg_sub_handler_name_of_t t   = "nd_trig_no_arg_sub_handler_"^t
let send_fetches_isobatch_name_of_t t = sp "sw_%s_send_fetches_isobatch" t
let rcv_fetch_isobatch_name_of_t t s = sp "nd_%s_%d_rcv_fetch_isobatch" t s
let rcv_put_isobatch_name_of_t t s = sp "nd_%s_%d_rcv_put_isobatch" t s
let rcv_stmt_isobatch_name_of_t t s = sp "nd_%s_%d_rcv_stmt_isobatch" t s
let send_push_isobatch_name_of_t c t s m = sp "nd_%s_%d_send_push_isobatch_%s" t s (m_nm c.p m)
let rcv_push_isobatch_name_of_t t s = sp "nd_%s_%d_rcv_push_isobatch" t s
let send_corrective_name_of_t c m = sp "nd_%s_send_correctives" (m_nm c.p m)
let do_complete_name_of_t t s = sp "nd_%s_%d_do_complete" t s
let rcv_corrective_name_of_t c t s m = sp "nd_%s_%d_rcv_corrective_%s" t s (m_nm c.p m)
let do_corrective_name_of_t c t s m = sp "nd_%s_%d_do_corrective_%s" t s (m_nm c.p m)

let sw_ack_rcv_trig_nm = "sw_ack_rcv"

let nd_rcv_corr_done_nm = "nd_rcv_corr_done"

let nd_rcv_warmup_push_nm = "rcv_warmup_push"

let nd_rcv_stmt_isobatch_nm = "nd_rcv_stmt_isobatch"

(*** trigger args. Needed here for polyqueues ***)

(* the trig header marks the args *)
let trig_save_arg_args_poly c t = args_of_t_with_v c t
let trig_save_arg_args c t = trig_save_arg_args_poly c t

(* slim trig header for pushes *)
(* TODO: make vid 16-bit offset from batch_id *)
let trig_load_arg_sub_handler_args_poly = ["vid", t_vid]
let trig_load_arg_sub_handler_args = ["batch_id", t_vid] @ trig_load_arg_sub_handler_args_poly

let trig_no_arg_sub_handler_args_poly = ["vid", t_vid]
let trig_no_arg_sub_handler_args = ["batch_id", t_vid] @ trig_no_arg_sub_handler_args_poly

(* rcv_put includes stmt_cnt_list_ship *)
let nd_rcv_put_args_poly = ["count2", t_int]
let nd_rcv_put_args c t = nd_rcv_put_args_poly @ args_of_t_with_v c t

let nd_rcv_put_isobatch_poly = ["count2", t_int]
let nd_rcv_put_isobatch_args c t = ["batch_id", t_vid] @ nd_rcv_put_isobatch_poly

(* rcv_fetch: data structure that is sent *)
let send_map_ids =
  (* this is a bag since no aggregation is done *)
  let e = ["map_id", t_map_id] in
  create_ds ~e "send_map_ids" @@ wrap_tbag' @@ snd_many e

let nd_rcv_fetch_args_poly c t = []
let nd_rcv_fetch_args c t = nd_rcv_fetch_args_poly c t @ args_of_t_with_v c t

let nd_do_complete_trig_args_poly c t = []
let nd_do_complete_trig_args c t = nd_do_complete_trig_args_poly c t @ args_of_t_with_v c t

let nd_rcv_push_args_poly = []
let nd_rcv_push_args = nd_rcv_push_args_poly @ ["has_data", t_bool; "count", t_int; "vid", t_vid]

let nd_rcv_push_isobatch_args_poly = ["batch_id", t_vid]
let nd_rcv_push_isobatch_args = ["count", t_int] @ nd_rcv_push_isobatch_args_poly

(* for do_corrective:
 * original values commonly used to send back to original do_complete *)
let orig_vals =
  ["orig_addr", t_int; "orig_stmt_id", t_stmt_id; "orig_vid", t_vid; "hop", t_int]

let nd_rcv_corr_args = orig_vals @ ["vid", t_vid]

let nd_rcv_corr_done_args = ["vid", t_vid; "stmt_id", t_stmt_id; "hop", t_int; "count", t_int]

(* for GC *)
let sw_ack_rcv_trig_args = ["addr", t_int; "vid", t_vid]

let get_global_poly_tags c =
  List.map (fun ti -> ti.itag, ti.tag, wrap_ttuple @@ snd_many ti.args) c.poly_tags

let get_poly_event_tags c =
  List.map (fun (i, (s, ts)) -> i, s, wrap_ttuple ts) c.event_tags

(* the input poly type filled in from the file *)
let poly_event_typedef_id = "poly_event_t"
let poly_event_typedef c = wrap_tpolyq @@ get_poly_event_tags c
let poly_event_queue = create_ds "poly_event" @@ t_alias poly_event_typedef_id

(* whether to use a unique poly queue *)
let use_unique_poly = create_ds ~init:mk_ctrue "use_unique_poly" @@ t_bool

(* global for avoiding huge tags *)
let empty_event_queue = create_ds "empty_event_queue" @@ poly_event_queue.t

(* the global poly type of the program *)
let poly_queue_typedef_id = "poly_queue_t"
(* unique poly queue *)
let upoly_queue_typedef_id = "upoly_queue_t"

let poly_queue_typedef c = wrap_tpolyq @@ get_global_poly_tags c
let upoly_queue_typedef c = wrap_tuniqpolyq @@ get_global_poly_tags c

(* hypothetical data structures *)
let poly_queue = create_ds "poly_queue" @@ t_alias poly_queue_typedef_id
let upoly_queue = create_ds "upoly_queue" @@ t_alias upoly_queue_typedef_id

(* global for avoiding huge tags *)
let empty_poly_queue = create_ds "empty_poly_queue" @@ poly_queue.t
let empty_upoly_queue = create_ds "empty_upoly_queue" @@ upoly_queue.t

let poly_queues =
  let e = ["queue", poly_queue.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_var "empty_poly_queue") @@ mk_var my_peers.id in
  create_ds ~e ~init "poly_queues" @@ wrap_tvector' @@ snd_many e

let poly_queue_bitmap = create_ds "poly_queue_bitmap" t_bitset

let upoly_queues =
  let e = ["uqueue", upoly_queue.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_var "empty_upoly_queue") @@ mk_var my_peers.id in
  create_ds ~e ~init "upoly_queues" @@ wrap_tvector' @@ snd_many e

let upoly_queue_bitmap = create_ds "upoly_queue_bitmap" t_bitset

(* switch poly queues - move into it between token ring triggers *)
let sw_poly_queues =
  let e = ["queue", poly_queue.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_var "empty_poly_queue") @@ mk_var my_peers.id in
  create_ds ~e ~init "sw_poly_queues" @@ wrap_tvector' @@ snd_many e

let sw_poly_queue_bitmap = create_ds "sw_poly_queue_bitmap" t_bitset

(* we use this to make sure some trig header gets sent at least once per sub-trigger handling *)
(* ip -> bool *)
let send_trig_header_bitmap = create_ds "send_trig_header_bitmap" t_bitset

(* keep track of which vid args we've sent in this batch *)
let send_trig_args_inner =
  let e = ["vid", t_vid; "_u", t_unit] in
  create_ds ~e "inner" @@ wrap_tmap @@ t_of_e e

(* ip -> vid -> () : which args have been sent *)
let send_trig_args_map =
  let e = ["inner", send_trig_args_inner.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_empty send_trig_args_inner.t) @@
    mk_var my_peers.id in
  create_ds ~e ~init "send_trig_args_map" @@ wrap_tvector @@ t_of_e e

let ip_arg = ["ip", t_int]
let ip_arg2 = ["ip2", t_int]
let stmt_arg = ["stmt", t_int]

(* Assistive data structure for easily clearing send_trig_args_map *)
(* {ip} *)
let send_trig_args_bitmap = create_ds "send_trig_args_bitmap" t_bitset

(* u is for unique *)
let p_idx  = ["idx", t_int]
let up_idx = ["uidx", t_int]

let p_off = ["offset", t_int]
let up_off = ["uoffset", t_int]

let p_tag = ["tag", t_int]
let up_tag = ["utag", t_int]

let poly_args = ["poly_queue", poly_queue.t] @ p_idx @ p_off
let upoly_args = ["upoly_queue", upoly_queue.t] @ up_idx @ up_off

let poly_args_partial = p_idx @ p_off
let upoly_args_partial = up_idx @ up_off

(* queue for next message batch -- contains polyqueue *)
let sw_event_queue =
  let e = ["poly_queue", poly_queue.t] in
  create_ds "sw_event_queue" (wrap_tlist' @@ snd_many e) ~e

(*** Polyqueues ***)

let sw_poly_batch_size = create_ds ~init:(mk_cint 5) "sw_poly_batch_size" @@ t_int

(* whether we carry out a reserve operation on polybuffs *)
let do_poly_reserve = create_ds ~init:mk_ctrue "do_poly_reserve" @@ t_bool

(* a conservative multiplier for needed space size relative to original batch number
   Consider the fact that we're going to take the maximum fixed type size *)
let reserve_mult = 2
let reserve_str_estimate = 4

(* maximum size of polyqueue entries *)
let max_poly_queue_csize c =
  let _, max = list_max_op U.csize_of_type @@
    List.map (fun ti -> wrap_ttuple @@ snd_many ti.args) c.poly_tags in
  max * reserve_mult

let max_event_queue_csize c =
  let max = list_max_op U.csize_of_type @@
    List.map (wrap_ttuple |- snd |- snd) c.event_tags in
  max

(*** Tags ***)

(* we create tags for events, with the full width of said events plus insert/delete field *)
let calc_event_tags c =
  let flat_types = Array.of_list @@ fst c.agenda_map in
  let events = StrMap.to_list @@ snd c.agenda_map in
  (* unmap from flat_types, and add a bool for insert/delete *)
  let l = ("sentinel", [t_unit]) ::
    List.map (fun (t, indices) -> t, t_bool :: List.map (fun i -> flat_types.(i)) indices) events
  in
  insert_index_fst l

let ti ?(fn="") ?(const_args=[]) ?(batch_id=false) ?(vid=false) ?(trig_args=false) tag tag_typ args =
  let fn = if fn = "" then tag else fn in
  {tag; itag=0; tag_typ; args; fn; batch_id; trig_args; const_args}

(* we create one global tag hashmap, which we use to populate polyqueues *)
(* format (tag, tag_type, types) *)
let calc_poly_tags c =
  let l =
    let for_all_trigs = P.for_all_trigs ~delete:c.gen_deletes in
    let events = fst_many @@ StrMap.to_list @@ snd c.agenda_map in
    (* static sentinel *)
    (ti "sentinel" Event ["_", t_unit])::
    (* event tags *)
    (List.map (fun t ->
         let args = try ("do_insert", t_bool)::args_of_t c ("insert_"^t)
                    with Bad_data _ -> [] in
         ti t Event args)
       events) @
    (* map ids for send fetch *)
    (List.map (fun m ->
         ti (P.map_name_of c.p m^"_id") (Ds false) []) @@
     P.get_map_list c.p) @
    (List.flatten @@ for_all_trigs c.p ~sys_init:true @@ fun t ->
      let s_r = P.stmts_with_rhs_maps_in_t c.p t in
      let s_no_r = P.stmts_without_rhs_maps_in_t c.p t in
      let s_rhs = P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt t in
      let s_rhs_corr = List.filter (fun (s, map) -> List.mem map c.corr_maps) s_rhs in
      let save_handler_nm = trig_save_arg_name_of_t t in
      let load_handler_nm = trig_load_arg_sub_handler_name_of_t t in
      let no_arg_handler_nm = trig_no_arg_sub_handler_name_of_t t in
      let sre = t = sys_init in
      (* save args *)
      (ti save_handler_nm ~batch_id:false (Trig false) @@ trig_save_arg_args_poly c t)::
      (* slim version of trigger: pull args from log, just vid *)
      (ti load_handler_nm ~batch_id:true (Trig true) trig_load_arg_sub_handler_args_poly)::
      (ti no_arg_handler_nm ~batch_id:true (Trig true) trig_no_arg_sub_handler_args_poly)::
      (List.flatten @@ List.map (fun s ->
           (* for isobatch mode: receive the vids of the isobatch *)
          (if sre then [] else [
           ti (rcv_stmt_isobatch_name_of_t t s) ~fn:nd_rcv_stmt_isobatch_nm ~const_args:[mk_cint s]
             (SubTrig(false, [load_handler_nm; no_arg_handler_nm])) [];
           (* rcv_put for isobatch: no trig args, just batch_id *)
           ti (rcv_put_isobatch_name_of_t t s) ~batch_id:true
             (Trig false) nd_rcv_put_isobatch_poly;
           ti (rcv_fetch_isobatch_name_of_t t s) ~batch_id:true ~trig_args:true
             (SubTrig(true, [load_handler_nm])) @@
              nd_rcv_fetch_args_poly c t;
          ])) s_r) @
      (* args for do completes without rhs maps *)
      (List.map (fun s -> ti (do_complete_name_of_t t s^"_trig") ~trig_args:true
             (SubTrig(false, [load_handler_nm])) @@
              nd_do_complete_trig_args_poly c t) s_no_r) @
      (* the types for nd_rcv_push. includes a separate, optional map component *)
      (* this isn't a dsTrig since pushes are aggregated at the dispatcher *)
      (List.flatten @@ List.map (fun s ->
          (* get all possible numbers of messages. we create a tag per number of
             messages and has_bool value *)
          let num_maps = List.length @@ P.rhs_maps_of_stmt c.p s in
          let r_maps = create_range ~first:1 @@ num_maps in
          List.flatten @@ List.map (fun n ->
              (if sre then [] else [
               ti (rcv_push_isobatch_name_of_t t s^"_"^soi n)
                ~fn:(rcv_push_isobatch_name_of_t t s) ~const_args:[mk_cint n]
                (Trig false) nd_rcv_push_isobatch_args_poly;
              ]))
            r_maps)
          s_r)@
      (* the types for rcv_push's maps *)
      (List.map (fun (s, m) ->
           ti (P.buf_of_stmt_map_id c.p s m) (Ds true) @@ P.map_ids_types_with_v_for c.p m)
          s_rhs) @
      (* rcv_corrective types. includes a separate, optional map + vids component *)
      (if c.gen_correctives then
         List.map (fun (s, m) ->
            ti (rcv_corrective_name_of_t c t s m) (Trig true) nd_rcv_corr_args)
          s_rhs_corr else [])) @
    (* the types for the maps without vid *)
    (P.for_all_maps c.p @@ fun m ->
      ti (P.map_name_of c.p m^"_map") (Ds false) @@ P.map_ids_types_for c.p m) @    (* warmup header *)
    (ti nd_rcv_warmup_push_nm (Trig true) []) ::
    (* the types for the maps without vid (for warmup) *)
    (P.for_all_maps c.p @@ fun m ->
       ti (P.map_name_of c.p m^"_warmup") (Ds false) (P.map_ids_types_for c.p m))
    @
    (* t_vid_list ds for correctives *)
    [ti "vids" (Ds false) ["batch_id", t_vid; "vid", t_vid]] @
    (if c.gen_correctives then
       [ti nd_rcv_corr_done_nm (Trig false) nd_rcv_corr_done_args] else []) @
    (* for GC (node->switch acks) *)
    [ti sw_ack_rcv_trig_nm (Trig false) sw_ack_rcv_trig_args]
  in
  List.map (fun (i,t) -> {t with itag=i}) @@ insert_index_fst l

let ios_tag c stag = (List.find (fun ti -> ti.tag = stag) c.poly_tags).itag
let soi_tag c itag = (List.find (fun ti -> ti.itag = itag) c.poly_tags).tag

(* name for master send request trigger (part of GC) *)
let ms_send_gc_req_nm = "ms_send_gc_req"

let nd_add_delta_to_buf_nm c map_id =
  let t = P.map_types_for c.p map_id in
  let s = maybe "" (soi |- fst) @@ get_map_indices c map_id in
  Printf.sprintf "nd_add_delta_to_%s%s"
    (String.concat "_" @@ List.map K3PrintSyntax.string_of_type t) s

let flatten_fn_nm t =
  "flatten_"^strcatmap ~sep:"_" K3PrintSyntax.string_of_type t

(* --- Begin frontier function code --- *)

(* Get the latest vals up to a certain vid
 * - This is needed both for sending a push, and for modifying a local slice
 * operation, as well as for GC
 * - slice_col is the k3 expression representing the collection.
 * - assumes a local 'vid' variable containing the border of the frontier
 * - pat assumes NO VID
 * - keep_vid indicates whether we need to remove the vid from the result collection
 *   (we usually need it removed only for modifying ast). While we remove the vid, we also
 *   convert to a bag (again, for modify_ast, to prevent losing duplicates)
 *)
let map_latest_vid_vals ?(vid_nm="vid") c slice_col m_pat map_id ~keep_vid : expr_t =
  let map_ds = map_ds_of_id ~global:true c map_id in
  let convert col = calc_of_map_t c ~keep_vid map_id col in
  let pat = match m_pat with
    | Some pat -> pat_of_flat_e map_ds ~add_vid:false pat
    | None     -> List.map (const mk_cunknown) (ds_e map_ds)
  in
  convert @@ mk_slice_lt slice_col @@ mk_var vid_nm :: pat

(* End of frontier function code *)

(* counter for ip *)
let ip = create_ds "ip" t_int_mut
let ip2 = create_ds "ip2" t_int_mut

(* counter for stmt *)
let stmt_ctr = create_ds "stmt_ctr" t_int_mut
let map_ctr = create_ds "map_ctr" t_int_mut

(* forward declaration to prevent circular inclusion *)
let sys_init_bindings = ref (fun (p:P.prog_data_t) (ast:program_t) -> assert false)

module Bindings = struct
  let all_trig_arg_stmt_binds ~sys_init p f =
    for_all_trigs ~sys_init ~corrective:true ~delete:true p @@ fun trig ->
      let t_args = fst_many @@ P.args_of_t p trig in
      let ss = List.map (find_stmt p) @@ stmts_of_t p trig in
      List.iter (fun s ->
          f s.stmt t_args s.P.lmap s.lmap_binds s.rmap_binds) ss

  let get_idx t_args binds =
    (* only count bound variabls *)
    IntSet.of_list @@ snd_many @@
      List.filter (fun x -> List.mem (fst x) t_args) binds

  (* generic routine to get individual map bindings *)
  let hash_add_intset hash map s =
    hashtbl_replace hash map @@ maybe (IntSetSet.singleton s) (IntSetSet.add s)

  (* @prune: don't accept full bound/empty. needed for multiindex *)
  let insert_idx ?(prune=false) ?(add_full=false) p hash t_args map binds =
    let idx = get_idx t_args binds in
    let map_ts = P.map_types_for p map in
    let add s = hash_add_intset hash map s in
    (* if asked to prune,
      prune out indices that have no key or entire key *)
    if prune then
      if IntSet.is_empty idx || IntSet.cardinal idx >= List.length map_ts - 1 then ()
      else add idx
    else add idx;
    (* add_fully bound *)
    if add_full then add @@ IntSet.of_list @@ create_range @@ List.length map_ts - 1 else ()

  (* generic routine to add left and right map individual bindings to a hash table *)
  (* used by both route and multi-index *)
  let insert_l_r_binds ?prune ?add_full p hash _ t_args lmap lbinds rbinds =
    let insert_idx = insert_idx ?prune ?add_full in
    insert_idx p hash t_args lmap lbinds;
    List.iter (fun (rmap, rbind) ->
        insert_idx p hash t_args rmap rbind)
      rbinds

  (* For multi-index *)
  (* We need access patterns for maps individually. We don't care about any combination *)
  let multi_idx_access_patterns p ast =
    let h = Hashtbl.create 40 in
    ignore(all_trig_arg_stmt_binds ~sys_init:false p @@ insert_l_r_binds ~prune:true p h);
    (* we can't use the above method for sys_init. we must extract from the ast *)
    let h2 = !sys_init_bindings p ast in
    (* prune out the full patterns, since they're automatic *)
    Hashtbl.iter (fun m pat -> hashtbl_replace h m @@ function
                   | None -> pat
                   | Some oldp -> IntSetSet.union oldp pat) h2;
    (* enumerate all patterns for all maps and number them *)
    let pats = snd @@
      Hashtbl.fold (fun map_id pat (i, acc) ->
        try ignore(IntSetSetMap.find pat acc);
            i, acc
        with Not_found ->
          i+1, IntSetSetMap.add pat i acc)
      h
      (1, IntSetSetMap.empty) in
    let h2 = Hashtbl.create 40 in
    (* create new hashtbl. map_id -> pat_num, set of sets (pattern) *)
    Hashtbl.iter (fun map_id idx ->
      let num = IntSetSetMap.find idx pats in
      Hashtbl.add h2 map_id (num, idx)) h;
    h2

  (* routine for route. gets
    a. individual map bindings
    b. all fully bound patterns
    c. patterns that occur when an rmap with 2 loop vars sends to an lmap *)
  let insert_route_binds p hash stmt t_args lmap lbinds rbinds =
    (* first, insert the individual map bound patterns *)
    insert_l_r_binds p ~prune:false ~add_full:true hash () t_args lmap lbinds rbinds;
    (* now, get bound patterns that occur on shuffle to lmap because of double loops *)
    match stmt_many_loop_vars p stmt with
    | Some lmap_loop_vars ->
        (* get the bound vars on the lmap *)
        let lmap_bound_vars = get_idx t_args lbinds in
        (* for each loop var, insert the bound vars *)
        IntSet.iter (fun i ->
            let s = IntSet.add i lmap_bound_vars in
            hash_add_intset hash lmap s)
          lmap_loop_vars
    | None -> ()

  (* for route optimization, we need individual map patterns per-map,
    and we need patterns from lmap-rmap, since those can cause free (conservative)
    variables.
     We sort based on set size and number each set
    - We also use this to know for partitioning optimization which maps can be
      spread out around the cluster (the ones without slices) *)
  let route_access_patterns ?(sys_init=true) p =
    let h = Hashtbl.create 40 in
    ignore(all_trig_arg_stmt_binds ~sys_init p @@ insert_route_binds p h);
    (* create maps from intset to int. sort the sets by decreasing size *)
    let h2 = Hashtbl.create 40 in
    Hashtbl.iter (fun map idx_set ->
      let l = IntSetSet.elements idx_set in
      (* deliberately use opposite function for decreasing order *)
      let l = List.sort (fun s1 s2 ->
          if IntSet.cardinal s1 > IntSet.cardinal s2 then (-1) else 1) l in
      let m = IntSetMap.of_list @@ insert_index_snd l in
      Hashtbl.replace h2 map m) h;
    h2

end

(* use pre-generated info for determining if we can use special routing *)
let is_opt_route_stmt c s =
  if c.use_opt_route then
    let info = IntMap.find s c.freevar_info in
    P.is_opt_route_stmt ~info c.p s
  else false

let opt_route_stmts c = List.filter (is_opt_route_stmt c) @@ P.get_stmt_list c.p

(* check if a particular stmt/map combo can have optimized routing *)
let is_opt_route_stmt_map c s m =
  is_opt_route_stmt c s &&
  (* we don't do optimized routing for maps without keys *)
  List.mem m @@ P.nonempty_rmaps_of_stmt c.p s

(* list the bound arguments within the statement in trig arg order *)
let bound_params_of_stmt c s =
  let info = IntMap.find s c.freevar_info in
  let lmap = fst info.lmap_bound in
  let lbound = List.map (fun (id, n) -> id, (lmap, n)) @@ snd info.lmap_bound in
  let rbound = List.flatten @@ List.map (fun (rmap, l) -> List.map (fun (id, n) -> id, (rmap, n)) l) @@
    info.rmaps_bound in
  let bound = lbound @ rbound in
  (* rearrange the bound args in trig order *)
  let trig_args = fst_many @@ P.args_of_t c.p @@ P.trigger_of_stmt c.p s in
  List.fold_right (fun arg acc ->
      try
        (List.find ((=) arg |- fst) bound)::acc
      with Not_found -> acc)
    trig_args
    []

(* inner member of isobatch_vid_map *)
let isobatch_map_inner2 = create_ds "inner2" @@ wrap_tbag t_vid

(* stmt -> bool *)
let isobatch_stmt_helper_bitmap_id = "isobatch_stmt_helper_bitmap"
let isobatch_stmt_helper_bitmap c = create_ds isobatch_stmt_helper_bitmap_id t_bitset

let isobatch_stmt_helper_has_content =
  create_ds "isobatch_stmt_helper_has_content" @@ mut t_bool

(* singleton vector for creating isobatch_vid_map easily *)
(* indexed by stmt_id *)
let isobatch_stmt_helper_id = "isobatch_stmt_helper"
let isobatch_stmt_helper_e = [isobatch_map_inner2.id, isobatch_map_inner2.t]
let isobatch_stmt_helper c =
  let e = isobatch_stmt_helper_e in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_empty isobatch_map_inner2.t) @@
    k3_container_of_list (t_int_vector) @@
    List.map (const @@ mk_cint 0) @@ P.get_stmt_list c.p in
  create_ds ~init ~e isobatch_stmt_helper_id @@ wrap_tvector @@ t_of_e e

(* batch_id -> vids *)
let isobatch_map_inner =
  let e = ["batch_id", t_vid; isobatch_map_inner2.id, isobatch_map_inner2.t] in
  create_ds ~e "inner" @@ wrap_tmap @@ t_of_e e

(* Keep track of the vids in an isobatch *)
(* stmt -> batch_id -> [vids] *)
let isobatch_vid_map_id = "isobatch_vid_map"
let isobatch_vid_map_e = [isobatch_map_inner.id, isobatch_map_inner.t]
let isobatch_vid_map c =
  (* indexed by stmt_id *)
  let e = isobatch_vid_map_e in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_empty isobatch_map_inner.t) @@
    k3_container_of_list (t_int_vector) @@
    List.map (const @@ mk_cint 0) @@ P.get_stmt_list c.p in
  create_ds ~init ~e isobatch_vid_map_id @@ wrap_tvector @@ t_of_e e

(* function to move batch,vids,stmts from the helper to the real ds *)
let move_isobatch_stmt_helper_nm = "move_isobatch_stmt_helper"
let move_isobatch_stmt_helper =
  mk_global_fn move_isobatch_stmt_helper_nm ["batch_id", t_vid] [] @@
  mk_block [
    (* iterate over the isobatch map helper, and move its contents to the isobatch_map *)
    mk_iter_bitmap' ~idx:stmt_ctr.id
      (mk_let ["x"] (mk_delete_at isobatch_stmt_helper_id @@ mk_var stmt_ctr.id) @@
        mk_update_at_with isobatch_vid_map_id (mk_var stmt_ctr.id) @@
          mk_lambda' isobatch_vid_map_e @@
            mk_insert_block "inner" [mk_var "batch_id"; mk_var "x"])
      isobatch_stmt_helper_bitmap_id;
    mk_assign isobatch_stmt_helper_has_content.id mk_cfalse;
  ]

(* singleton vector for creating isobatch_buffered_fetch quickly *)
(* stmt_map_id -> [vids] *)
let isobatch_buffered_fetch_helper_id = "isobatch_buffered_fetch_helper"
let isobatch_buffered_fetch_helper_e = [isobatch_map_inner2.id, isobatch_map_inner2.t]
let isobatch_buffered_fetch_helper c =
  let e = isobatch_buffered_fetch_helper_e in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_empty isobatch_map_inner2.t) @@
    k3_container_of_list t_int_vector @@
    List.map mk_cint @@ fst_many @@ P.stmt_map_ids c.p in
  create_ds ~init ~e isobatch_buffered_fetch_helper_id @@ wrap_tvector @@ t_of_e e

(* for saving the vids of buffered fetches *)
(* stmt_map_id -> batch -> [vids] *)
let isobatch_buffered_fetch_vid_map_id = "isobatch_buffered_fetch_vid_map"
let isobatch_buffered_fetch_vid_map_e = isobatch_vid_map_e
let isobatch_buffered_fetch_vid_map c =
  (* indexed by stmt_map_id *)
  let e = isobatch_buffered_fetch_vid_map_e in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_empty isobatch_map_inner.t) @@
    k3_container_of_list t_int_vector @@
    List.map mk_cint @@ fst_many @@ P.stmt_map_ids c.p in
  create_ds ~init ~e isobatch_buffered_fetch_vid_map_id @@ wrap_tvector @@ t_of_e e

(* whether we do isobatches *)
let isobatch_mode = create_ds ~init:(mk_ctrue) "isobatch_mode" @@ t_bool

let isobatch_threshold = create_ds ~init:(mk_cint 4) "isobatch_threshold" @@ t_int

(* tags for profiling and post-analysis *)
let do_profiling = create_ds ~init:mk_cfalse "do_profiling" @@ mut t_bool

let prof_tag_pre_send_fetch   = 0
let prof_tag_post_send_fetch  = 1
let prof_tag_rcv_fetch        = 2
let prof_tag_buffered_push    = 3
let prof_tag_push_done        = 4
let prof_tag_do_complete_done = 5
let prof_tag_corr_done        = 6
let prof_tag_corr_send        = 7
let prof_tag_push_cnts        = 8
let prof_tag_push_decr        = 9
let prof_tag_fetch_route      = 10
let prof_tag_send_put         = 11
let prof_tag_gc_start         = 12
let prof_tag_gc_done          = 13
let prof_tag_node_process     = 14

(* @t_s_id: trig or stmt id *)
type prof_event =
                      (* vid_nm, tag/stmt id *)
      | ProfLatency of string * string

                      (* vid_nm, num_empty, num_full *)
      | ProfMsgCounts of string * string * string

                      (* vid_nm, tag/stmt id, barrier_count *)
      | ProfPushBarrier of string * string * string

                      (* vid nm, tag/stmt id, route key, bucket, node idx *)
      | ProfFetchRoute of string * string * string * string * string

                      (* vid nm, tag/stmt id, dest node idx, barrier count *)
      | ProfSendPut of string * string * string * string

                      (* batch_id, ip,     poly *)
      | ProfSendPoly of string * string * string

                      (* batch_id, ip,      poly,    upoly *)
      | ProfSendUPoly of string * string * string * string

(* @cond_mod: modifier function for condition *)
let prof_property ?(cond_mod=id_fn) ?(flush=false) (tag:int) event =
  let p = match event with
    | ProfLatency(vid_nm, t_s_id) ->
      sp "MosaicPreEvent(lbl=[# mosaic], tl=[$ %d], ve=[$ %s], ce=[$ %s])" tag vid_nm t_s_id

    | ProfMsgCounts(vid_nm, num_empty, num_full) ->
      sp "MosaicMsgCounts(lbl=[# mosaic], tl=[$ %d], ve=[$ %s], ace=[$ %s], bce=[$ %s])" tag vid_nm num_empty num_full

    | ProfPushBarrier(vid_nm, t_s_id, barrier_count) ->
      sp "MosaicPushBarrier(lbl=[# mosaic], tl=[$ %d], ve=[$ %s], ace=[$ %s], bce=[$ %s])" tag vid_nm t_s_id barrier_count

    | ProfFetchRoute(vid_nm, t_s_id, key, bucket, ip) ->
      sp "MosaicFetchRoute(lbl=[# mosaic], tl=[$ %d], ve=[$ %s], ace=[$ %s], bce=[$ %s], cce=[$ %s], dce=[$ %s])" tag vid_nm t_s_id key bucket ip

    | ProfSendPut(vid_nm, t_s_id, dest, barrier_count) ->
      sp "MosaicSendPut(lbl=[# mosaic], tl=[$ %d], ve=[$ %s], ace=[$ %s], bce=[$ %s], cce=[$ %s])" tag vid_nm t_s_id dest barrier_count

    | ProfSendPoly(vid_nm, ip, poly) ->
      sp "MosaicSendPoly(lbl=[# mosaic], ace=[$ %s], bce=[$ %s], cce=[$ %s])" vid_nm ip poly

    | ProfSendUPoly(vid_nm, ip, poly, upoly) ->
      sp "MosaicSendUPoly(lbl=[# mosaic], ace=[$ %s], bce=[$ %s], cce=[$ %s], dce=[$ %s])" vid_nm ip poly upoly
  in
  let target_expr =
    if flush then
      mk_tuple ~force:true [U.add_property "Flush" mk_cunit]
    else mk_cunit in
  mk_if (cond_mod @@ mk_var do_profiling.id)
    (U.add_annotation p target_expr)
    mk_cunit

(* calling all functions for profiling *)
let profile_funcs_start =
  mk_block [
    mk_apply' "vmapStart" [G.me_var];
    U.add_annotation "IfMachineMaster" @@ mk_apply' "jemallocStart" [];
    U.add_annotation "IfMachineMaster" @@ mk_apply' "jemallocTotalSizeStart" [];
  ]

let profile_funcs_stop =
  mk_block [
    mk_apply' "vmapStop" [];
    U.add_annotation "IfMachineMaster" @@ mk_apply' "jemallocStop" [];
    U.add_annotation "IfMachineMaster" @@ mk_apply' "jemallocTotalSizeStop" [];
    prof_property ~flush:true (-1) @@ ProfLatency("-1", "-1");
    prof_property ~flush:true (-1) @@ ProfMsgCounts("-1", "-1", "-1");
    prof_property ~flush:true (-1) @@ ProfPushBarrier("-1", "-1", "-1");
    prof_property ~flush:true (-1) @@ ProfFetchRoute("-1", "-1", "-1", "-1", "-1");
    prof_property ~flush:true (-1) @@ ProfSendPut("-1", "-1", "-1", "-1");
    prof_property ~flush:true (-1) @@ ProfSendPoly("-1", "me", "empty_poly_queue");
    prof_property ~flush:true (-1) @@ ProfSendUPoly("-1", "me", "empty_poly_queue", "empty_upoly_queue");
  ]

let prof_num_empty = create_ds "prof_num_empty" @@ mut t_int
let prof_num_full  = create_ds "prof_num_full" @@ mut t_int

(* print out statements about tags, vids etc *)
let do_tracing = create_ds "do_tracing" t_bool
(* for trace, use only 2 letters *)
let trace_trig t =
  str_take 2 @@
  str_drop (String.length "insert_") t

let if_trace print_e e =
  mk_block [
    mk_if (mk_var do_tracing.id) print_e mk_cunit;
    e
  ]

(* for debugging *)
let do_trace nm l expr =
  let elem (t, e) = match t.typ with
      | TInt    -> mk_apply' "string_of_int" [e]
      | TString -> e
      | TFloat  -> mk_apply' "string_of_float" [e]
      | _ -> failwith "unhandled type"
  in
  let rest, last = list_split (-1) l in
  if_trace
    (mk_print @@
     mk_concat (mk_cstring @@ nm^" ") @@
     List.fold_right
      (fun p acc ->
        let v  = elem p in
        let v' = mk_concat v @@ mk_cstring " " in
        mk_concat v' acc)
      rest @@
     elem @@ hd last)
    expr

(* debugging: only run protocol to some stage *)
let debug_run_sw_send_tuples = create_ds "debug_run_sw_send_tuples" t_int ~init:(mk_cint 1)
let debug_run_sw_send_all = create_ds "debug_run_sw_send_all" t_int ~init:(mk_cint 2)
let debug_run_normal = create_ds "debug_run_normal" t_int ~init:(mk_cint 255)
let debug_run = create_ds "debug_run" t_int ~init:(mk_var debug_run_normal.id)

(* debug_run: check if we only want certain message types *)
let debug_run_test_var ?(default=mk_cunit) var e  =
  mk_if (mk_leq (mk_var debug_run.id) @@ mk_var var.id)
    default
    e

(* index used to handle multiple switches for csv source *)
let sw_csv_index = create_ds "sw_csv_index" @@ t_int

let do_warmup = create_ds "do_warmup" @@ t_bool

let functions c =
  [ move_isobatch_stmt_helper; ]

let global_vars c dict =
  (* replace default inits with ones from ast *)
  let replace_init ds =
    try
      let init = some @@ IntMap.find (unwrap_some ds.map_id) dict in
      {ds with init}
    with Not_found -> ds
  in
  let l =
    [ my_local_peers;
      me_int;
      local_master;
      interpreter_mode;
      g_init_vid;
      g_max_int;
      g_min_vid;
      g_max_vid;
      g_start_vid;
      job_master;
      job_switch;
      job_local_master;
      job_node;
      job_timer;
      job;
      jobs;  (* inserted dynamically by interpreter *)
      master_addr;
      timer_addr;
      nodes;
      local_masters;
      switches;
      num_peers;
      peer_master_map;
      num_switches;
      num_nodes;
      map_ids c;
      nd_stmt_cntr_size;
      nd_stmt_cntrs c;
      nd_corr_map;
      nd_log_master;
      sw_init;
      sw_seen_sentinel;
      sw_event_queue;
      ms_start_time;
      ms_end_time;
      nd_num_stashed;
      sw_event_driver_sleep;
      (* for no-corrective mode *)
      corrective_mode;
      (* poly queue stuff *)
      sw_poly_batch_size;
      do_poly_reserve;
      empty_poly_queue;
      empty_upoly_queue;
      empty_event_queue;
      poly_queues;
      poly_queue_bitmap;
      upoly_queues;
      upoly_queue_bitmap;
      sw_poly_queues;
      sw_poly_queue_bitmap;
      send_trig_header_bitmap;
      send_trig_args_map;
      send_trig_args_bitmap;
      nd_fetch_buffer c;
      nd_stmt_cntrs_per_map c;
      nd_lmap_of_stmt_id c;
      do_profiling;
      do_tracing;
      use_unique_poly;
      prof_num_empty;
      prof_num_full;
      sw_csv_index;
      isobatch_mode;
      isobatch_threshold;
      isobatch_stmt_helper_bitmap c;
      isobatch_stmt_helper c;
      isobatch_stmt_helper_has_content;
      isobatch_vid_map c; (* stmt, batch -> vids *)
      isobatch_buffered_fetch_helper c;
      isobatch_buffered_fetch_vid_map c;
      do_warmup;
      debug_run_sw_send_tuples;
      debug_run_sw_send_all;
      debug_run_normal;
      debug_run;
      nd_trig_arg_notifications;
    ] @
    lm_log_ds c @
    (* buffers for efficient trig arg saving *)
    lm_log_buffer_ds c @
    (* indirections for nds to hold the trig args for a batch *)
    nd_log_buffer_ind_ds c @
    nd_log_buffer_bitmap ::
    (* combine generic map inits with ones from the ast *)
    (List.map replace_init @@ maps c) @
    (List.map replace_init @@ map_buffers c)
  in
  decl_global my_peers ::
  [ int_of_addr;
    addr_of_int;
  ] @
  List.map decl_global l @
  [local_master_of_peer]

