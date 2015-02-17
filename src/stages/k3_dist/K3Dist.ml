(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
open K3.AST
module U = K3Util
module G = K3Global
module P = ProgInfo

module IdMap = Map.Make(struct type t = id_t let compare = String.compare end)

type shuffle_fn_entry = {
  stmts : IntSet.t;
  rmap : map_id_t;
  lmap : map_id_t;
  binding : IntIntSet.t; (* rmap idx, lmap idx *)
  name : string;
}

type config = {
  p : P.prog_data_t;
  (* a mapping from K3 map ids to index sets we build up as we slice *)
  map_idxs : IndexSet.t IntMap.t;
  (* a mapping from new map names to index sets we build up as we slice *)
  mapn_idxs : IndexSet.t StrMap.t;
  shuffle_meta : shuffle_fn_entry list;
  use_multiindex : bool;
  enable_gc : bool;
  (* a file to use as the stream to switches *)
  stream_file : string;
  gen_deletes : bool;
  gen_correctives : bool;
}

let string_of_map_idxs c map_idxs =
  let b = Buffer.create 100 in
  IntMap.iter (fun map_id idx ->
    Printf.bprintf b "map %s:\n" @@ P.map_name_of c.p map_id;
    IndexSet.iter (Printf.bprintf b "%s\n" |- K3Printing.string_of_index) idx
  ) map_idxs;
  Buffer.contents b

(* location of vid in tuples *)
let vid_idx = 0
let vid_shift = (+) 1
let add_vid_idx l = l @ [vid_idx]

(* generic function to turn a list of tuple elements into an index. Puts the vid last *)
(* NOTE: takes a list not including vid and value *)
let make_into_index l =
  let l' = List.map vid_shift @@ fst_many @@ insert_index_fst l in
  OrdIdx(add_vid_idx l', IntSet.of_list l')

(*
(* add an index to the config structure and update it *)
let add_index id (idx_set_kind_l:(IntSet.t * index_kind) list) (c:config) =
  let cur_idx = try IdMap.find id c.map_idxs with Not_found -> [] in
  let rec loop cur_idx' = function
    | [] -> cur_idx'
    | (idx_set, kind)::tail ->
        (* look for a matching index column set and add to it *)
        let found, idx_src' = mapfold (fun found idx ->
          if IntSet.equal idx.mm_indices idx_set then
            (* iterate with sub-indices *)
            let mm_submaps = loop idx.mm_submaps tail in
            (* change to ordered type if requested *)
            true, match kind with
              | Ordered -> {idx with mm_submaps; mm_idx_kind=Ordered}
              | _       -> {idx with mm_submaps}

          else found, idx)
          false cur_idx'
        in
        if found then cur_idx'
        else (* if not found, add the index *)
          { mm_indices = idx_set;
            mm_comp_fn = None;
            mm_idx_kind = kind;
            mm_submaps = loop [] tail; (* empty current index *)
          }::idx_src'
  in
  let idx_l' = loop cur_idx idx_set_kind_l in
  c.map_idxs <- IdMap.add id idx_l' c.map_idxs

(* add an index from a k3 pattern *)
let add_index_pat id pat_kind_l c =
  let idx_set_of_pat pat =
    insert_index_fst 0 pat
    |> List.filter (fun (_,x) ->
        match U.tag_of_expr x with Const(CUnknown) -> false | _ -> true)
    |> fst_many |> intset_of_list
  in
  let idx_kind_l = List.map (first idx_set_of_pat) pat_kind_l in
  add_index id idx_kind_l c
*)

let t_vid_list = wrap_tlist t_vid

(* what the generic type of the global maps is *)
let wrap_t_of_map = wrap_tbag
let wrap_t_of_map' = wrap_tbag'

(* wrap with the index type of the map, if requested *)
let wrap_t_map_idx c map_id =
  if c.use_multiindex then
    try
      let idxs = IntMap.find map_id c.map_idxs in
      wrap_tmmap idxs
    with Not_found ->
      prerr_string @@ "Map_idxs:\n"^string_of_map_idxs c c.map_idxs;
      failwith @@ "Failed to find map index for map id "^P.map_name_of c.p map_id
  else
    wrap_tbag

let wrap_t_map_idx' c map_id = wrap_t_map_idx c map_id |- wrap_ttuple

(* global declaration of default vid to put into every map *)
let g_init_vid =
                      (* epoch=0, counter=0 *)
  let init = mk_tuple [mk_cint 0; mk_cint 0] in
  create_ds "g_init_vid" t_vid ~init

(* trigger argument manipulation convenience functions *)
let arg_types_of_t c trig_nm = snd_many @@ P.args_of_t c.p trig_nm
let arg_names_of_t c trig_nm = fst_many @@ P.args_of_t c.p trig_nm
let args_of_t_as_vars c trig_nm = ids_to_vars (arg_names_of_t c trig_nm)

let args_of_t_with_v ?(vid="vid") c trig_nm = (vid, t_vid)::P.args_of_t c.p trig_nm
let arg_types_of_t_with_v c trig_nm = t_vid::arg_types_of_t c trig_nm
let args_of_t_as_vars_with_v ?(vid="vid") c trig_nm =
  mk_var vid::args_of_t_as_vars c trig_nm

(**** global data structures ****)

let num_peers =
  let init = mk_size_slow G.peers in
  create_ds "num_peers" (mut t_int) ~init

let g_min_vid = create_ds "g_min_vid" t_vid ~init:min_vid_k3
let g_max_vid =
  let init =
    let max = mk_apply' "get_max_int" mk_cunit in
    mk_tuple [max; max] in
  create_ds "g_max_vid" t_vid ~init

(* specifies the job of a node: master/switch/node *)
let job_master_v = 0
let job_switch_v = 1
let job_node_v   = 2
let job_timer_v  = 3
let job_none_v   = 4

let job_master = create_ds "job_master" t_int ~init:(mk_cint job_master_v)
let job_switch = create_ds "job_switch" t_int ~init:(mk_cint job_switch_v)
let job_node   = create_ds "job_node" t_int ~init:(mk_cint job_node_v)
let job_timer  = create_ds "job_timer" t_int ~init:(mk_cint job_timer_v)

let job =
  let init =
    mk_if (mk_eq (mk_var G.role.id) @@ mk_cstring "master") (mk_var job_master.id) @@
    mk_if (mk_eq (mk_var G.role.id) @@ mk_cstring "switch") (mk_var job_switch.id) @@
    mk_if (mk_eq (mk_var G.role.id) @@ mk_cstring "node")   (mk_var job_node.id)   @@
    mk_if (mk_eq (mk_var G.role.id) @@ mk_cstring "timer")  (mk_var job_timer.id)  @@
    mk_error "failed to find proper role"
  in
  create_ds "job" (mut t_int) ~init

let job_of_str = function
  | "master" -> job_master_v
  | "switch" -> job_switch_v
  | "node"   -> job_node_v
  | "timer"  -> job_timer_v
  | _        -> job_none_v

(* this must be created for the specific run *)
(* we fill it dynamically in the interpreter *)
let jobs =
  let e = ["addr", t_addr; "job", t_int] in
  let t = mut @@ wrap_tmap' @@ snd_many e in
  create_ds "jobs" t ~e

(* address of master node *)
let master_addr = create_ds "master_addr" (mut t_addr)

(* address of timer peer *)
let timer_addr =
  let d_init =
    mk_case_sn
      (mk_peek @@ mk_filter
        (mk_lambda' jobs.e @@
          mk_eq (mk_var "job") @@ mk_var job_timer.id) @@
        mk_var jobs.id)
      "timer"
      (mk_fst @@ mk_var "timer") @@
      mk_error "no timer peer found" in
  create_ds "timer_addr" (mut t_addr) ~d_init

let nodes =
  let d_init =
    mk_fst_many (snd_many jobs.e) @@
      mk_filter
        (mk_lambda' jobs.e @@ mk_eq (mk_var job.id) @@ mk_var job_node.id) @@
        mk_var jobs.id in
  let e = ["addr", t_addr] in
  create_ds "nodes" (mut @@ wrap_tbag' @@ snd_many e) ~e ~d_init

let switches =
  let d_init =
    mk_fst_many (snd_many jobs.e) @@
      mk_filter
        (mk_lambda' jobs.e @@ mk_eq (mk_var job.id) @@ mk_var job_switch.id) @@
        mk_var jobs.id in
  let e = ["addr", t_addr] in
  create_ds "switches" (mut @@ wrap_tbag' @@ snd_many e) ~e ~d_init

let num_switches =
  let d_init = mk_size_slow switches in
  create_ds "num_switches" (mut t_int) ~d_init

let num_nodes =
  let d_init = mk_size_slow nodes in
  create_ds "num_nodes" (mut t_int) ~d_init

let sw_driver_trig_nm = "sw_driver_trig"

(* timing data structures *)
let ms_start_time = create_ds "ms_start_time" @@ mut t_int
let ms_end_time = create_ds "ms_end_time" @@ mut t_int

(**** Protocol Init code ****)

let ms_init_counter = create_ds "ms_init_counter" (mut t_int) ~init:(mk_cint 0)
(* whether we can begin operations on this node/switch *)
let init_flag = create_ds "init_flag" (mut t_bool) ~init:(mk_cfalse)

let ms_rcv_init_trig_nm = "ms_init_trig"
let rcv_init_trig_nm = "init_trig"

(* code for all nodes+switches to check in before starting *)
let send_init_to_master =
  let init = mk_send ms_rcv_init_trig_nm (mk_var master_addr.id) [G.me_var] in
  create_ds "send_init_to_master" t_unit ~init

(* code for master to verify that all peers have answered and to begin *)
let ms_rcv_init_trig =
  mk_code_sink' ms_rcv_init_trig_nm ["_", t_addr] [] @@
  mk_block [
    (* increment init counter *)
    mk_assign ms_init_counter.id @@
      mk_add (mk_var ms_init_counter.id) (mk_cint 1);
    (* check if >= to num_peers *)
    mk_if (mk_geq (mk_var ms_init_counter.id) @@ mk_var num_peers.id)
      (* send rcv_init to all peers *)
      (mk_iter
        (mk_lambda (wrap_args ["peer", t_addr]) @@
          mk_send rcv_init_trig_nm (mk_var "peer") [mk_cunit]) @@
        mk_var "peers")
      mk_cunit
  ]

(* code for nodes/switches to set init to true *)
let rcv_ms_init_trig =
  mk_code_sink' rcv_init_trig_nm ["_", t_unit] [] @@
  mk_assign init_flag.id (mk_cbool true)

(**** End of init code ****)

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

(* combine all the trig args into a minimal set *)
(* adds an int for trigger selector *)
let combine_trig_args c =
  let t_data = P.for_all_trigs ~deletes:c.gen_deletes c.p @@
    fun t -> P.trigger_id_for_name c.p t, "sw_"^t, P.args_of_t c.p t
  in
  let sort t = List.sort (fun x y -> fst x - fst y) t in
  let types, map, _ =
    List.fold_left (fun (types, map, sz) (tid, tnm, targs) -> 
      let rem_ts, used_ts, tmap, sz =
        (* fold over the args *)
        List.fold_left (fun (rem_ts, used_ts, tmap, sz) (_, t) ->
          (* look for a spare type *)
          let yes, no = List.partition ((=) t |- snd) rem_ts in
          match yes with
          (* no match found, so add a type *)
          | []    -> rem_ts, (sz, t)::used_ts, sz::tmap, sz+1
          (* found a match, so add it to our map *)
          | y::ys -> ys@no,  y::used_ts, (fst y)::tmap, sz
        ) (types, [], [], sz) targs
      in
      sort @@ rem_ts@used_ts, (tid, tnm, List.rev tmap)::map, sz
    ) ([], [], 1) t_data
  in
  t_int :: (snd_many @@ sort types), map

(* global containing mapping of trig id to trig_name *)
let trig_ids_id = "trig_ids"
let trig_ids c = 
  let inner_t = wrap_tlist t_int in
  let e = ["trig_id", t_int; "trig_name", t_string; "indices", inner_t] in
  let t = wrap_tbag' @@ snd_many e in
  let init =
    let _, map = combine_trig_args c in
    let ts = List.map (fun (id, nm, l) ->
      mk_tuple [mk_cint id; mk_cstring nm;
        k3_container_of_list inner_t @@ List.map mk_cint l]) map in
    k3_container_of_list t ts in
  create_ds trig_ids_id t ~e ~init

(* stmt_cntrs - (vid, stmt_id, counter) *)
(* TODO: change to mmap with index on vid, stmt *)
let nd_stmt_cntrs =
  let e = ["vid", t_vid; "stmt_id", t_int; "counter", t_int] in
  create_ds "nd_stmt_cntrs" (wrap_tbag' @@ snd_many e) ~e

(* master log *)
(* TODO: change to ordered DS, like ordered map *)
let nd_log_master =
  let e = ["vid", t_vid; "trig_id", t_trig_id; "stmt_id", t_stmt_id] in
  create_ds "nd_log_master" (wrap_tbag' @@ snd_many e) ~e

(* names for log *)
let nd_log_for_t t = "nd_log_"^t

(* log data structures *)
(* TODO: change to maps on vid *)
let log_ds c : data_struct list =
  let log_struct_for trig =
    let e = args_of_t_with_v c trig in
    create_ds (nd_log_for_t trig) (wrap_tbag' @@ snd_many e) ~e
  in
  P.for_all_trigs ~deletes:c.gen_deletes c.p log_struct_for

(* create a map structure: used for both maps and buffers *)
let make_map_decl c map_name map_id =
  let e = P.map_ids_types_with_v_for c.p map_id in
  let t' = wrap_t_map_idx' c map_id @@ snd_many e in
  (* for indirections, we need to create initial values *)
  let init = mk_ind @@ mk_empty t' in
  create_ds map_name (wrap_tind t') ~e ~init ~map_id

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

(* State of switch:
 * 0: idle
 * 1: sending
 * 2: waiting for vid *)
let sw_state_pre_init = create_ds "sw_state_pre_init" t_int ~init:(mk_cint 0)
let sw_state_idle     = create_ds "sw_state_idle"     t_int ~init:(mk_cint 1)
let sw_state_sending  = create_ds "sw_state_sending"  t_int ~init:(mk_cint 2)
let sw_state_wait_vid = create_ds "sw_state_wait_vid" t_int ~init:(mk_cint 3)
let sw_state_done     = create_ds "sw_state_done"     t_int ~init:(mk_cint 4)
let sw_state = create_ds "sw_state" (mut t_int) ~init:(mk_var sw_state_pre_init.id)

(* State of the node:
  * 0: normal
  * 1: done (look for being done)
*)
let nd_state_normal = create_ds "nd_state_normal" t_int ~init:(mk_cint 0)
let nd_state_done   = create_ds "nd_state_done"   t_int ~init:(mk_cint 1)
let nd_state = create_ds "nd_state" (mut t_int) ~init:(mk_var nd_state_normal.id)

(* buffers for insert/delete -- we need a per-trigger list *)
(* these buffers don't inlude a vid, unlike the logs in the nodes *)
let sw_trig_buf_prefix = "sw_buf_"
let sw_trig_bufs (c:config) =
  P.for_all_trigs ~deletes:c.gen_deletes c.p @@ fun t ->
    create_ds (sw_trig_buf_prefix^t) (wrap_tlist' @@ snd_many @@ P.args_of_t c.p t)

(* list for next message -- contains trigger id *)
let sw_trig_buf_idx =
  let e = ["trig_id", t_int] in
  create_ds "sw_trig_buf_idx" (wrap_tlist' @@ snd_many e) ~e

(* name for master send request trigger (part of GC) *)
let ms_send_gc_req_nm = "ms_send_gc_req"

let nd_add_delta_to_buf_nm c map_id =
  let t = P.map_types_for c.p map_id in
  "nd_add_delta_to_"^String.concat "_" @@
    List.map K3PrintSyntax.string_of_type t


(* --- Begin frontier function code --- *)

(* the function name for the frontier function *)
(* takes the types of the map *)
(* NOTE: assumes the first type is vid *)
let frontier_name c map_id =
  let m_t = P.map_types_for c.p map_id in
  "frontier_"^String.concat "_" @@
    List.map K3PrintSyntax.string_of_type m_t

let get_idx idx map_id =
  try IntMap.find map_id idx
  with Not_found -> failwith @@ "Failed to find index for map "^soi map_id

(* Get the latest vals up to a certain vid
 * - This is needed both for sending a push, and for modifying a local slice
 * operation, as well as for GC
 * - slice_col is the k3 expression representing the collection.
 * - assumes a local 'vid' variable containing the border of the frontier
 * - pat assumes NO VID
 * - keep_vid indicates whether we need to remove the vid from the result collection
 *   (we usually need it removed only for modifying ast *)
let map_latest_vid_vals ?(vid_nm="vid") c slice_col m_pat map_id ~keep_vid : expr_t =
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" c.p map_id in
  (* function to remove the vid from the collection *)
  let remove_vid_if_needed = if keep_vid then id_fn else
      mk_map (mk_lambda (wrap_args m_id_t_v) @@
        mk_tuple @@ list_drop 1 @@ ids_to_vars @@ fst_many m_id_t_v)
  in
  let pat = match m_pat with
    | Some pat -> pat
    | None     -> List.map (const mk_cunknown) @@ P.map_types_for c.p map_id
  in
  if c.use_multiindex then
    (* create the corresponding index for the pattern *)
    let idx  = fst_many @@
      List.filter (fun (_,x) -> U.tag_of_expr x <> Const(CUnknown)) @@
      insert_index_fst @@ P.map_add_v mk_cunknown pat
    in
    let idx' = add_vid_idx idx in (* vid is always matched last in the index *)
    let idx = OrdIdx(idx', IntSet.of_list idx) in
    remove_vid_if_needed @@
      (* filter out anything that doesn't have the same parameters *)
      (* the multimap layer implements extra eq key filtering *)
      (mk_slice_idx' ~idx ~comp:LT slice_col @@ mk_var vid_nm::pat)

  else (* no multiindex *)
    (* create a function name per type signature *)
    let access_k3 =
      (* slice with an unknown for the vid so we only get the effect of
      * any bound variables *)
      mk_slice slice_col @@ P.map_add_v mk_cunknown pat
    in
    let simple_app =
      mk_apply (mk_var @@ frontier_name c map_id) @@
        mk_tuple [mk_var vid_nm; access_k3]
    in
    if keep_vid then simple_app else remove_vid_if_needed simple_app

(* Create a function for getting the latest vals up to a certain vid *)
(* This is needed both for sending a push, and for modifying a local slice
 * operation, as well as for GC *)
(* Returns the type pattern for the function, and the function itself *)
(* NOTE: this is only necessary when not using multiindexes *)
let frontier_fn c map_id =
  let max_vid = "max_vid" in
  let map_vid = "map_vid" in
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" c.p map_id in
  let m_t_v = wrap_ttuple @@ snd_many @@ m_id_t_v in
  let m_t_v_bag = wrap_t_of_map m_t_v in
  let m_id_t_no_val = P.map_ids_types_no_val_for c.p map_id in
  (* create a function name per type signature *)
  (* if we have bound variables, we should slice first. Otherwise,
      we don't need a slice *)
  (* a routine common to both methods of slicing *)
  let common_vid_lambda =
    mk_assoc_lambda
      (wrap_args ["acc", m_t_v_bag; max_vid, t_vid])
      (wrap_args m_id_t_v)
      (mk_if
        (* if the map vid is less than current vid *)
        (mk_lt (mk_var map_vid) @@ mk_var "vid")
        (* if the map vid is equal to the max_vid, we add add it to our
        * accumulator and use the same max_vid *)
        (mk_if
          (mk_eq (mk_var map_vid) @@ mk_var max_vid)
          (mk_block [
            mk_insert "acc" @@ ids_to_vars @@ fst_many m_id_t_v;
            mk_tuple [mk_var "acc"; mk_var max_vid]
          ]) @@
          (* else if map vid is greater than max_vid, make a new
          * collection and set a new max_vid *)
          mk_if
            (mk_gt (mk_var map_vid) @@ mk_var max_vid)
            (mk_tuple
              [mk_singleton m_t_v_bag @@ ids_to_vars @@ fst_many m_id_t_v;
              mk_var map_vid]) @@
            (* else keep the same accumulator and max_vid *)
            mk_tuple [mk_var "acc"; mk_var max_vid])
        (* else keep the same acc and max_vid *)
        (mk_tuple [mk_var "acc"; mk_var max_vid]))
  in
  (* a regular fold is enough if we have no keys *)
  (* get the maximum vid that's less than our current vid *)
  let action = match m_id_t_no_val with
  | [] ->
    mk_fst @@
      mk_agg
        common_vid_lambda
        (mk_tuple [mk_empty m_t_v_bag; mk_var g_min_vid.id])
        (mk_var "input_map")
  | _ ->
      mk_flatten @@ mk_map
        (mk_assoc_lambda
          (wrap_args ["_", wrap_ttuple @@ snd_many m_id_t_no_val]) (* group *)
          (wrap_args ["project", m_t_v_bag; "_", t_vid]) @@
          mk_var "project"
        ) @@
        mk_gbagg
          (mk_lambda (wrap_args m_id_t_v) @@
            (* group by the keys (not vid, not values) *)
            mk_tuple @@ ids_to_vars @@ fst_many m_id_t_no_val)
          (* get the maximum vid that's less than our current vid *)
          common_vid_lambda
          (mk_tuple [mk_empty m_t_v_bag; mk_var g_min_vid.id])
          (mk_var "input_map")
  in
  mk_global_fn (frontier_name c map_id) ["vid", t_vid; "input_map", m_t_v_bag] [m_t_v_bag] @@
      action

(* End of frontier function code *)

(**** End of code ****)

let global_vars c dict =
  (* replace default inits with ones from ast *)
  let replace_init ds =
    try
      let init = some @@ IntMap.find (unwrap_some ds.map_id) dict in
      {ds with init}
    with Not_found -> ds
  in
  let l =
    [ g_init_vid;
      g_min_vid;
      g_max_vid;
      job_master;
      job_switch;
      job_node;
      job_timer;
      job;
      jobs;  (* inserted dynamically by interpreter *)
      master_addr;
      timer_addr;
      nodes;
      switches;
      num_peers;
      num_switches;
      num_nodes;
      map_ids c;
      trig_ids c;
      nd_stmt_cntrs;
      nd_log_master;
      nd_state_normal;
      nd_state_done;
      nd_state;
      sw_state_pre_init;
      sw_state_idle;
      sw_state_sending;
      sw_state_wait_vid;
      sw_state_done;
      sw_state;
      sw_trig_buf_idx;
      ms_start_time;
      ms_end_time;
    ] @
    sw_trig_bufs c @
    log_ds c @
    (* combine generic map inits with ones from the ast *)
    (List.map replace_init @@ maps c) @
    (List.map replace_init @@ map_buffers c)
  in
  List.map decl_global l

(* foreign functions *)
let declare_foreign_functions =
  [ mk_foreign_fn "hash_addr" t_addr t_int;
    mk_foreign_fn "hash_int" t_int t_int;
    mk_foreign_fn "hash_float" t_float t_int;
    mk_foreign_fn "hash_string" t_string t_int;
    mk_foreign_fn "hash_date" t_date t_int;
    mk_foreign_fn "parse_sql_date" t_string t_int;
    mk_foreign_fn "int_of_float" t_float t_int;
    mk_foreign_fn "float_of_int" t_int t_float;
    mk_foreign_fn "get_max_int" t_unit t_int;
    mk_foreign_fn "mod" (wrap_ttuple [t_int; t_int]) t_int;
  ]


