(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
open K3.AST
module U = K3Util
module G = K3Global
module P = ProgInfo

module IdMap = Map.Make(struct type t = id_t let compare = String.compare end)

type config = {
  p : P.prog_data_t;
  (* a mapping from K3 map ids to index sets we build up as we slice *)
  map_idxs : IndexSet.t IntMap.t;
  (* a mapping from new map names to index sets we build up as we slice *)
  mapn_idxs : IndexSet.t StrMap.t;
  use_multiindex : bool;
  force_correctives : bool;
  enable_gc : bool;
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
let init_vid =
                      (* epoch, counter=0, node hash *)
  let init = some @@ mk_tuple [mk_cint 0; mk_cint 0; mk_apply (mk_var "hash_addr") G.me_var] in
  {id="init_vid"; t=t_vid; e=[]; init}

(* trigger argument manipulation convenience functions *)
let arg_types_of_t c trig_nm = snd_many @: P.args_of_t c.p trig_nm
let arg_names_of_t c trig_nm = fst_many @: P.args_of_t c.p trig_nm
let args_of_t_as_vars c trig_nm = ids_to_vars (arg_names_of_t c trig_nm)

let args_of_t_with_v ?(vid="vid") c trig_nm = (vid, t_vid)::P.args_of_t c.p trig_nm
let arg_types_of_t_with_v c trig_nm = t_vid::arg_types_of_t c trig_nm
let args_of_t_as_vars_with_v ?(vid="vid") c trig_nm =
  mk_var vid::args_of_t_as_vars c trig_nm


(* vid comparison names and code *)
let v_op op l r = mk_apply (mk_var op) @: mk_tuple [l;r]
let vid_eq = "vid_eq"
let v_eq = v_op vid_eq
let vid_neq = "vid_neq"
let v_neq = v_op vid_neq
let vid_lt = "vid_lt"
let v_lt = v_op vid_lt
let vid_gt = "vid_gt"
let v_gt = v_op vid_gt
let vid_geq = "vid_geq"
let v_geq = v_op vid_geq
let vid_leq = "vid_leq"
let v_leq = v_op vid_leq

(* global variable moved from GenDist.ml *)

(* log, buffer names *)
let log_master_write_nm = "log_master_write"
let log_write_for p trig_nm = "log_write_"^trig_nm (* varies with bound *)
let log_get_bound_for _ trig_nm = "log_get_bound_"^trig_nm
let log_read_geq = "log_read_geq" (* takes vid, returns (trig, vid)list >= vid *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen.
 * Also used for adding to map buffers and for correctives *)
let add_delta_to_map c map_id =
  let m_t = map_types_for c.p map_id in
  "add_delta_to_"^String.concat "_" @:
    List.map K3PrintSyntax.string_of_value_type m_t

(* foreign functions *)
let hash_addr = "hash_addr"
let error_nm = "error"
let declare_foreign_functions _ =
  let foreign_hash_addr = mk_foreign_fn hash_addr t_addr t_int in
  let foreign_error_fn  = mk_foreign_fn error_nm t_unit t_unknown in
  (* function needed to parse sql dates. Called by m3tok3 *)
  let sql_func = mk_foreign_fn "parse_sql_date" t_string t_int in
  foreign_hash_addr::
  foreign_error_fn::
  sql_func::
  []

(* global data structures
 * ---------------------------------------------- *)

(* determine the master switch by lowest address *)
let master_addr =
  let jobs = G.jobs [] in
  let init = some @@ mk_agg_fst
              (mk_assoc_lambda' ["min_addr", t_addr] jobs.e @@
                mk_if (mk_and (mk_lt (mk_var "addr") @@ mk_var "min_addr") @@
                              (mk_eq (mk_var "job") @@ mk_cint G.job_switch))
                  (mk_var "addr") @@
                  mk_var "min_addr") @@
              mk_var jobs.id
  in
  {id="master_addr"; t=mut t_addr; init; e=[]}

let is_master =
  let init = some @@ mk_eq (mk_var master_addr.id) G.me_var in
  {id="is_master"; t=mut t_bool; e=[]; init}

(* epoch *)
let epoch_counter = {id="epoch_counter"; t=mut t_int; e=[]; init=some @@ mk_cint 0}

let num_peers =
  let init =
    some @@ mk_agg
      (mk_lambda (wrap_args ["acc", t_int; "_", t_addr]) @@
        mk_add (mk_var "acc") @@ mk_cint 1)
      (mk_cint 0) @@
      mk_var (G.peers []).id
  in
  {id="num_peers"; t=mut t_int; e=[]; init}

(* --- Protocol Init code --- *)

let ms_init_counter = {id="ms_init_counter"; t=mut t_int; e=[]; init=some @@ mk_cint 0}
(* whether we can begin operations on this node/switch *)
let init_flag = {id="init_flag"; t=mut t_bool; e=[]; init = some @@ mk_cfalse}

let ms_rcv_init_trig_nm = "ms_init_trig"
let rcv_init_trig_nm = "init_trig"

(* code for all nodes+switches to check in before starting *)
let send_init_to_master =
  let init = some @@ mk_send ms_rcv_init_trig_nm (mk_var master_addr.id) G.me_var in
  {id="send_init_to_master"; t=t_unit; e=[]; init}

(* code for master to verify that all peers have answered and to begin *)
let ms_rcv_init_trig =
  mk_code_sink' ms_rcv_init_trig_nm ["_", t_addr] [] @@
  mk_block [
    (* increment counter *)
    mk_assign ms_init_counter.id @@
      mk_add (mk_var ms_init_counter.id) (mk_cint 1);
    (* check if >= to num_peers *)
    mk_if (mk_geq (mk_var ms_init_counter.id) @@ mk_var num_peers.id)
      (* send to all peers *)
      (mk_iter
        (mk_lambda (wrap_args ["peer", t_addr]) @@
          mk_send rcv_init_trig_nm (mk_var "peer") mk_cunit) @@
        mk_var "peers")
      mk_cunit
  ]

(* code for nodes/switches to set init to true *)
let rcv_ms_init_trig =
  mk_code_sink' rcv_init_trig_nm ["_", t_unit] [] @@
  mk_assign init_flag.id (mk_cbool true)

(* --- End of init code --- *)

(* global containing mapping of map_id to map_name and dimensionality *)
let map_ids_id = "map_ids"
let map_ids c =
  let e = ["map_idx", t_int; "map_name", t_string; "map_dim", t_int] in
  let t = wrap_tbag' @@ snd_many e in
  let init =
    some @@ k3_container_of_list t @@
      P.for_all_maps c.p @@ fun i ->
        mk_tuple [mk_cint i;
                  mk_cstring @@ P.map_name_of c.p i;
                  mk_cint @@ List.length @@ P.map_types_for c.p i] in
  {id=map_ids_id; e; t; init}

(* stmt_cntrs - (vid, stmt_id, counter) *)
(* NOTE: change to mmap with index on vid, stmt *)
let nd_stmt_cntrs =
  let e = ["vid", t_vid; "stmt_id", t_int; "counter", t_int] in
  {id="nd_stmt_cntrs"; e; t=wrap_tbag' @@ snd_many e; init=None}

(* master log *)
let nd_log_master =
  let e = ["vid", t_vid; "trig_id", t_trig_id; "stmt_id", t_stmt_id] in
  {id="nd_log_master"; e; t=wrap_tbag' @@ snd_many e; init=None}

(* names for log *)
let log_for_t t = "nd_log_"^t

(* log data structures *)
let log_ds c : data_struct list =
  let log_struct_for trig =
    let e = args_of_t_with_v c trig in
    {id=log_for_t trig; e; t=wrap_tbag' @@ snd_many e; init=None}
  in
  P.for_all_trigs c.p log_struct_for

(* create a map structure: used for both maps and buffers *)
let make_map_decl c map_name map_id =
  let e = P.map_ids_types_with_v_for c.p map_id in
  let t' = wrap_t_map_idx' c map_id @@ snd_many e in
  (* for indirections, we need to create initial values *)
  let init = some @@ mk_ind @@ mk_empty t' in
  ({id=map_name; e; init; t=wrap_tind t'}, map_id)

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

(* buffers for insert/delete -- we need a per-trigger list *)
let sw_trig_buf_prefix = "sw_buf_"
let sw_trig_bufs (c:config) =
  P.for_all_trigs c.p @@ fun t ->
  {id=sw_trig_buf_prefix^t; t=wrap_tlist' @@ snd_many @@ P.args_of_t c.p t; e=[]; init=None}

(* list for next message -- contains trigger id *)
let sw_trig_buf_idx =
  let e = ["trig_id", t_int] in
  {id="sw_trig_buf_idx"; e; t=wrap_tlist' @@ snd_many e; init=None}

(* counter for msgs to send *)
let sw_msgs_to_send_ctr =
  {id="sw_msgs_to_send_ctr"; e=[]; t=mut t_int; init=some @@ mk_cint 0}

(* --- Begin frontier function code --- *)

(* the function name for the frontier function *)
(* takes the types of the map *)
(* NOTE: assumes the first type is vid *)
let frontier_name c map_id =
  let m_t = P.map_types_for c.p map_id in
  "frontier_"^String.concat "_" @:
    List.map K3PrintSyntax.string_of_value_type m_t

let get_idx idx map_id =
  try IntMap.find map_id idx
  with Not_found -> failwith @@ "Failed to find index for map "^soi map_id

(* Get the latest vals up to a certain vid
 * - This is needed both for sending a push, and for modifying a local slice
 * operation
 * - slice_col is the k3 expression representing the collection.
 * - assumes a local 'vid' variable containing the border of the frontier
 * - pat assumes NO VID
 * - keep_vid indicates whether we need to remove the vid from the result collection
 *   (we usually need it removed only for modifying ast *)
let map_latest_vid_vals c slice_col m_pat map_id ~keep_vid : expr_t =
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" c.p map_id in
  (* function to remove the vid from the collection *)
  let remove_vid_if_needed = if keep_vid then id_fn else
      mk_map (mk_lambda (wrap_args m_id_t_v) @:
        mk_tuple @: list_drop 1 @: ids_to_vars @: fst_many m_id_t_v)
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
      (mk_slice_idx' ~idx ~comp:LT slice_col @@ mk_var "vid"::pat)

  else (* no multiindex *)
    (* create a function name per type signature *)
    let access_k3 =
      (* slice with an unknown for the vid so we only get the effect of
      * any bound variables *)
      mk_slice slice_col @:
        mk_tuple @: P.map_add_v mk_cunknown pat
    in
    let simple_app =
      mk_apply (mk_var @: frontier_name c map_id) @:
        mk_tuple [mk_var "vid"; access_k3]
    in
    if keep_vid then simple_app else remove_vid_if_needed simple_app

(* Create a function for getting the latest vals up to a certain vid *)
(* This is needed both for sending a push, and for modifying a local slice
 * operation *)
(* Returns the type pattern for the function, and the function itself *)
(* NOTE: this is only necessary when not using multiindexes *)
let frontier_fn c map_id =
  let max_vid = "max_vid" in
  let map_vid = "map_vid" in
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" c.p map_id in
  let m_t_v = wrap_ttuple @: snd_many @: m_id_t_v in
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
        (v_lt (mk_var map_vid) (mk_var "vid"))
        (* if the map vid is equal to the max_vid, we add add it to our
        * accumulator and use the same max_vid *)
        (mk_if
          (v_eq (mk_var map_vid) (mk_var max_vid))
          (mk_tuple
            [mk_combine
              (mk_singleton m_t_v_bag (mk_tuple @: ids_to_vars @: fst_many m_id_t_v)) @:
                  mk_var "acc";
            mk_var max_vid])
          (* else if map vid is greater than max_vid, make a new
          * collection and set a new max_vid *)
          (mk_if
            (v_gt (mk_var map_vid) (mk_var max_vid))
            (mk_tuple
              [mk_singleton m_t_v_bag (mk_tuple @: ids_to_vars @: fst_many m_id_t_v);
              mk_var map_vid])
            (* else keep the same accumulator and max_vid *)
            (mk_tuple [mk_var "acc"; mk_var max_vid])
          )
        )
        (* else keep the same acc and max_vid *)
        (mk_tuple [mk_var "acc"; mk_var max_vid])
      )
  in
  (* a regular fold is enough if we have no keys *)
  (* get the maximum vid that's less than our current vid *)
  let action = match m_id_t_no_val with
  | [] ->
    mk_fst [m_t_v_bag; t_vid] @:
      mk_agg
        common_vid_lambda
        (mk_tuple [mk_empty m_t_v_bag; min_vid_k3])
        (mk_var "input_map")
  | _ ->
      mk_flatten @: mk_map
        (mk_assoc_lambda
          (wrap_args ["_", wrap_ttuple @: snd_many m_id_t_no_val]) (* group *)
          (wrap_args ["project", m_t_v_bag; "_", t_vid]) @:
          mk_var "project"
        ) @:
        mk_gbagg
          (mk_lambda (wrap_args m_id_t_v) @:
            (* group by the keys (not vid, not values) *)
            mk_tuple @: ids_to_vars @: fst_many m_id_t_no_val)
          (* get the maximum vid that's less than our current vid *)
          common_vid_lambda
          (mk_tuple [mk_empty m_t_v_bag; min_vid_k3])
          (mk_var "input_map")
  in
  mk_global_fn (frontier_name c map_id) ["vid", t_vid; "input_map", m_t_v_bag] [m_t_v_bag] @:
      action

(* End of frontier function code *)
