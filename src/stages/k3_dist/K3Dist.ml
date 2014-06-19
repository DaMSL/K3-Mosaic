(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
module G = K3Global
module P = ProgInfo

(* initial vid to put in initialization statements *)
let init_vid = "__init_vid__"

let map_ids = "__map_ids__"

(* what the generic type of the global maps is *)
let wrap_t_of_map = wrap_tbag

(* global declaration of default vid to put into every map *)
let init_vid_k3 =
  mk_global_val_init init_vid t_vid @:
    (* epoch, counter=0, node hash *)
    mk_tuple [mk_cint 0; mk_cint 0; mk_apply (mk_var "hash_addr") G.me_var]

let min_vid_k3 = mk_tuple [mk_cint 0; mk_cint 0; mk_cint 0]
let max_vid_k3 = mk_tuple [mk_cint max_int; mk_cint max_int; mk_cint max_int ]

(* trigger argument manipulation convenience functions *)
let arg_types_of_t p trig_nm = extract_arg_types (args_of_t p trig_nm)
let arg_names_of_t p trig_nm = extract_arg_names (args_of_t p trig_nm)
let args_of_t_as_vars p trig_nm = ids_to_vars (arg_names_of_t p trig_nm)

let args_of_t_with_v ?(vid="vid") p trig_nm = (vid, t_vid)::args_of_t p trig_nm
let arg_types_of_t_with_v p trig_nm = t_vid::arg_types_of_t p trig_nm
let args_of_t_as_vars_with_v ?(vid="vid") p trig_nm =
  mk_var vid::args_of_t_as_vars p trig_nm


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
let log_get_bound_for p trig_nm = "log_get_bound_"^trig_nm
let log_read_geq = "log_read_geq" (* takes vid, returns (trig, vid)list >= vid *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen. 
 * Also used for adding to map buffers and for correctives *)
let add_delta_to_map p map_id =
  let m_t = map_types_for p map_id in
  "add_delta_to_"^String.concat "_" @:
    List.map K3PrintSyntax.string_of_value_type m_t

(* foreign functions *)
let hash_addr = "hash_addr"
let declare_foreign_functions p = 
  let foreign_hash_addr = mk_foreign_fn hash_addr t_addr t_int in
  (* function needed to parse sql dates. Called by m3tok3 *)
  let sql_func = mk_foreign_fn "parse_sql_date" t_string t_int in
  foreign_hash_addr::
  sql_func::
  []

(* global data structures
 * ---------------------------------------------- *)

(* vid counter used to assign vids *)
let vid_counter_name = "__vid_counter__"
let vid_counter = mk_var vid_counter_name
let vid_counter_t = wrap_tbag @: t_int


(* epoch
 * TODO
 * Need to combine vid_counter, epoch and hash together
 * Create a global hash_me variale? Intstead of doing hash
 * everytime need a vid? *)
let epoch_name = "__epoch__"
let epoch_var = mk_var epoch_name
let epoch_t = wrap_tbag @: t_int

(* stmt_cntrs - (vid, stmt_id, counter) *)
let stmt_cntrs_name = "__stmt_cntrs__"
let stmt_cntrs = mk_var stmt_cntrs_name

let stmt_cntrs_id_type_vid_name = "vid"
let stmt_cntrs_id_type_stmt_id_name = "stmt_id"
let stmt_cntrs_id_type_counter_name = "counter"

let stmt_cntrs_ids = [
  stmt_cntrs_id_type_vid_name;
  stmt_cntrs_id_type_stmt_id_name;
  stmt_cntrs_id_type_counter_name
]

let stmt_cntrs_id_type = [(stmt_cntrs_id_type_vid_name, t_vid);
                          (stmt_cntrs_id_type_stmt_id_name, t_int);
                          (stmt_cntrs_id_type_counter_name, t_int)]

let stmt_cntrs_wrap = wrap_tbag
let stmt_cntrs_type = stmt_cntrs_wrap @: wrap_ttuple @:
  snd @: List.split stmt_cntrs_id_type

(* names for log *)
let log_for_t t = "log_"^t
let log_master = "log__master"

(* the function name for the frontier function *)
(* takes the types of the map *)
(* NOTE: assumes the first type is vid *)
let frontier_name p map_id =
  let m_t = P.map_types_for p map_id in
  "frontier_"^String.concat "_" @:
    List.map K3PrintSyntax.string_of_value_type m_t

(* Get the latest vals up to a certain vid 
 * - This is needed both for sending a push, and for modifying a local slice
 * operation
 * - slice_col is the k3 expression representing the collection.
 * - m_pat is an optional pattern for slicing the data first 
 *   It doesn't include a vid
 * - assumes a local 'vid' variable containing the border of the frontier
 * - keep_vid indicates whether we need to rmove the vid from the result collection
 *   (we usually need it removed only for modifying ast *)
let map_latest_vid_vals p slice_col m_pat map_id ~keep_vid =
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" p map_id in
  (* create a function name per type signature *)
  let access_k3 = match m_pat with
    | Some pat ->
        (* slice with an unknown for the vid so we only get the effect of
        * any bound variables *)
        mk_slice slice_col @:
          mk_tuple @: P.map_add_v mk_cunknown pat
    | None     -> slice_col
  in
  let simple_app = 
    mk_apply (mk_var @: frontier_name p map_id) @:
      mk_tuple [mk_var "vid"; access_k3]
  in
  if keep_vid then simple_app
  else
    (* remove the vid from the collection *)
    mk_map 
      (mk_lambda 
        (wrap_args m_id_t_v) @:
        mk_tuple @: list_drop 1 @: ids_to_vars @: fst_many m_id_t_v)
      simple_app

(* Create a function for getting the latest vals up to a certain vid *)
(* This is needed both for sending a push, and for modifying a local slice
 * operation *)
(* Returns the type pattern for the function, and the function itself *)
let frontier_fn p map_id =
  let max_vid = "max_vid" in
  let map_vid = "map_vid" in
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" p map_id in
  let m_t_v = wrap_ttuple @: snd_many @: m_id_t_v in
  let m_t_v_bag = wrap_t_of_map m_t_v in
  let m_id_t_no_val = P.map_ids_types_no_val_for p map_id in
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
  mk_global_fn (frontier_name p map_id) ["vid", t_vid; "input_map", m_t_v_bag] [m_t_v_bag] @:
      action
