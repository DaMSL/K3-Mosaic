(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
module G = K3Global
module P = ProgInfo

(* initial vid to put in initialization statements *) 
let init_vid = "__init_vid__"

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

let args_of_t_with_v p trig_nm = ("vid", t_vid)::args_of_t p trig_nm
let arg_types_of_t_with_v p trig_nm = t_vid::arg_types_of_t p trig_nm
let args_of_t_as_vars_with_v p trig_nm = 
  mk_var "vid"::args_of_t_as_vars p trig_nm


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
let log_write_for p trig_nm = "log_write_"^trig_nm (* varies with bound *)
let log_get_bound_for p trig_nm = "log_get_bound_"^trig_nm 
let log_read_geq = "log_read_geq" (* takes vid, returns (trig, vid)list >= vid *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen *)
let add_delta_to_buffer_for_map p map_id = 
  "add_delta_to_buffer_"^map_name_of p map_id

(* foreign functions *)
let hash_addr = "hash_addr"
let foreign_hash_addr = mk_foreign_fn hash_addr t_addr t_int
let declare_foreign_functions p = foreign_hash_addr::[]

(* global data structures 
 * ---------------------------------------------- *)

(* vid counter used to assign vids *)
let vid_counter_name = "__vid_counter__"
let vid_counter = mk_var vid_counter_name
let vid_counter_t = wrap_tset @: t_int_mut


(* epoch 
 * TODO 
 * Need to combine vid_counter, epoch and hash together 
 * Create a global hash_me variale? Intstead of doing hash
 * everytime need a vid? *)
let epoch_name = "__epoch__"
let epoch_var = mk_var epoch_name
let epoch_t = wrap_tset @: t_int_mut

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

let stmt_cntrs_id_type = [(stmt_cntrs_id_type_vid_name, t_vid_mut);
                          (stmt_cntrs_id_type_stmt_id_name, t_int_mut);
                          (stmt_cntrs_id_type_counter_name, t_int_mut)] 

let stmt_cntrs_type = wrap_tset @: wrap_ttuple_mut @: 
  snd @: List.split stmt_cntrs_id_type
  (*
let stmt_cntrs_type = wrap_tset_mut @: wrap_ttuple_mut 
      [t_vid_mut; t_int_mut; t_int_mut] 
*)
(* names for log *)
let log_for_t t = "log_"^t
let log_master = "log__master"

(* get the latest vals up to a certain vid *)
(* This is needed both for sending a push, and for modifying a local slice
 * operation *)
(* slice_col is the k3 expression representing the collection.
 * m_pat is an optional pattern for slicing the data first *)
let map_latest_vid_vals p slice_col m_pat map_id ~keep_vid =
  let max_vid = "max_vid" in
  let map_vid = "map_vid" in
  let m_id_t = 
    if keep_vid then P.map_ids_types_with_v_for ~vid:map_vid p map_id
    else P.map_ids_types_for p map_id in
  let m_ids = fst_many m_id_t in
  let m_ts = snd_many m_id_t in
  let m_t_set = wrap_tset @: wrap_ttuple @: m_ts in
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" p map_id in 
  let m_id_t_no_val = P.map_ids_types_no_val_for p map_id in
  let m_id_no_val = fst_many @: m_id_t_no_val in
  let m_t_no_val = snd_many @: m_id_t_no_val in
  let num_keys = List.length m_id_t_no_val in 
  (* if we have bound variables, we should slice first. Otherwise, 
      we don't need a slice *)
  let access_k3 = begin match m_pat with
    | Some pat ->
        (* slice with an unknown for the vid so we only get the effect of
        * any bound variables *)
        mk_slice slice_col @: 
          mk_tuple @: P.map_add_v mk_cunknown pat
    | None     -> slice_col
    end 
  in
  (* we need to get the latest vid for every possible key, and then filter 
   * only those latest values *)
  let num_pat_unknowns = match m_pat with
    | Some pat -> 
        (* sanity check *)
        let p_len = List.length pat in
        if p_len <> num_keys + 1 then (* keys + value *)
          let s =
            Printf.sprintf "expected pattern length %d but got %d" num_keys p_len
          in invalid_arg s 
        else 
          (* get number of bound value *)
          let open K3.AST in
          let l = List.length @:
            List.filter (fun e -> K3Util.tag_of_expr e <> Const CUnknown) pat
          in
          (* another sanity check *)
          if l > num_keys then invalid_arg "too many values specified in pattern"
          else l
    | None -> 0
  in
  (* a routine common to both methods of slicing *)
  let common_vid_lambda =
    mk_assoc_lambda 
      (wrap_args ["acc", m_t_set; max_vid, t_vid])
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
              (mk_singleton m_t_set (mk_tuple @: ids_to_vars m_ids)) @:
                  mk_var "acc";
            mk_var max_vid])
          (* else if map vid is greater than max_vid, make a new
          * collection and set a new max_vid *)
          (mk_if
            (v_gt (mk_var map_vid) (mk_var max_vid))
            (mk_tuple
              [mk_singleton m_t_set (mk_tuple @: ids_to_vars m_ids); 
              mk_var map_vid])
            (* else keep the same accumulator and max_vid *)
            (mk_tuple [mk_var "acc"; mk_var max_vid])
          )
        )
        (* else keep the same acc and max_vid *)
        (mk_tuple [mk_var "acc"; mk_var max_vid])
      ) 
  in
  if num_keys - num_pat_unknowns <= 0 then
    (* a regular fold is enough if we have no keys or if every key is bound *)
    (* get the maximum vid that's less than our current vid *)
    mk_fst [m_t_set; t_vid] @:
      mk_agg 
        common_vid_lambda
        (mk_tuple [mk_empty m_t_set; min_vid_k3])
        access_k3
  else
    (* we have free keys so we need to do some more work *)
    mk_flatten @: mk_map
      (mk_assoc_lambda
        (wrap_args ["_", wrap_ttuple m_t_no_val]) (* group *)
        (wrap_args ["project", m_t_set; "_", t_vid]) @:
        mk_var "project"
      ) @:
      mk_gbagg 
        (mk_lambda (wrap_args m_id_t_v) @:
          (* group by the keys (not vid, not values) *)
          mk_tuple @: ids_to_vars @: m_id_no_val)
        (* get the maximum vid that's less than our current vid *)
        common_vid_lambda
        (mk_tuple [mk_empty m_t_set; min_vid_k3])
        access_k3
