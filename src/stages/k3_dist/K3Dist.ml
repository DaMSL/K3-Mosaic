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

(* get the latest vals up to a certain vid *)
(* This is needed both for sending a push, and for modifying a local slice
 * operation *)
(* slice_col is the k3 expression representing the collection.
 * pat_m is an optional pattern for slicing the data first *)
let map_latest_vid_vals p slice_col pat_m map_id ~keep_vid =
  let max_vid = "max_vid" in
  let map_vid = "map_vid" in
  let m_id_t = 
    if keep_vid then P.map_ids_types_with_v_for ~vid:map_vid p map_id
    else P.map_ids_types_for p map_id in
  let m_ids = fst_many m_id_t in
  let m_ts = snd_many m_id_t in
  let m_t_set = wrap_tset @: wrap_ttuple @: m_ts in
  let m_id_t_v = P.map_ids_types_with_v_for ~vid:"map_vid" p map_id in 
  (* if we have bound variables, we need to slice first. Otherwise, 
      we don't need a slice *)
  let access_k3 = begin match pat_m with
    | Some pat ->
        (* slice with an unknown for the vid so we only get the effect of
        * any bound variables *)
        mk_slice slice_col @: 
          mk_tuple @: P.map_add_v mk_cunknown pat
    | None     -> slice_col
    end 
  in 
  (* get the maximum vid that's less than our current vid *)
  mk_fst [m_t_set; t_vid] @:
    mk_agg 
      (mk_assoc_lambda 
        (* we project out the vid at the same time *)
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
      )
      (mk_tuple [mk_empty m_t_set; min_vid_k3])
      access_k3
