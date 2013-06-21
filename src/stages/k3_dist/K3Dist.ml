(* File that includes global distributed values *)
open Util
open ProgInfo
open K3Helpers
module G = K3Global

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

