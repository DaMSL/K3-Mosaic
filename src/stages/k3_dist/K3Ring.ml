(* K3 version of ring node for consistent hashing *)

open Util
open K3.AST
open K3Helpers

(* address, role, name, hash *)
let id_t_node_for hash_name = K3Global.peers_id_type @ [hash_name, t_int]
let id_t_node = id_t_node_for "hash"
let id_t_node_no_hash = list_drop_end 1 id_t_node
let id_node = fst @: List.split id_t_node
let id_node_no_hash = fst @: List.split id_t_node_no_hash
let t_node = snd @: List.split id_t_node
let t_ring = wrap_tlist' t_node

let node_ring_nm = "node_ring"
let node_ring_code =
  let c = mk_global_val node_ring_nm t_ring
  in mk_anno_sort c [2] (* sort by 3rd field *)

let replicas_nm = "replicas"
let replicas_code = mk_global_val_init replicas_nm (wrap_tset t_int) @:
  mk_singleton (wrap_tset t_int) (mk_cint 8)

let ring_foreign_funcs =
  mk_foreign_fn "hash_int" t_int t_int ::
  mk_foreign_fn "hash_addr" t_addr t_int ::
  mk_foreign_fn "int_of_float" t_float t_int::
  mk_foreign_fn "float_of_int" t_int t_float ::
  mk_foreign_fn "get_max_int" t_unit t_int ::
  []

(* function to set the number of replicas *)
let set_replicas_code =
  let var_replicas = mk_var replicas_nm in
  mk_global_fn "set_replicas"
  ["n", t_int] [t_unit] @:
    mk_update replicas_nm (mk_peek_or_error var_replicas) (mk_var "n")

let add_node_name = "add_node"

(* function to add a node in the consistent hashing ring *)
let add_node_code =
  mk_global_fn add_node_name
  id_t_node_no_hash [t_unit] @:
  mk_let "rng" (wrap_tlist t_int)
    (mk_range TList
      (mk_cint 1) (mk_cint 1) @:
        mk_peek_or_error @:
          mk_slice (mk_var replicas_nm) mk_cunknown) @:
  mk_let "new_elems" t_ring
    (mk_map
      (mk_lambda (wrap_args ["i", t_int]) @:
        mk_tuple @:
          ids_to_vars id_node_no_hash @
            [mk_apply (mk_var "hash_int") @:
                mk_add (mk_var "i") @:
                    (mk_apply (mk_var "hash_addr") @: mk_var "addr")
            ]
      ) @:
      mk_var "rng"
    ) @:
  mk_block
  [ mk_iter (* insert each new element *)
      (mk_lambda
        (wrap_args ["x", wrap_ttuple t_node]) @:
        mk_insert
          node_ring_nm @:
          mk_var "x"
      ) @:
      (mk_var "new_elems")
    ;
    (* sort by the hash *)
    mk_let "temp_ring" t_ring
      (mk_sort
        (mk_assoc_lambda
          (wrap_args @: id_t_node_for "hash1")
          (wrap_args @: id_t_node_for "hash2") @:
          mk_gt (mk_var "hash1") (mk_var "hash2"))
        (mk_var node_ring_nm)
      ) @:
    (* delete everything in the ring and insert the contents of the temp ring.
     * This wouldn't be necessary if we had annotations that worked *)
    mk_block
    [
      mk_iter
        (mk_lambda (wrap_args ["node", wrap_ttuple t_node]) @:
          mk_delete node_ring_nm @:
            (mk_var "node")
        ) @:
        (mk_var node_ring_nm)
      ;
      mk_iter
        (mk_lambda (wrap_args ["node", wrap_ttuple t_node]) @:
          mk_insert node_ring_nm @:
            (mk_var "node")
        ) @:
        (mk_var "temp_ring")
    ]
  ]


let remove_node_code =
  mk_global_fn "remove_node"
  id_t_node_no_hash [t_unit] @:
    mk_let "nodes_to_delete" t_ring
        (mk_slice (mk_var node_ring_nm) @:
        mk_tuple @: ids_to_vars id_node_no_hash @ [mk_cunknown]
        ) @:
    mk_iter
        (mk_lambda (wrap_args ["x", wrap_ttuple t_node]) @:
        mk_delete node_ring_nm (mk_var "x")
        ) @:
        mk_var "nodes_to_delete"

(* function to get the node for an int. Returns the node's address *)
(* note about scaling: ocaml's hash function's range is 0 to the maximum hash
 * value, which is 2^30 *)
let get_ring_node_code =
  let results_nm = "results" in
  let results = mk_var results_nm in
  mk_global_fn "get_ring_node"
  ["data", t_int; "max_val", t_int] [t_addr] @:
  mk_let "scaled" t_int
    (mk_apply (mk_var "int_of_float") @:
      mk_mult
        (mk_apply (mk_var "float_of_int") @:
          mk_apply (mk_var "get_max_int") mk_cunit) @:
        mk_apply (mk_var "divf") @: mk_tuple
          [mk_apply (mk_var "float_of_int") @: mk_var "data";
          mk_apply (mk_var "float_of_int") @: mk_var "max_val"]
    ) @:
  mk_let results_nm t_ring
    (mk_filter (* filter to only hashes greater than data *)
      (mk_lambda
        (wrap_args @: id_t_node) @:
        mk_geq (mk_var "hash") (mk_var "scaled")
      ) @:
      mk_var node_ring_nm
    ) @:
  mk_let_many (List.map (function ("addr",_) as x -> x | _,t -> "_",t) id_t_node)
    (* if we have results, peek. otherwise, take the first node *)
    (mk_case_sn (mk_peek results) "res_val"
      (mk_var "res_val")
      (mk_peek_or_error @@ mk_var node_ring_nm)) @@
    mk_var "addr"

(* k3 function to get all of the nodes in the node ring *)
let get_all_uniq_nodes_nm = "get_all_uniq_nodes"
let get_all_nodes_code =
  let t_ring_bag = wrap_tbag @: snd @: snd @: unwrap_vcol t_ring in
  mk_global_fn get_all_uniq_nodes_nm
  ["_", t_unit] [wrap_tbag t_addr] @:
  mk_fst_many [t_addr; t_unit] @: (* project out just the sorted value *)
    mk_gbagg
      (mk_lambda (wrap_args id_t_node) @:
        mk_var "addr")
      (mk_lambda (wrap_args ["_", t_unit; "_", wrap_ttuple t_node]) @: mk_cunit)
      (mk_cunit) @:
      mk_convert_col t_ring t_ring_bag @:
        mk_var node_ring_nm


let gen_ring_code =
  ring_foreign_funcs @
  node_ring_code ::
  replicas_code ::
  set_replicas_code ::
  add_node_code ::
  remove_node_code ::
  get_ring_node_code ::
  get_all_nodes_code ::
  []



