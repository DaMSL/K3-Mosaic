(* K3 version of ring node for consistent hashing *)

open Util
open K3.AST
open K3Helpers

(* address, maybe name, hash *)
let id_t_node = K3Global.peers_id_type @ ["hash", t_int]
let id_t_node_no_hash = list_drop_end 1 id_t_node
let id_node = fst @: List.split id_t_node
let id_node_no_hash = fst @: List.split id_t_node_no_hash
let t_node = snd @: List.split id_t_node
let t_ring = wrap_tset_mut @: wrap_ttuple t_node (* should be a sorted set *)

let node_ring_nm = "node_ring"
let node_ring_code = 
  let c = mk_global_val node_ring_nm t_ring
  in mk_anno_sort c [2] (* sort by 3rd field *)

let replicas_nm = "replicas"
let replicas_code = mk_global_val replicas_nm (wrap_tset_mut t_int) 

let ring_foreign_funcs = 
  mk_foreign_fn "hash_int" t_int t_int ::
  mk_foreign_fn "hash_addr" t_addr t_int ::
  mk_foreign_fn "int_of_float" t_float t_int::
  mk_foreign_fn "float_of_int" t_int t_float ::
  mk_foreign_fn "divf" (wrap_ttuple [t_float;t_float]) t_float ::
  mk_foreign_fn "get_max_int" t_unit t_int ::
  []

(* function to set the number of replicas *)
let set_replicas_code = 
  let var_replicas = mk_var replicas_nm in
  mk_global_fn "set_replicas"
  ["n", t_int] [t_unit] @:
  mk_block 
    [
      mk_iter 
      (mk_lambda (wrap_args ["x", t_int]) @:
        mk_delete var_replicas @: mk_var "x") @:
      (mk_slice var_replicas (mk_const CUnknown))
      ;
      mk_insert var_replicas (mk_var "n")
    ]

let add_node_name = "add_node"

(* function to add a node in the consistent hashing ring *)
let add_node_code = 
  mk_global_fn add_node_name
  id_t_node_no_hash [t_unit] @:
  mk_let "rng" (wrap_tlist t_int)
    (mk_range TList 
      (mk_const_int 1) (mk_const_int 1) @: 
        mk_peek @: 
          mk_slice (mk_var replicas_nm) (mk_const CUnknown)) @:
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
  mk_iter (* insert each new element *)
    (mk_lambda
      (wrap_args ["x", wrap_ttuple t_node]) @:
      mk_insert
        (mk_var node_ring_nm) @:
        mk_var "x"
    ) @:
    (mk_var "new_elems")

let remove_node_code =
  mk_global_fn "remove_node"
  id_t_node_no_hash [t_unit] @:
    mk_let "nodes_to_delete" t_ring
        (mk_slice (mk_var node_ring_nm) @:
        mk_tuple @: ids_to_vars id_node_no_hash @ [mk_const CUnknown]
        ) @:
    mk_iter
        (mk_lambda (wrap_args ["x", wrap_ttuple t_node]) @:
        mk_delete (mk_var node_ring_nm) (mk_var "x")
        ) @:
        mk_var "nodes_to_delete"

(* function to get the node for an int. Returns the node's address *)
(* note about scaling: ocaml's hash function's range is 0 to max_int *)
let get_ring_node_code = 
  mk_global_fn "get_ring_node"
  ["data", t_int; "max_val", t_int] [t_addr] @:
  mk_let "scaled" t_int
    (mk_apply (mk_var "int_of_float") @: 
      mk_mult
        (mk_apply (mk_var "float_of_int") @: 
          mk_apply (mk_var "get_max_int") @: mk_const CUnit) @:
        mk_apply (mk_var "divf") @: mk_tuple
          [mk_apply (mk_var "float_of_int") @: mk_var "data";
          mk_apply (mk_var "float_of_int") @: mk_var "max_val"]
    ) @:
  mk_let "results" t_ring 
    (mk_filtermap (* filter to only hashes greater than data *)
      (mk_lambda
        (wrap_args @: id_t_node) @:
        mk_geq (mk_var "hash") (mk_var "scaled")
      )
      (mk_id t_node) @:
      mk_var node_ring_nm
    ) @:
  mk_let_many (List.map (function ("addr",_) as x -> x | _,t -> "_",t) id_t_node)
    (mk_if (* if we have results, peek. otherwise, take the first node *)
      (mk_is_empty (mk_var "results") t_ring)
      (mk_peek @: mk_var node_ring_nm) @:
      mk_peek @: mk_var "results"
    ) @:
    mk_var "addr"

let gen_ring_code =
  ring_foreign_funcs @
  node_ring_code ::
  replicas_code ::
  set_replicas_code ::
  add_node_code ::
  remove_node_code ::
  get_ring_node_code ::
  []



