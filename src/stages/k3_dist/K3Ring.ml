(* K3 version of ring node for consistent hashing *)

open Util
open K3.AST
open K3Helpers

let t_node = [canonical TString; t_addr; t_int]
let t_ring = wrap_tset_mut @: wrap_ttuple t_node (* should be a sorted set *)
let node_ring_nm = "node_ring"
let node_ring_code = 
  let c = mk_global_val node_ring_nm t_ring
  in mk_anno_sort c [2] (* sort by 3rd field *)

let replicas_nm = "replicas"
let replicas_code = mk_global_val replicas_nm t_int_mut

let ring_foreign_funcs = 
  mk_foreign_fn "hash_string" t_string t_int ::
  mk_foreign_fn "string_concat" (wrap_ttuple @: [t_string;t_string]) t_string ::
  mk_foreign_fn "string_of_int" t_int t_string ::
  []

(* function to set the number of replicas *)
let set_replicas_code = 
  mk_global_fn "set_replicas"
  ["n", t_int] [canonical TUnit] @:
  mk_assign (mk_var replicas_nm) (mk_var "n")

(* function to add a node in the consistent hashing ring *)
let add_node_code = 
  mk_global_fn "add_node"
  ["name", canonical TString; "address", t_addr] [canonical TUnit] @:
  mk_let "range" (wrap_tlist t_int)
    (mk_range TList 
      (mk_const @: CInt 1) (mk_const @: CInt 1) @: mk_var replicas_nm) @:
  mk_let "new_elems" t_ring
    (mk_map
      (mk_lambda (wrap_args ["i", t_int]) @:
        mk_tuple
          [mk_var "name";
            mk_var "address";
            mk_apply (mk_var "hash_string") @: 
              mk_apply (mk_var "string_concat") @:
                mk_tuple
                  [mk_var "name";
                    mk_apply (mk_var "string_of_int") @: mk_var "i"
                  ]
          ]
      ) @:
      mk_var "range"
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
  ["name", t_string] [t_unit] @:
  mk_let "nodes_to_delete" t_ring
    (mk_slice (mk_var node_ring_nm) @:
      mk_tuple [mk_var "name"; mk_const CUnknown; mk_const CUnknown]
    ) @:
  mk_iter
    (mk_lambda (wrap_args ["x", wrap_ttuple t_node]) @:
      mk_delete (mk_var node_ring_nm) (mk_var "x")
    ) @:
    mk_var "nodes_to_delete"

(* function to get the node for a hashed value. Returns the node's address *)
let get_ring_node_code = 
  mk_global_fn "get_ring_node"
  ["data", t_int] [t_addr] @:
  mk_let "results" t_ring 
    (mk_filtermap (* filter to only hashes greater than data *)
      (mk_lambda
        (wrap_args ["_", t_int; "addr", t_addr; "hash", t_int]) @:
        mk_geq (mk_var "hash") (mk_var "data")
      )
      (mk_lambda (* id *)
        (wrap_args ["x", wrap_ttuple [t_string; t_addr; t_int]]) @:
        mk_var "x"
      ) @:
      mk_var node_ring_nm
    ) @:
  mk_let_many (list_zip ["_";"__snd";"_"] t_node) 
    (mk_if (* if we have results, peek. otherwise, take the first node *)
      (mk_is_empty (mk_var "results") t_ring)
      (mk_peek @: mk_var node_ring_nm) @:
      mk_peek @: mk_var "results"
    ) @:
    mk_var "__snd"

let gen_ring_code =
  node_ring_code ::
  replicas_code ::
  ring_foreign_funcs @
  set_replicas_code ::
  add_node_code ::
  remove_node_code ::
  get_ring_node_code ::
  []



