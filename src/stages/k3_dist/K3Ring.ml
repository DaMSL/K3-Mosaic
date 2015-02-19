(* K3 version of ring node for consistent hashing *)

open Util
open K3.AST
open K3Helpers

module D = K3Dist
module G = K3Global

(* address, job, hash *)
let id_t_node_for hash_name = G.peers.e @ [hash_name, t_int]
let id_t_node = id_t_node_for "hash"
let id_t_node_no_hash = list_drop_end 1 id_t_node
let id_node = fst @@ List.split id_t_node
let id_node_no_hash = fst @@ List.split id_t_node_no_hash
let t_node = snd @@ List.split id_t_node
let t_ring = wrap_tlist' t_node

let add_node_nm = "add_node"

let node_ring = create_ds "node_ring" (mut t_ring)

let ring_init =
  let address = hd @@ fst_many @@ D.nodes.e in
  mk_iter (mk_lambda' D.nodes.e @@
      mk_apply (mk_var add_node_nm) @@ mk_var address) @@
    mk_var "nodes"

let replicas = create_ds "replicas" (mut t_int) ~init:(mk_cint 8)

(* function to add a node in the consistent hashing ring *)
let add_node_fn =
  mk_global_fn add_node_nm id_t_node_no_hash [] @@
  mk_let ["rng"]
    (mk_range TList (mk_cint 1) (mk_cint 1) @@ mk_var replicas.id) @@
  mk_let ["new_elems"]
    (mk_map
      (mk_lambda' ["i", t_int] @@
        mk_tuple @@
          ids_to_vars id_node_no_hash @
            (* hash the address, then add a number and hash again *)
            [mk_apply (mk_var "hash_int") @@
              mk_add (mk_var "i") @@ mk_apply (mk_var "hash_addr") @@ mk_var "addr"]) @@
      mk_var "rng") @@
  mk_block [
    (* insert the new elements *)
    mk_assign node_ring.id @@
      mk_combine (mk_var "new_elems") @@ mk_var node_ring.id;
    (* sort by hash *)
    mk_assign node_ring.id @@
      mk_sort
        (mk_assoc_lambda' (id_t_node_for "hash1") (id_t_node_for "hash2") @@
          mk_gt (mk_var "hash1") @@ mk_var "hash2") @@
        mk_var node_ring.id
  ]

(* function to get the node for an int. Returns the node's address *)
(* note about scaling: ocaml's hash function's range is 0 to the maximum hash
 * value, which is 2^30 *)
(* NOTE: this would be much better with a binary search, or using a multiindex with
 * a 'just bigger than' query *)
let get_ring_node_nm = "get_ring_node"
let get_ring_node_fn =
  let results = "results" in
  mk_global_fn get_ring_node_nm
  ["data", t_int; "max_val", t_int] [t_addr] @@
  mk_let ["scaled"]
    (mk_apply (mk_var "int_of_float") @@
      mk_mult
        (mk_apply (mk_var "float_of_int") @@
          mk_apply (mk_var "get_max_int") mk_cunit) @@
        mk_apply (mk_var "divf") @@ mk_tuple
          [mk_apply (mk_var "float_of_int") @@ mk_var "data";
           mk_apply (mk_var "float_of_int") @@ mk_var "max_val"]) @@
  mk_let [results]
    (mk_filter (* filter to only hashes greater than data *)
      (mk_lambda' id_t_node @@
        mk_geq (mk_var "hash") @@ mk_var "scaled") @@
      mk_var node_ring.id) @@
  mk_let (List.map (function ("addr", _) -> "addr" | _ -> "_") id_t_node)
    (* if we have results, peek to get the min. otherwise, take the first node *)
    (mk_case_sn (mk_peek' results) "x"
      (mk_var "x") @@
      (* take the first node *)
      mk_peek_or_error "empty node ring" @@ mk_var node_ring.id) @@
    mk_var "addr"

let functions =
  [ add_node_fn;
    get_ring_node_fn;
  ]

let global_vars =
  List.map decl_global
  [ node_ring;
    replicas
  ]
