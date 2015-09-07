(* K3 version of ring node for consistent hashing *)

open Util
open K3.AST
open K3Helpers

module D = K3Dist
module G = K3Global

(* address, job, hash *)
let id_t_node_for hash_name = D.my_peers.e @ [hash_name, t_int]
let id_t_node = id_t_node_for "hash"
let id_t_node_no_hash = list_drop_end 1 id_t_node
let id_node = fst @@ List.split id_t_node
let id_node_no_hash = fst @@ List.split id_t_node_no_hash
let t_node = snd @@ List.split id_t_node
let t_ring = wrap_tsortedmap' t_node

let add_node_nm = "add_node"

let replicas = create_ds "replicas" (mut t_int) ~init:(mk_cint 8)

let node_ring =
  let e = ["hash", t_int; "addr", t_addr] in
  create_ds "node_ring" (wrap_tsortedmap' @@ snd_many e) ~e

let node_ring_init =
  mk_iter (mk_lambda' D.nodes.e @@
      mk_apply (mk_var add_node_nm) [mk_var "addr"]) @@
    mk_var "nodes"

(* function to add a node in the consistent hashing ring *)
let add_node_fn =
  mk_global_fn add_node_nm ["addr", t_addr] [] @@
  mk_let ["rng"]
    (mk_range TList (mk_cint 1) (mk_cint 1) @@ mk_var replicas.id) @@
    mk_iter
      (mk_lambda' ["i", t_int] @@
        mk_insert node_ring.id
        (* hash the address, then add a number and hash again *)
          [mk_apply' "abs" @@ singleton @@ mk_apply' "hash_int" @@ singleton @@
            mk_add
              (mk_mult (mk_var "i") @@ mk_cint 2683) @@
              mk_apply' "hash_addr" [mk_var "addr"];
              mk_var "addr"]) @@
      mk_var "rng"

(* function to get the node for an int. Returns the node's address *)
(* note about scaling: ocaml's hash function's range is 0 to the maximum hash
 * value, which is 2^30 *)
let get_ring_node_nm = "get_ring_node"
let get_ring_node_fn =
  mk_global_fn get_ring_node_nm
  ["data", t_int; "max_val", t_int] [t_addr] @@
    mk_let ["scaled"]
      (mk_apply' "int_of_float" @@ singleton @@
        mk_mult
          (mk_apply' "float_of_int" @@ singleton @@
            mk_apply' "get_max_int" [mk_cunit]) @@
          mk_apply' "divf"
            [mk_apply' "float_of_int" [mk_var "data"];
            mk_apply' "float_of_int" [mk_var "max_val"]]) @@
    mk_case_sn
      (mk_peek @@ mk_slice_geq' node_ring.id [mk_var "scaled"; mk_cunknown])
      "val"
      (mk_snd @@ mk_var "val") @@
      (* if we haven't found, take the minimum *)
      mk_min_with (mk_var node_ring.id)
        (mk_lambda'' unit_arg @@ mk_error "empty node_ring")
        (mk_lambda' node_ring.e @@ mk_var "addr")

let functions =
  [ add_node_fn;
    get_ring_node_fn;
  ]

let global_vars =
  List.map decl_global
  [ node_ring;
    replicas
  ]
