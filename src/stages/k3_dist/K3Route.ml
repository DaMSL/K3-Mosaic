(* K3 version of route and shuffle functions *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers
open ProgInfo
open K3Dist
module D = K3Dist
module P = ProgInfo
module U = K3Util

(* TODO: the code here uses an index to refer to the right map id. This means
 * that in K3Helpers, the base for ranges cannot be changed from 0. This should
 * be sorted out in the future *)

(* Explanation: we route via the following algorithm:
  * Take key (x,y,z) and hash each component.
  * For each component, limit to a certain maximum using mod.
  * Combine the components algebraically with x + y*x + z*y*x.
  * This gives us a mapping within the cube x,y,z, that is partially consistent with a square x,y. Even for values
    where z > 0, the splits in x and y correspond between the cube and the square.
  * Map the result to a node, in our case using consistent hashing:
  * Scale the result to the entire hashable range, and find a corresponding node on the clock.
  *)

(* Memoization:
   We first calculate the bound bucket of the bound variables.
   We use this value together with the particular bind pattern out of the possible bind patterns
   for this map, to index into a memoized table of bitmaps representing ips
   *)

(* route function name *)
let route_for ~bound p map_id =
  let m_t = P.map_types_no_val_for p map_id in
  (if bound then "bound_" else "free_")^
  "route_"^String.concat "_" @@
    List.map K3PrintSyntax.string_of_type m_t

let t_two_ints = [t_int; t_int]
let inner_cart_prod_type = wrap_tlist' t_two_ints
let free_cart_prod_type = wrap_tlist @@ wrap_tlist' t_two_ints
let free_bucket_type = wrap_tlist' t_two_ints
let sorted_ip_inner_type = [t_addr; t_unit]

(* map_parameter starts at 0 *)
(*             map_name * (map_parameter * modulo)  *)
type part_map_t = (id_t * (int * int) list) list

let buckets =
  (* indexed by dimension *)
  let e = ["mod", t_int] in
  let t = wrap_tvector' @@ snd_many e in
  create_ds "buckets" t ~e

(* list equal to inner_pmap, to preserve ordering *)
let inner_plist =
  let e = ["pos", t_int; "mod", t_int] in
  let t = wrap_tlist' @@ snd_many e in
  create_ds "inner_plist" t ~e

let rng = create_ds "rng" @@ wrap_tvector t_int

(* offset by 1. dim 0 is a default of 1, [0] for code generation purposes *)
let dim_bounds =
  let e = ["dim_size", t_int; "rng", rng.t] in
  let t = wrap_tvector' @@ snd_many e in
  create_ds "dim_bounds" t ~e

let free_dims =
  let e = ["dim", t_int] in
  let t = wrap_tbag' @@ snd_many e in
  create_ds "free_dims" t ~e

let pmap =
  let e = [buckets.id, buckets.t;
           "dim_bounds", dim_bounds.t;
           "max_val", t_int] in
  let t = wrap_ttuple @@ snd_many e in
  create_ds "pmap" t  ~e

let builtin_route = create_ds "builtin_route" t_bool

let route_bitmap =
  let e = ["val", t_bool] in
  let init =
    mk_map (mk_lambda' ["_", t_unknown] mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds "route_bitmap" (wrap_tvector' @@ snd_many e) ~e ~init

let all_nodes_bitmap =
  let e = ["val", t_bool] in
  (* create an equal number of bits to my_peers *)
  let init =
    mk_map (mk_lambda' ["_", t_unknown] mk_cfalse) @@ mk_var D.my_peers.id
  in
  let id = "route_nodes_bitmap" in
  (* populate only the nodes *)
  let d_init =
    mk_iter (mk_lambda'' ["i", t_int] @@
             mk_insert_at id (mk_var "i") [mk_ctrue]) @@
    mk_var D.nodes.id in
  create_ds id (wrap_tvector' @@ snd_many e) ~e ~init ~d_init

let calc_dim_bounds =
  mk_global_fn ~wr_all:true "calc_dim_bounds"
    ["map_id", t_int; "pmap", inner_plist.t] [buckets.t; dim_bounds.t; t_int] @@
    (* create full range for all dimensions *)
    (* also, pre-create the dims vector with the right size *)
    mk_let ["num_dims"]
      (mk_thd @@ mk_peek_or_error "range" @@ mk_slice' D.map_ids_id
        [mk_var "map_id"; mk_cunknown; mk_cunknown]) @@
    mk_let ["rng"; "pre_dims"]
      (mk_tuple
        [mk_convert_col (wrap_tlist t_int) rng.t @@
           mk_range TList (mk_cint 0) (mk_cint 1) @@ mk_var "num_dims";
         mk_let ["acc"] (mk_empty dim_bounds.t) @@
         mk_block [
           mk_insert_at "acc" (mk_var "num_dims") [mk_cint 0; mk_empty rng.t];
           mk_var "acc"
         ]]) @@
    mk_let ["buckets"]
      (mk_map (mk_lambda' unknown_arg @@ mk_cint 0) @@ mk_var "rng") @@
    (* calculate the size of the bucket of each dimensioned we're partitioned on
    * This is order-dependent in pmap *)
    mk_let ["dims"; "final_size"]
      (mk_agg
        (mk_lambda2'
          ["xs", dim_bounds.t; "acc_size", t_int]
          ["pos", t_int; "bin_size", t_int] @@
          mk_block [
            mk_insert_at "xs"
              (* add 1 for allowing 0 to be the identity *)
              (mk_add (mk_var "pos") @@ mk_cint 1) @@
                [
                  mk_var "acc_size";
                  mk_convert_col (wrap_tlist t_int) rng.t @@
                    mk_range TList (mk_cint 0) (mk_cint 1) @@ mk_var "bin_size"];
            mk_tuple [
              mk_var "xs";
              mk_mult (mk_var "bin_size") @@ mk_var "acc_size"]])
        (mk_tuple [mk_var "pre_dims"; mk_cint 1]) @@
        mk_var "pmap") @@
    (* fill in missing dimensions *)
    mk_let ["dims"]
      (mk_agg
        (mk_lambda2'
          ["xs", dim_bounds.t] ["n", t_int] @@
          (* read the number *)
          mk_let ["x"]
            (mk_at_with' "xs" (mk_var "n") @@ mk_id_fn dim_bounds) @@
          (* check if unfilled. If so, insert default value *)
          mk_block [
            mk_if (mk_eq (mk_fst @@ mk_var "x") @@ mk_cint 0)
              (mk_insert_at "xs" (mk_var "n") [mk_cint 1; mk_singleton rng.t [mk_cint 0]])
              mk_cunit;
            mk_var "xs"
          ])
        (mk_var "dims") @@
        mk_var "rng") @@
    mk_tuple [mk_var "buckets"; mk_var "dims"; mk_var "final_size"]

(* map from map_id to inner_pmap *)
let pmap_data_id = "pmap_data"
let pmap_data p =
  let e = [pmap.id, pmap.t] in
  let t = wrap_tvector' @@ snd_many e in
  let num_maps = List.length @@ P.get_map_list p in
  let init =
    mk_let ["agg"] (mk_empty t) @@
    (* initialize the map to be the size we need *)
    mk_block [
      mk_insert_at "agg" (mk_cint @@ num_maps) (* maps start at 1 *)
        [mk_empty buckets.t; mk_empty dim_bounds.t; mk_cint 0];
      (* partition map as input by the user (with map names) *)
      (* calculate the size of the bucket of each dimensioned we're partitioned on
      * This is order-dependent in pmap *)
      mk_agg
        (mk_lambda2' ["acc", t]
                    ["map_name", t_string; "map_types", inner_plist.t] @@
          mk_let ["map_id"]
            (mk_fst @@ mk_peek_or_error "can't find map in map_ids" @@
              mk_slice' K3Dist.map_ids_id
                [mk_cunknown; mk_var "map_name"; mk_cunknown]) @@
          mk_let ["buckets"; "dim_bounds"; "last_size"]
            (mk_apply' "calc_dim_bounds" [mk_var "map_id"; mk_var "map_types"]) @@
          mk_insert_at_block "acc" (mk_var "map_id")
            [mk_var "buckets"; mk_var "dim_bounds"; mk_var "last_size"])
      (mk_var "agg") @@
      mk_var "pmap_input"
    ]
  in
  create_ds pmap_data_id t ~e ~init

(* create memoization tables for every map *)
(* vectors of bound_var * pattern -> bitmap *)
let route_memo_for p map = "route_memo_map_" ^ map_name_of p map
let route_memo c =
  for_all_maps c.p (fun map ->
    let t = wrap_tvector @@ wrap_tvector t_bool in
    create_ds (route_memo_for c.p map) t)

(* convert human-readable map name to map id *)

(* convert a k3 data structure with partition map data to an ocaml list with the
 * same data for easy manipulation *)
let list_of_k3_partition_map k3maps =
  let error str = invalid_arg @@ "Invalid partition map: "^str in
  let parse_map e = let map_tup = U.decompose_tuple e in
    let parse_int e = match U.tag_of_expr e with
      | Const(CInt(i)) -> i
      | _ -> error "no integer found" in
    let name = match U.tag_of_expr @@ List.hd map_tup with
     | Const(CString(s)) -> s
     | Var(s) -> s
     | _ -> error "no map name found" in
    let parse_tup e = let l = U.decompose_tuple e in
      let i = parse_int @@ List.nth l 0 in
      let d = parse_int @@ List.nth l 1 in
      (i, d) in
    let tuple_list = list_of_k3_container @@ List.nth map_tup 1 in
    let mdata = List.map parse_tup tuple_list in
    (name, mdata) in
  let maps_with_data = match List.hd k3maps with
    | (Global(_,_,Some e), _) ->
        begin try list_of_k3_container e
        with Invalid_argument msg -> invalid_arg @@ msg^" "^K3Printing.string_of_expr e end
    | _ -> error "no global variable found" in
  List.map parse_map maps_with_data

(* convert a list defining a partition map with map names to an equivalent k3
 * structure except using the map names *)
let k3_partition_map_of_list p l =
  (* handle the case of no input partition map ie. a default partition map *)
  (* we just create an empty partition map for all possible map ids *)
  if null l || fst (hd l) = "empty" then
    let map_names = for_all_maps p (map_name_of p) in
    let k3_pmap = list_map (fun s ->
      mk_tuple [mk_cstring s; mk_empty inner_plist.t]
    ) map_names in
    k3_container_of_list inner_plist.t k3_pmap
  else
    let one_map_to_k3 (m, ds) =
      let check_index i = let ts = map_types_for p @@ map_id_of_name p m
        in try ignore(List.nth ts i); true with Failure _ -> false in
      let k3tuplize (a, b) = mk_tuple [mk_cint a; mk_cint b] in
      let newdata = List.map
        (fun (i, d) -> if check_index i then k3tuplize (i,d)
          else invalid_arg @@ "index "^string_of_int i^" out of range in map "^m)
        ds
      in mk_tuple [mk_cstring m; k3_container_of_list inner_plist.t newdata] in
    let new_l = List.map one_map_to_k3 l in
    k3_container_of_list inner_plist.t new_l

exception NoHashFunction of K3.AST.base_type_t

let hash_func_for typ =
  let rec inner t = match t.typ with
    | TInt              -> "int"
    | TDate             -> "date"
    | TFloat            -> "float"
    | TBool             -> "bool"
    | TString           -> "string"
    | TAddress          -> "addr"
    | TCollection(_, v) -> "C_"^inner v^"_c"
    | TTuple(vs)        -> "T_"^ String.concat "_" (List.map inner vs) ^"_t"
    | x                 -> raise @@ NoHashFunction x
  in "hash_"^inner typ

let clean_results = mk_set_all route_bitmap.id [mk_cfalse]

let gen_route_fn p map_id =
  let map_types = map_types_no_val_for p map_id in
  (* it's very important that the index for ranges start with 0, since we use
   * them for indexing *)
  let map_range = create_range @@ List.length map_types in
  let key_types = wrap_tupmaybes map_types in
  let prefix = "key_" in
  let key_ids =
    fst @@ List.split @@ map_ids_types_no_val_for ~prefix:prefix p map_id in
  let to_id i = List.nth key_ids i in

  match map_types with
  | [] -> (* if no keys, for now we just route to one place *)
    [mk_global_fn (route_for ~bound:true p map_id)
      ["_", t_int; "_", t_unit] [t_int] @@
        mk_cint 0
     ;
     mk_global_fn (route_for ~bound:false p map_id)
       ["_", t_int; "_", t_int; "_", t_unit] [] @@
        mk_block [
          clean_results;
          mk_insert_at route_bitmap.id (mk_apply' "get_ring_node" [mk_cint 1; mk_cint 1]) [mk_ctrue]
        ]
    ]

  | _  -> (* we have keys *)
    [mk_global_fn (route_for ~bound:true p map_id)
      (("map_id", t_map_id)::types_to_ids_types prefix key_types) [t_int] @@
      (* get the info for the current map and bind it to "pmap" *)
      mk_at_with' pmap_data_id (mk_var "map_id") @@
        mk_lambda' ["pmap_data", pmap.t] @@

        (* deep bind *)
        mk_let ["buckets"; "dim_bounds"; "max_val"] (mk_var "pmap_data") @@

        List.fold_left
          (fun acc_code index ->
            let temp_id = to_id index in
            let id_unwrap = temp_id^"_unwrap" in
            let temp_type = List.nth map_types index in
            let hash_func = hash_func_for temp_type in
            mk_add
              (* check if we have a binding in this index *)
              (mk_case_tup_ns (mk_var temp_id) id_unwrap
                (mk_cint 0) @@ (* no contribution *)
                (* check if we don't partition by this index *)
                mk_let ["bucket_mod"] (mk_at' "buckets" @@ mk_cint index) @@
                mk_if_eq (mk_var "bucket_mod") (mk_cint 0)
                  (mk_cint 0) @@
                  mk_let ["value"]
                    (mk_apply (mk_var "mod")
                        (* we hash first. This could seem like it destroys locality,
                          * but it really doesn't, since we're only concerned about
                          * point locality *)
                        [mk_apply' "abs" @@ singleton @@
                          mk_apply' hash_func [mk_var id_unwrap];
                        mk_var "bucket_mod"]) @@
                  mk_at_with' "dim_bounds" (mk_cint @@ index + 1) @@
                    mk_lambda' dim_bounds.e @@
                      mk_mult (mk_var "value") @@ mk_var "dim_size")
                  acc_code)
          (mk_cint 0)
          map_range
     ;

     (* function to get only the free dimensions (and addresses) *)
     mk_global_fn (route_for ~bound:false p map_id)
      (["bound_bucket", t_int; "map_id", t_map_id] @ types_to_ids_types prefix key_types) [] @@

      mk_at_with' pmap_data_id (mk_var "map_id") @@
        mk_lambda' ["pmap_data", pmap.t] @@

        (* deep bind *)
        mk_let ["pmap"; "dim_bounds"; "_m"] (mk_var "pmap_data") @@

      mk_at_with' pmap_shifts_id (mk_var "map_id") @@
        mk_lambda' pmap_shifts_e @@

        (* handle the case of no partitioning. Send to all *)
        mk_if (mk_eq (mk_size @@ mk_var "pmap") @@ mk_cint 0)
          (mk_set_all route_bitmap.id [mk_ctrue]) @@

        let len = List.length map_range in
        mk_block [
          clean_results;
          snd @@
          (* loop over all dims and get ips for cartesian product *)
          (* for code gen, we need to do all of our let bindings/lookup first *)
          (List.fold_left
            (fun (num, acc_code) index ->
              let add_idx s = s ^ soi index in
              num - 1,
              mk_let ["index"]
                (mk_if (mk_is_tup_nothing @@ mk_var @@ to_id index)
                  (mk_cint @@ index + 1) @@
                  mk_cint 0) @@
                mk_at_with' "dim_bounds" (mk_var "index") @@
                  mk_lambda' (List.map (first add_idx) dim_bounds.e) acc_code)
            (len, snd @@
              List.fold_left
                (fun (num, acc_code) index ->
                  let add_idx s = s ^ soi index in
                  num - 1,
                  mk_iter
                    (mk_lambda'' ["x", t_int] @@
                      mk_let [add_idx "val"]
                        (mk_mult (mk_var "x") @@ mk_var @@ add_idx "dim_size")
                        acc_code) @@
                    mk_var @@ add_idx "rng")
                  (* zero: insertion of ip *)
                (len,
                  mk_insert_at route_bitmap.id
                    (mk_apply' "get_ring_node"
                      (* add up all the values and the bound_bucket *)
                      (* add the shift for this map id *)
                      [mk_add (mk_var "map_shift") @@
                         List.fold_left (fun acc x ->
                        mk_add (mk_var @@ "val"^soi x) acc) (mk_var "bound_bucket") map_range;
                      mk_var "map_max"])
                    [mk_ctrue])
                map_range)
            map_range)
          ]
    ]

(* convert an index set to a pattern to be fed to route_free *)
let route_arg_pat_of_index p map_id idx =
  let i_ts = insert_index_fst @@ P.map_types_no_val_for p map_id in
  if i_ts = [] then [mk_cunit] else
  List.map (fun (i, t) ->
      if IntSet.mem i idx then mk_tuple [mk_ctrue; default_value_of_t t]
      else mk_tuple [mk_cfalse; default_value_of_t t])
    i_ts

(* fill in all memoized routes for a map *)
let memo_init_all_nm = "route_memo_init_all"
let memo_init_all c =
  let memo_init c (map_id:map_id_t) =
    let route_idx = Hashtbl.find c.route_indices map_id in
    let patterns = IntSetMap.to_list route_idx in
    let sz = IntSetMap.cardinal route_idx in
    let route_keys = P.map_types_no_val_for c.p map_id in
    mk_let ["rng_max"]
      (mk_at_with' pmap_data_id (mk_cint map_id) @@
        mk_lambda' pmap.e @@ mk_var "max_val") @@
    mk_let ["rng_buckets"]
      (mk_convert_col (wrap_tlist t_int) (wrap_tvector t_int) @@
      mk_range TList (mk_cint 0) (mk_cint 1) @@ mk_var "rng_max") @@
    (* get number indices of patterns used for lookup *)
    mk_let ["rng_patterns"]
      (k3_container_of_list (wrap_tvector t_int) @@
       List.map mk_cint @@ List.sort (-) @@ snd_many patterns) @@
    (* handle map with no keys *)
    if route_keys = [] then
      mk_insert_at (route_memo_for c.p map_id) (mk_cint 0) @@
         singleton @@ mk_block [
           mk_apply' (route_for ~bound:false c.p map_id) [mk_cint 0; mk_cint 0; mk_cunit];
           mk_var route_bitmap.id
         ]
    else
      (* else, we have keys *)
      mk_iter (mk_lambda'' ["i", t_int] @@
        mk_iter (mk_lambda'' ["j", t_int] @@
          mk_insert_at (route_memo_for c.p map_id)
            (mk_add (mk_mult (mk_var "i") @@ mk_cint sz) @@ mk_var "j")
              [mk_block
                (* fill in the matching pattern *)
                [List.fold_left (fun acc_code (pat, pat_idx) ->
                  let args = [mk_var "i"; mk_cint map_id] @
                            route_arg_pat_of_index c.p map_id pat in
                  mk_if (mk_eq (mk_var "j") @@ mk_cint pat_idx)
                    (mk_apply' (route_for ~bound:false c.p map_id) args)
                    acc_code)
                  (mk_error "unsupported pattern")
                  patterns;
                mk_var "route_bitmap"]
              ]) @@
          mk_var "rng_patterns") @@
        mk_var "rng_buckets"
  in
  mk_global_fn memo_init_all_nm unit_arg [] @@
    mk_block @@ P.for_all_maps c.p (memo_init c)

(* code to perform full route *)
let route_lookup c map_id key pat_idx lambda_body =
  let sz = IntSetMap.cardinal @@ Hashtbl.find c.route_indices map_id in
  mk_at_with' (route_memo_for c.p map_id)
    (mk_add
       (mk_mult
          (mk_apply' (route_for ~bound:true c.p map_id) key) @@ mk_cint sz)
       pat_idx) @@
    mk_lambda'' [route_bitmap.id, route_bitmap.t] lambda_body

(* create all code needed for route functions, including foreign funcs*)
let global_vars c =
  List.map decl_global @@
  [ builtin_route;
    route_bitmap;
    all_nodes_bitmap;
    pmap_data c.p] @
    route_memo c

let functions c =
  (* create a route for each map type, using only the key types *)
  (List.flatten @@ List.map (fun m -> gen_route_fn c.p m) @@
    List.map (hd |- snd |- snd) @@
    D.uniq_types_and_maps ~uniq_indices:false ~type_fn:P.map_types_no_val_for c) @
  [memo_init_all c]

