(* K3 version of route and shuffle functions *)

open Util
open K3.AST
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

(* route function name *)
let route_for p map_id =
  let m_t = P.map_types_no_val_for p map_id in
  "route_to_"^String.concat "_" @@
    List.map K3PrintSyntax.string_of_type m_t

let t_two_ints = [t_int; t_int]
let inner_cart_prod_type = wrap_tlist' t_two_ints
let free_cart_prod_type = wrap_tlist @@ wrap_tlist' t_two_ints
let free_bucket_type = wrap_tlist' t_two_ints
let sorted_ip_inner_type = [t_addr; t_unit]
let output_type = wrap_tbag t_addr

(* map_parameter starts at 0 *)
(*             map_name * (map_parameter * modulo)  *)
type part_map_t = (id_t * (int * int) list) list

let inner_pmap =
  let e = ["pos", t_int; "mod", t_int] in
  let t = wrap_tmap' @@ snd_many e in
  create_ds "inner_pmap" t ~e

(* list equal to inner_pmap *)
let inner_plist =
  let e = ["pos", t_int; "mod", t_int] in
  let t = wrap_tlist' @@ snd_many e in
  create_ds "inner_plist" t ~e

let dim_bounds =
  let e = ["dim", t_int; "bound", t_int] in
  let t = wrap_tmap' @@ snd_many e in
  create_ds "dim_bounds" t ~e

let free_dims =
  let e = ["dim", t_int; "bound", t_int] in
  let t = wrap_tmap' @@ snd_many e in
  create_ds "free_dims" t ~e

let pmap =
  let e = [inner_pmap.id, inner_pmap.t;
           "dim_info", wrap_ttuple [dim_bounds.t; t_int]] in
  let t = wrap_ttuple @@ snd_many e in
  create_ds "pmap" t  ~e

let calc_dim_bounds =
  mk_global_fn "calc_dim_bounds"
    ["map_id", t_int; "pmap", inner_plist.t] [dim_bounds.t; t_int] @@
    (* create full range for all dimensions *)
    mk_let ["rng"]
      (mk_range TList (mk_cint 0) (mk_cint 1) @@
        mk_sub
          (mk_subscript 3 @@ mk_peek_or_error "range" @@
            mk_slice' D.map_ids_id
              [mk_var "map_id"; mk_cunknown; mk_cunknown]) @@
          mk_cint 1) @@
    (* calculate the size of the bucket of each dimensioned we're partitioned on
    * This is order-dependent in pmap *)
    mk_let ["dims"; "final_size"]
      (mk_agg
        (mk_lambda2'
          ["xs", dim_bounds.t; "acc_size", t_int]
          ["pos", t_int; "bin_size", t_int] @@
          mk_block [
            mk_insert "xs" [mk_var "pos"; mk_var "acc_size"];
            mk_tuple [
              mk_var "xs";
              mk_mult (mk_var "bin_size") @@ mk_var "acc_size"]])
        (mk_tuple [mk_empty @@ dim_bounds.t; mk_cint 1]) @@
        mk_var "pmap") @@
    (* fill in missing dimensions *)
    mk_let ["dims"]
      (mk_fst @@ mk_agg
        (mk_lambda2'
          ["xs", dim_bounds.t; "last", t_int] ["n", t_int] @@
          (* read the number *)
          mk_let ["next"]
            (mk_case_ns
              (mk_peek @@ mk_slice' "xs" [mk_var "n"; mk_cunknown]) "x"
              (mk_var "last") @@
              mk_snd @@ mk_var "x") @@
          (* update the ds *)
          mk_block [
            mk_upsert_with "xs" [mk_var "n"; mk_cunknown]
              (mk_lambda'' unit_arg @@ mk_tuple [mk_var "n"; mk_var "next"]) @@
              mk_id_fn dim_bounds
            ;
            mk_tuple [mk_var "xs"; mk_var "next"]
          ])
        (mk_tuple [mk_var "dims"; mk_cint 1]) @@
        mk_var "rng") @@
    mk_tuple [mk_var "dims"; mk_var "final_size"]


(* map from map_id to inner_pmap *)
let pmap_data =
  let e = ["map_id", t_int; pmap.id, pmap.t] in
  let t = wrap_tmap' @@ snd_many e in
  let init =
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
        mk_block [
          mk_insert "acc"
            [mk_var "map_id";
              mk_tuple [
                (* convert map_types to map *)
                mk_convert_col inner_plist.t inner_pmap.t @@
                  mk_var "map_types";
                (* pre-calculate dim bounds data *)
                mk_apply' "calc_dim_bounds" [mk_var "map_id"; mk_var "map_types"]]
            ]
          ;
          mk_var "acc"])
      (mk_empty t) @@
      mk_var "pmap_input"
  in
  create_ds "pmap_data" t ~e ~init

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

let pmap_input p partmap =
  let e = ["map", t_string; inner_plist.id, inner_plist.t] in
  let t = wrap_tlist' @@ snd_many e in
  let init = k3_partition_map_of_list p partmap in
  create_ds "pmap_input" t ~e ~init

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

let gen_route_fn p map_id =
  let map_types = map_types_no_val_for p map_id in
  (* it's very important that the index for ranges start with 0, since we use
   * them for indexing *)
  let map_range = create_range 0 @@ List.length map_types in
  let key_types = wrap_tmaybes map_types in
  let prefix = "key_" in
  let key_ids =
    fst @@ List.split @@ map_ids_types_no_val_for ~prefix:prefix p map_id in
  let to_id i = List.nth key_ids i in
  match map_types with

  | [] -> (* if no keys, for now we just route to one place *)
  mk_global_fn (route_for p map_id)
    ["_", t_int; "_", t_unit]
    [output_type] @@ (* return *)
      mk_singleton output_type
        [mk_apply' "get_ring_node" [mk_cint 1; mk_cint 1]]

  | _  -> (* we have keys *)
  mk_global_fn (route_for p map_id)
    (("map_id", t_map_id)::types_to_ids_types prefix key_types)
    [output_type] @@ (* return *)
    (* get the info for the current map and bind it to "pmap" *)
    mk_let ["pmap_lookup"]
    (mk_snd @@ mk_peek_or_error "can't find map_id in pmap_data" @@
        mk_slice' pmap_data.id @@ [mk_var "map_id"; mk_cunknown]) @@

    (* deep bind *)
    mk_let ["pmap"; "dim_info"] (mk_var "pmap_lookup") @@
    mk_let ["dim_bounds"; "max_val"] (mk_var "dim_info") @@

    (* handle the case of no partitioning at all *)
    mk_if (mk_eq (mk_size (mk_var "pmap")) @@ mk_cint 0)
      (mk_var "nodes") @@

    (* calc_bound_bucket *)
    (* we calculate the contribution of the bound components *)
    mk_let ["bound_bucket"]
    (List.fold_left
      (fun acc_code index ->
        let temp_id = to_id index in
        let id_unwrap = temp_id^"_unwrap" in
        let temp_type = List.nth map_types index in
        let hash_func = hash_func_for temp_type in
        mk_add
          (* check if we have a binding in this index *)
          (mk_case_ns (mk_var temp_id) id_unwrap
            (mk_cint 0) @@ (* no contribution *)
            (* bind the slice for this index *)
            mk_let ["pmap_slice"]
              (mk_slice' "pmap" [mk_cint index; mk_cunknown]) @@
            (* check if we don't partition by this index *)
            (mk_case_ns (mk_peek @@ mk_var "pmap_slice") "peek_slice"
              (mk_cint 0) @@
              mk_let ["value"]
              (mk_apply (mk_var "mod")
                  (* we hash first. This could seem like it destroys locality,
                    * but it really doesn't, since we're only concerned about
                    * point locality *)
                  [mk_apply' "abs" @@ singleton @@
                    mk_apply' hash_func [mk_var id_unwrap];
                  mk_snd @@ mk_var "peek_slice"]) @@
              mk_mult
                (mk_var "value") @@
                mk_snd @@ mk_peek_or_error ("can't find "^soi index^" in dim_bounds") @@
                    mk_slice' "dim_bounds" [mk_cint index; mk_cunknown])
          ) acc_code
      )
      (mk_cint 0)
      map_range
    ) @@
    (* now calculate the free parameters' contribution *)
    mk_let ["free_dims"] (mk_empty free_dims.t) @@
      mk_block @@
        (List.map (fun x ->
          let type_x = List.nth key_types x in
          let id_x = to_id x in
          mk_if (mk_eq (mk_var id_x) @@ mk_nothing type_x)
            (mk_insert "free_dims"
              [mk_peek_or_zero ~zero:(mk_tuple [mk_cint x; mk_cint 1]) @@
                mk_slice' "pmap" [mk_cint x; mk_cunknown]])
            mk_cunit)
        map_range) @
    (* a list of ranges from 0 to the bucket size, for every free variable *)
    singleton @@ mk_let ["free_domains"]
      (mk_map
        (mk_lambda' ["i", t_int; "b_i", t_int] @@
          mk_tuple [mk_var "i"; mk_range TList
            (mk_cint 0) (mk_cint 1) @@ mk_var "b_i"]) @@
        mk_var "free_dims") @@
    (* calculate the cartesian product to get every possible bucket *)
    mk_let ["free_cart_prod"]
      (mk_agg
        (mk_assoc_lambda' ["prev_cart_prod", free_cart_prod_type]
          ["i", t_int; "domain", wrap_tlist t_int] @@
          mk_flatten @@ mk_map
            (* for every domain element in the domain *)
            (mk_lambda' ["domain_element", t_int] @@
              mk_is_empty (mk_var "prev_cart_prod")
                ~y:(mk_singleton free_cart_prod_type
                     [mk_singleton inner_cart_prod_type
                       [mk_var "i"; mk_var "domain_element"]])
                ~n:(mk_map
                     (* add current element to every previous sublist *)
                     (mk_lambda' ["rest_tup", inner_cart_prod_type] @@
                       mk_combine (mk_var "rest_tup") @@
                         mk_singleton inner_cart_prod_type
                           [mk_var "i"; mk_var "domain_element"]) @@
                    mk_var "prev_cart_prod")) @@
            mk_var "domain")
        (mk_empty free_cart_prod_type) @@
        mk_var "free_domains") @@
    (* We now add in the value of the bound variables as a constant
     * and calculate the result for every possibility *)
    (* TODO: this can be turned into sets *)
    mk_let ["sorted_ip_list"]
      (mk_gbagg
        (mk_lambda' ["ip", t_addr] @@ mk_var "ip")
        (mk_lambda'' ["_", t_unit; "_", t_unit] mk_cunit)
        mk_cunit @@
        (* convert to bag *)
        mk_agg
          (mk_lambda''
             ["acc_ips", output_type; "free_bucket", free_bucket_type] @@
              mk_combine
                (mk_var "acc_ips") @@
                mk_singleton output_type
                  [mk_apply' "get_ring_node"
                    [mk_agg
                      (mk_assoc_lambda' ["acc", t_int]
                                        ["i", t_int; "val", t_int] @@
                        mk_add (mk_var "acc") @@
                          mk_mult (mk_var "val") @@
                            mk_snd @@
                              mk_peek_or_error "can't find i in dim_bounds" @@
                              mk_slice' "dim_bounds" [mk_var "i"; mk_cunknown])
                      (mk_var "bound_bucket") @@ (* start with this const *)
                      mk_var "free_bucket";
                    mk_var "max_val"]])
          (mk_empty output_type) @@
          mk_var "free_cart_prod"
      ) @@
    mk_is_empty (mk_var "sorted_ip_list")
      ~y:(mk_singleton output_type
           [mk_apply' "get_ring_node" (* empty ip list *)
             [mk_var "bound_bucket"; mk_var "max_val"]])
      ~n:(mk_fst_many sorted_ip_inner_type @@ mk_var "sorted_ip_list")

(* create all code needed for route functions, including foreign funcs*)
let global_vars p partmap =
  List.map decl_global
  [ pmap_input p partmap;
    pmap_data;
  ]

let functions c partmap =
  (* create a route for each map type, using only the key types *)
  (List.map (gen_route_fn c.p) @@ List.map (hd |- snd |- snd) @@
    D.uniq_types_and_maps ~uniq_indices:false ~type_fn:P.map_types_no_val_for c)
