(* K3 version of route and shuffle functions *)

open Util
open K3.AST
open K3Helpers
open ProgInfo
module P = ProgInfo
module U = K3Util

(* TODO: the code here uses an index to refer to the right map id. This means
 * that in K3Helpers, the base for ranges cannot be changed from 0. This should
 * be sorted out in the future *)

(* route function name *)
let route_for p map_id =
  let m_t = P.map_types_no_val_for p map_id in
  "route_to_"^String.concat "_" @:
    List.map K3PrintSyntax.string_of_type m_t

let t_two_ints = [t_int; t_int]
let t_list_two_ints = wrap_tlist' t_two_ints
let dim_bounds_type = wrap_tlist' t_two_ints
let pmap_types = wrap_tlist' t_two_ints
let pmap_per_map_types = [t_map_id; pmap_types]
let full_pmap_types = wrap_tlist' pmap_per_map_types
let free_dims_type = wrap_tlist' t_two_ints
let free_domains_type = wrap_tlist' [t_int; wrap_tlist t_int]
let inner_cart_prod_type = wrap_tlist' t_two_ints
let free_cart_prod_type = wrap_tlist @: wrap_tlist' t_two_ints
let free_bucket_type = wrap_tlist' t_two_ints
let sorted_ip_inner_type = [t_addr; t_unit]
let sorted_ip_list_type = wrap_tbag' sorted_ip_inner_type
let output_type = wrap_tbag t_addr

(* map_parameter starts at 0 *)
(*             map_name * (map_parameter * modulo)  *)
type part_map_t = (id_t * (int * int) list) list

(* convert a k3 data structure with partition map data to an ocaml list with the
 * same data for easy manipulation *)
let list_of_k3_partition_map k3maps =
  let error str = invalid_arg @: "Invalid partition map: "^str in
  let parse_map e = let map_tup = U.decompose_tuple e in
    let parse_int e = match U.tag_of_expr e with
      | Const(CInt(i)) -> i
      | _ -> error "no integer found" in
    let name = match U.tag_of_expr @: List.hd map_tup with
     | Const(CString(s)) -> s
     | Var(s) -> s
     | _ -> error "no map name found" in
    let parse_tup e = let l = U.decompose_tuple e in
      let i = parse_int @: List.nth l 0 in
      let d = parse_int @: List.nth l 1 in
      (i, d) in
    let tuple_list = list_of_k3_container @: List.nth map_tup 1 in
    let mdata = List.map parse_tup tuple_list in
    (name, mdata) in
  let maps_with_data = match List.hd k3maps with
    | (Global(_,_,Some e), _) ->
        begin try list_of_k3_container e
        with Invalid_argument msg -> invalid_arg @: msg^" "^K3Printing.string_of_expr e end
    | _ -> error "no global variable found" in
  List.map parse_map maps_with_data

(* convert a list defining a partition map with map names to an equivalent k3
 * structure except using the map names *)
let k3_partition_map_of_list p l =
  (* handle the case of no input partition map ie. a default partition map *)
  (* we just create an empty partition map for all possible map ids *)
  if null l then
    let map_names = for_all_maps p (map_name_of p) in
    let k3_pmap = list_map (fun s ->
      mk_tuple [mk_cstring s; mk_empty pmap_types]
    ) map_names in
    k3_container_of_list full_pmap_types k3_pmap
  else
    let one_map_to_k3 (m, ds) =
      let check_index i = let ts = map_types_for p @: map_id_of_name p m
        in try ignore(List.nth ts i); true with Failure _ -> false in
      let k3tuplize (a, b) = mk_tuple [mk_cint a; mk_cint b] in
      let newdata = List.map
        (fun (i, d) -> if check_index i then k3tuplize (i,d)
          else invalid_arg @: "index "^string_of_int i^" out of range in map "^m)
        ds
      in mk_tuple [mk_cstring m; k3_container_of_list pmap_types newdata] in
    let new_l = List.map one_map_to_k3 l in
    k3_container_of_list full_pmap_types new_l

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

let hash_funcs_foreign p : (declaration_t * annotation_t) list =
  let map_types = (* all the map types we have *)
    ListAsSet.uniq @: List.flatten @: for_all_maps p @: (map_types_for p) in
  let names_types = List.map (fun t -> (t, hash_func_for t)) map_types in
  List.map (fun (t, name) -> mk_foreign_fn name t t_int) names_types

let route_foreign_funcs p =
  mk_foreign_fn "mod" (wrap_ttuple [t_int; t_int]) t_int ::
  (hash_funcs_foreign p)

(* partition map as input by the user (with map names) *)
let pmap_input = "pmap_input"
let global_pmap_input p partmap =
  mk_global_val_init pmap_input
  (wrap_tlist @: wrap_ttuple [t_string; pmap_types]) @:
  k3_partition_map_of_list p partmap

(* convert human-readable map name to map id *)
let pmap_data = "pmap_data"
let global_pmaps =
  mk_global_val_init pmap_data full_pmap_types @:
    mk_map
      (mk_lambda (wrap_args ["map_name", t_string; "map_types", pmap_types]) @:
        mk_tuple
          [project_from_tuple [t_int; t_string; t_int] ~total:3 ~choice:1 @:
            mk_peek_or_error @:
              mk_slice (mk_var K3Dist.map_ids_id) @:
                mk_tuple [mk_cunknown; mk_var "map_name"; mk_cunknown]
          ; mk_var "map_types"]
      ) @:
      mk_var "pmap_input"

(* calculate the size of the bucket of each dimensioned we're partitioned on
 * This is order-dependent in pmap *)
let calc_dim_bounds_code =
  mk_global_fn "calc_dim_bounds"
  ["pmap", pmap_types] (*args*) [dim_bounds_type; t_int] (* return *) @:
    mk_agg
      (mk_assoc_lambda
        (wrap_args ["xs", wrap_tlist @: wrap_ttuple [t_int; t_int];
          "acc_size", t_int])
        (wrap_args ["pos", t_int; "bin_size", t_int]) @:
        mk_tuple [mk_combine (mk_var "xs") @:
          mk_singleton t_list_two_ints @:
            mk_tuple [mk_var "pos"; mk_var "acc_size"];
          mk_mult (mk_var "bin_size") (mk_var "acc_size")]
      )
      (mk_tuple [mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int];
        mk_cint 1])
      (mk_var "pmap")

let gen_route_fn p map_id =
  let map_types = map_types_no_val_for p map_id in
  (* it's very important that the index for ranges start with 0, since we use
   * them for indexing *)
  let map_range = create_range 0 @: List.length map_types in
  let key_types = wrap_tmaybes map_types in
  let prefix = "key_" in
  let key_ids =
    fst @: List.split @: map_ids_types_no_val_for ~prefix:prefix p map_id in
  let to_id i = List.nth key_ids i in
  match map_types with

  | [] -> (* if no keys, for now we just route to one place *)
  mk_global_fn (route_for p map_id)
    ["_", t_int; "_", t_unit]
    [output_type] @: (* return *)
      mk_singleton output_type @:
        mk_apply (mk_var "get_ring_node") @:
          mk_tuple [mk_cint 1; mk_cint 1]

  | _  -> (* we have keys *)
  mk_global_fn (route_for p map_id)
    (("map_id", t_map_id)::types_to_ids_types prefix key_types)
    [output_type] @: (* return *)
    (* get the info for the current map and bind it to "pmap" *)
    mk_let ["pmap"]
      (mk_snd pmap_per_map_types @:
        mk_peek_or_error @: mk_slice (mk_var pmap_data) @:
          mk_tuple [mk_var "map_id"; mk_cunknown]
      ) @:

    (* handle the case of no partitioning at all *)
    mk_if (mk_eq (mk_var "pmap") (mk_empty pmap_types))
      (mk_apply (mk_var K3Ring.get_all_uniq_nodes_nm) mk_cunit) @:

    (* calculate the dim bounds ie. the bucket sizes when linearizing *)
    mk_let ["dim_bounds"; "max_val"]
      (mk_apply (mk_var "calc_dim_bounds") @: mk_var "pmap") @:
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
            (mk_cint 0) @: (* no contribution *)
            (* bind the slice for this index *)
            mk_let ["pmap_slice"]
              (mk_slice (mk_var "pmap") @:
                mk_tuple [mk_cint index; mk_cunknown]) @:
            (* check if we don't partition by this index *)
            (mk_case_ns (mk_peek @@ mk_var "pmap_slice") "peek_slice"
              (mk_cint 0) @:
              mk_let ["value"]
              (mk_apply (mk_var "mod") @:
                mk_tuple
                  (* we hash first. This could seem like it destroys locality,
                    * but it really doesn't, since we're only concerned about
                    * point locality *)
                  [mk_apply (mk_var hash_func) @: mk_var id_unwrap;
                  mk_snd t_two_ints @: mk_var "peek_slice"]
              ) @:
              mk_mult
                (mk_var "value") @:
                mk_snd t_two_ints @:
                  mk_peek_or_error @: mk_slice (mk_var "dim_bounds") @:
                    mk_tuple [mk_cint index; mk_cunknown])
          ) acc_code
      )
      (mk_cint 0)
      map_range
    ) @:
    (* now calculate the free parameters' contribution *)
    mk_let ["free_dims"]
      (List.fold_left
        (fun acc_code x ->
          let type_x = List.nth key_types x in
          let id_x = to_id x in
          mk_combine
          (mk_if
            (* we only care about indices that are nothing *)
            (mk_neq (mk_var @: id_x) @: mk_nothing type_x)
            (mk_empty free_dims_type) @:
            mk_slice (mk_var "pmap") @:
              mk_tuple [mk_cint x; mk_cunknown]
          ) acc_code
        )
        (mk_empty free_dims_type)
        (map_range)
      ) @:
    (* a list of ranges from 0 to the bucket size, for every free variable *)
    mk_let ["free_domains"]
      (mk_map
        (mk_lambda (wrap_args ["i", t_int; "b_i", t_int]) @:
          mk_tuple [mk_var "i"; mk_range TList
            (mk_cint 0) (mk_cint 1) @:
            (mk_var "b_i")]
        ) @:
        mk_var "free_dims"
      ) @:
    (* calculate the cartesian product to get every possible bucket *)
    mk_let ["free_cart_prod"]
      (mk_agg
        (mk_assoc_lambda (wrap_args ["prev_cart_prod", free_cart_prod_type])
          (wrap_args ["i", t_int; "domain", wrap_tlist t_int]) @:
          mk_flatten @: mk_map
            (* for every domain element in the domain *)
            (mk_lambda (wrap_args ["domain_element", t_int]) @:
              mk_if (mk_is_empty (mk_var "prev_cart_prod") free_cart_prod_type)
                (mk_singleton free_cart_prod_type @:
                  mk_singleton inner_cart_prod_type @:
                    mk_tuple [mk_var "i"; mk_var "domain_element"]
                ) @:
                mk_map
                  (* add current element to every previous sublist *)
                  (mk_lambda (AVar("rest_tup", inner_cart_prod_type)) @:
                    mk_combine (mk_var "rest_tup") @:
                      mk_singleton inner_cart_prod_type @:
                        mk_tuple [mk_var "i"; mk_var "domain_element"]
                  ) @:
                  mk_var "prev_cart_prod"
            ) @:
            mk_var "domain"
        )
        (mk_empty free_cart_prod_type) @:
        mk_var "free_domains"
      ) @:
    (* We now add in the value of the bound variables as a constant
     * and calculate the result for every possibility *)
    (* TODO: this can be turned into sets *)
    mk_let ["sorted_ip_list"]
      (mk_gbagg
        (mk_lambda (wrap_args ["ip", t_addr]) @: mk_var "ip")
        (mk_lambda (wrap_args ["_", t_unit; "_", t_unit]) @:
          mk_cunit
        )
        mk_cunit @:
        (* convert to bag *)
        mk_agg
          (mk_lambda
             (wrap_args ["acc_ips", output_type;
                         "free_bucket", free_bucket_type]) @:
              mk_combine
                (mk_var "acc_ips") @:
                mk_singleton output_type @:
                  mk_apply (mk_var "get_ring_node") @: mk_tuple
                    [mk_agg
                      (mk_assoc_lambda (wrap_args ["acc", t_int])
                        (wrap_args ["i", t_int; "val", t_int]) @:
                        mk_add (mk_var "acc") @:
                          mk_mult (mk_var "val") @:
                            mk_snd t_two_ints @:
                              mk_peek_or_error @: mk_slice (mk_var "dim_bounds") @:
                                mk_tuple [mk_var "i"; mk_cunknown]
                      )
                      (mk_var "bound_bucket") @: (* start with this const *)
                      mk_var "free_bucket";
                    mk_var "max_val"]
          )
          (mk_empty output_type) @:
          mk_var "free_cart_prod"
      ) @:
    mk_if
      (mk_is_empty (mk_var "sorted_ip_list") @: sorted_ip_list_type)
      (mk_singleton output_type @:
        mk_apply (mk_var "get_ring_node") @: mk_tuple (* empty ip list *)
          [mk_var "bound_bucket"; mk_var "max_val"]
      ) @:
      mk_fst_many sorted_ip_inner_type @:
        mk_var "sorted_ip_list"

(* create all code needed for route functions, including foreign funcs*)
let gen_route_code p partmap =
  K3Ring.gen_ring_code @
  global_pmap_input p partmap ::
  global_pmaps ::
  route_foreign_funcs p @
  calc_dim_bounds_code ::
  (* create a route for each map type, using only the key types *)
  List.map (gen_route_fn p) (List.map (hd |- snd) @:
    P.uniq_types_and_maps ~type_fn:P.map_types_no_val_for p)

