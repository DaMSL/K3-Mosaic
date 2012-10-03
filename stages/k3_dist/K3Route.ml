(* K3 version of route and shuffle functions *)

open Util
open K3.AST
open K3Helpers
open ProgInfo

(* route and shuffle function names *)
let route_for p map_id = "route_to_"^map_name_of p map_id

let t_two_ints = [t_int; t_int]
let t_list_two_ints = wrap_tlist @: wrap_ttuple t_two_ints
let dim_bounds_type = wrap_tlist @: wrap_ttuple t_two_ints
let bmod_types = wrap_tlist @: wrap_ttuple t_two_ints
let free_dims_type = wrap_tlist @: wrap_ttuple t_two_ints
let free_domains_type = wrap_tlist @: wrap_ttuple [t_int; wrap_tlist t_int]
let inner_cart_prod_type = wrap_tlist @: wrap_ttuple t_two_ints
let free_cart_prod_type = wrap_tlist @: wrap_tlist @: wrap_ttuple t_two_ints
let free_bucket_type = wrap_tlist @: wrap_ttuple t_two_ints
let sorted_ip_inner_type = [t_addr; wrap_tlist t_addr]
let sorted_ip_list_type = wrap_tlist @: wrap_ttuple sorted_ip_inner_type

let route_foreign_funcs = 
  mk_foreign_fn "mod" (wrap_ttuple [t_int; t_int]) t_int ::
  mk_foreign_fn "hash_int" t_int t_int ::
  mk_foreign_fn "hash_float" t_float t_int ::
  []

let bmod_data = "bmod_data"
let bmod_per_map_types = [t_map_id; bmod_types]
let global_bmods =
  mk_global_val bmod_data @: 
    wrap_tset_mut @: wrap_ttuple bmod_per_map_types

let calc_dim_bounds_code = mk_global_fn "calc_dim_bounds" 
  ["bmod", bmod_types] (*args*) [dim_bounds_type] (* return *) @:
  mk_fst [dim_bounds_type; t_int] @:
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
        mk_const @: CInt(1)]
      )
      (mk_var "bmod")

let hash_func_for typ = "hash_"^match typ with
  | TIsolated(TImmutable(TInt,_)) -> "int"
  | TIsolated(TImmutable(TFloat,_)) -> "float"
  | _ -> invalid_arg "No hash function for this type"

let key_map_types_for p map_id =
  list_drop_end 1 @: map_types_for p map_id

let gen_route_fn p map_id = 
  let map_types_full = map_types_for p map_id in
  let map_types = list_drop_end 1 map_types_full in
  let map_range = create_range 0 (List.length map_types) in
  let key_types = wrap_tmaybes map_types in
  let prefix = "key_id_" in
  let to_id i = int_to_temp_id i prefix in
  match map_types with 
  | [] -> (* if no keys, for now we just route to one place *)
  mk_global_fn (route_for p map_id)
    ["_", canonical TUnit]
    [wrap_tlist t_addr] @: (* return *)
      mk_singleton (wrap_tlist t_addr) @:
        mk_apply (mk_var "get_ring_node") @: mk_const @: CInt 1
  | _  -> (* we have keys *)
  mk_global_fn (route_for p map_id)
    ["key", wrap_ttuple key_types]
    [wrap_tlist t_addr] @: (* return *)
    mk_let "bmod" bmod_types
      (mk_snd bmod_per_map_types @:
        mk_peek @: mk_slice (mk_var bmod_data) @:
          mk_tuple [mk_const @: CInt map_id; mk_const CUnknown]
      ) @:
    mk_let "dim_bounds" dim_bounds_type 
      (mk_apply (mk_var "calc_dim_bounds") @: mk_var "bmod") @:
    mk_destruct_tuple "key" key_types prefix
    @:
    (* calc_bound_bucket *)
    mk_let "bound_bucket" t_int 
    (List.fold_left
      (fun acc_code index -> 
        let temp_id = to_id index in
        let id_unwrap = temp_id^"_unwrap" in
        let temp_type = List.nth map_types index in
        let maybe_type = wrap_tmaybe temp_type in
        let hash_func = hash_func_for temp_type in
        mk_add 
          (mk_if (mk_eq (mk_var temp_id) @: mk_const CNothing) 
            (mk_const @: CInt 0) (* no contribution *)
            (mk_unwrap_maybe [temp_id, maybe_type] @:
              mk_let "value" t_int
              (mk_apply (mk_var "mod") @:
                mk_tuple
                  [mk_apply (mk_var hash_func) @: mk_var id_unwrap;
                   mk_fst t_two_ints @: mk_peek @:
                     mk_slice (mk_var "bmod") @:
                       mk_tuple [mk_const @: CInt index; mk_const CUnknown]]
              ) @:
            mk_mult
              (mk_var "value") @:
               mk_snd t_two_ints @:
                 mk_peek @: mk_slice (mk_var "dim_bounds") @:
                  mk_tuple [mk_const @: CInt index; mk_const CUnknown]
            )
          ) acc_code
      )
      (mk_const @: CInt 0)
      map_range
    ) @:
    mk_let "free_dims" free_dims_type
      (List.fold_left
        (fun acc_code x -> 
          mk_combine 
          (mk_if 
            (mk_eq (mk_var @: to_id x) @: mk_const CNothing)
            (mk_empty free_dims_type) @:
            mk_slice (mk_var "bmod") @: 
              mk_tuple [mk_var @: to_id x; mk_const CUnknown]
          ) acc_code
        )
        (mk_empty free_dims_type)
        (map_range)
      ) @:
    mk_let "free_domains" free_domains_type
      (mk_map
        (mk_lambda (wrap_args ["i", t_int; "b_i", t_int]) @:
          mk_tuple [mk_var "i"; mk_range TList 
            (mk_const @: CInt 0) (mk_const @: CInt 1) @: 
            mk_add (mk_var "b_i") @: mk_const @: CInt (-1)]
        ) @:
        mk_var "free_dims"
      ) @:
    mk_let "free_cart_prod" free_cart_prod_type
      (mk_agg
        (mk_assoc_lambda (wrap_args ["prev_cart_prod", free_cart_prod_type])
          (wrap_args ["i", t_int; "domain", wrap_tlist t_int])
          (mk_flatten @: mk_map
            (* for every domain element in the domain *)
            (mk_lambda (wrap_args ["domain_element", t_int]) @:
              mk_if (mk_is_empty (mk_var "prev_cart_prod") free_cart_prod_type)
                (mk_singleton free_cart_prod_type @:
                  mk_singleton inner_cart_prod_type @:
                    mk_tuple [mk_var "i"; mk_var "domain_element"]
                )
                (mk_map
                  (* add current element to every previous sublist *)
                  (mk_lambda (AVar("rest_tup", inner_cart_prod_type)) @:
                    mk_combine (mk_var "rest_tup") @:
                      mk_singleton inner_cart_prod_type @:
                        mk_tuple [mk_var "i"; mk_var "domain_element"]
                  )
                  (mk_var "prev_cart_prod")
                )
            )
            (mk_var "domain")
          )
        )
        (mk_empty free_cart_prod_type)
        (mk_var "free_domains")
      ) @:
    mk_let "sorted_ip_list" (sorted_ip_list_type)
      (* We now add in the value of the bound variables as a constant
       * and calculate the result for every possibility *)
      (mk_gbagg
        (mk_lambda (wrap_args ["ip", t_addr]) @: mk_var "ip")
        (mk_lambda (wrap_args ["acc", wrap_tlist t_addr; "ip", t_addr]) @:
          mk_combine 
            (mk_singleton (wrap_tlist t_addr) @: mk_var "ip") @: 
            mk_var "acc"
        )
        (mk_empty @: wrap_tlist t_addr) @:
        mk_map
          (mk_lambda (wrap_args ["free_bucket", free_bucket_type]) @:
            mk_apply (mk_var "get_ring_node") @:
              mk_agg
                (mk_assoc_lambda (wrap_args ["acc", t_int])
                  (wrap_args ["i", t_int; "val", t_int]) @:
                  mk_add (mk_var "acc") @:
                    mk_mult (mk_var "val") @:
                      mk_fst t_two_ints @:
                        mk_peek @: mk_slice (mk_var "dim_bounds") @:
                          mk_tuple [mk_var "i"; mk_const CUnknown]
                )
                (mk_var "bound_bucket") @: (* start with this const *)
                mk_var "free_bucket"
          ) @:
          mk_var "free_cart_prod" 
      ) @:
    mk_if
      (mk_is_empty (mk_var "sorted_ip_list") @: sorted_ip_list_type)
      (mk_singleton (wrap_tlist t_addr) @:
        mk_apply (mk_var "get_ring_node") @: (* empty ip list *)
        mk_var "bound_bucket"
      ) @:
      mk_map
        (mk_lambda 
          (wrap_args @: list_zip ["__fst";"__snd"] sorted_ip_inner_type) @:
          mk_var "__fst"
        ) @:
        mk_var "sorted_ip_list"
        
(* create all code needed for route functions, including foreign funcs*)
let gen_route_code p =
  K3Ring.gen_ring_code @
  global_bmods ::
  route_foreign_funcs @ 
  calc_dim_bounds_code ::
  List.map (gen_route_fn p) (get_map_list p)




    

  

