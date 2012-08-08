(* K3 version of route and shuffle functions *)

open Util
open K3Helpers
open ProgInfo

(* route and shuffle function names *)
let route_for p map_id = "route_to_"^map_name_of p map_id
(* we don't really need the lhs_map to the shuffle but it makes things a little
 * clearer in the code *)
let shuffle_for p stmt_id rhs_map_id lhs_map_id = 
  "shuffle_s"^string_of_int stmt_id^"_"^map_name_of p rhs_map_id^"_"^
  map_name_of p lhs_map_id

let dim_bounds_type = wrap_tlist @: wrap_ttuple [t_int, t_int]
let bmod_types = wrap_tlist @: wrap_ttuple [t_int, t_int]
let free_dims_type = bmod_types
let free_domains_type = wrap_tlist @: wrap_ttuple [t_int; wrap_tlist t_int]
let inner_cart_prod_type = bmod_types
let free_cart_prod_type = wrap_tlist @: wrap_tlist @: wrap_ttuple [t_int; t_int]
let free_bucket_type = wrap_tlist @: wrap_ttuple [t_int; t_int]
let sorted_ip_list_type = wrap_tlist @: wrap_ttuple [t_ip; wrap_tlist t_ip]

let calc_dim_bounds_code = mk_global_fn "calc_dim_bounds" 
  ["bmod", bmod_types] (*args*) [dim_bounds_type] (* return *)
  (mk_agg 
    (mk_assoc_lambda 
      (ATuple["xs", wrap_tlist @: wrap_ttuple [t_int, t_int];
        "acc_size", t_int])
      (ATuple["pos", t_int; "bin_size", t_int]) @:
      mk_tuple [mk_combine xs @: mk_tuple [mk_var "pos"; mk_var "acc_size"];
        mk_mult (mk_var "bin_size") (mk_var "acc")]
    )
    (mk_tuple [mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int];
      mk_const CInt(1)]
    )
    (mk_var "bmod")


let route_fn p map_id = 
  let map_types = map_types_for p map_id in
  let map_range = create_range 1 @: List.length map_types in
  let to_id x = "__id_"^string_for_int x in
  let map_ids = List.map to_id map_range in
  let map_ids_types = list_zip map_ids map_types in
  in
  mk_global_fn (route_for p map_id)
    ["bmod", bmod_types; "key", wrap_ttuple map_types]
    [wrap_tlist t_ip] (* return *)
    (mk_let "dim_bounds" dim_bounds_type 
      (mk_apply "calc_dim_bounds" @: mk_var "bmod")
    (mk_let_many map_ids_types (mk_var "key") @:
    (* calc_bound_bucket *)
    mk_let "bound_bucket" t_int 
    (List.fold_left
      (fun acc_code index -> 
        let id_var = mk_var @: to_id index in
        mk_add 
          (mk_if (mk_eq id_var CNothing) (mk_const @: CInt 0)
            (mk_let "value" (t_int) @:
              (mk_apply (mk_var "mod") @:
                mk_tuple
                  [mk_apply (mk_var "hash") id_var;
                   mk_fst bmod_types @: mk_peek @:
                     mk_slice (mk_var "bmod") @:
                       mk_tuple [mk_const @: CInt index; mk_const CUnknown]]
                
              ) @:
            mk_mult
              (mk_var "value") @:
               mk_slice (mk_var "dim_bounds") @:
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
            (mk_empty free_dims_type)
            (mk_slice (mk_var "bmod") @: 
              mk_tuple [mk_var @: to_id x; mk_const CUnknown])
          ) acc_code
        (mk_empty free_dims_type)
        (map_range) @:
    mk_let "free_domains" free_domains_type
      (mk_map
        (mk_lambda (ATuple["i", t_int; "b_i", t_int]) @:
          mk_tuple [mk_var "i"; mk_range TList 0 1 @: mk_add b_i (-1)]
        )
        (mk_var "free_dims")
      )
    mk_let "free_cart_prod" free_cart_prod_type
      (mk_agg
        (mk_assoc_lambda (AVar("prev_cart_prod", free_cart_prod_type))
          (ATuple("i", t_int; "domain", wrap_tlist t_int))
          (mk_map
            (* for every domain element in the domain *)
            (mk_lambda (AVar("domain_element", t_int))
              mk_if (mk_is_empty "prev_cart_prod" free_cart_prod_type)
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
      (mk_gba
        (mk_lambda (wrap_args ["ip", ip_t]) @: mk_var "ip")
        (mk_lambda (wrap_args ["acc", wrap_tlist ip_t; "ip", ip_t]) @:
          mk_combine (mk_var "ip") @: mk_var "acc"
        )
        (mk_empty @: wrap_tlist ip_t) @:
        mk_map
          (mk_lambda (wrap_args ["free_bucket", free_bucket_type]) @:
            mk_apply (mk_var "get_ring_node") @:
              mk_agg
                (mk_assoc_lambda (wrap_args ["acc", t_int])
                  (wrap_args ["i", t_int; "val", t_int]) @:
                  mk_add (mk_var "acc") @:
                    mk_mult (mk_var "val") @:
                      mk_fst (wrap_ttuple [t_int; t_int]) @:
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
      (mk_apply (mk_var "get_ring_node") @: (* empty ip list *)
        mk_var "bound_bucket"
      ) @:
      mk_map
        (mk_fst sorted_ip_list_type) @:
        mk_var "sorted_ip_list"
        







    

  

