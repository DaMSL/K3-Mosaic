open Lazy
open Util
open K3.AST
open K3Helpers
open ProgInfo
open K3Route

(* type for searching for shuffle functions and their bindings *)
(* stmt list * l_map * r_map * (l_map index * r_map index) list * func_name *)
type shuffle_fn_entry = int list * int * int * (int * int) list * string
let shuffle_fn_entries = ref []

let shuffle_for p lhs_map_id rhs_map_id bindings = 
  "shuffle_"^map_name_of p rhs_map_id^"_"^map_name_of p lhs_map_id^
  List.fold_left (fun acc (r,l) -> acc^"_"^string_of_int r^"t"^string_of_int l)
    "" bindings

let get_fn_name ((_,_,_,_,name):shuffle_fn_entry) = name

let find_shuffle stmt_id lhs_id rhs_id = 
  get_fn_name @: 
    List.find 
      (fun (ss,lmap,rmap,_,_) -> rmap = rhs_id && lmap == lhs_id &&
        List.exists (fun x -> x = stmt_id) ss
      ) !shuffle_fn_entries

let find_shuffle_by_binding lhs_id rhs_id binding =
  get_fn_name @: 
    List.find
      (fun (_,lmap,rmap,bind,_) -> rmap = rhs_id && lmap == lhs_id &&
        bind = binding
      ) !shuffle_fn_entries

let add_shuffle_fn stmt_id lmap rmap binding name =
  shuffle_fn_entries := 
    ([stmt_id],lmap,rmap,binding,name)::(!shuffle_fn_entries)

let add_stmt_to_shuffle_fn stmt_id fn_name =
  let match_l, mismatch_l = List.partition 
    (fun f -> get_fn_name f = fn_name)
    !shuffle_fn_entries 
  in 
  match match_l with
    | [] -> raise Not_found
    | [ss,lmap,rmap,bind,nm] ->
        shuffle_fn_entries := (ss@[stmt_id],lmap,rmap,bind,nm)::mismatch_l
    | _ -> invalid_arg "Bad input to add_stmt_to_shuffle_fn" 

let gen_shuffle_fn p lmap rmap bindings fn_name =
  let lmap_types = map_types_for p lmap in
  let rmap_types_with_v = map_types_with_v_for p rmap in
  let tuple_types_unwrap = rmap_types_with_v in
  let tuple_types = wrap_ttuple rmap_types_with_v in
  let result_types = wrap_tlist @: wrap_ttuple [t_addr; tuple_types] in
  let lkey_types = wrap_tmaybes @: key_map_types_for p lmap in
  let id_l = "__id_l" in let id_r = "__id_r" in
  let to_rkey i = int_to_temp_id i id_r in
  let to_lkey i = int_to_temp_id i id_l in
  let lmap_range = mk_tuple_range lmap_types in
  let full_lkey = List.map (* use bindings to construct lkey *)
    (fun x -> try mk_var @: to_rkey @: adjust_key_id_for_v 
        (List.assoc x bindings)
      with Not_found -> mk_var @: to_lkey x) lmap_range
  in
  (* functions to change behavior for non-key routes *)
  let pred = List.length lkey_types > 0 in
  let if_lkey f g = if pred then force f @: g else g in 
  mk_global_fn fn_name
    (if pred then ["l_key", wrap_ttuple lkey_types] else [] @
    ["tuples", wrap_tlist tuple_types; 
    "shuffle_on_empty", canonical TBool])
    [result_types] @: (* return *)
    if_lkey (* only destruct lkey if we have lkey *)
      (* destruct the key *)
      (lazy (mk_destruct_tuple "l_key" lkey_types id_l)) @: 
      mk_let "all_targets" result_types
        (mk_if 
          (mk_eq (mk_var "shuffle_on_empty") @: mk_const @: CBool(true))
          (* in shuffle on empty case, we prepare all the routing that must
           * be done for empty packets *)
          (mk_map
            (mk_lambda (wrap_args ["ip", t_addr]) @:
              mk_tuple [mk_var "ip"; mk_empty @: wrap_tlist tuple_types]
            ) @:
            mk_apply
              (mk_var @: route_for p lmap) @:
              if pred then mk_var "l_key" else mk_const CUnit
          ) @:
          mk_empty result_types
        ) @:
      mk_gbagg (* sort by IPs *)
        (mk_lambda (wrap_args ["ip", t_addr; "tuple", tuple_types]) @: 
          mk_var "ip" (* grouping func *)
        )
        (mk_assoc_lambda (wrap_args ["acc", wrap_tlist tuple_types]) 
          (wrap_args ["ip", t_addr; "tuple", tuple_types]) @:
          mk_combine 
            (mk_singleton (wrap_tlist tuple_types) @: mk_var "tuple") @: 
            mk_var "acc"
        )
        (mk_empty @: wrap_tlist tuple_types) @: (* [] *)
        mk_combine 
          (mk_var "all_targets") @:
          mk_flatten @: mk_map
            (mk_lambda (wrap_args ["r_tuple", tuple_types]) @:
              (* start with partial l_key and build up an l_key using data
               * from the tuple, that can be used for routing *)
              mk_destruct_tuple "r_tuple" tuple_types_unwrap id_r @:
              if_lkey (* only evaluate full lkey if we have lkey *)
                (lazy (mk_let "full_lkey" (wrap_ttuple lkey_types)
                  (mk_tuple full_lkey)
                ))
                (mk_map
                  (mk_lambda (wrap_args ["ip", t_addr]) @:
                    mk_tuple [mk_var "ip"; mk_var "r_tuple"]
                  ) @:
                  mk_apply (* route each full l_key *)
                    (mk_var @: route_for p lmap) @:
                      mk_tuple 
                        [if pred then mk_var "full_lkey" else mk_const CUnit] 
                )
              ) @:
              mk_var "tuples"

let gen_shuffle_functions p trig =
  let trig_data = s_and_over_stmts_in_t p lhs_rhs_of_stmt trig in
  List.fold_left
    (fun acc (s, (lmap, rmap)) ->
      let bindings = get_map_bindings_in_stmt p s lmap rmap in
      try let shuffle_fn = find_shuffle_by_binding lmap rmap bindings in
        add_stmt_to_shuffle_fn s shuffle_fn; (* increment in list *)
        acc (* don't add *)
      with Not_found -> 
        let name = shuffle_for p lmap rmap bindings in
        add_shuffle_fn s lmap rmap bindings name;
        acc@[gen_shuffle_fn p lmap rmap bindings name] (* add to list *)
    )
    []
    trig_data

