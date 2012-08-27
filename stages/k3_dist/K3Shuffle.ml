open Util
open K3Helpers
open ProgInfo
open K3Route

exception FunctionNotFound of string

(* type for searching for shuffle functions and their bindings *)
(* stmt list * l_map * r_map * (l_map index * r_map index) list * func_name *)
type shuffle_fn_entry = int list * int * int * (int * int) list * string
let shuffle_fn_entries = ref []

let get_fn_name ((_,_,_,_,name):shuffle_fn_entry) -> name

let find_shuffle stmt_id lhs_id rhs_id = 
  get_fn_name @: List.find 
    (fun (ss,lmap,rmap,_,_) -> rmap = rhs_id && lmap == lhs_id &&
      List.exists (fun x -> x = stmt_id) ss
    ) !shuffle_fn_entries

let find_shuffle_by_binding lhs_id rhs_id binding =
  get_fn_name @: List.find
    (fun (_,lmap,rmap,bind,_) -> rmap = rhs_id && lmap == lhs_id &&
      bind = binding
    ) !shuffle_fn_entries

let add_shuffle_fn stmt_id lmap rmap binding name =
  shuffle_fn_entries := 
    ([stmt_id],lmap,rmap,binding,name)::(!shuffle_fn_entries)

let add_stmt_to_shuffle_fn stmt_id fn_name =
  ((ss,lmap,rmap,bind,nm),part_list) = List.partition 
    (fun (_,_,_,_,name) -> name=fn_name)
    !shuffle_fn_entries in
  shuffle_fn_entries := (ss@stmt_id,lmap,rmap,bind,nm)::part_list
  
let shuffle_for p lhs_map_id rhs_map_id bindings = 
  "shuffle_"^map_name_of p rhs_map_id^"_"^map_name_of p lhs_map_id^
  List.fold_left (fun acc (r,l) -> acc^"_"^string_of_int r^"t"^string_of_int l)
    "" bindings

let gen_shuffle_fn p lmap rmap bindings fn_name =
  let lmap_types = map_types_for p lmap in
  let rmap_types = map_types_for p rmap in
  let rmap_types_with_v = map_types_with_v_for p rmap in
  let route_to_l = route_for p lmap_id in
  let tuple_types = wrap_ttuple rmap_types_with_v in
  let result_types = wrap_tlist @: wrap_ttuple [t_ip; tuple_types] in
  let lkey_types = wrap_ttuple @: wrap_tmaybe lmap_types in
  let id_l = "__id_l" in let id_r = "__id_r" in
  let to_rkey i = int_to_temp_id i id_r in
  let to_lkey i = int_to_temp_id i id_l in
  in
  mk_global_fn fn_name
    ["l_bmod", bmod_types; "l_key", lkey_types;
    "tuples", wrap_tlist @: wrap_ttuple lmap_types_with_v; 
    "shuffle_on_empty", canonical TBool]
    [wrap_tlist @: wrap_ttuple lmap_types_with_v] @: (* return *)
    mk_destruct_tuple "l_key" lkey_types id_l @: (* destruct the key *)
    mk_let "all_targets" result_types
      (mk_if 
        (mk_eq (mk_var "shuffle_on_empty") @: mk_const @: CBool(true))
        (* in shuffle on empty case, we prepare all the routing that must
         * be done for empty packets *)
        (mk_map
          (mk_lambda (wrap_args ["ip", ip_t]) @:
            mk_tuple [mk_var "ip"; mk_empty @: wrap_tlist tuple_types]
          ) @:
          mk_apply
            (mk_var route_for p lmap) @:
            mk_var "l_key"
        ) @:
        mk_empty result_types
      ) @:
    mk_gba (* sort by IPs *)
      let arg_types = wrap_args ["ip", ip_t; "tuple", tuple_types] in
      (mk_lambda (arg_types) @: mk_var "ip") (* grouping func *)
      (mk_assoc_lambda (wrap_args ["acc"]) (arg_types) @: 
        mk_const (mk_var "tuple") @: mk_var "acc"
      )
      (mk_var "all_targets") @:
      mk_flatten @: mk_map
        (mk_lambda (wrap_args ["r_tuple", tuple_types]) @:
          (* start with partial l_key and build up an l_key that can be used for
           * routing *)
          mk_destruct_tuple "tuple" tuple_types id_r @:
          mk_let "full_lkey" lkey_types 
            (let range = create_range 0 (List.length lmap_types) in
            let full_lkey = List.map
              (fun x -> try to_rkey (List.assoc x bindings)
                with Not_found -> to_lkey x)
              range
            in
            mk_tuple @: full_lkey
            ) @:
          mk_map 
            (mk_lambda (wrap_args ["ip"; t_ip]) @:
              mk_tuple [mk_var "ip"; mk_var "r_tuple"]
            ) @:
            mk_apply
              (route_for p lmap_id) @:
                mk_tuple [mk_var "l_bmod"; mk_var "full_lkey"] 

let gen_shuffle_functions p trig =
  let trig_data = s_and_over_stmts_in_t p lhs_rhs_of_stmt in
  List.flatten @: List.map
    (fun (s, (lhs, rhs)) ->
      let bindings = get_map_bindings_in_stmt p s lhs rhs in
      try let shuffle_fn = find_shuffle_by_binding rhs lhs bindings in
        add_stmt_to_shuffle_fn s shuffle_fn; (* increment in list *)
        []
      with Not_found -> 
        let name = shuffle_for p lmap rmap bindings in
        gen_shuffle_fn s lmap rmap bindings name; (* add to list *)
        [get_shuffle_fn p lhs rhs bindings name]
    )
    trig_data

