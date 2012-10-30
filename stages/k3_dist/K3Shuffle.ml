open Lazy
open Util
open K3.AST
open K3Helpers
open ProgInfo
open K3Route

(* type for searching for shuffle functions and their bindings *)
(* stmt list * r_map * l_map * (r_map index * l_map index) list * func_name *)
type shuffle_fn_entry = int list * int * int * (int * int) list * string
let shuffle_fn_entries = ref []

exception NoShuffle of string

let string_of_stmts = List.fold_left (fun acc i -> acc^string_of_int i^" ") ""
let string_of_binds = 
  List.fold_left (fun acc (r,l) -> acc^"r:"^string_of_int r^" l:"^string_of_int
  l) "" 
let string_of_shuffles () = 
  List.fold_left 
    (fun acc (ss,r,l,bb,nm) -> acc^"ss:("^string_of_stmts ss^") r:"^
    string_of_int r^" l:"^string_of_int l^" bb:("^string_of_binds bb^
    ") nm:"^nm^"\n") "" !shuffle_fn_entries

let shuffle_for p rhs_map_id lhs_map_id bindings = 
  "shuffle_"^map_name_of p rhs_map_id^"_"^map_name_of p lhs_map_id^
  List.fold_left (fun acc (r,l) -> acc^"_"^string_of_int r^"t"^string_of_int l)
    "" bindings

let get_fn_name ((_,_,_,_,name):shuffle_fn_entry) = name

let find_shuffle stmt_id rhs_id lhs_id = 
  get_fn_name @: 
    try List.find 
      (fun (ss,rmap,lmap,_,_) -> rmap = rhs_id && lmap == lhs_id &&
        List.exists (fun x -> x = stmt_id) ss
      ) !shuffle_fn_entries
    with
    Not_found -> raise (NoShuffle ("Couldn't find shuffle for stmt "^
      string_of_int stmt_id^ " rhs_map "^ string_of_int rhs_id^" lhs_map "^
      string_of_int lhs_id^"\n\n"^string_of_shuffles ()))

let find_shuffle_by_binding rhs_id lhs_id binding =
  get_fn_name @: 
    List.find
      (fun (_,rmap,lmap,bind,_) -> rmap = rhs_id && lmap == lhs_id &&
        bind = binding
      ) !shuffle_fn_entries

let add_shuffle_fn stmt_id rmap lmap binding name =
  shuffle_fn_entries := 
    ([stmt_id],rmap,lmap,binding,name)::(!shuffle_fn_entries)

let add_stmt_to_shuffle_fn stmt_id fn_name =
  let match_l, mismatch_l = List.partition 
    (fun f -> get_fn_name f = fn_name)
    !shuffle_fn_entries 
  in 
  match match_l with
    | [] -> raise Not_found
    | [ss,rmap,lmap,bind,nm] ->
        shuffle_fn_entries := (ss@[stmt_id],rmap,lmap,bind,nm)::mismatch_l
    | _ -> invalid_arg "Bad input to add_stmt_to_shuffle_fn" 

let gen_shuffle_fn p rmap lmap bindings fn_name =
  let tuple_types_unwrap = map_types_with_v_for p rmap in
  let tuple_types = wrap_ttuple tuple_types_unwrap in
  let list_of_tuples = wrap_tlist tuple_types in
  let result_types = wrap_tlist @: wrap_ttuple [t_addr; list_of_tuples] in
  (* deducts the last map type which is the value *)
  let lkey_types = wrap_tmaybes @: key_map_types_for p lmap in
  let id_l = "__id_l" in let id_r = "__id_r" in
  let to_rkey i = int_to_temp_id i id_r in
  let to_lkey i = int_to_temp_id i id_l in
  let lmap_range = mk_tuple_range lkey_types in
  let full_lkey = 
    List.map (* use bindings to construct lkey. Also tuple -> just var *)
      (fun x -> try mk_just @: mk_var @: to_rkey @: adjust_key_id_for_v 
          (List.assoc x bindings)
        with Not_found -> mk_var @: to_lkey x) 
      lmap_range
  in
  (* functions to change behavior for non-key routes *)
  let pred = List.length lkey_types > 0 in
  let if_lkey f g = if pred then force f @: g else g in 
  mk_global_fn fn_name
    ((if pred then ["l_key", wrap_ttuple lkey_types] else []) @
    ["tuples", list_of_tuples; 
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
        (mk_lambda (wrap_args ["ip", t_addr; "tuple", list_of_tuples]) @: 
          mk_var "ip" (* grouping func *)
        )
        (mk_assoc_lambda (wrap_args ["acc", wrap_tlist tuple_types]) 
          (wrap_args ["ip", t_addr; "tuple", list_of_tuples]) @:
          mk_combine (mk_var "tuple") @: mk_var "acc"
        )
        (mk_empty @: wrap_tlist tuple_types) @: (* [] *)
        mk_combine 
          (mk_var "all_targets") @:
          mk_flatten @: 
          mk_map
            (mk_lambda (wrap_args ["r_tuple", tuple_types]) @:
              (* start with partial l_key and build up an l_key using data
               * from the tuple, that can be used for routing *)
              mk_destruct_tuple "r_tuple" tuple_types_unwrap id_r @:
              if_lkey (* only evaluate full lkey if we have lkey *)
                (lazy (mk_let "full_lkey" (wrap_ttuple lkey_types) @:
                  mk_tuple full_lkey
                ))
                (mk_map
                  (mk_lambda (wrap_args ["ip", t_addr]) @:
                    mk_tuple [mk_var "ip"; 
                      mk_singleton list_of_tuples @: mk_var "r_tuple"]
                  ) @:
                  mk_apply (* route each full l_key *)
                    (mk_var @: route_for p lmap) @:
                      mk_tuple 
                        [if pred then mk_var "full_lkey" else mk_const CUnit] 
                )
              ) @:
              mk_var "tuples"

let gen_shuffle_functions p trig =
  let trig_data = s_and_over_stmts_in_t p rhs_lhs_of_stmt trig in
  List.fold_left
    (fun acc (s, (rmap, lmap)) ->
      let bindings = get_map_bindings_in_stmt p s rmap lmap in
      try let shuffle_fn = find_shuffle_by_binding rmap lmap bindings in
        add_stmt_to_shuffle_fn s shuffle_fn; (* increment in list *)
        acc (* don't add *)
      with Not_found -> 
        let name = shuffle_for p rmap lmap bindings in
        add_shuffle_fn s rmap lmap bindings name;
        acc@[gen_shuffle_fn p rmap lmap bindings name] (* add to list *)
    )
    []
    trig_data

(* function to generate all needed shuffle/route stuff *)
let gen_shuffle_route_code p =
  K3Route.gen_route_code p @
  List.flatten @: List.map (gen_shuffle_functions p) (get_trig_list p)

