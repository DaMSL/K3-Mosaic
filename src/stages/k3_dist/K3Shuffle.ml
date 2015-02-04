open K3Dist
open Lazy
open Util
open K3.AST
open K3Helpers
open ProgInfo
open K3Route

(* type for searching for shuffle functions and their bindings *)
(* stmt list * r_map * l_map * (r_map index * l_map index) list * func_name *)

(* Part of the name is the binding pattern. So long as the combination of
 * binding patterns is the same, we can use the same shuffle function *)
let shuffle_for p rhs_map_id lhs_map_id bindings =
  let binds = List.map (fun (r, l) -> Printf.sprintf "%dt%d" r l) bindings in
  let bind_s = String.concat "_" binds in
  let bind_s = if bind_s = "" then "" else "_bind_"^bind_s in
  Printf.sprintf "shuffle_%s_to_%s%s" (map_name_of p rhs_map_id) (map_name_of p lhs_map_id) bind_s

let find_shuffle_by_binding shuffle_fns r l b =
  match List.partition (fun fn -> fn.rmap = r && fn.lmap = l && fn.binding = b) shuffle_fns with
  | [is], isnot -> is, isnot
  | _           -> raise Not_found

let gen_shuffle_fn p rmap lmap bindings fn_name =
  let tuple_types_unwrap = map_types_with_v_for p rmap in
  let tuple_types = wrap_ttuple tuple_types_unwrap in
  let many_tuples_type = wrap_t_of_map tuple_types in
  let base_result_type = [t_addr; many_tuples_type] in
  let result_types = wrap_tbag' base_result_type in
  let all_targets_type = wrap_tbag' base_result_type in
  (* deducts the last map type which is the value *)
  let lkey_types = wrap_tmaybes @@ map_types_no_val_for p lmap in
  (* lkey refers to the access pattern from trig args. rkey is from the tuples*)
  let id_l = "lkey_" in let id_r = "rkey_" in
  let to_rkey i = int_to_temp_id id_r i in
  let to_lkey i = int_to_temp_id id_l i in
  let lmap_range = mk_tuple_range lkey_types in
  let full_lkey_vars =
    List.map (* use bindings to construct lkey. Also tuple -> just var *)
      (fun x -> try mk_just @@ mk_var @@ to_rkey @@ adjust_key_id_for_v
          (List.assoc x bindings)
        with Not_found -> mk_var @@ to_lkey x)
      lmap_range
  in
  (* functions to change behavior for non-key routes *)
  let pred = List.length lkey_types > 0 in
  let tuples, shuffle_on_empty = "tuples", "shuffle_on_empty" in
  let l_key_ids_types = types_to_ids_types id_l lkey_types in
  mk_global_fn fn_name
  ((if pred then l_key_ids_types else ["_", t_unit]) @
    [tuples, many_tuples_type;
    shuffle_on_empty, canonical TBool])
    [result_types] @@ (* return *)
      mk_let ["all_targets"]
        (mk_if
          (mk_eq (mk_var shuffle_on_empty) @@ mk_cbool true)
          (* in shuffle on empty case, we prepare all the routing that must
           * be done for empty packets *)
          (mk_map
            (mk_lambda (wrap_args ["ip", t_addr]) @@
              mk_tuple [mk_var "ip"; mk_empty @@ wrap_t_of_map tuple_types]
            ) @@
            mk_apply
              (mk_var @@ route_for p lmap) @@
                mk_tuple @@ mk_cint lmap ::
                  if pred then ids_to_vars @@ fst_many l_key_ids_types
                  else [mk_cunit]
          ) @@
          mk_empty all_targets_type
        ) @@
      mk_gbagg (* sort by IPs *)
        (mk_lambda (wrap_args ["ip", t_addr; "tuple", many_tuples_type]) @@
          mk_var "ip" (* grouping func *)
        )
        (* we don't need uniqueness here since tuples are supposed to be unique
         * as it is *)
        (mk_assoc_lambda (wrap_args ["acc", many_tuples_type])
          (wrap_args ["ip", t_addr; "tuple", many_tuples_type]) @@
          mk_combine (mk_var "tuple") @@ mk_var "acc"
        )
        (mk_empty @@ many_tuples_type) @@ (* [] *)
        mk_combine
          (mk_var "all_targets") @@
          mk_flatten @@
          mk_map
            (mk_lambda (wrap_args ["r_tuple", tuple_types]) @@
              (* start with partial l_key and build up an l_key using data
               * from the tuple, that can be used for routing *)
              mk_destruct_tuple "r_tuple" tuple_types_unwrap id_r @@
                (mk_map
                  (mk_lambda (wrap_args ["ip", t_addr]) @@
                    mk_tuple [mk_var "ip";
                      mk_singleton many_tuples_type [mk_var "r_tuple"]]) @@
                  mk_apply (* route each full l_key *)
                    (mk_var @@ route_for p lmap) @@
                      mk_tuple @@ mk_cint lmap :: if pred then full_lkey_vars
                                                  else [mk_cunit])) @@
            mk_var tuples

(* generate all meta information about functions *)
let gen_meta p =
  let gen_trig_meta trig =
    let trig_data = s_and_over_stmts_in_t p rhs_lhs_of_stmt trig in
    List.fold_left
      (fun acc_meta (stmt, (rmap, lmap)) ->
        let binding = get_map_bindings_in_stmt p stmt rmap lmap in
        try
          let fn, others = find_shuffle_by_binding acc_meta rmap lmap binding in
          let fn = {fn with stmts=IntSet.add stmt fn.stmts} in
          fn :: others
        with Not_found ->
          let name = shuffle_for p rmap lmap binding in
          let stmts = IntSet.singleton stmt in
          let fn = {name; rmap; lmap; binding; stmts} in
          fn :: acc_meta)
      [] trig_data
  in
  for_all_trigs p @@ gen_trig_meta

(* generate all shuffle functions *)
let functions c =
  List.map (fun x -> gen_shuffle_fn c.p x.rmap x.lmap x.binding x.name) c.shuffle_meta

(* external function to find a shuffle function *)
let find_shuffle_nm c s rmap lmap =
  try
    let f = List.find (fun x -> x.rmap = rmap && x.lmap = lmap && IntSet.mem s x.stmts) c.shuffle_meta
    in f.name
  with
    Not_found -> failwith @@
      Printf.sprintf "Couldn't find shuffle for stmt %d, rmap %d, lmap %d" s rmap lmap

