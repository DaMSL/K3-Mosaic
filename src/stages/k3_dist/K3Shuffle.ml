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
  let binds = List.map (fun (r, l) -> sp "%dt%d" r l) @@
    IntIntSet.elements bindings in
  let bind_s = String.concat "_" binds in
  let bind_s = if bind_s = "" then "" else "_bind_"^bind_s in
  sp "shuffle_%s_to_%s%s" (map_name_of p rhs_map_id) (map_name_of p lhs_map_id) bind_s

let find_shuffle_by_binding shuffle_fns r l b =
  match List.partition (fun fn -> fn.rmap = r && fn.lmap = l && IntIntSet.equal fn.binding b) shuffle_fns with
  | [], _       -> raise Not_found
  | [is], isnot -> is, isnot
  | _, _        -> failwith "too many functions"

let gen_shuffle_fn p rmap lmap bindings fn_name =
  let bindings = IntIntSet.elements bindings in
  let tuple_types = map_types_with_v_for p rmap in
  let tuple_col_t = wrap_t_calc' tuple_types in
  let result_types = wrap_tbag' [t_addr; tuple_col_t] in
  (* deducts the last map type which is the value *)
  let lkey_types = wrap_tupmaybes @@ map_types_no_val_for p lmap in
  (* lkey refers to the access pattern from trig args. rkey is from the tuples*)
  let id_l, id_r = "lkey_" , "rkey_" in
  let to_rkey, to_lkey = int_to_temp_id id_r, int_to_temp_id id_l in
  let lmap_range = mk_tuple_range lkey_types in
  (* use bindings to construct lkey. Also tuple -> just var *)
  let full_key_vars' = List.map (fun i ->
      try `Rkey(adjust_key_id_for_v @@ List.assoc i bindings)
      with Not_found -> `Lkey i)
    lmap_range in
  (* if we have all lkeys, we can lift the route and do it once *)
  let all_lkeys =
    List.for_all (function `Lkey _ -> true | _ -> false) full_key_vars' in
  let convert_keys l = List.map (function
      | `Lkey i -> mk_var @@ to_lkey i
      | `Rkey i -> mk_tup_just @@ mk_var @@ to_rkey i) l in
  let full_key_vars = convert_keys full_key_vars' in
  let used_rkeys =
    List.map (function `Rkey i -> mk_var @@ to_rkey i, at tuple_types i | _ -> failwith "whoops") @@
    List.filter (function `Rkey _ -> true | _ -> false) full_key_vars' in

  (* functions to change behavior for non-key routes *)
  let pred = List.length lkey_types > 0 in
  let tuples, shuffle_on_empty = "tuples", "shuffle_on_empty" in
  let l_key_ids_types = types_to_ids_types id_l lkey_types in

  mk_global_fn fn_name
  ((if pred then l_key_ids_types else ["_", t_unit]) @
    [shuffle_on_empty, t_bool; tuples, tuple_col_t])
    [result_types] @@

      (* if we have only lkeys, we only need to route once *)
      if all_lkeys then
        (* try to be most efficient (move) for common case *)
        mk_if (mk_eq (mk_size @@ mk_var "ips") @@ mk_cint 1)
          (mk_singleton result_types
            [mk_peek_or_error "whoops" @@ mk_var "ips"; mk_var "tuples"]) @@
          mk_agg
            (mk_lambda2' ["acc_col", result_types] ["ip", t_addr] @@
              mk_block [
                mk_insert "acc_col" [mk_var "ip"; mk_var "tuples"];
                mk_var "acc_col"
              ])
            (mk_empty result_types) @@
            (* ips *)
            (mk_apply' (route_for p lmap) @@
              mk_cint lmap :: if pred then full_key_vars else [mk_cunit])
      (* else, full shuffling *)
      else
        mk_let ["normal_targets"]
          (* find ip of each group of tuples *)
          (mk_flatten @@ mk_map
            (mk_lambda' ["_u", wrap_ttuple @@ snd_many used_rkeys; "xs", tuple_col_t] @@
              mk_let ["x"] (mk_peek_or_error "whoops2" @@ mk_var "xs") @@
              mk_destruct_tuple "x" tuple_types id_r @@
              (* add xs to ip for group *)
              mk_agg
                (mk_lambda2' ["acc", result_types] ["ip", t_addr] @@
                  mk_block [
                    mk_insert "acc" [mk_var "ip"; mk_var "xs"];
                    mk_var "acc"
                  ])
                (mk_empty result_types) @@
                (* ips from route *)
                mk_apply' (* route a sample tuple *)
                  (route_for p lmap) @@
                    mk_cint lmap :: if pred then full_key_vars else [mk_cunit]) @@
            (* group by meaningful rtuple ids *)
            mk_gbagg
              (mk_lambda'' ["x", wrap_ttuple tuple_types] @@
                mk_destruct_tuple "x" tuple_types id_r @@
                mk_tuple @@ fst_many used_rkeys)
              (mk_lambda2' ["acc", tuple_col_t] ["x", wrap_ttuple tuple_types] @@
                mk_block [
                  mk_insert "acc" [mk_var "x"];
                  mk_var "acc"])
              (mk_empty tuple_col_t) @@
              mk_var "tuples") @@

        (* extra targets for when we have to send all possible ips *)
        mk_let ["all_targets"]
          (mk_if (mk_var shuffle_on_empty)
            (* if we get shuffle_on_empty, add empty targets for all ips *)
            (mk_combine
              (mk_var "normal_targets") @@
              (* in shuffle on empty case, we prepare all the routing that must
              * be done for empty packets *)
              (mk_map
                (mk_lambda' ["ip", t_addr] @@
                  mk_tuple [mk_var "ip"; mk_empty tuple_col_t]) @@
                mk_apply'
                  (route_for p lmap) @@
                    mk_cint lmap ::
                      if pred then ids_to_vars @@ fst_many l_key_ids_types
                      else [mk_cunit])) @@
            mk_var "normal_targets") @@

        (* group by IPs *)
        mk_gbagg
          (* grouping func *)
          (mk_lambda' ["ip", t_addr; "xs", tuple_col_t] @@ mk_var "ip")
          (mk_lambda2'
            ["acc", tuple_col_t]
            ["ip", t_addr; "xs", tuple_col_t] @@
              mk_combine (mk_var "acc") @@ mk_var "xs")
          (mk_empty tuple_col_t) @@
          mk_var "all_targets"


(* generate all meta information about functions *)
let gen_meta p =
  let g_meta = ref [] in
  let gen_trig_meta trig =
    let trig_data = s_and_over_stmts_in_t p rhs_lhs_of_stmt trig in
    let meta = List.fold_left
      (fun acc_meta (stmt, (rmap, lmap)) ->
        let binding = get_map_bindings_in_stmt p stmt rmap lmap in
        try
          let fn, others =
            find_shuffle_by_binding acc_meta rmap lmap binding in
          let fn = {fn with stmts=IntSet.add stmt fn.stmts} in
          fn :: others
        with Not_found ->
          let name = shuffle_for p rmap lmap binding in
          let stmts = IntSet.singleton stmt in
          let fn = {name; rmap; lmap; binding; stmts} in
          fn :: acc_meta)
      !g_meta trig_data
    in
    g_meta := meta
  in
  ignore(for_all_trigs ~sys_init:true p @@ gen_trig_meta);
  !g_meta

(* generate all shuffle functions *)
let functions c =
  List.map (fun x -> gen_shuffle_fn c.p x.rmap x.lmap x.binding x.name) c.shuffle_meta

(* external function to find a shuffle function *)
let find_shuffle_nm c s rmap lmap =
  try
    let fs = List.filter (fun x ->
      x.rmap = rmap && x.lmap = lmap && IntSet.mem s x.stmts) c.shuffle_meta in
    begin match fs with
    | []  -> failwith "missing shuffle fn"
    | [f] -> f.name
    | _   -> failwith @@ sp "%d duplicate functions" (List.length fs)
    end
  with
    Not_found -> failwith @@
      sp "Couldn't find shuffle for stmt %d, rmap %d, lmap %d" s rmap lmap

