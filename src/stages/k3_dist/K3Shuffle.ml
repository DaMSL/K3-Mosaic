open K3Dist
open Lazy
open Util
open K3.AST
open K3Helpers
open ProgInfo
open K3Route

(* type for searching for shuffle functions and their bindings *)
(* stmt list * r_map * l_map * (r_map index * l_map index) list * func_name *)

let shuffle_bitmap = create_ds "shuffle_bitmap" @@ wrap_tvector t_bool

let shuffle_indices =
  create_ds "shuffle_indices" @@ wrap_tbag t_int

let shuffle_result =
  let e = ["has_data", t_bool; "indices", shuffle_indices.t] in
  create_ds "shuffle_result" @@ wrap_tvector' @@ snd_many e

(* Part of the name is the binding pattern. So long as the combination of
 * binding patterns is the same, we can use the same shuffle function *)
let shuffle_for p rhs_map_id lhs_map_id bindings =
  let binds = List.map (fun (r, l) -> sp "%dt%d" r l) @@
    IntMap.to_list bindings in
  let bind_s = String.concat "_" binds in
  let bind_s = if bind_s = "" then "" else "_bind_"^bind_s in
  sp "shuffle_%s_to_%s%s" (map_name_of p rhs_map_id) (map_name_of p lhs_map_id) bind_s

let find_shuffle_by_binding shuffle_fns r l b =
  match List.partition (fun fn ->
      fn.rmap = r && fn.lmap = l && IntMap.equal (fun x y -> x - y = 0) fn.binding b) shuffle_fns with
  | [], _       -> raise Not_found
  | [is], isnot -> is, isnot
  | _, _        -> failwith "too many functions"

let gen_shuffle_fn p rmap lmap bindings fn_name =
  let tuple_types = map_types_with_v_for p rmap in
  let tuple_col_t = wrap_t_calc' tuple_types in
  (* whether it's a fake send or a real send *)
  let tup_option_t = wrap_ttuple [t_bool; tuple_col_t] in
  let map_elem_t = wrap_ttuple [t_int; tup_option_t] in
  let result_types = wrap_tmap map_elem_t in
  (* deducts the last map type which is the value *)
  let lkey_types = wrap_tupmaybes @@ map_types_no_val_for p lmap in
  (* lkey refers to the access pattern from trig args. rkey is from the tuples*)
  let id_l, id_r = "lkey_" , "rkey_" in
  let to_rkey, to_lkey = int_to_temp_id id_r, int_to_temp_id id_l in
  let map_range = Array.of_list @@ List.map (fun x -> `Lkey x) @@ mk_tuple_range lkey_types in
  (* use bindings to construct lkey. *)
  List.iteri (fun i _ ->
      try
        let dest = IntMap.find i bindings in
        map_range.(dest) <- `Rkey(adjust_key_id_for_v i)
      with Not_found -> ()) @@
    map_types_for p rmap;
  let full_key_vars' = Array.to_list map_range in
  let used_rkeys = filter_map (function
    | `Rkey i -> Some (mk_var @@ to_rkey i, at tuple_types i)
    | _ -> None) full_key_vars' in
  (* if we have all lkeys, we can lift the route and do it once *)
  let all_lkeys =
    List.for_all (function `Lkey _ -> true | _ -> false) full_key_vars' in
  let no_lkeys =
    List.for_all (function `Rkey _ -> true | _ -> false) full_key_vars' in
  let convert_keys ?(use_rkeys=true) l = List.map (function
      | `Lkey i -> mk_var @@ to_lkey i
      | `Rkey i when use_rkeys -> mk_tup_just @@ mk_var @@ to_rkey i
      | `Rkey i -> mk_tup_nothing @@ List.nth tuple_types i) l  in
  let full_key_vars = convert_keys full_key_vars' in
  let no_rkey_key_vars = convert_keys ~use_rkeys:false full_key_vars' in

  (* functions to change behavior for non-key routes *)
  let pred = List.length lkey_types > 0 in
  let tuples, shuffle_on_empty = "tuples", "shuffle_on_empty" in
  let l_key_ids_types = types_to_ids_types id_l lkey_types in
  let ip_pat = [mk_var "ip"; mk_cunknown] in

  mk_global_fn fn_name
  ((if pred then l_key_ids_types else ["_", t_unit]) @
    [shuffle_on_empty, t_bool; tuples, tuple_col_t])
    [result_types] @@

      (* if we have only lkeys, we only need to route once *)
      if all_lkeys then
        mk_block [
          mk_apply' (route_for p lmap) @@
            mk_cint lmap :: if pred then full_key_vars else [mk_cunit];
          mk_snd @@ mk_agg
            (mk_lambda2' ["ip", t_int; "acc", result_types] ["has_val", t_bool] @@
              mk_block [
                mk_if (mk_var "has_val")
                  (mk_insert "acc" [mk_var "ip"; mk_tuple [mk_ctrue; mk_var "tuples"]])
                  mk_cunit
                ;
                mk_tuple [mk_add (mk_var "ip") @@ mk_cint 1; mk_var "acc"]
              ])
            (mk_tuple [mk_cint 0; mk_empty result_types]) @@
            mk_var K3Route.route_bitmap.id
        ]
      (* else, full shuffling *)
      else
        mk_let ["normal_targets"]
          (* if we have no lkeys in our final keys, we can avoid the pre-aggregation
           * of relevant tuples *)
          (if no_lkeys then
            (* find ip of each tuple *)
            mk_agg
              (mk_lambda2' ["acc", result_types] ["x", wrap_ttuple tuple_types] @@
                mk_destruct_tuple "x" tuple_types id_r @@
                mk_block [
                  (* ips from route *)
                  mk_apply'
                    (route_for p lmap) @@
                      mk_cint lmap :: if pred then full_key_vars else [mk_cunit];
                  mk_snd @@ mk_agg
                    (mk_lambda2' ["ip", t_int; "acc", result_types] ["has_val", t_bool] @@
                      mk_block [
                        mk_if (mk_var "has_val")
                          (mk_upsert_with "acc" ip_pat
                            (mk_lambda'' unit_arg @@
                              mk_tuple [mk_var "ip";
                                        mk_tuple [mk_ctrue;
                                                  mk_singleton tuple_col_t [mk_var "x"]]]) @@
                            mk_lambda' ["y", map_elem_t] @@
                              mk_insert_block ~path:[2; 2] "y" [mk_var "x"])
                          mk_cunit ;
                        mk_tuple [mk_add (mk_var "ip") @@ mk_cint 1; mk_var "acc"]
                    ])
                    (mk_tuple [mk_cint 0; mk_var "acc"]) @@
                    mk_var @@ K3Route.route_bitmap.id
                ])
              (mk_empty result_types) @@
              mk_var "tuples"
          else
            (* we have some lkeys. Pre-aggregate by rkeys *)
            (* find ip of each group of tuples *)
            mk_agg
              (mk_lambda2'
                 ["acc", result_types]
                 ["_u", wrap_ttuple @@ snd_many used_rkeys; "xs", tuple_col_t] @@
                mk_let ["x"] (mk_peek_or_error "whoops2" @@ mk_var "xs") @@
                mk_destruct_tuple "x" tuple_types id_r @@
                mk_block [
                  (* ips from route *)
                  mk_apply' (* route a sample tuple *)
                    (route_for p lmap) @@
                    mk_cint lmap :: if pred then full_key_vars else [mk_cunit];
                  (* add xs to ip for group *)
                  mk_snd @@ mk_agg
                    (mk_lambda2' ["ip", t_int; "acc", result_types] ["has_val", t_bool] @@
                     mk_block [
                       mk_if (mk_var "has_val")
                         (mk_upsert_with "acc" ip_pat
                           (mk_lambda'' unit_arg @@ mk_tuple
                             [mk_var "ip"; mk_tuple [mk_ctrue; mk_var "xs"]]) @@
                           mk_lambda' ["y", map_elem_t] @@
                             mk_extend_block ~path:[2; 2] "y" @@ mk_var "xs")
                         mk_cunit ;
                       mk_tuple [mk_add (mk_var "ip") @@ mk_cint 1; mk_var "acc"]
                    ])
                    (mk_tuple [mk_cint 0; mk_var "acc"]) @@
                    mk_var K3Route.route_bitmap.id
                ])
              (mk_empty result_types) @@
              (* group by meaningful rtuple ids *)
              mk_gbagg
                (mk_lambda'' ["x", wrap_ttuple tuple_types] @@
                  mk_destruct_tuple "x" tuple_types id_r @@
                  mk_tuple @@ fst_many used_rkeys)
                (mk_lambda2' ["acc", tuple_col_t] ["x", wrap_ttuple tuple_types] @@
                  mk_insert_block "acc" [mk_var "x"])
                (mk_empty tuple_col_t) @@
                mk_var "tuples") @@

        (* extra targets for when we have to send all possible ips *)
        mk_if (mk_var shuffle_on_empty)
          (* if we get shuffle_on_empty, add empty targets for all ips *)
          (mk_block [
            mk_apply' (route_for p lmap) @@
                mk_cint lmap :: if pred then no_rkey_key_vars else [mk_cunit];
            mk_snd @@ mk_agg
             (mk_lambda2' ["ip", t_int; "acc", result_types] ["has_val", t_bool] @@
              mk_block [
                mk_if (mk_var "has_val")
                  (mk_upsert_with "acc" ip_pat
                    (mk_lambda'' unit_arg @@ mk_tuple
                      [mk_var "ip"; mk_tuple [mk_cfalse; mk_empty tuple_col_t]]) @@
                    mk_id_fn' (unwrap_ttuple map_elem_t))
                  mk_cunit
                ;
                mk_tuple [mk_add (mk_var "ip") @@ mk_cint 1; mk_var "acc"]
              ])
             (mk_tuple [mk_cint 0; mk_var "normal_targets"])
             (mk_var K3Route.route_bitmap.id)
            ]) @@
          mk_var "normal_targets"

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

(* generate all shuffle functions *)
let functions c =
  List.map (fun x -> gen_shuffle_fn c.p x.rmap x.lmap x.binding x.name) c.shuffle_meta

let globals =
  List.map decl_global
    [ shuffle_bitmap;
      shuffle_result;
    ]


