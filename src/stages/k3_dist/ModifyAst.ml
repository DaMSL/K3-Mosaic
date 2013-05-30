open Util
open K3.AST
open K3Helpers

module U = K3Util
module P = ProgInfo
module T = Tree
module PR = K3Printing
module Set = ListAsSet
module G = K3Global

exception InvalidAst of string

(* get the relative offset of the stmt in the trigger *)
let stmt_idx_in_t p trig stmt = 
  let ss = P.stmts_of_t p trig in
  foldl_until (fun acc s -> if s = stmt then Left acc else Right (acc+1)) 0 ss

(* get the nth member in a block. If it's not a block and we only ask for the
 * first item, return the not-a-block *)
let block_nth exp i = match U.tag_of_expr exp with
  | Block        -> begin
          try List.nth (U.decompose_block exp) i
          with Failure "nth" -> raise (InvalidAst("block_nth: Block has no "^
                                string_of_int i^"th member")) end
  | _ when i = 0 -> exp
  | _            -> raise (InvalidAst("block_nth: Not a block"))

(* return the AST for a given stmt *)
let ast_for_s p ast stmt (trig:P.trig_name_t) = 
  let trig_decl = U.trigger_of_program trig ast in
  let trig_ast = U.expr_of_code trig_decl in
  let s_idx = stmt_idx_in_t p trig stmt in
  block_nth trig_ast s_idx

(* return the corrective args and AST for a given stmt, map, trig *)
let corr_ast_for_m_s p ast map stmt trig =
  let map_name = P.map_name_of p map in
  let s_with_m = P.s_and_over_stmts_in_t p P.rhs_maps_of_stmt trig in
  let s_with_m_filter = List.filter (fun (_,m) -> m = map) s_with_m in
  let s_i = insert_index_snd 0 @: fst @: List.split s_with_m_filter in
  let stmt_idx = List.assoc stmt s_i in

  let trig_name = "correct_"^map_name^"_for_"^trig in
  let trig_decl = 
    try U.trigger_of_program trig_name ast 
    with Not_found -> failwith @: "Missing corrective for "^trig_name in
  let trig_ast = U.expr_of_code trig_decl in
  let args = U.typed_vars_of_arg @: U.args_of_code trig_decl in
  let trig_args = P.args_of_t p trig in 
  (* remove the trigger args from the list of args in the corrective trigger *)
  let args2 = Set.diff args trig_args
  in (args2, block_nth trig_ast stmt_idx)

let maps_with_existing_out_tier p stmt =
  let maps = P.map_names_ids_of_stmt p stmt in
  let lmap = P.lhs_map_of_stmt p stmt in
  ("existing_out_tier", lmap) :: maps

(* check for a non-map (a variable) in a stmt *)
let get_stmt_0d_maps p stmt =
  let maps = maps_with_existing_out_tier p stmt in
  List.filter (fun (_,m) -> P.map_types_no_val_for p m = []) maps
  
(* Change maps with no input dimensions to access by vid *)
let modify_0d_map_access p ast stmt =
  let maps_n_id = get_stmt_0d_maps p stmt in
  let modify_0d_map tree m_nm_id =
    let m = fst m_nm_id in
    let m_t = P.map_types_with_v_for p @: snd m_nm_id in
    (* note: our second pass will add vid to the map lookup *)
    (*let map_lookup = ids_to_vars @: P.map_ids_add_v ["_"] in*)
    let modify e = 
      if U.is_peek e && U.is_var_match m @: U.decompose_peek e 
      then mk_snd m_t @:
             mk_peek @: mk_slice (mk_var m) @:
               mk_tuple [mk_const CUnknown] (* map_lookup *)
      else e in
    T.modify_tree_bu tree modify in
  List.fold_left modify_0d_map ast maps_n_id

exception UnhandledModification of string

(* modify the internals of a type. A function gets both the unwrapped type and
 * the original type (wrapped) in case it wants to use that *)
let modify_tuple_type fn typ = 
  let unwrap_m = function
    | TMutable(t, a)   -> TMutable(fn t typ, a)
    | TImmutable(t, a) -> TImmutable(fn t typ, a)
  in match typ with
    | TIsolated(m)  -> TIsolated(unwrap_m m)
    | TContained(m) -> TContained(unwrap_m m)

(* modify a lambda to have a vid included in its arguments *)
let add_vid_to_lambda_args lambda =
  let body = U.decompose_lambda lambda in
  let vid_avar = AVar("vid", t_vid) in
  let args = U.arg_of_lambda lambda in
  let new_args = match args with
    | Some(ATuple(vs)) -> ATuple(P.map_add_v vid_avar vs)
    | Some(v) -> ATuple(P.map_add_v vid_avar [v])
    | _ -> raise(UnhandledModification("lambda not found")) in
  mk_lambda new_args body

(* add vid to all map accesses *)
let modify_map_access p ast stmt =
  let lmap = P.lhs_map_of_stmt p stmt in
  let lmap_name = P.map_name_of p lmap in
  let lmap_types = P.map_types_with_v_for p lmap in
  let lr_map_names = fst @: List.split @: P.map_names_ids_of_stmt p stmt in
  let maps_n_id = maps_with_existing_out_tier p stmt in
  let var_vid = mk_var "vid" in
  let modify_tuple t =
    match U.tag_of_expr t with
      | Tuple  -> let xs = U.decompose_tuple t in
                  mk_tuple @: P.map_add_v var_vid xs
      | x      -> mk_tuple @: P.map_add_v var_vid [t] 
  in
  (* Sometimes we only find out that a node needs to have a vid from a higher
   * vid. The main use for this right now is for combine *)
  let rec add_vid_from_above e =
    match U.tag_of_expr e with
    | Flatten ->
        let body = U.decompose_flatten e in
        mk_flatten @: add_vid_from_above body
    | Lambda args ->
        let body = U.decompose_lambda e in
        mk_lambda args @: add_vid_from_above body 
    | Map -> let lambda, col = U.decompose_map e in
        mk_map
          (add_vid_from_above lambda) col
    | Combine -> let x, y = U.decompose_combine e in
        mk_combine (add_vid_from_above x) (add_vid_from_above y)
    | Singleton t -> let x = U.decompose_singleton e in
        mk_singleton t @: add_vid_from_above x
    | Apply -> (* this should be a let *)
        let lambda, input = U.decompose_apply e in
        mk_apply (add_vid_from_above lambda) input
    | _ -> modify_tuple e
    (* TODO: handle combining more variables *)
  in

  (* we return whether the higher level needs to be aware of the vid addition,
   * and the new tree node *)
  let rec modify e msgs path = 
    let get_msg i = if null msgs then false else List.nth msgs i in
    match U.tag_of_expr e with
    | Insert -> let (col, elem) = U.decompose_insert e in
      false, mk_insert col @: modify_tuple elem
    | Delete -> let (col, elem) = U.decompose_delete e in
      false, mk_delete col @: modify_tuple elem
    | Slice -> (* first check what's our parent *)
      (* a simple modification of a slice. Useful for structural access to the
       * map, such as when deleting *)
      let modify_slice e = 
        let (col, pat) = U.decompose_slice e in
        mk_slice col @: modify_tuple pat 
      in 
      (* a projected slice modification. 
       * Removes the vid from the slice. In most cases this is what we want *)
      let project_modify_slice e = 
        let (col, pat) = U.decompose_slice e in
        begin match U.tag_of_expr col with 
          | Var id -> 
            let m = try List.assoc id maps_n_id 
              with Not_found -> raise(UnhandledModification("No "^id^
                " map found in stmt "^string_of_int stmt)) in
            let m_id_t = P.map_ids_types_for p m in
            let m_ids = extract_arg_names m_id_t in
            let m_id_t_v = P.map_ids_types_with_v_for p m in 
            (* don't set modified -- looks unmodified because projected *)
            mk_map  
              (mk_lambda (wrap_args m_id_t_v) @:
                mk_tuple @: ids_to_vars @: m_ids
              ) @: modify_slice e
          | _ -> raise (UnhandledModification ("Cannot handle non-var in slice"))
        end 
      in
      let parent = list_head path in
      begin match U.tag_of_expr parent with
       (* if we have delete above us, we need the full structure of the map. If
        * not, we can project onto a collection without the vid *)
        | Iterate -> let (lambda, _) = U.decompose_iterate parent in
          let body = U.decompose_lambda lambda in
          begin match U.tag_of_expr body with
            | Delete -> true, modify_slice e
            | _  -> false, project_modify_slice e
          end
        | _      -> false, project_modify_slice e (* the default *)
      end
     (* handle a case where we're iterating over a collection that's modified *)
    | Iterate -> let (lambda, col) = U.decompose_iterate e in
      if get_msg 1 then 
        let mod_lambda = add_vid_to_lambda_args lambda in
        false, mk_iter mod_lambda col
      else false, e

    (* handle the case of map, which appears in some files *)
    | Map -> let (lambda, col) = U.decompose_map e in
      if get_msg 1 then 
        (* a lower level collection has a vid, so we must include it in our
        * arguments *)
        let mod_lambda = add_vid_to_lambda_args lambda in
        false, mk_map mod_lambda col

      else false, e

    (* handle a case of a lambda applied to an lmap (i.e. a let statement *)
    (* in this case, we modify the types of the lambda vars themselves *)
    | Apply  -> let (lambda, arg) = U.decompose_apply e in
      begin match U.tag_of_expr arg with
        | Var id when id = lmap_name -> 
          begin match (U.typed_vars_of_lambda lambda, U.decompose_lambda lambda) with
            | ([id, t],b) -> false,
              mk_apply 
                (mk_lambda 
                  (wrap_args [id, wrap_tset @: wrap_ttuple lmap_types]) b)
                arg
            | _ -> raise (UnhandledModification(PR.string_of_expr e)) end
        | _ -> false, e
      end
    | Combine -> let x, y = U.decompose_combine e in
      begin match get_msg 0, get_msg 1 with
      | true, true -> true, e
      | true, _    -> true, mk_combine x @: add_vid_from_above y
      | _, true    -> true, mk_combine (add_vid_from_above x) y
      | _, _       -> false, e
      end

    | Var id when List.mem id lr_map_names -> true, e

    | _ -> false, e
  in T.modify_tree_bu_with_path_and_msgs ast modify

(* this delta extraction is very brittle, since it's tailored to the way the M3
 * to K3 calculations are written *)
let modify_delta p ast stmt target_trigger =
  let lmap = P.lhs_map_of_stmt p stmt in
  let lmap_types = P.map_types_with_v_for p lmap in
  let lmap_bindings = P.find_lmap_bindings_in_stmt p stmt lmap in
  let lmap_bind_ids_v = P.map_ids_add_v @: fst @: List.split lmap_bindings in
  let (lambda, arg) = U.decompose_apply ast in
  let body = U.decompose_lambda lambda in
  let params = match U.tag_of_expr lambda with Lambda a -> a 
    | _ -> raise(UnhandledModification("No lambda")) in
  match U.tag_of_expr body with
  | Apply -> (* simple modification - sending a single piece of data *)
    let (lambda2, arg2) = U.decompose_apply body in
    let body2 = U.decompose_lambda lambda2 in
    let params2 = begin match U.tag_of_expr lambda2 with 
      | Lambda a -> a
      | _ -> raise(UnhandledModification("No inner lambda")) end in
    let delta_name = list_head @: U.vars_of_lambda lambda2 in
    let delta_types = extract_arg_types @: U.typed_vars_of_arg params2 in
    let full_vars_code = 
      let full_types = P.map_add_v t_vid @: lmap_types @ delta_types in
      let full_names = lmap_bind_ids_v @ [delta_name] in
      mk_tuple @:
        P.map_add_v (mk_var "vid") @:
          [mk_singleton 
            (wrap_tset @: wrap_ttuple full_types) @:
            mk_tuple @: ids_to_vars full_names 
          ] in
    mk_apply
      (mk_lambda params @:
          mk_apply 
            (mk_lambda params2 @:
                mk_block @:
                  body2 ::
                  [mk_send 
                    (mk_const @: CTarget target_trigger)
                    G.me_var @:
                    full_vars_code
                  ]
            ) arg2
      ) arg
  | Iterate -> (* more complex modification *)
    let (lambda2, col) = U.decompose_iterate body in
    let params2 = begin match U.tag_of_expr lambda2 with 
      | Lambda a -> a
      | _ -> raise(UnhandledModification("No inner lambda")) end in
    let delta_name = "__delta_values__" in
    let delta_ids_types = U.typed_vars_of_arg params2 in
    let delta_types = extract_arg_types delta_ids_types in
    let delta_col_type = wrap_tset @: wrap_ttuple delta_types in
    let delta_ids = extract_arg_names delta_ids_types in
    let delta_last_id = list_take_end 1 delta_ids in
    mk_apply
      (mk_lambda params @:
          mk_let delta_name (delta_col_type) col @:
          mk_block @:
            (mk_iter lambda2 @: 
              mk_var delta_name)::
            [mk_send
              (mk_const @: CTarget target_trigger)
              G.me_var @:
              mk_tuple @:
                (mk_var "vid"):: (* project vid into collection *)
                  [mk_map
                    (mk_lambda 
                      (wrap_args delta_ids_types) @:
                        mk_tuple @: 
                          ids_to_vars @: lmap_bind_ids_v@delta_last_id
                    ) @:
                    mk_var delta_name
                  ]
            ]
      ) 
      arg
  | _ -> raise (UnhandledModification(PR.string_of_expr ast))

(* return a modified version of the original ast for s *)
let modify_ast_for_s p ast stmt trig target_trig = 
  let ast = ast_for_s p ast stmt trig in
  let ast = modify_map_access p ast stmt in
  let ast = modify_0d_map_access p ast stmt in
  (* do we need to send a delta to another trigger *)
  match target_trig with
    | Some t -> modify_delta p ast stmt t
    | None -> ast

(* return a modified version of the corrective update *)
let modify_corr_ast p ast map stmt trig =
  let (args, ast) = corr_ast_for_m_s p ast map stmt trig in
  let ast = modify_map_access p ast stmt in
  let ast = modify_0d_map_access p ast stmt in
  (args, ast)


