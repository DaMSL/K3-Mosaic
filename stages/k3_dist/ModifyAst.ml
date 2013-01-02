open Util
open K3.AST
open K3Helpers

module U = K3Util
module P = ProgInfo
module T = Tree
module PR = K3Printing
module Set = ListAsSet

exception InvalidAst of string

let subst_meta m e =
  T.mk_tree (((U.id_of_expr e, U.tag_of_expr e), m), T.sub_tree e)

let add_meta v e = subst_meta (v::U.meta_of_expr e) e

let remove_meta e =
  let meta = U.meta_of_expr e in
  let new_meta = List.filter 
    (function K3.Annotation.Modification _ -> false | _ -> true) meta
  in subst_meta new_meta e

(* insert a clean meta into the given tree *)
let clean_ast_meta ast = T.modify_tree_bu ast remove_meta
let set_modified e = add_meta 
  (K3.Annotation.Modification true) e
let is_modified e = List.exists 
  (function K3.Annotation.Modification _ -> true | _ -> false) 
  @: U.meta_of_expr e

let id_of_trig = function
  Trigger (id, _, _, _) -> id
  | _ -> invalid_arg "id_of_trig: not a trigger"

let expr_of_trig = function
  Trigger (_, _, _, e) -> e
  | _ -> invalid_arg "expr_of_trig: not a trigger"

let args_of_trig = function
  Trigger (_, a, _, _) -> a
  | _ -> invalid_arg "args_of_trig: not a trigger"

let decl_for_t ast trig = 
  fst @: List.find (fun d -> try (id_of_trig |- fst) d = trig 
                      with Invalid_argument _ -> false) ast

(* get the relative offset of the stmt in the trigger *)
let stmt_idx_in_t p trig stmt = 
  let ss = P.stmts_of_t p trig in
  foldl_until (fun acc _ -> acc + 1) (fun acc s -> s = stmt) 0 ss

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
  let trig_decl = decl_for_t ast trig in
  let trig_ast = expr_of_trig trig_decl in
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
  let trig_decl = decl_for_t ast trig_name in
  let trig_ast = expr_of_trig trig_decl in
  let args = U.typed_vars_of_arg @: args_of_trig trig_decl in
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

(* add vid to all map accesses *)
let modify_map_access p ast stmt =
  let lmap = P.lhs_map_of_stmt p stmt in
  let lmap_name = P.map_name_of p lmap in
  let lmap_types = P.map_types_with_v_for p lmap in
  let maps_n_id = maps_with_existing_out_tier p stmt in
  let var_vid = mk_var "vid" in
  let modify e path = match U.tag_of_expr e with
    | Insert -> let (col, elem) = U.decompose_insert e in
      set_modified @: mk_insert col @:
        begin match U.tag_of_expr elem with
          | Tuple  -> let xs = U.decompose_tuple elem in
                      mk_tuple @: P.map_add_v var_vid xs
          | x      -> mk_tuple @: P.map_add_v var_vid [elem]
        end
    | Delete -> let (col, elem) = U.decompose_delete e in
      set_modified @: mk_delete col @:
        begin match U.tag_of_expr elem with
          | Tuple  -> let xs = U.decompose_tuple elem in
                      mk_tuple @: P.map_add_v var_vid xs
          | x      -> mk_tuple @: P.map_add_v var_vid [elem]
        end
    | Slice -> (* first check what's our parent *)
      (* a simple modification of a slice. Useful for structural access to the
       * map, such as when deleting *)
      let modify_slice e = 
        let (col, pat) = U.decompose_slice e in
          set_modified @: mk_slice col @:
            begin match U.tag_of_expr pat with
              | Tuple -> let xs = U.decompose_tuple pat in
                         mk_tuple @: P.map_add_v var_vid xs
              | x     -> mk_tuple @: P.map_add_v var_vid [pat]
            end
      in 
      (* a projected slice modification. Useful for access to the data of the
       * map *)
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
        end in
      let parent = list_head path in
      begin match U.tag_of_expr parent with
       (* if we have delete above us, we need the full structure of the map. If
        * not, we can project onto a collection without the vid *)
        | Delete -> modify_slice e  (* doesn't exist **** TODO *)
        | Iterate -> let (lambda, _) = U.decompose_iterate parent in
          let body = U.decompose_lambda lambda in
          begin match U.tag_of_expr body with
            | Delete -> modify_slice e
            | _ -> project_modify_slice e
          end
        | _      -> project_modify_slice e
      end
     (* handle a case where we're iterating over a collection that's modified *)
    | Iterate -> let (lambda, col) = U.decompose_iterate e in
      if is_modified col then
        let body = U.decompose_lambda lambda in
        let vid_avar = AVar("vid", t_vid) in
        let args = U.arg_of_lambda lambda in
        let mod_fn typ orig_typ = match typ with
          | TTuple(ts) -> TTuple(P.map_add_v t_vid ts)
          | t -> TTuple(P.map_add_v t_vid [orig_typ])
        in let new_args = begin match args with
          | Some(ATuple(vs)) -> ATuple(P.map_add_v vid_avar vs)
          | Some(v) -> ATuple(P.map_add_v vid_avar [v])
          (*| Some(AVar(v, t)) -> AVar(v, modify_tuple_type mod_fn t)*)
          | _ -> raise(UnhandledModification("lambda not found"))
        end
        in set_modified @: mk_iter (mk_lambda new_args body) col
      else e
    (* handle a case when the map is applied to a lambda *)
    | Apply  -> let (l, arg) = U.decompose_apply e in
      begin match U.tag_of_expr arg with
        | Var id when id = lmap_name -> 
          begin match (U.typed_vars_of_lambda l, U.decompose_lambda l) with
            | ([id, t],b) -> 
              set_modified @: mk_apply 
                (mk_lambda 
                  (wrap_args [id, wrap_tset @: wrap_ttuple lmap_types]) b)
                arg
            | _ -> raise (UnhandledModification(PR.string_of_expr e))
          end
        | _ -> mk_apply l arg
      end
    | _ -> e
  in T.modify_tree_bu_with_path ast modify

(* this delta extraction is very brittle, since it's tailored to the way the K3
 * calculations are written *)
let modify_delta p ast stmt target_trigger =
  let lmap = P.lhs_map_of_stmt p stmt in
  let lmap_name = P.map_name_of p lmap in
  let lmap_types = P.map_types_with_v_for p lmap in
  let (lambda, arg) = U.decompose_apply ast in
  let body = U.decompose_lambda lambda in
  let params = match U.tag_of_expr lambda with Lambda a -> a 
    | _ -> raise(UnhandledModification("No lambda")) in
  match U.tag_of_expr body with
  | Apply -> (* simple modification *)
    let (lambda2, arg2) = U.decompose_apply body in
    let body2 = U.decompose_lambda lambda2 in
    let params2 = begin match U.tag_of_expr lambda2 with 
      | Lambda a -> a
      | _ -> raise(UnhandledModification("No inner lambda")) end in
    let delta_name = list_head @: U.vars_of_lambda lambda2 in
    let delta_types = extract_arg_types @: U.typed_vars_of_arg params2 in
    let full_vars_code = 
      let bindings = P.find_lmap_bindings_in_stmt p stmt lmap in
      let full_types = P.map_add_v t_vid @: lmap_types @ delta_types in
      let full_names = P.map_ids_add_v @: 
         (fst @: List.split bindings) @ [delta_name] in
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
                    (mk_var "loopback") @:
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
    let delta_ids_types_with_v = P.map_ids_types_add_v delta_ids_types in
    let delta_ids = extract_arg_names delta_ids_types in
    let delta_ids_with_v = P.map_ids_add_v delta_ids in
    mk_apply
      (mk_lambda params @:
          mk_let delta_name (delta_col_type) col @:
          mk_block @:
            (mk_iter lambda2 @: 
              mk_var delta_name)::
            [mk_send
              (mk_const @: CTarget target_trigger)
              (mk_var "loopback") @:
              mk_tuple @:
                (mk_var "vid"):: (* project vid into collection *)
                  [mk_map
                    (mk_lambda 
                      (wrap_args delta_ids_types) @:
                      mk_tuple @: ids_to_vars delta_ids_with_v
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
  let ast = match target_trig with
    | Some t -> modify_delta p ast stmt t
    | None -> ast
  in clean_ast_meta ast (* remove the meta info we used *)

(* return a modified version of the corrective update *)
let modify_corr_ast p ast map stmt trig =
  let (args, ast) = corr_ast_for_m_s p ast map stmt trig in
  let ast = modify_map_access p ast stmt in
  let ast = modify_0d_map_access p ast stmt in
  (args, clean_ast_meta ast)


