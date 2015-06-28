open Util
open K3.AST
open K3Helpers
open K3Dist

module U = K3Util
module P = ProgInfo
module PR = K3Printing
module D = K3Dist

exception InvalidAst of string

(* find any loaders in the ast *)
let loader_tables ast =
  let bu_fn acc e =
    try
      let fn, arg = U.decompose_apply e in
      if U.decompose_var fn = K3StdLib.csv_loader_name then
        match U.decompose_const arg with
        | CString f -> (Filename.chop_extension @@ Filename.basename f, f)::acc
        | _ -> failwith "bad filename"
      else acc
    with Failure _ -> acc
  in
  U.fold_over_exprs (fun acc t ->
    Tree.fold_tree_th_bu bu_fn acc t
  ) [] ast

(* --- Map declarations --- *)

(* change the initialization values of global maps to have the vid as well *)
(* receives the new types to put in and the starting expression to change *)
(* inserts a reference to the default vid var for that node *)
let add_vid_to_init_val ds old_ds ~add_unit e =
  let vid_var = mk_var g_init_vid.id in
  let add_u l = if add_unit then mk_cunit :: l else l in
  let rec loop e = match U.tag_of_expr e with
    | Combine -> let x, y = U.decompose_combine e in
        mk_combine (loop x) (loop y)
    | Empty t -> mk_empty ds.t
    | Singleton t -> let x = U.decompose_singleton e in
        mk_singleton ds.t [loop x]
    | Tuple -> let xs = U.decompose_tuple e in
        mk_tuple @@ P.map_add_v vid_var xs
    (* this should only be encountered if there's no tuple *)
    | Const _ | Var _ -> mk_tuple @@ P.map_add_v vid_var @@ add_u [e]
    (* modify loading from a file *)
    | Map ->
        let old_pat = pat_of_ds old_ds in
        let new_pat = pat_of_flat_e ~add_vid:true ~vid_nm:g_min_vid.id
                        ~has_vid:false ds @@ fst_many old_pat in
          mk_agg (mk_lambda2' ["acc", ds.t] (ds_e old_ds) @@
          mk_block [
            mk_insert "acc" new_pat;
            mk_var "acc"
          ])
          (mk_empty ds.t)
          e
    | _ -> U.dist_fail e "add_vid_to_init_val: unhandled modification"
  in loop e

(* add a vid to global value map declarations *)
let get_global_map_inits c = function
  (* TODO: handle this gracefully for maps *)
  (* filter to have only map declarations *)
  | Global(name, typ, m_expr),_ ->
    begin try
      let map_id = P.map_id_of_name c.p name in
      let ds = map_ds_of_id ~global:true c map_id in
      let add_unit = List.length ds.e = 1 in
      (* change load_csv to use a witness variable *)
      let change_load_csv e =
        Tree.modify_tree_bu e (fun e ->
          try
            let fn, arg = U.decompose_apply e in
            if U.decompose_var fn = K3StdLib.csv_loader_name then
              match U.decompose_const arg with
              | CString f ->
                  let table = Filename.chop_extension @@ Filename.basename f in
                  mk_apply (mk_var @@ K3StdLib.csv_loader_name^"2") @@
                    mk_tuple [mk_var @@ table^"_path"; 
                              (* witness type so the function knows what to return *)
                              mk_empty (M3ToK3.wrap_map' @@ ProgInfo.map_types_no_val_for c.p map_id)]
              | _ -> failwith "bad filename"
            else e
          with Failure _ -> e)
      in
      begin match m_expr with
        | None   -> []
        | Some e ->
            let old_ds = map_ds_of_id ~global:false ~vid:false c map_id in
            [map_id, mk_ind @@ add_vid_to_init_val ds old_ds ~add_unit @@ change_load_csv e]
                    (* add a vid *)
      end
    with Not_found | ProgInfo.Bad_data _ -> [] end
  | _ -> []

(* return ast for map initializations, adding the vid *)
let map_inits_from_ast c ast : expr_t IntMap.t =
  let decls = U.globals_of_program ast in
  intmap_of_list @@ List.flatten @@ List.map (get_global_map_inits c) decls

(* --- Trigger modification --- *)

(* get the relative offset of the stmt in the trigger *)
let stmt_idx_in_t c trig stmt =
  let ss = P.stmts_of_t c.p trig in
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
let ast_for_s_t c ast (stmt:P.stmt_id_t) (trig:P.trig_name_t) =
  let trig_decl = U.trigger_of_program trig ast in
  let args      = U.args_of_code trig_decl in
  let trig_ast  = U.expr_of_code trig_decl in
  let s_idx     = stmt_idx_in_t c trig stmt in
  args, block_nth trig_ast s_idx

let ast_for_s c ast (stmt:P.stmt_id_t) =
  let trig = P.trigger_of_stmt c.p stmt in
  ast_for_s_t c ast stmt trig

(* return the corrective (args, stmt_id, AST) for a given stmt, map, trig *)
let corr_ast_for_m_s c ast map stmt trig =
  (* find the specific statement in the corrective trigger that deals with our map *)
  let map_name = P.map_name_of c.p map in
  let s_with_m = P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt trig in
  let s_with_m_filter = List.filter (fun (s,m) -> m = map) s_with_m in
  (* find all the statements in the trigger dealing with our map and count them.
   * This will tell us how far to go in the corrective trigger for the map *)
  let s_i = insert_index_snd @@ fst_many s_with_m_filter in
  let stmt_idx = List.assoc stmt s_i in

  let trig_name = "correct_"^map_name^"_for_"^trig in
  let trig_decl =
    try U.trigger_of_program trig_name ast
    with Not_found -> failwith @@ "Missing corrective for "^trig_name in
  let trig_min_stmt = list_min @@ P.stmts_of_t c.p trig_name in
  let trig_ast = U.expr_of_code trig_decl in
  let args = U.typed_vars_of_arg @@ U.args_of_code trig_decl in
  let trig_args = P.args_of_t c.p trig in
  (* remove the trigger args from the list of args in the corrective trigger *)
  let args2 = ListAsSet.diff args trig_args in
  let stmt_block = block_nth trig_ast stmt_idx
  in args2, trig_min_stmt + stmt_idx, stmt_block

exception UnhandledModification of string

(* modify a lambda to have a vid included in its arguments *)
(* TODO: add support for LET *)
let add_vid_to_lambda_args lambda =
  let vid_avar = AVar("vid", t_vid) in
  let args, body = U.decompose_lambda lambda in
  let new_args = match args with
    | ATuple(vs) -> ATuple(P.map_add_v vid_avar vs)
    | v          -> ATuple(P.map_add_v vid_avar [v])
  in
  mk_lambda new_args body

(* messages about possible modifications to the ast for map accesses *)
(* these are passed around while folding over the tree *)
type msg_t = NopMsg | DelMsg

(* the maps here have a buffer suffix *)

(* add vid to all map accesses. This is a complicated function that
 * has to dig through the entire AST, but it's pretty resilient *)
let modify_dist (c:config) ast stmt =
  let lmap_alias = "existing_out_tier" in
  let lmap = P.lhs_map_of_stmt c.p stmt in
  let maps_with_existing_out_tier stmt =
    let maps = P.map_names_ids_of_stmt c.p stmt in
    (lmap_alias, lmap) :: maps
  in
  let lmap_name  = P.map_name_of c.p lmap in
  let lmap_types = P.map_types_with_v_for c.p lmap in
  let maps_n_id  = maps_with_existing_out_tier stmt in
  (* names of maps with one dimension in this statement *)
  let maps_n_1d  = fst_many @@ List.filter
    (fun (_, m) -> null @@ P.map_types_no_val_for c.p m) maps_n_id in
  let var_vid    = mk_var "vid" in
  (* add a vid to a tuple *)
  let modify_tuple e =
    P.map_add_v var_vid @@ U.extract_if_tuple e
  in
  (* modify a direct map read, such as in a slice or a peek. col is the
   * collection, pat_m is an option pattern (for slice),
   * expr_m is a default expression. if we have a default expression, we don't fail *)
  let modify_map_read c col pat_m expr_m =
    (* we either fail or return the default expression, if given one *)
    let err_or_ret s = maybe_f (fun () -> U.dist_fail col s) id_fn expr_m in
    match U.tag_of_expr col with
    | Var id ->
        begin try
          (* includes existing_out_tier *)
          let map = List.assoc id maps_n_id in
          let buf_col =
            (* if this isn't the lmap (in which case it's stored locally)
            * adjust the name of the map to be a buffer *)
            if id <> lmap_alias && id <> lmap_name then
              mk_var @@ P.buf_of_stmt_map stmt id
            else col in
          (* get the latest vid values for this map *)
          mk_bind buf_col "__x" @@
            map_latest_vid_vals c (mk_var "__x") pat_m map ~keep_vid:false
        with Not_found -> err_or_ret @@ Printf.sprintf "No %s map found in stmt %d" id stmt end

    | _ -> err_or_ret "Cannot handle non-var in slice"
  in

  (* we need messages between levels. For example, if we have a Delete that we
   * want to prune out, that whole branch needs to be removed *)
  let rec modify e msgs path =
    let get_msg i = at msgs i in
    let msg_del i = if null msgs then false else at msgs i = DelMsg in
    match U.tag_of_expr e with

    (* a lambda simply passes through a message *)
    | Lambda _ -> get_msg 0, e
    | Insert -> let col, elem = U.decompose_insert e in
        NopMsg, mk_insert (U.decompose_var col) @@ modify_tuple elem

    (* deletes need to be removed, since we have versioning ie. we don't delete
     * anything *)
    | Delete -> DelMsg, e

    | Slice ->
      (* If we have any bound variable, we should slice on those *)
      let col, pat = U.decompose_slice e in
      let pat = U.extract_if_tuple pat in
      let has_bound = List.exists (fun e -> match U.tag_of_expr e with
          | Const(CUnknown) -> false
          | _ -> true)
        pat in
      let pat_m = if has_bound then Some pat else None
      in
      NopMsg, modify_map_read c col pat_m None

    | Iterate ->
      (* if our lambda requests a deletion, we delete *)
      if msg_del 0 then DelMsg, e else NopMsg, e

    | Combine ->
        (* could be combining global maps *)
        let l, r = U.decompose_combine e in
        let l' = modify_map_read c l None @@ Some l in
        let r' = modify_map_read c r None @@ Some r in
        NopMsg, mk_combine l' r'

    | Map ->
        (* could be mapping over a global map *)
        let lam, col = U.decompose_map e in
        let col' = modify_map_read c col None @@ Some col in
        NopMsg, mk_map lam col'

    (* handle a case of a lambda applied to an lmap (i.e. a let statement *)
    (* in this case, we modify the types of the lambda vars themselves *)
    | Apply  -> let (lambda, arg) = U.decompose_apply e in
      begin match U.tag_of_expr arg with
        | Var id when id = lmap_name ->
          begin match (U.typed_vars_of_lambda lambda, snd @@ U.decompose_lambda lambda) with
            | ([arg_id, t],b) -> NopMsg,
              mk_apply
                (mk_lambda
                  (wrap_args [arg_id, wrap_t_of_map' c.map_type lmap_types]) b)
                arg
            | _ -> raise (UnhandledModification("At Apply: "^PR.string_of_expr e)) end
        | _ -> NopMsg, e
      end

    (* if any statements in a block requested deletion, delete
     * those statements - they're not relevant *)
    | Block -> let ss = U.decompose_block e in
      let s_msg = list_zip ss msgs in
      begin match fst_many @@ List.filter (fun (_, msg) -> msg <> DelMsg) s_msg with
      | []  -> DelMsg, mk_block ss
      | [s] -> NopMsg, s
      | ss  -> NopMsg, mk_block ss
      end

    | Peek -> let col = U.decompose_peek e in
      begin match U.tag_of_expr col with
      (* match only for maps with one dimension *)
      | Var name when List.mem name maps_n_1d ->
          (* we use the same function as Seek, except we don't supply a pattern
          * (no bound vars) *)
          NopMsg, mk_peek @@ modify_map_read c col None None
      | _ -> NopMsg, e
      end

    (* we need to change all singletons into bags *)
    | Singleton t -> let e' = U.decompose_singleton e in
      let _, elem_t = unwrap_tcol t in
      NopMsg, mk_singleton (wrap_tbag elem_t) [e']

    | _ -> NopMsg, e
  in Tree.modify_tree_bu_with_path_and_msgs ast modify

(* this delta extraction is very brittle, since it's tailored to the way the M3
 * to K3 calculations are written. *)
let delta_action c ast stmt after_fn =
  let lmap = P.lhs_map_of_stmt c.p stmt in
  let lmap_id_t = P.map_ids_types_for c.p lmap in
  let lmap_col_t = wrap_t_calc @@ wrap_ttuple @@ snd_many lmap_id_t in
  (* we need to know how the map is accessed in the statement. *)
  let lmap_bindings = P.find_lmap_bindings_in_stmt c.p stmt lmap in
  (* let existing_out_tier = ..., which we remove *)
  let id, bound, expr = U.decompose_let ast in
  if id <> ["existing_out_tier"] then failwith "sanity check fail: expected existing_out_tier" else
  match U.tag_of_expr expr with
  | Let _ ->
    (* simple modification - sending a single tuple of data *)
    (* this is something like prod_ret_x's let *)
      let delta_names, bound, expr = U.decompose_let expr in
      let full_names = fst_many lmap_bindings @ delta_names in
      let lmap_v_col_t = wrap_t_calc @@ wrap_ttuple @@ P.map_types_with_v_for c.p lmap in
      let full_vars = mk_singleton lmap_v_col_t @@ ids_to_vars full_names in
      (* modify the delta itself *)
      mk_let delta_names
        (* contains original computation code *)
        bound @@ after_fn full_vars

  | Iterate -> (* more complex modification *)
    (* col contains the calculation code, lambda is the delta addition *)
    let lambda, col = U.decompose_iterate expr in
    (* we need to handle the possibility of having narrow tuples. we take our info from the insert
     * iteration, comparing the iterate lambda args and the insert statement *)
    let lam_id_t = U.typed_vars_of_lambda lambda in
    let lam_id = fst_many lam_id_t in
    let lmap_name = P.map_name_of c.p lmap in
    (* lambda to find the insert ids *)
    let find_insert_vars acc x =
      if acc <> [] then acc else
      try
        let id, y = U.decompose_insert x in
        let id = U.decompose_var id in
        if id <> lmap_name then acc else
        vars_to_ids @@ U.decompose_tuple y
      with Failure _ -> acc in
    (* get all ids from insert expression *)
    let insert_ids = Tree.fold_tree_th_bu find_insert_vars [] lambda in
    if null insert_ids then
      failwith "modifyast: Failed to find ids in insert expression" else
    let wrap_map =
      (* drop the vid and value *)
      let common_ids = list_drop 1 @@ list_drop_end 1 insert_ids in
      if not @@ ListAsSet.seteq common_ids lam_id then
        (* wrap with a map that restores the value name *)
        let ids = common_ids @ list_take_end 1 lam_id in
        mk_map
          (mk_lambda' lam_id_t @@ mk_tuple @@ ids_to_vars ids)
      else id_fn (* no need to wrap *)
    in
    (* if we have a concat operation, we need to enforce uniqueness with addition for the same key *)
    let has_concat =
      let find_concat found x =
        if found || U.tag_of_expr x = Combine then true else false in
      Tree.fold_tree_th_bu find_concat false expr in
    (* this wrapper is either a conversion to the outside or a uniqueness operation *)
    let wrap_out col_e =
      let wrap_uniq col_e =
        let last_id, last_t = list_last lmap_id_t in
        let lmap_t_no_val = P.map_types_no_val_for c.p lmap in
        let subscript_rng = create_range 1 @@ List.length lmap_t_no_val in
        let subs = match subscript_rng with
          | [_] -> [mk_var "g"]
          | _   -> List.map (flip mk_subscript @@ mk_var "g") subscript_rng in
        (* convert the gbagg to external type *)
        mk_agg
          (mk_assoc_lambda' ["acc", lmap_col_t] ["g", wrap_ttuple lmap_t_no_val; "val", last_t] @@
            mk_block [
              (* combine the tuple with val *)
              mk_insert "acc" @@ subs @ [mk_var "val"];
              mk_var "acc" ])
          (mk_empty lmap_col_t) @@
          (* group for uniqueness *)
          mk_gbagg
            (* group by key *)
            (mk_lambda' lmap_id_t @@
              mk_tuple @@ ids_to_vars @@ fst_many @@ list_drop_end 1 lmap_id_t)
            (* add is our combination operator *)
            (mk_assoc_lambda' ["acc", last_t] lmap_id_t @@
              mk_add (mk_var "acc") @@ mk_var @@ last_id)
            (mk_cint 0)
            col_e
      in
      (* we need to convert the bags to the external type *)
      let wrap_convert col_e = mk_convert_col (wrap_tbag' @@ snd_many lmap_id_t) lmap_col_t col_e in
      if has_concat then wrap_uniq col_e else wrap_convert col_e
    in
    let delta_name = "delta_values" in
    (* col2 contains the calculation code *)
    mk_let [delta_name] (wrap_out @@ wrap_map col) @@
      (* any function *)
      after_fn (mk_var delta_name)

  | _ -> raise @@ UnhandledModification(
     Printf.sprintf "Bad tag [%d]: %s" (U.id_of_expr expr) @@ PR.string_of_expr expr)

(* rename a variable in an ast *)
let rename_var old_var_name new_var_name ast =
  Tree.modify_tree_bu ast @@ fun e ->
    match U.tag_of_expr e with
    | Var v when v = old_var_name -> mk_var new_var_name
    | _ -> e

(* return a modified version of the original ast for stmt s *)
let modify_ast_for_s (c:config) ast stmt trig after_fn =
  let _, ast = ast_for_s_t c ast stmt trig in
  let ast = modify_dist c ast stmt in
  let ast = delta_action c ast stmt after_fn in
  ast

(* return a modified version of the corrective update *)
let modify_corr_ast c ast map stmt trig after_fn =
  let args, corr_stmt, ast = corr_ast_for_m_s c ast map stmt trig in
  let ast = modify_dist c ast stmt in
  args, delta_action c ast corr_stmt after_fn
