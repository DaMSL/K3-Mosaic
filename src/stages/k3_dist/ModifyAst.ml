open Util
open K3.AST
open K3Helpers
open K3Dist

module U = K3Util
module P = ProgInfo
module PR = K3Printing
module D = K3Dist

exception InvalidAst of string

(* find bound slice patterns in ast *)
let extract_slice_patterns ?(zero=StrMap.empty) ast =
  let f acc e =
    try
      let col, pat = U.decompose_slice e in
      let id = U.decompose_var col in
      let es = insert_index_fst @@ U.unwrap_tuple pat in
      let iset = IntSet.of_list @@ fst_many @@
        List.filter (not |- is_unknown |- snd) es in
      StrMap.update_with id (function
          | None    -> some @@ IntSetSet.singleton iset
          | Some ss -> some @@ IntSetSet.add iset ss)
        acc
    with Failure _ -> acc
  in
  Tree.fold_tree_th_bu f zero ast

(* find unused arguments in triggers *)
let unused_trig_args ast =
  let rec toplevel_args = function
    | ATuple args -> List.flatten @@ List.map toplevel_args args
    | AVar(x,_) -> [x]
    | _ -> failwith "unexpected non-standard arg in dbtoaster k3 code"
  in
  (* for a single trigger *)
  let unused_args trig_flow =
    let args = StrSet.of_list @@ toplevel_args @@ U.args_of_code trig_flow in
    (* remove var ids from set *)
    let remove_fn a t = match U.tag_of_expr t with
      | Var id -> StrSet.remove id a
      | _      -> a
    in
    Tree.fold_tree_th_bu remove_fn args (U.expr_of_code trig_flow)
  in
  let l =
    List.map (fun (nm, t) -> P.remove_trig_prefix nm, unused_args t) @@
    List.filter (fun (nm, _) -> P.is_insert_t nm || P.is_delete_t nm) @@
    List.map (fun t -> U.id_of_code t, t) @@
    U.triggers_of_program ast
  in
  (* combine insert and delete trigs, and take their intersection. The reason we do this is that
   * in the rare case of some variables used by one of insert/delete, we don't have a clean enough
   * separation between the two (e.g. in agenda_mapping) to really handle them seprately, and getting
   * their intersection should be useful enough *)
  List.fold_left (fun (acc:StrSet.t StrMap.t) (nm, set) ->
    try
      let s  = StrMap.find nm acc in
      let s' = StrSet.inter s set in
      StrMap.add nm s' acc
    with Not_found ->
      StrMap.add nm set acc) StrMap.empty l

let string_of_unused_trig_args (m:StrSet.t StrMap.t) =
  let l = list_of_strmap m in
  strcatmap (fun (k,v) -> k^" => ("^(String.concat ", " @@ StrSet.elements v)^")") l

(* find any csv loaders in the ast *)
let loader_tables ast =
  let bu_fn acc e =
    try
      let fn, args = U.decompose_apply e in
      if U.decompose_var fn = K3StdLib.csv_loader_name then
        match U.decompose_const (hd args) with
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
let modify_init_vals ds old_ds ~add_unit e =
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
    (* modify loading from a csv file (csv loader) *)
    | Map ->
        let old_pat = pat_of_ds old_ds in
        let new_pat = pat_of_flat_e ~add_vid:true ~vid_nm:g_min_vid.id
                        ~has_vid:false ds @@ fst_many old_pat in
          mk_agg (mk_lambda2' ["acc", ds.t] (ds_e old_ds) @@
            mk_insert_block "acc" new_pat)
          (mk_empty ds.t)
          e
    | _ -> U.dist_fail e "modify_init_vals: unhandled modification"
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
            let fn, args = U.decompose_apply e in
            if U.decompose_var fn = K3StdLib.csv_loader_name then
              match U.decompose_const (hd args) with
              | CString f ->
                  let table = Filename.chop_extension @@ Filename.basename f in
                  mk_apply (mk_var @@ K3StdLib.csv_loader_name^"2")
                    [mk_var @@ table^"_path";
                      (* witness type so the function knows what to return *)
                      mk_empty (M3ToK3.wrap_map' @@
                        ProgInfo.map_types_no_val_for c.p map_id)]
              | _ -> failwith "bad filename"
            else e
          with Failure _ -> e)
      in
      begin match m_expr with
        | None   -> []
        | Some e ->
            let old_ds = map_ds_of_id ~global:false ~vid:false c map_id in
            [map_id, mk_ind @@ modify_init_vals ds old_ds ~add_unit @@ change_load_csv e]
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
let ast_for_s_t p ast (stmt:P.stmt_id_t) (trig:P.trig_name_t) =
  let trig_decl = U.trigger_of_program trig ast in
  let args      = U.args_of_code trig_decl in
  let trig_ast  = U.expr_of_code trig_decl in
  let s_idx     = stmt_idx_in_t p trig stmt in
  args, block_nth trig_ast s_idx

let ast_for_s p ast (stmt:P.stmt_id_t) =
  let trig = P.trigger_of_stmt p stmt in
  ast_for_s_t p ast stmt trig

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
        with Not_found -> err_or_ret @@ Printf.sprintf "No %s map found in stmt %d. lmap:%s" id stmt lmap_name end

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

    | GroupByAggregate ->
        let lam1, lam2, zero, col = U.decompose_gbagg e in
        let col' = modify_map_read c col None (Some col) in
        NopMsg, mk_gbagg lam1 lam2 zero col'

    | Aggregate ->
        let lam, zero, col = U.decompose_aggregate e in
        let col' = modify_map_read c col None (Some col) in
        NopMsg, mk_agg lam zero col'

    (* handle a case of a lambda applied to an lmap (i.e. a let statement *)
    (* in this case, we modify the types of the lambda vars themselves *)
    | Apply  ->
        let lambda, args = U.decompose_apply e in
        begin match U.tag_of_expr (hd args) with
          | Var id when id = lmap_name ->
            begin match (U.typed_vars_of_lambda lambda, snd @@ U.decompose_lambda lambda) with
              | ([arg_id, t],b) -> NopMsg,
                mk_apply
                  (mk_lambda
                    (wrap_args [arg_id, wrap_t_of_map' c lmap lmap_types]) b)
                  args
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
      NopMsg, mk_singleton (wrap_t_calc elem_t) [e']

    | _ -> NopMsg, e
  in Tree.modify_tree_bu_with_path_and_msgs ast modify

(* this delta extraction is very brittle, since it's tailored to the way the M3
 * to K3 calculations are written. *)
let delta_action c ast stmt =
  let lmap = P.lhs_map_of_stmt c.p stmt in
  let lmap_id_t = P.map_ids_types_for c.p lmap in
  let lmap_col_t = wrap_t_calc @@ wrap_ttuple @@ snd_many lmap_id_t in
  (* we need to know how the map is accessed in the statement. *)
  let lmap_bindings = P.find_lmap_bindings_in_stmt c.p stmt lmap in
  (* let existing_out_tier = ..., which we remove *)
  let id, _, expr = U.decompose_let ast in
  if id <> ["existing_out_tier"] then failwith "sanity check fail: expected existing_out_tier" else
  match U.tag_of_expr expr with
  | Let _ ->
    (* simple modification - sending a single tuple of data *)
    (* this is something like prod_ret_x's let *)
      let delta_names, calc, expr = U.decompose_let expr in
      let full_names = fst_many lmap_bindings @ delta_names in
      let full_vars = mk_singleton lmap_col_t @@ ids_to_vars full_names in
      (* modify the delta itself *)
      (* calc contains original computation code *)
      let code = mk_let delta_names calc full_vars in
      (* TODO: use the boolean to pass only a tuple *)
      true, code

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
        let subscript_rng = create_range ~first:1 @@ List.length lmap_t_no_val in
        let subs = match subscript_rng with
          | [_] -> [mk_var "g"]
          | _   -> List.map (flip mk_subscript @@ mk_var "g") subscript_rng in
        (* convert the gbagg to external type *)
        mk_agg
          (mk_assoc_lambda' ["acc", lmap_col_t] ["g", wrap_ttuple lmap_t_no_val; "val", last_t] @@
            (* combine the tuple with val *)
            mk_insert_block "acc" @@ subs @ [mk_var "val"])
          (mk_empty lmap_col_t) @@
          (* group for uniqueness *)
          mk_gbagg
            (* group by key *)
            (mk_lambda' lmap_id_t @@
              mk_tuple @@ ids_to_vars @@ fst_many @@ list_drop_end 1 lmap_id_t)
            (* add is our combination operator *)
            (mk_assoc_lambda' ["acc", last_t] lmap_id_t @@
              mk_add (mk_var "acc") @@ mk_var @@ last_id)
            (default_value_of_t last_t)
            col_e
      in
      (* we need to convert the bags to the external type *)
      let wrap_convert col_e = mk_convert_col (wrap_tbag' @@ snd_many lmap_id_t) lmap_col_t col_e in
      if has_concat then wrap_uniq col_e else wrap_convert col_e
    in
    (* col contains the calculation code *)
    true, wrap_out @@ wrap_map col

  | _ -> raise @@ UnhandledModification(
     Printf.sprintf "Bad tag [%d]: %s" (U.id_of_expr expr) @@ PR.string_of_expr expr)

(* rename a variable in an ast *)
let rename_var old_var_name new_var_name ast =
  Tree.modify_tree_bu ast @@ fun e ->
    match U.tag_of_expr e with
    | Var v when v = old_var_name -> mk_var new_var_name
    | _ -> e

(* get bindings for system_event. we can't use proginfo since it's unreliable in this case *)
let sys_init_bindings ~prune p ast =
  let h = Hashtbl.create 10 in
  let t = "system_ready_event" in
  let ss = try P.stmts_of_t p t with P.Bad_data _ -> [] in
  let set =
    List.fold_left (fun acc s ->
        extract_slice_patterns ~zero:acc @@ snd @@ ast_for_s_t p ast s t)
      StrMap.empty ss in
  List.iter (fun (nm, ss) ->
      let m = P.map_id_of_name p nm in
      let num_ts = List.length @@ P.map_types_no_val_for p m in
      let ss =
        if prune then
          IntSetSet.filter (fun s -> IntSet.cardinal s < num_ts) ss
        else ss
      in
      if not (IntSetSet.is_empty ss) then Hashtbl.add h m ss else ()) @@
    StrMap.to_list set;
  h

let () = K3Dist.sys_init_bindings := sys_init_bindings ~prune:true

(* return a modified version of the original ast for stmt s *)
let modify_ast c ast stmt trig =
  let _, ast = ast_for_s_t c.p ast stmt trig in
  let ast = modify_dist c ast stmt in
  let is_col, ast = delta_action c ast stmt in
  is_col, ast

(* return a modified version of the corrective update *)
let modify_corr_ast c ast map stmt trig =
  let args, corr_stmt, ast = corr_ast_for_m_s c ast map stmt trig in
  let ast = modify_dist c ast stmt in
  let is_col, ast = delta_action c ast corr_stmt in
  args, is_col, ast
