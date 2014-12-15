open Util
open K3.AST
open K3Helpers
open K3Dist

module U = K3Util
module P = ProgInfo
module PR = K3Printing

exception InvalidAst of string

(* --- Map declarations --- *)

(* find all access patterns on maps in the code. We could probably get this info from
 * dbtoaster, but it's not hard to just get it here *)
let get_map_access_patterns ast : IndexSet.t StrMap.t =
  (* for top-down, get any existing_out_tier value *)
  let td_fn out_tier n =
      match U.tag_of_expr n with
      | Apply -> let lam, app = U.decompose_apply n in
                 begin try
                   let arg, _   = U.decompose_lambda lam in
                   begin match arg, U.tag_of_expr app with
                   | AVar ("existing_out_tier", _), Var x -> Some x
                   | _ -> out_tier
                   end
                 with Failure _ -> out_tier end
      | _      -> out_tier
  in
  (* for bottom-up, get access patterns *)
  let bu_fn out_tier bu_results n =
      (* merge 2 values for a map *)
      let merge_fn k ma mb = match ma, mb with
        | Some x, None
        | None, Some x     -> Some x
        | Some xs, Some ys -> Some (IndexSet.union xs ys)
        | _                -> None
      in
      (* combine the bottom-up results *)
      let map = List.fold_left (fun acc x ->
        StrMap.merge merge_fn acc x) StrMap.empty bu_results in
      match U.tag_of_expr n with
      | Slice  ->
          let col, pat = U.decompose_slice n in
          (* get tuple pattern *)
          let pat = U.unwrap_tuple pat |> insert_index_fst
                 |> List.filter (fun (_,x) -> U.tag_of_expr x <> Const CUnknown)
                 |> fst_many |> IntSet.of_list
          in
          let add_to_map k =
             begin try
               let v = StrMap.find k map in
               StrMap.add k (IndexSet.add (HashIdx pat) v) map
             with Not_found ->
               StrMap.add k (IndexSet.singleton @@ HashIdx pat) map
             end
          in
          begin match U.tag_of_expr col, out_tier with
          | Var "existing_out_tier", Some id -> add_to_map id
          | Var "existing_out_tier", _       -> failwith "missing existing out tier binding"
          | Var k, _ -> add_to_map k
          | _        -> map
          end
      | _      -> map
  in
  U.fold_over_exprs (fun acc t ->
    Tree.fold_tree td_fn bu_fn None acc t
  ) StrMap.empty ast

(* Convert map indices from non-vid versions to be ordered and handle vid *)
(* vid is always the last thing to be matched on *)
(* NOTE: we assume vid is 0 here !!! *)
let map_indices_add_vid idxs =
  let map_idx_add_vid = function
    | HashIdx s    -> 
        let l = List.map vid_shift @@ IntSet.elements s in
        OrdIdx (add_vid_idx l, IntSet.of_list l)
    | OrdIdx(l,eq) -> 
        let eq' = IntSet.of_list @@ List.map vid_shift @@ IntSet.elements eq in
        OrdIdx (add_vid_idx (List.map vid_shift l), eq')
  in
  let add_vid_all_idxs is =
    IndexSet.fold (fun x acc -> IndexSet.add (map_idx_add_vid x) acc) is IndexSet.empty
  in
  IntMap.map add_vid_all_idxs idxs

(* convert to a per-mapid representation *)
let get_map_access_patterns_ids c ast =
  let pats = get_map_access_patterns ast in
  let pats = StrMap.fold (fun nm v acc ->
      IntMap.add (ProgInfo.map_id_of_name c.p nm) v acc)
    pats IntMap.empty in
  (* add in the patterns for singletons *)
  let map_types = P.for_all_maps c.p (fun id -> id, P.map_types_for c.p id) in
  let singleton_maps = List.filter (function (_,[_]) -> true | _ -> false) map_types in
  let pats = List.fold_left (fun acc (id,_) ->
               IntMap.add id (IndexSet.singleton @@ OrdIdx([],IntSet.empty)) acc)
             pats singleton_maps
  in
  map_indices_add_vid pats

(* change the initialization values of global maps to have the vid as well *)
(* receives the new types to put in and the starting expression to change *)
(* inserts a reference to the default vid var for that node *)
let rec add_vid_to_init_val types e =
  let add = add_vid_to_init_val types in
  let vid_var = mk_var init_vid in
  match U.tag_of_expr e with
  | Combine -> let x, y = U.decompose_combine e in
      mk_combine (add x) (add y)
  | Empty t -> mk_empty types
  | Singleton t -> let x = U.decompose_singleton e in
      mk_singleton types (add x)
  | Tuple -> let xs = U.decompose_tuple e in
      mk_tuple (P.map_add_v vid_var xs)
  (* this should only be encountered if there's no tuple *)
  | Const _ | Var _ -> mk_tuple @@ P.map_add_v vid_var [e]
  | _ -> failwith "add_vid_to_init_val: unhandled modification"

(* add a vid to global value map declarations *)
let modify_global_map c = function
  (* filter to have only map declarations *)
  | Global(name, TValue typ, m_expr),_ ->
    begin try
      let map_id = P.map_id_of_name c.p name in
      let map_type =
        wrap_t_map_idx' c map_id @@ P.map_types_with_v_for c.p map_id in
      let map_type_ind = wrap_tind map_type in
      begin match m_expr with
        | None   -> [mk_global_val_init name map_type_ind @@
                      mk_ind @@ mk_empty map_type]
        | Some e -> (* add a vid *)
          let e' = mk_ind @@ add_vid_to_init_val map_type e in
          [mk_global_val_init name map_type_ind e']
      end
    with Not_found -> [] end
  | _ -> []

(* return ast for map declarations, adding the vid *)
let modify_map_decl_ast c ast =
  let decls = U.globals_of_program ast in
  init_vid_k3 ::
    (List.flatten @@ list_map (modify_global_map c) decls)

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
  let trig_ast = U.expr_of_code trig_decl in
  let s_idx = stmt_idx_in_t c trig stmt in
  block_nth trig_ast s_idx

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
  let vid_avar = AVar("vid", t_vid) in
  let args, body = U.decompose_lambda lambda in
  let new_args = match args with
    | ATuple(vs) -> ATuple(P.map_add_v vid_avar vs)
    | v          -> ATuple(P.map_add_v vid_avar [v])
  in
  mk_lambda new_args body

(* messages about possible modifications to the ast for map accesses *)
(* these are passed around while folding over the tree *)
type msg_t = AddVidMsg | NopMsg | DelMsg

(* the maps here have a buffer suffix *)

(* add vid to all map accesses. This is a complicated function that
 * has to dig through the entire AST, but it's pretty resilient *)
let modify_map_add_vid (c:config) ast stmt =
  let lmap_alias = "existing_out_tier" in
  let lmap = P.lhs_map_of_stmt c.p stmt in
  let maps_with_existing_out_tier stmt =
    let maps = P.map_names_ids_of_stmt c.p stmt in
    (lmap_alias, lmap) :: maps
  in
  let lmap_name  = P.map_name_of c.p lmap in
  let lmap_types = P.map_types_with_v_for c.p lmap in
  let maps_n_id  = maps_with_existing_out_tier stmt in
  let map_names  = fst_many maps_n_id in
  (* names of maps with one dimension in this statement *)
  let maps_n_1d  = fst_many @@ List.filter
    (fun (_, m) -> null @@ P.map_types_no_val_for c.p m) maps_n_id in
  let var_vid    = mk_var "vid" in
  (* add a vid to a tuple *)
  let modify_tuple e =
    mk_tuple @@ P.map_add_v var_vid @@ U.extract_if_tuple e
  in
  (* Sometimes we only find out that a node needs to have a vid from a higher
   * vid in the tree, and we need to add it top-down. The main use for this
   * right now is for combine *)
  let rec add_vid_from_above e =
    match U.tag_of_expr e with
    | Flatten ->
        let body = U.decompose_flatten e in
        mk_flatten @@ add_vid_from_above body
    | Lambda args ->
        let _, body = U.decompose_lambda e in
        mk_lambda args @@ add_vid_from_above body
    | Map -> let lambda, col = U.decompose_map e in
        mk_map
          (add_vid_from_above lambda) col
    | Combine -> let x, y = U.decompose_combine e in
        mk_combine (add_vid_from_above x) (add_vid_from_above y)
    | Singleton t -> let x = U.decompose_singleton e in
        mk_singleton t @@ add_vid_from_above x
    | Apply -> (* this should be a let *)
        let lambda, input = U.decompose_apply e in
        mk_apply (add_vid_from_above lambda) input
    | _ -> modify_tuple e
    (* TODO: handle combining more variables *)
  in
  (* modify a direct map read, such as in a slice or a peek. col is the
   * collection, pat_m is an option pattern (for slice) *)
  let modify_map_read c col pat_m =
  match U.tag_of_expr col with
    | Var id ->
      let m = try List.assoc id maps_n_id (* includes existing_out_tier *)
              with Not_found -> raise @@ UnhandledModification(
                "No "^id^ " map found in stmt "^string_of_int stmt)
      in
      let buf_col =
        (* if this isn't the lmap (in which case it's stored locally)
         * adjust the name of the map to be a buffer *)
        if id <> lmap_alias && id <> lmap_name then
          mk_var @@ P.buf_of_stmt_map stmt id
        else col in
      (* get the latest vid values for this map *)
      mk_bind buf_col "__x" @@
      map_latest_vid_vals c (mk_var "__x") pat_m m ~keep_vid:false

    | _ -> raise (UnhandledModification ("Cannot handle non-var in slice"))
  in

  (* we return a message to the higher levels in the tree,
   * and the new tree node *)
  let rec modify e msgs path =
    let get_msg i = at msgs i in
    let msg_vid i = if null msgs then false else at msgs i = AddVidMsg in
    let msg_del i = if null msgs then false else at msgs i = DelMsg in
    match U.tag_of_expr e with

    (* a lambda simply passes through a message *)
    | Lambda _ -> get_msg 0, e

    | Insert _ -> let (col, elem) = U.decompose_insert e in
        NopMsg, mk_insert col @@ modify_tuple elem

    (* deletes need to be removed, since we have versioning ie. we don't delete
     * anything *)
    | Delete _ -> DelMsg, e

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
      NopMsg, modify_map_read c col pat_m

    | Iterate ->
      let (lambda, col) = U.decompose_iterate e in
      (* if our lambda requests a deletion, we delete *)
      if msg_del 0 then DelMsg, e
      (* if our collection added a vid, we need to do it in the lambda *)
      else if msg_vid 1 then
        let lambda' = add_vid_to_lambda_args lambda in
        NopMsg, mk_iter lambda' col
      else NopMsg, e

    (* handle the case of map, which appears in some files *)
    | Map -> let (lambda, col) = U.decompose_map e in
      if msg_vid 1 then
        (* if our collection added a vid, we need to do it in the lambda *)
        let mod_lambda = add_vid_to_lambda_args lambda in
        NopMsg, mk_map mod_lambda col
      else NopMsg, e

    (* handle a case of a lambda applied to an lmap (i.e. a let statement *)
    (* in this case, we modify the types of the lambda vars themselves *)
    | Apply  -> let (lambda, arg) = U.decompose_apply e in
      begin match U.tag_of_expr arg with
        | Var id when id = lmap_name ->
          begin match (U.typed_vars_of_lambda lambda, snd @@ U.decompose_lambda lambda) with
            | ([arg_id, t],b) -> NopMsg,
              mk_apply
                (mk_lambda
                  (wrap_args [arg_id, wrap_t_of_map @@ wrap_ttuple lmap_types]) b)
                arg
            | _ -> raise (UnhandledModification("At Apply: "^PR.string_of_expr e)) end
        | _ -> NopMsg, e
      end

    | Combine -> let x, y = U.decompose_combine e in
      begin match msg_vid 0, msg_vid 1 with
      | true, true -> AddVidMsg, e
      | true, _    -> AddVidMsg, mk_combine x @@ add_vid_from_above y
      | _, true    -> AddVidMsg, mk_combine (add_vid_from_above x) y
      | _, _       -> NopMsg, e
      end

    | Var name when List.mem name map_names -> AddVidMsg, e

    (* if any statements in a block requested deletion, delete
     * those statements *)
    | Block -> let ss = U.decompose_block e in
      let s_msg = list_zip ss msgs in
      let ss' = fst @@ List.split @@
        List.filter (fun (_, msg) -> msg <> DelMsg) s_msg in
      NopMsg, mk_block ss'

    | Peek -> let col = U.decompose_peek e in
      begin match U.tag_of_expr col with
      (* match only for maps with one dimension *)
      | Var name when List.mem name maps_n_1d ->
          (* we use the same function as Seek, except we don't supply a pattern
          * (no bound vars) *)
          NopMsg, mk_peek @@ modify_map_read c col None
      | _ -> NopMsg, e
      end

    | _ -> NopMsg, e
  in Tree.modify_tree_bu_with_path_and_msgs ast modify

(* this delta extraction is very brittle, since it's tailored to the way the M3
 * to K3 calculations are written. *)
let delta_action c ast stmt m_target_trigger ~corrective =
  let lmap = P.lhs_map_of_stmt c.p stmt in
  let lmap_types = P.map_types_with_v_for c.p lmap in
  let lmap_type = wrap_t_of_map @@ wrap_ttuple lmap_types in
  (* we need to know how the map is accessed in the statement. *)
  let lmap_bindings = P.find_lmap_bindings_in_stmt c.p stmt lmap in
  let lmap_bind_ids_v = P.map_ids_add_v @@ fst_many lmap_bindings
  in
  (* let existing_out_tier = ..., which we remove *)
  let lambda, arg = U.decompose_apply ast in
  let params, body = U.decompose_lambda lambda in
  if U.vars_of_arg params <> ["existing_out_tier"] then
    failwith "sanity check fail: expected existing_out_tier";
  match U.tag_of_expr body with
  | Apply ->
    (* simple modification - sending a single tuple of data *)
    (* this is something like prod_ret_x's let *)
    let lambda2, arg2 = U.decompose_apply body in
    let params2, body2 = U.decompose_lambda lambda2 in
    let delta_name = hd @@ U.vars_of_lambda lambda2 in
    let full_names = lmap_bind_ids_v @ [delta_name] in
    let full_vars =
        P.map_add_v (mk_var "vid") @@
          [mk_singleton
            lmap_type @@
            mk_tuple @@ ids_to_vars full_names
          ] in

    (* modify the delta itself *)
    mk_apply
      (mk_lambda params2 @@
        mk_block @@
          [ (* we add the delta to all following vids,
              * and we send it for correctives *)
            mk_apply
            (mk_var @@ add_delta_to_map c lmap) @@
              (* create a single tuple to send *)
              mk_tuple @@
                (mk_var @@ P.map_name_of c.p lmap)::
                mk_cbool (if corrective then true else false)::full_vars]
            @
            (* do we need to send to another trigger *)
            begin match m_target_trigger with
            | None   -> []
            | Some t ->
              [mk_send
                (mk_ctarget t)
                K3Global.me_var @@
                mk_tuple @@ full_vars]
            end
      ) arg2 (* this is where the original calculation code is *)

  | Iterate -> (* more complex modification *)
    (* col2 contains the calculation code, lambda2 is the delta addition *)
    let lambda2, col2 = U.decompose_iterate body in
    let params2, _ = U.decompose_lambda lambda2 in
    let delta_name = "__delta_values__" in
    let delta_v_name = "__delta_with_vid__" in
    let delta_ids_types = U.typed_vars_of_arg params2 in
    let delta_types = snd_many delta_ids_types in
    (* let delta_col_type = wrap_t_of_map @@ wrap_ttuple lmap_types_no_v in *)
    (* let delta_ids_types = types_to_ids_types "d" lmap_types_no_v in *)
    let delta_col_type = wrap_t_of_map @: wrap_ttuple delta_types in
    let delta_ids = fst_many delta_ids_types in
    let delta_last_id = hd @@ list_take_end 1 delta_ids in
    let slice_ids_types = types_to_ids_types "sl" lmap_types in
    let slice_vars = ids_to_vars @@ fst_many slice_ids_types in
    (* col2 contains the calculation code *)
    mk_let delta_name delta_col_type col2 @@
    (* project vid into collection, adding any repeat values along the way *)
    mk_let delta_v_name lmap_type
      (mk_agg
        (mk_assoc_lambda'
          ["acc", lmap_type]
          delta_ids_types @@
            mk_let "slice" lmap_type
            (mk_slice
              (mk_var "acc") @@
              mk_tuple @@ (ids_to_vars @@ lmap_bind_ids_v) @ [mk_cunknown]) @@
            mk_case_ns (mk_peek @@ mk_var "slice") "slice_d"
              (* if we don't have this value, just insert *)
              (mk_block [
                mk_insert "acc" @@
                  mk_tuple @@ ids_to_vars @@ lmap_bind_ids_v@[delta_last_id];
                mk_var "acc"])
              (* otherwise add *)
              (mk_block [
                mk_let_many
                  slice_ids_types
                  (mk_var "slice_d") @@
                mk_update "acc"
                  (mk_tuple slice_vars) @@
                  mk_tuple @@ list_drop_end 1 slice_vars @
                    [mk_add
                      (list_last slice_vars) @@
                      mk_var delta_last_id];
                mk_var "acc"]))
        (mk_empty lmap_type) @@
        mk_var delta_name) @@
    mk_block @@
      (* add delta values to all following vids *)
      [mk_apply
        (mk_var @@ add_delta_to_map c lmap) @@
        mk_tuple @@
          (mk_var @@ P.map_name_of c.p lmap)::
            mk_cbool (if corrective then true else false)::
            [mk_var "vid"; mk_var delta_v_name]]
      @
      begin match m_target_trigger with
      | None -> []
      | Some t ->
        [mk_send (* send to a (corrective) target *)
          (mk_ctarget t)
          K3Global.me_var @@
          mk_tuple [mk_var "vid"; mk_var delta_v_name]]
      end

  | _ -> raise @@ UnhandledModification(
     Printf.sprintf "Bad tag [%d]: %s" (U.id_of_expr body) (PR.string_of_expr body))

(* rename a variable in an ast *)
let rename_var old_var_name new_var_name ast =
  Tree.modify_tree_bu ast @@ fun e ->
    match U.tag_of_expr e with
    | Var v when v = old_var_name -> mk_var new_var_name
    | _ -> e

(* return a modified version of the original ast for stmt s *)
let modify_ast_for_s (c:config) ast stmt trig send_to_trig =
  let ast = ast_for_s_t c ast stmt trig in
  let ast = modify_map_add_vid c ast stmt in
  let ast = delta_action c ast stmt send_to_trig ~corrective:false in
  ast

(* return a modified version of the corrective update *)
let modify_corr_ast c ast map stmt trig send_to_trig =
  let args, corr_stmt, ast = corr_ast_for_m_s c ast map stmt trig in
  let ast = modify_map_add_vid c ast stmt in
  args, delta_action c ast corr_stmt send_to_trig ~corrective:true
