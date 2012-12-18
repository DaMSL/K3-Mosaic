open Util
open K3.AST
open K3Helpers

module U = K3Util
module P = ProgInfo
module T = Tree
module PR = K3Printing

exception InvalidAst of string

let id_of_trig = function
  Trigger (id, _, _, _) -> id
  | _ -> invalid_arg "id_of_trig: not a trigger"

let expr_of_trig = function
  Trigger (_, _, _, e) -> e
  | _ -> invalid_arg "expr_of_trig: not a trigger"

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

(* check for a non-map (a variable) in a stmt *)
let get_stmt_0d_maps p stmt =
  let lmap = P.lhs_map_of_stmt p stmt in
  let rmaps = P.rhs_maps_of_stmt p stmt in
  let maps = ListAsSet.uniq @: lmap::rmaps in
  List.filter (fun m -> P.map_types_no_val_for p m = []) maps

(* Change maps with no input dimensions to access by vid *)
let modify_0d_map_access p ast stmt =
  let maps = get_stmt_0d_maps p stmt in
  let lmap = P.lhs_map_of_stmt p stmt in
  let map_names_types2 = 
    List.map (fun m -> (P.map_name_of p m, P.map_types_with_v_for p m)) maps in
  (* add existing_out_tier if lmap is 0 dim *)
  let map_names_types = if List.exists (fun m -> m = lmap) maps
    then ("existing_out_tier", P.map_types_with_v_for p lmap)::map_names_types2
    else map_names_types2 in
  let modify_0d_map tree m_nm_tp =
    let m = fst m_nm_tp in
    let m_t = snd m_nm_tp in
    (* note: our second pass will add vid to the map lookup *)
    (*let map_lookup = ids_to_vars @: P.map_ids_add_v ["_"] in*)
    let modify e = 
      if U.is_peek e && U.is_var_match m @: U.decompose_peek e 
      then mk_snd m_t @:
             mk_peek @: mk_slice (mk_var m) @:
               mk_tuple [mk_const CUnknown] (* map_lookup *)
      else e in
    T.modify_tree_bu tree modify in
  List.fold_left modify_0d_map ast map_names_types

exception UnhandledModification of string

(* add vid to all map accesses *)
let modify_map_access p ast stmt =
  let lmap = P.lhs_map_of_stmt p stmt in
  let lmap_name = P.map_name_of p lmap in
  let lmap_types = P.map_types_with_v_for p lmap in
  let var_vid = mk_var "vid" in
  let modify e = match U.tag_of_expr e with
    | Insert -> let (col, elem) = U.decompose_insert e in
      mk_insert col @:
        begin match U.tag_of_expr elem with
          | Tuple  -> let xs = U.decompose_tuple e in
                      mk_tuple @: P.map_add_v var_vid xs
          | x      -> mk_tuple @: P.map_add_v var_vid [elem]
        end
    | Slice -> let (col, pat) = U.decompose_slice e in
      mk_slice col @:
        begin match U.tag_of_expr pat with
          | Tuple -> let xs = U.decompose_tuple e in
                     mk_tuple @: P.map_add_v var_vid xs
          | x     -> mk_tuple @: P.map_add_v var_vid [pat]
        end
    (* handle a case when the map is applied to a lambda *)
    | Apply  -> let (l, arg) = U.decompose_apply e in
      begin match U.tag_of_expr arg with
        | Var id when id = lmap_name -> 
          begin match (U.typed_vars_of_lambda l, U.decompose_lambda l) with
            | ([id, t],b) -> 
              mk_apply 
                (mk_lambda 
                  (wrap_args [id, wrap_tset @: wrap_ttuple lmap_types]) b)
                arg
            | _ -> raise (UnhandledModification(PR.string_of_expr e))
          end
        | _ -> mk_apply l arg
      end
    | _ -> e
  in T.modify_tree_bu ast modify

(* return a modified version of the original ast for s *)
let modify_ast_for_s p ast stmt trig = 
  let ast = ast_for_s p ast stmt trig in
  let ast = modify_0d_map_access p ast stmt in
  modify_map_access p ast stmt
  (*ast*)

