open Util
open K3.AST
open K3Helpers

module KU = K3Util
module P = ProgInfo
module T = Tree

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
let block_nth exp i = match KU.tag_of_expr exp with
  | Block        -> begin
          try List.nth (KU.decompose_block exp) i
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

let modify_0d_map_access p ast stmt =
  let maps = get_stmt_0d_maps p stmt in
  let map_name m = P.map_name_of p m in
  let modify e = e in
  T.modify_tree_bu ast modify 

(* return a modified version of the original ast for s *)
let modify_ast_for_s p ast stmt trig = 
  let ast = ast_for_s p ast stmt trig in
  modify_0d_map_access p ast stmt

