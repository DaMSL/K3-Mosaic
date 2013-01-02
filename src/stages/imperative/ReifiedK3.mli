open Tree
open K3.AST

type reified_node_t = (id_t * type_t * bool * bool) * expr_t
type reified_expr_t = reified_node_t tree_t

val print_reified_expr : reified_expr_t -> unit
val string_of_reified_expr : reified_expr_t -> string
  
(* Top-down name generation for reification
 * Arguments:
 * i. label * reified name option of current node (and ancestors)
 * ii. expression to reify.
 * Outputs: label * reified name option for both current node and
 *          all children.
 *)
val name_of_reification :
  (id_t * arg_t) list
  -> (int * (id_t * type_t * bool * bool)) list   
  -> expr_t
  -> (int * (id_t * type_t * bool * bool)) list  

(* bottom-up reification of an expression, given a target variable, and
 * child reifications and their variable usages.
 *)
val reify_node :
  (int * (id_t * type_t * bool * bool)) list
  -> (reified_expr_t * bool) list list
  -> expr_t
  -> (reified_expr_t * bool) list

(* reifies/reverses a whole expression *)
val reify_expr : (id_t * arg_t) list -> expr_t -> reified_expr_t
val unreify_expr : reified_expr_t -> expr_t
