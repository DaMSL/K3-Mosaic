open Tree
open K3
open K3Typechecker

type 'a reified_node_t = (id_t * type_t * bool * bool) * ('a texpr_t)
type 'a reified_expr_t = ('a reified_node_t) tree_t

val print_reified_expr : 'a reified_expr_t -> unit
val string_of_reified_expr : 'a reified_expr_t -> string
  
(* Top-down name generation for reification
 * Arguments:
 * i. label * reified name option of current node (and ancestors)
 * ii. expression to reify.
 * Outputs: label * reified name option for both current node and
 *          all children.
 *)
val name_of_reification :
  (int * (id_t * type_t * bool * bool)) list   
  -> 'a texpr_t
  -> (int * (id_t * type_t * bool * bool)) list  

(* bottom-up reification of an expression, given a target variable, and
 * child reifications and their variable usages.
 *)
val reify_node :
  (int * (id_t * type_t * bool * bool)) list
  -> ('a reified_expr_t * bool) list list
  -> 'a texpr_t
  -> ('a reified_expr_t * bool) list

(* reifies/reverses a whole expression *)
val reify_expr : 'a texpr_t -> 'a reified_expr_t
val unreify_expr : 'a reified_expr_t -> 'a texpr_t
