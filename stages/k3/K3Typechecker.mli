(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError

type 'a texpr_t = (((int * expr_tag_t) * type_t) * 'a) tree_t

val type_of: 'a texpr_t -> type_t

val check_tag_arity: expr_tag_t -> 'child list -> bool

val deduce_constant_type: constant_t -> value_type_t
