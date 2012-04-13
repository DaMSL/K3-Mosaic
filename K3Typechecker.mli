(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError

val base_of_value_type: value_type_t -> base_type_t
val (!:): value_type_t -> base_type_t

val equivalent_upto_base_type: value_type_t -> value_type_t -> bool
val (=~): value_type_t -> value_type_t -> bool

val ref_value_coerceable: value_type_t -> value_type_t -> bool
val (=~>): value_type_t -> value_type_t -> bool

val check_tag_arity: expr_tag_t -> 'child list -> bool
val type_of: (int * type_t) expr_t -> type_t
val deduce_constant_type: constant_t -> type_t
val get_base_type: type_t -> base_type_t
val deduce_expr_type: (id_t * type_t) list -> int expr_t -> (int * type_t) expr_t
