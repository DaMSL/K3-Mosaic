(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError of int * int

type 'a texpr_t = (type_t * 'a) expr_t

val type_of: 'a texpr_t -> type_t

val mutable_of: value_type_t -> mutable_type_t
val base_of: value_type_t -> base_type_t
val contained_of: value_type_t -> value_type_t
val canonical: base_type_t -> value_type_t

val assignable: value_type_t -> value_type_t -> bool
val passable: value_type_t -> value_type_t -> bool

val check_tag_arity: expr_tag_t -> 'child list -> bool

val deduce_constant_type: constant_t -> value_type_t
val deduce_arg_type: arg_t -> value_type_t
val deduce_expr_type: (id_t * type_t) list -> 'a expr_t -> 'a texpr_t

val deduce_program_type: (id_t * type_t) list -> 'a program_t -> (type_t * 'a) program_t
