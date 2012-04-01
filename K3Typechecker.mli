(* K3 Typechecker *)

open Tree
open K3

val type_of: (int * type_t) expr_t -> type_t
val deduce_constant_type: constant_t -> type_t
val deduce_type: (id_t * type_t) list -> int expr_t -> (int * type_t) expr_t
