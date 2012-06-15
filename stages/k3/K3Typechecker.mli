(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError

type 'a texpr_t = (((int * expr_tag_t) * type_t) * 'a) tree_t

val (<|): 'a -> ('a -> 'b) -> 'b
val (|>): ('a -> 'b) -> 'a -> 'b

val (+++): ('b -> 'x -> 'c) -> ('a -> 'x -> 'b) -> 'a -> 'x -> 'c
val (++%): ('b -> 'x -> 'c) -> ('a -> 'b) -> 'a -> 'x -> 'c
val (%++): ('b -> 'c) -> ('a -> 'x -> 'b) -> 'a -> 'x -> 'c

val type_of: 'a texpr_t -> type_t

val is_value: type_t -> exn -> value_type_t
val is_function: type_t -> exn -> value_type_t * value_type_t
val is_collection: base_type_t -> exn -> container_type_t * value_type_t
val is_mutable: mutable_type_t -> exn -> base_type_t
val is_immutable: mutable_type_t -> exn -> base_type_t

val base_of: value_type_t -> base_type_t
val contained_of: value_type_t -> value_type_t

val check_tag_arity: expr_tag_t -> 'child list -> bool

val deduce_constant_type: constant_t -> value_type_t
val deduce_arg_type: arg_t -> value_type_t
val deduce_expr_type: (id_t * type_t) list -> 'a expr_t -> 'a texpr_t
