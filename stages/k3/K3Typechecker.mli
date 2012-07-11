(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError of int * int (* uuid, location in program *)

val mutable_of: value_type_t -> mutable_type_t
val base_of: value_type_t -> base_type_t
val contained_of: value_type_t -> value_type_t
val canonical: base_type_t -> value_type_t

(* function to deduce the type of a K3 program *)
val deduce_program_type: 'a program_t -> (type_t * 'a) program_t
