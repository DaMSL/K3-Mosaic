(* K3 Typechecker *)

open Tree
open K3

exception TypeError of int * string

(* Export the signature of typed expressions for use in downstream
 * toolchain components. *)
type 'a texpr_t = (type_t * 'a) expr_t
type 'a tprogram_t = (type_t * 'a) program_t

type error_type =
    | TMismatch of type_t * type_t * string
    | VTMismatch of value_type_t * value_type_t * string
    | BTMismatch of base_type_t * base_type_t * string
    | TBad of type_t
    | VTBad of value_type_t
    | BTBad of base_type_t
    | MTBad of mutable_type_t
    | TMsg of string

val t_error : int -> string -> error_type -> unit -> unit

val type_of_texpr: 'a texpr_t -> type_t
val meta_of_texpr : 'a texpr_t -> 'a
  
val collection_of : base_type_t -> (unit -> unit) -> container_type_t * value_type_t
val mutable_of: value_type_t -> mutable_type_t
val base_of: value_type_t -> base_type_t
val contained_of: value_type_t -> value_type_t
val canonical: base_type_t -> value_type_t

val assignable: value_type_t -> value_type_t -> bool
val passable: value_type_t -> value_type_t -> bool

val check_tag_arity: expr_tag_t -> 'child list -> bool

(* Type deduction for parts of a K3 program *)
val deduce_constant_type: int -> (id_t * type_t) list -> constant_t -> value_type_t
val deduce_arg_type: arg_t -> value_type_t
val deduce_expr_type: (id_t * type_t) list -> (id_t * type_t) list -> 'a expr_t -> 'a texpr_t

val deduce_program_type: 'a program_t -> 'a tprogram_t
