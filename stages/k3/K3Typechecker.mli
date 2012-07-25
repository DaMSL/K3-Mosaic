(* K3 Typechecker *)

open Tree
open K3

exception TypeError of int * string

type error_type =
    | TMismatch of type_t * type_t
    | VTMismatch of value_type_t * value_type_t
    | BTMismatch of base_type_t * base_type_t
    | TBad of type_t
    | VTBad of value_type_t
    | BTBad of base_type_t
    | MTBad of mutable_type_t
    | TMsg of string

val t_error : int -> string -> error_type -> unit -> unit
  
val collection_of : base_type_t -> (unit -> unit) -> container_type_t * value_type_t
val mutable_of: value_type_t -> mutable_type_t
val base_of: value_type_t -> base_type_t
val contained_of: value_type_t -> value_type_t
val canonical: base_type_t -> value_type_t

(* Export the signature of typed expressions for use in downstream
 * toolchain components. *)
type 'a texpr_t = (type_t * 'a) expr_t
type 'a tprogram_t = (type_t * 'a) program_t

(* function to deduce the type of a K3 program *)
val deduce_program_type: 'a program_t -> 'a tprogram_t
