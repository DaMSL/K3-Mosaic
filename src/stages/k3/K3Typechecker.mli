(* K3 Typechecker *)

open Tree

open K3.AST
open K3.Annotation

exception MalformedTree

type type_bindings_t = (id_t * type_t) list
type event_type_bindings_t = (id_t * (id_t * (type_t list)) list) list

type error_type =
    | TMismatch of type_t * type_t * string
    | VTMismatch of value_type_t * value_type_t * string
    | BTMismatch of base_type_t * base_type_t * string
    | TBad of type_t
    | VTBad of value_type_t
    | BTBad of base_type_t
    | MTBad of mutable_type_t
    | TMsg of string

exception TypeError of int * string * error_type

val t_error : int -> string -> error_type -> unit -> unit
val type_of_expr: expr_t -> type_t

(* Operators *)
val (<|): 'a -> ('a -> 'b) -> 'b
val (|>): ('a -> 'b) -> 'a -> 'b

val (+++): ('b -> 'x -> 'c) -> ('a -> 'x -> 'b) -> 'a -> 'x -> 'c
val (++%): ('b -> 'x -> 'c) -> ('a -> 'b) -> 'a -> 'x -> 'c
val (%++): ('b -> 'c) -> ('a -> 'x -> 'b) -> 'a -> 'x -> 'c

(* Type extractors *)
val collection_of : base_type_t -> (unit -> unit) -> container_type_t * value_type_t
val mutable_of: value_type_t -> mutable_type_t
val base_of: value_type_t -> base_type_t
val annotation_of : value_type_t -> annotation_t
val contained_of: value_type_t -> value_type_t
val value_of: type_t -> (unit -> unit) -> value_type_t
val canonical: base_type_t -> value_type_t

(* get a canonical value for a specific type *)
val canonical_value_of_type : value_type_t -> expr_t

(* AST integrity *)
val check_tag_arity: expr_tag_t -> 'child list -> bool

(* Type deduction for parts of a K3 program *)
val deduce_constant_type: int -> (id_t * type_t) list -> constant_t -> value_type_t

(* takes trigger environment, environment and expression and returns a typed
 * expression *)
val deduce_expr_type: type_bindings_t -> type_bindings_t -> expr_t -> expr_t

(* given a program, returns the typechecked program, its environment, trigger
 * environment, and resource environment *)
val type_bindings_of_program :
  program_t -> 
  program_t * type_bindings_t * type_bindings_t * event_type_bindings_t

val deduce_program_type : program_t -> program_t

val deduce_program_test_type : program_test_t -> program_test_t
