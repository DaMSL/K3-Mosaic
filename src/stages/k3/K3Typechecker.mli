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

(* Type comparators *)
val assignable: value_type_t -> value_type_t -> bool
val passable: value_type_t -> value_type_t -> bool

(* AST integrity *)
val check_tag_arity: expr_tag_t -> 'child list -> bool

(* Type deduction for parts of a K3 program *)
val deduce_constant_type: int -> (id_t * type_t) list -> constant_t -> value_type_t
val deduce_expr_type: (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t

val type_bindings_of_program :
  program_t
  -> program_t * type_bindings_t * type_bindings_t * event_type_bindings_t

val deduce_program_type: program_t -> program_t

