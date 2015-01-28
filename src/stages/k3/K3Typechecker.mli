(* K3 Typechecker *)

open Tree

open K3.AST
open K3.Annotation

exception MalformedTree

type type_bindings_t = (id_t * type_t) list
type event_type_bindings_t = (id_t * (id_t * (type_t list)) list) list

type error_type =
    | TMismatch of type_t * type_t * string
    | BTMismatch of base_type_t * base_type_t * string
    | TBad of type_t * string
    | BTBad of base_type_t * string
    | InvalidTypeAnnotation
    | MultiplePossibleTypes of string
    | UntypedExpression
    | TMsg of string

exception TypeError of int * string * error_type

val t_error : int -> string -> error_type -> unit -> unit
val type_of_expr: expr_t -> type_t

(* Operators to simulate haskell's `fun` *)
val (<|): 'a -> ('a -> 'b) -> 'b
val (|>): ('a -> 'b) -> 'a -> 'b

(* Thread an exception between applied functions *)
val (+++): ('b -> 'x -> 'c) -> ('a -> 'x -> 'b) -> 'a -> 'x -> 'c

(* get a canonical value for a specific type *)
val canonical_value_of_type : type_t -> expr_t

(* AST integrity *)
val check_tag_arity: expr_tag_t -> 'child list -> bool

(* Type deduction for parts of a K3 program *)
val deduce_constant_type: int -> (id_t * type_t) list -> constant_t -> type_t

(* takes trigger environment, environment and expression and returns a typed
 * expression *)
val deduce_expr_type: ?override:bool -> type_bindings_t -> type_bindings_t -> expr_t -> expr_t

(* given a program, returns the typechecked program, its environment, trigger
 * environment, and resource environment *)
val type_bindings_of_program :
  program_t ->
  program_t * type_bindings_t * type_bindings_t * event_type_bindings_t

val deduce_program_type : program_t -> program_t

val deduce_program_test_type : program_test_t -> program_test_t
