open K3.AST

exception RuntimeError of int * string

(* Interpreter representation of values *)

type eval_t = VDeclared of value_t ref | VTemp of value_t
and foreign_func_t = env_t -> env_t * eval_t

and value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VByte of char
    | VString of string
    | VTuple of value_t list
    | VOption of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * expr_t
    | VForeignFunction of arg_t * foreign_func_t
    | VAddress of address
    | VTarget of id_t

and frame_t = (id_t * value_t) list
(* an env_t is global values and frames (functional environment) *)
and env_t = (id_t * value_t ref) list * (frame_t list)
type trigger_env_t = (id_t * (env_t -> value_t -> unit)) list
type program_env_t = trigger_env_t * env_t

(* Value comparison *)
val equal_values : value_t -> value_t -> bool

(* Value stringification *)
val repr_of_value : value_t -> string
val string_of_value : value_t -> string

(* Environment stringification *)
val print_env : env_t -> unit
val print_program_env : program_env_t -> unit

val string_of_env : env_t -> string
val string_of_program_env : program_env_t -> string

(* Conversion between values and other types *)
val value_of_const : constant_t -> value_t
val type_of_value : int -> value_t -> value_type_t
val expr_of_value : int -> value_t -> expr_t

