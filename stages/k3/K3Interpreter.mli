open Tree
open K3
open K3Typechecker

exception RuntimeError of int

type value_t
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
    | VFunction of arg_t * int texpr_t
    | VAddress of string * int (* ip, port *)
    | VTarget of id_t

type eval_t = VDeclared of value_t ref | VTemp of value_t

type frame_t = (id_t * value_t) list
type env_t = (id_t * value_t ref) list * (frame_t list)

val string_of_value: value_t -> string

val value_of_eval : eval_t -> value_t

val eval_expr : env_t -> int texpr_t -> env_t * eval_t

val eval_program : int tprogram_t -> unit