open K3
open K3Typechecker

exception RuntimeError of int

(* Interpreter representation of values *)

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

type frame_t = (id_t * value_t) list
type env_t = (id_t * value_t ref) list * (frame_t list)
type trigger_env_t = (id_t * (env_t -> value_t -> unit)) list
type program_env_t = trigger_env_t * env_t

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list
