open Tree
open K3
open K3Typechecker

exception RuntimeError of int

(* TODO: remaining constant types, byte, string, addresses and targets *)
type value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VTuple of value_t list
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * int texpr_t

type frame_t = (id_t * value_t) list
type env_t = frame_t list

val string_of_value: value_t -> string

val eval_expr: env_t -> int texpr_t -> env_t * value_t
