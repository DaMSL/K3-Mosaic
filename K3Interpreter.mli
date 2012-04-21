open Tree
open K3

type value_t
    = VUnit
    | VBool of bool
    | VByte of int
    | VInt of int
    | VFloat of float
    | VString of string
    | VTuple of value_t list
    | VRef of value_t ref
    | VMaybe of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of ((id_t * value_t) list -> value_t -> (id_t * value_t) list * value_t)

val eval: (id_t * value_t) list -> (int * type_t) expr_t -> (id_t * value_t) list * value_t
