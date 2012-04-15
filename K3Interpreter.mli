open Tree
open K3

type ocaml_K3_value_t
    = OUnit
    | OBool of bool
    | OByte of int
    | OInt of int
    | OFloat of float
    | OString of string
    | OTuple of ocaml_K3_value_t list
    | OMaybe of ocaml_K3_value_t option
    | OSet of ocaml_K3_value_t list
    | OBag of ocaml_K3_value_t list
    | OList of ocaml_K3_value_t list

val eval: (id_t * ocaml_K3_value_t) list -> (int * type_t) expr_t -> ocaml_K3_value_t
