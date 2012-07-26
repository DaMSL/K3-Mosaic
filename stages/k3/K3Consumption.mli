(* Dealing with Sources and Consumption. *)
open K3
open K3Interpreter

type source_t
    = CSV of string

type senv_t = (id_t * source_t) list

val construct: senv_t -> (consumable_t -> value_t option * consumable_t option)
