(* Dealing with Sources and Consumption. *)
open K3
open K3Interpreter

type source_t
    = CSV of string

val resolve_next: consumable_t -> source_t
val pull_source: source_t -> value_t
val pull: consumable_t -> value_t * consumable_t
