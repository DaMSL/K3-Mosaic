(* Dealing with Sources and Consumption. *)
open K3
open K3Values
open K3Interpreter

val pull: consumable_t -> value_t option * consumable_t option
val pull_source: id_t -> type_t -> source_t -> value_t option
val open_file_sources: consumable_t -> consumable_t
