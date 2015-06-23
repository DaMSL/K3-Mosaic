open K3Values
open K3Values.Value
open K3.AST

type entry_t = type_t * arg_t * foreign_func_t

(* load path for csv files *)
val g_load_path : string ref

val lookup : id_t -> entry_t
val lookup_value : id_t -> value_t
val lookup_type : id_t -> type_t

val funcs : unit -> (string * entry_t) list

val csv_loader_name : string
