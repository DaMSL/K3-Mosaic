open Imperative

(* AST accessors *)

val id_of_expr : 'a expr_t -> int 
val tag_of_expr : 'a expr_t -> expr_tag_t
val meta_of_expr : 'a expr_t -> 'a

val id_of_cmd : 'a cmd_t -> int 
val tag_of_cmd : 'a cmd_t -> 'a cmd_tag_t
val meta_of_cmd : 'a cmd_t -> 'a

(* Stringification *)
val string_of_type: type_t -> string
val string_of_decl: 'a decl_t -> string
val string_of_expr: 'a expr_t -> string
val string_of_cmd: 'a cmd_t -> string

val string_of_program: 'a program_t -> string