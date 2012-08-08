open K3
open Imperative

module Util : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	(* AST accessors *)
	val id_of_expr : 'a expr_t -> int 
	val tag_of_expr : 'a expr_t -> expr_tag_t
	val meta_of_expr : 'a expr_t -> 'a
	
	val id_of_cmd : 'a cmd_t -> int 
	val tag_of_cmd : 'a cmd_t -> 'a cmd_tag_t
	val meta_of_cmd : 'a cmd_t -> 'a
	
	(* Stringification *)
  val print_type : type_t -> unit
  val print_decl : ('a -> string) -> 'a decl_t -> unit
  val print_expr : ('a -> string) -> 'a expr_t -> unit
  val print_cmd  : ('a -> string) -> 'a cmd_t -> unit
  val print_program : ('a -> string) -> 'a program_t -> unit

	val string_of_type: ?fresh:bool -> type_t -> string
	val string_of_decl: ('a -> string) -> 'a decl_t -> string
	val string_of_expr: ('a -> string) -> 'a expr_t -> string
	val string_of_cmd: ('a -> string) -> 'a cmd_t -> string
	val string_of_program: ('a -> string) -> 'a program_t -> string
	
	val var_ids_of_expr : 'a expr_t -> id_t list
	val var_ids_of_decl : 'a decl_t -> id_t list
	val var_ids_of_cmd : 'a cmd_t -> id_t list
end