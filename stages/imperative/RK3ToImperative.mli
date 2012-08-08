open K3
open K3Typechecker
open ReifiedK3
open Imperative

module Make : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	val imperative_of_reified_expr :
	  (unit -> 'a) -> (id_t * arg_t) list -> 'a reified_expr_t
    -> (type_t * 'a) cmd_t list
	
	val imperative_of_program :
	  (unit -> 'a) -> 'a tprogram_t -> (type_t * 'a) program_t
end