open K3
open K3Annotations
open K3Typechecker
open ReifiedK3
open Imperative

module Make : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	val imperative_of_reified_expr :
	  (unit -> annotations_t) -> (id_t * arg_t) list -> annotations_t reified_expr_t
    -> (type_t * annotations_t) cmd_t list
	
	val imperative_of_program :
	  (unit -> annotations_t) -> annotations_t tprogram_t
    -> (type_t * annotations_t) program_t

end