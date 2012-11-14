open K3.AST
open K3.Annotation
open K3Typechecker
open ReifiedK3
open Imperative

(* Functorized imperative construction *)
module Make : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	val imperative_of_reified_expr :
	  (unit -> annotation_t) -> (id_t * K3.AST.arg_t) list -> reified_expr_t
    -> (type_t * annotation_t) cmd_t list * protocol_spec
	
	val imperative_of_program :
	  (unit -> annotation_t) -> K3.AST.program_t
    -> (type_t * annotation_t) program_t

end