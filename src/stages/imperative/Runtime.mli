open K3.AST
open K3.Annotation
open K3Streams
open Imperative

val k3_include : string

(* Trigger target CG data *)
val target_var_id    : id_t
val target_class_id  : id_t
val target_include   : string

(* Runtime CG data *)
val runtime_var_id   : id_t
val runtime_class_id : id_t
val runtime_include  : string
val options_class_id : id_t

(* Functorized imperative construction *)
module Make : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

  val mk_target_var_id : id_t -> id_t
	val mk_trigger_dispatch_id : id_t -> id_t
  val mk_trigger_dispatch_var_id : id_t -> id_t
	val mk_queue_trigger_internal_id : id_t -> id_t
	val mk_queue_trigger_external_id : id_t -> id_t

  val runtime_type : type_t

  val mk_target_var : (unit -> annotation_t) -> id_t -> (type_t * annotation_t) expr_t

  val generate_targets :
    (unit -> annotation_t) -> protocol_spec -> (type_t * annotation_t) decl_t

  val generate_runtime :
    (unit -> annotation_t) -> protocol_spec -> (type_t * annotation_t) decl_t

end
