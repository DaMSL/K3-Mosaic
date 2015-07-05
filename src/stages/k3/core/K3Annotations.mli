(* A placeholder type definition for annotations *)

open K3AST

(* Opaque annotations *)
module type ASTAnnotationType = sig
  type annotation_t
end


(* Module signature of default annotation implementation *)
module type AnnotationType = sig

	include ASTCommon

	(* Language components *)
	type type_t
	type expr_t

	type k3_annotation_t =
	  | Property of string
	  | Type    of type_t

	type annotation_t = k3_annotation_t list

end
