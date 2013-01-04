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
	
	(* Unnamed tuple descriptor *)
	type positions = int list
	
	type dependency = Element | Positions of positions
	
	type rigidity_t = Constraint | Hint
	
	type data_annotation_t = 
	  | FunDep   of positions * dependency
	  | MVFunDep of positions * dependency
	  | Unique   of positions
	  | Ordered  of positions
	  | Sequential
	  | RandomAccess
	
	type control_annotation_t =
	  | Effect of id_t list (* Variables ranged over by the effect *)
	  | Parallel of int     (* Degree of parallelism *)
	
	type k3_annotation_t =
	  | Data    of rigidity_t * data_annotation_t
	  | Control of rigidity_t * control_annotation_t
	  | Type    of type_t
	
	type annotation_t = k3_annotation_t list

end
