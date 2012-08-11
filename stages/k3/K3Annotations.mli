(* A placeholder type definition for annotations *)

open K3AST

module type ASTAnnotationType = sig
  type annotation_t
end

module type AnnotationType = sig

include ASTCommon

(* Language components *)
type type_t
type expr_t

(* Unnamed tuple descriptor *)
type positions = int list

type rigidity_t =
  | Constraint
  | Hint

type data_annotation_t = 
  | FunDep  of positions * positions
  | Index   of positions
  | Unique  of positions
  | Ordered of positions
  | Sorted  of positions

type control_annotation_t =
  | Effect of id_t list (* Variables ranged over by the effect *)
  | Parallel of int     (* Degree of parallelism *)

type k3_annotation_t =
  | Data    of rigidity_t * data_annotation_t
  | Control of rigidity_t * control_annotation_t
  | Type    of type_t

type annotation_t = k3_annotation_t list

end