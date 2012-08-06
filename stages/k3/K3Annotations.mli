(* A placeholder type definition for annotations *)
open K3

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

type annotation_t =
  | Data    of rigidity_t * data_annotation_t
  | Control of rigidity_t * control_annotation_t

type annotations_t = annotation_t list

val string_of_annotations : annotations_t -> string