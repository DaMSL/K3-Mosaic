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

let string_of_rigidity r = match r with Constraint -> "Constraint" | _ -> "Hint"

let string_of_data_annotation da =
  let string_of_positions p = "["^(String.concat ";" (List.map string_of_int p))^"]" in
  let my_tag tag p = tag^"("^(string_of_positions p)^")" in
  match da with
  | FunDep  (s,d) -> (string_of_positions s)^"->"^(string_of_positions d)
  | Index   p -> my_tag "Index" p
  | Unique  p -> my_tag "Unique" p
  | Ordered p -> my_tag "Ordered" p
  | Sorted  p -> my_tag "Sorted" p

let string_of_control_annotation ca = match ca with
  | Effect ids -> "Effect("^(String.concat "," ids)^")"
  | Parallel deg -> "Parallel("^string_of_int deg^")" 

let string_of_annotation a = match a with
  | Data (r,da) -> (string_of_rigidity r)^"("^(string_of_data_annotation da)^")"
  | Control (r,ca) -> (string_of_rigidity r)^"("^(string_of_control_annotation ca)^")"

let string_of_annotations ann =
  String.concat ", " (List.map string_of_annotation ann)
