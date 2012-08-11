(* The K3 Programming Language *)

open Tree
open K3AST
open K3Annotations

(****************************
 * Functors
 ****************************)
module GenericAST(Annotation : ASTAnnotationType) = 
struct

include ASTCommonImpl

(* Annotations *)
type annotation_t = Annotation.annotation_t

type container_type_t
    = TSet
    | TBag
    | TList

type base_type_t
    = TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TFloat
    | TString
    | TMaybe        of value_type_t
    | TTuple        of value_type_t list
    | TCollection   of container_type_t * value_type_t
    | TAddress
    | TTarget       of base_type_t

and mutable_type_t
    = TMutable      of base_type_t
    | TImmutable    of base_type_t

and value_type_t
    = TIsolated     of mutable_type_t
    | TContained    of mutable_type_t

type type_t
    = TFunction of value_type_t * value_type_t
    | TValue    of value_type_t

(* Arguments *)
type arg_t
    = AVar      of id_t * value_type_t
    | ATuple    of (id_t * value_type_t) list

(* Constants *)
type constant_t
    = CUnit
    | CUnknown
    | CBool     of bool
    | CInt      of int
    | CFloat    of float
    | CString   of string
    | CAddress  of address
    | CTarget   of id_t             (* trigger name *)
    | CNothing

(* Expressions *)
type expr_tag_t
    = Const of constant_t
    | Var   of id_t
    | Tuple

    | Just

    | Empty     of value_type_t
    | Singleton of value_type_t
    | Combine

    | Range     of container_type_t

    | Add
    | Mult
    | Neg
    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda        of arg_t
    | Apply

    | Block
    | Iterate
    | IfThenElse

    | Map
    | FilterMap
    | Flatten
    | Aggregate
    | GroupByAggregate
    | Sort

    | Peek
    | Slice
    | Insert
    | Delete
    | Update

    | Assign
    | Deref

    | Send

(* Expression Tree *)
type expr_t = ((int * expr_tag_t) * annotation_t) tree_t

type stop_behavior_t
    = UntilCurrent
    | UntilEmpty
    | UntilEOF

(* Stream and program role AST *)

(* The types of sources we can have, along with the information to uniquely
 * identify them. *)
type stream_format_t = CSV | JSON

type stream_type_t
    = File       of string
    | Network    of address

type stream_channel_t = stream_type_t * stream_format_t

type stream_pattern_t =
    | Terminal      of id_t
    | Choice        of stream_pattern_t list
    | Sequence      of stream_pattern_t list
    | Optional      of stream_pattern_t
    | Repeat        of stream_pattern_t * stop_behavior_t

type stream_t
    = Source        of id_t * type_t * stream_channel_t
    | Sink          of id_t * type_t * stream_channel_t
    | Derived       of id_t * stream_pattern_t

(* TODO: produce, listen instructions *)
type instruction_t
    = Consume of id_t

type stream_statement_t =
    | Stream        of stream_t
    | Bind          of id_t * id_t
    | Instruction   of instruction_t

type stream_program_t = stream_statement_t list

(* Top-Level Declarations *)
type declaration_t
    = Global        of id_t * type_t  * expr_t option
    | Foreign       of id_t * type_t
    | Trigger       of id_t * arg_t * (id_t * value_type_t * annotation_t) list * expr_t
    | Role          of id_t * stream_program_t
    | DefaultRole   of id_t

(* K3 Programs *)
type program_t = (declaration_t * annotation_t) list

end


(****************************
 * K3 AST implementation
 ****************************)

module rec AST : ( ASTType with type annotation_t = Annotation.annotation_t )
= GenericAST(Annotation)

and Annotation : ( AnnotationType with type type_t = AST.type_t 
                                   and type expr_t = AST.expr_t )
= struct

include ASTCommonImpl

(* Language components *)
type type_t = AST.type_t
type expr_t = AST.expr_t

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