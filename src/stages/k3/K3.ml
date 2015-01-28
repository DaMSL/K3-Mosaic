(* The K3 Programming Language *)

open Util
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

(* LTA: All LT *)
type comp_t = LTA | LT | EQ | GT | GTA

(* multimap index *)
type index_t = HashIdx of IntSet.t
             | OrdIdx of int list * IntSet.t

let index_t_cmp x y = match x, y with
  | HashIdx s, HashIdx s'         -> IntSet.compare s s'
  (* we deliberately ignore the equality set, which is extra info *)
  | OrdIdx(l, e),  OrdIdx(l', e') -> compare l l'
  | OrdIdx _, HashIdx _           -> -1
  | HashIdx _, OrdIdx _           -> 1

module IndexSet = Set.Make(struct type t = index_t let compare = index_t_cmp end)
module IndexMap = Map.Make(struct type t = index_t let compare = index_t_cmp end)


type container_type_t
    = TSet
    | TBag
    | TList
    | TMap
    | TMultimap of IndexSet.t

type base_type_t
    = TTop
    | TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TDate (* treated like TInt, but gives us extra info *)
    | TFloat
    | TString
    | TMaybe        of type_t
    | TTuple        of type_t list
    | TCollection   of container_type_t * type_t
    | TAddress
    | TTarget       of type_t
    | TFunction     of type_t * type_t
    | TIndirect     of type_t

and mutable_t = bool

and type_t = { 
  typ: base_type_t;
  mut: bool;
  anno: annotation_t;
}

(* Arguments *)
type arg_t
    = AIgnored
    | AVar      of id_t * type_t
    | AMaybe    of arg_t
    | ATuple    of arg_t list

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

(* Expressions *)
type expr_tag_t
    = Const of constant_t
    | Var   of id_t
    | Tuple

    | Just
    | Nothing   of type_t

    | Empty     of type_t
    | Singleton of type_t
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
    | Subscript     of int (* for tuple access *)

    | Block
    | Iterate
    | IfThenElse
    | CaseOf of id_t (* first child is some, second is none *)

    | Map
    | Filter
    | Flatten
    | Aggregate
    | GroupByAggregate
    | Sort

    | Peek
    | Slice
    | SliceIdx of index_t * comp_t
    | Insert of id_t
    | Delete of id_t
    | Update of id_t

    | Assign of id_t
    | Indirect
    | BindAs of id_t

    | Send

(* Expression Tree *)
type expr_t = ((int * expr_tag_t) * annotation_t) tree_t

type stop_behavior_t
    = UntilCurrent
    | UntilEmpty
    | UntilEOF

(* Flow program AST *)

(* The types of sources we can have, along with the information to uniquely
 * identify them. *)
type channel_format_t = CSV | JSON

type channel_type_t
    = File       of string
    | Network    of address

type stream_type_t
    = RandomStream of int    (* num of entries *)
    | ConstStream  of expr_t (* constants only *)

type resource_pattern_t =
    | Terminal      of id_t
    | Choice        of resource_pattern_t list
    | Sequence      of resource_pattern_t list
    | Optional      of resource_pattern_t
    | Repeat        of resource_pattern_t * stop_behavior_t

(* TODO: produce, listen instructions *)
type instruction_t = Consume of id_t

type flow_resource_t =
  | Handle  of type_t * channel_type_t * channel_format_t
  | Stream of type_t * stream_type_t
  | Pattern of resource_pattern_t

type flow_endpoint_t =
  | Resource   of id_t * flow_resource_t
  | Code       of id_t * arg_t * (id_t * type_t * annotation_t) list * expr_t

type flow_statement_t =
  | Source      of flow_endpoint_t
  | Sink        of flow_endpoint_t
  | BindFlow    of id_t * id_t
  | Instruction of instruction_t

type flow_program_t = (flow_statement_t * annotation_t) list

(* Top-Level Declarations *)
type declaration_t
    = Global        of id_t * type_t  * expr_t option
    | Foreign       of id_t * type_t
    | Flow          of flow_program_t
    | Role          of id_t * flow_program_t
    | DefaultRole   of id_t

(* K3 Programs *)
type program_t = (declaration_t * annotation_t) list

(* Testing *)
type check_expr_t = FileExpr of string | InlineExpr of expr_t

type program_test_t =
    | ProgTest    of program_t * (expr_t * check_expr_t) list
    | NetworkTest of program_t * (expr_t * check_expr_t) list
      (* Declarations, expression to evaluate, expected value *)
    | ExprTest    of (program_t * expr_t * check_expr_t) list
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
