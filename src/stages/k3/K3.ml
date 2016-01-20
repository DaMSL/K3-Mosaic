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

type container_type_t
    = TSet
    | TBag
    | TList
    | TMap
    | TSortedMap
    | TSortedSet
    | TVector
    | TVMap of IntSetSet.t option
                  (* unique values per tag? *)
    | TPolyQueue of bool * poly_tags
    | TBitSet

(* {int,string} tag * possible types *)
and poly_tags = (int * string * type_t) list

and base_type_t
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
    | TFunction     of type_t list * type_t
    | TIndirect     of type_t
    | TAlias        of id_t

and mutable_t = bool

and type_t = {
  mutable typ: base_type_t;
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

type op_t = OGt | OGeq | OLt | OLeq

(* Expressions *)
type expr_tag_t
    = Const of constant_t
    | Var   of id_t
    | Tuple
    | Ignore

    | Just
    | Nothing   of type_t

    | Empty     of type_t
    | Singleton of type_t
    | Combine
    | Size

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
    | AggregateV       (* vmap version *)
    | GroupByAggregate
    | Sort
    | Equijoin

    | At
    | AtWith
    | MinWith
    | Peek
    | PeekWithVid       (* retain the vid for a vmap *)
    | Slice
    | SliceOp of op_t     (* slice with frontier (one value >, <, etc) *)
    | FilterOp of op_t    (* all the values <, > etc *)
    | Extend
    | Insert
    | InsertAt
    | SetAll
    | Update
    | UpdateAtWith
    | UpsertWith        (* update with a default handler *)
    | UpsertWithBefore  (* update reading a frontier with a default handler *)
    | UpdateSuffix      (* update values > vid *)
    | Delete
    | DeletePrefix      (* delete <= a certain vid. save frontier at vid *)
    | DeleteAllPrefix   (* delete all values <= certain vid. save frontier at vid *)
    | DeleteAt          (* delete a vector entry and return that entry *)
    | DeleteWith        (* delete a ds entry and return that entry in a lambda if it exists *)
    | Pop
    | ClearAll          (* clean without releasing ds memory *)

    | Assign
    | Indirect
    | BindAs of id_t
    | Let of id_t list

    | Send

    (* polybuffer methods *)
    | PolyIter
    | PolyIterTag of string
    | PolyFold
    | PolyFoldTag of string
    | PolyAt of string
    | PolyAtWith of string
    | PolyInsert of string
    | PolyTagAt
    | PolySkip of bool * string (* all, tag *)
    | PolyUnpack
    | PolyReserve

(* Expression Tree *)
type expr_t = ((int * expr_tag_t) * annotation_t) tree_t

type stop_behavior_t
    = UntilCurrent
    | UntilEmpty
    | UntilEOF

(* Flow program AST *)

(* The types of sources we can have, along with the information to uniquely
 * identify them. *)
type channel_format_t = CSV | JSON | BIN

type channel_type_t
    = File          of string
    | PolyFileSeq   of id_t * id_t (* fileseq var, inorder var *)
    | Network       of address

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
    | Typedef       of id_t * type_t

(* K3 Programs *)
type program_t = (declaration_t * annotation_t) list

(* Testing *)
type check_expr_t = FileExpr of string | InlineExpr of string * expr_t

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

and Annotation : ( AnnotationType with type type_t := AST.type_t
                                   and type expr_t := AST.expr_t )
= struct

include ASTCommonImpl

(* Language components *)
type type_t = AST.type_t
type expr_t = AST.expr_t

type k3_annotation_t =
  | Property of bool * string (* is_anno *)
  | Type    of type_t

type annotation_t = k3_annotation_t list

end
