(* The K3 AST type signature *)

open Tree

module type ASTCommon = sig
  type id_t = string
  type address = string * int (* IP * port *)
end

module ASTCommonImpl : ASTCommon


(* AST type signature *)
module type ASTType = sig 

include ASTCommon

(* Annotations *)
type annotation_t

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
    | TDate         (* like TInt *)
    | TFloat
    | TString
    | TMaybe        of value_type_t
    | TTuple        of value_type_t list
    | TCollection   of container_type_t * value_type_t
    | TAddress
    | TTarget       of base_type_t

and mutable_type_t
    = TMutable      of base_type_t * annotation_t
    | TImmutable    of base_type_t * annotation_t

and value_type_t
    = TIsolated     of mutable_type_t
    | TContained    of mutable_type_t

type type_t
    = TFunction of value_type_t * value_type_t
    | TValue    of value_type_t

(* Arguments *)
type arg_t
    = AIgnored
    | AVar      of id_t * value_type_t
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
    | Nothing   of value_type_t

    | Empty     of value_type_t
    | Singleton of value_type_t
    | Combine

    (* Range(start:TInt, stride:TInt, steps:TInt) -> TCollection(C, TInt):
     * Creates a new collection of length (`steps' + 1) of integers starting at
     * `start' at intervals of `stride'.
     *)
    | Range     of container_type_t

    (* Add(x:T, y:T') -> T'': Overloaded disjunction; acts as addition for
     * numeric types and logical disjunction for booleans. `TInt' is promoted to
     * `TFloat' if required.
     *)
    | Add

    (* Mult(x:T, y:T') -> T'': Overloaded conjunction; acts as multiplication
     * for numeric types and logical conjunction for booleans. `TInt' is
     * promoted to `TFloat' if required.
     *)
    | Mult

    (* Neg(x:T) -> T: Overloaded negation for numeric and boolean types. *)
    | Neg

    (* [=,<,!=,<=](x:T, y:T -> TBool): Comparison Operators *)
    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda        of arg_t

    | Apply

    (* Block(e_1:TUnit, e_2:TUnit, ... , e_n:TUnit, e:T) -> T: Execute
     * side-effecting operations `e_1' - `e_n', then execute and return the
     * value of `e'.
     *)
    | Block

    (* Iterate(c:TCollection(C, T), f:(T -> TUnit)) -> TUnit: Execute the
     * side-effecting operation `f' on each element of collection `c'.
     *)
    | Iterate

    (* IfThenElse(p:TBool, t:T, e:T) -> T: `t' if `p' is True, `e' otherwise. *)
    | IfThenElse

    (* Map(f:(T -> T'), c:TCollection(C, T)) -> TCollection(C, T'): Create a new
     * collection by applying function `f' to each element in `c'.
     *)
    | Map

    (* FilterMap(p:(T' -> TBool), f:(T -> T'), c:TCollection(C, T)): Create a
     * new collection by applying function `f' to each element in `c', and
     * keeping only those that satisfy the predicate `p'.
     *)
    | FilterMap

    (* Flatten(c:TCollection(C, TCollection(C', T))) -> TCollection(C', T):
     * Remove one layer of collection nesting.
     *)
    | Flatten

    (* Aggregate(f:((T', T) -> T'), z:T', c:TCollection(C, T)) -> T': Aggregate
     * the elements of a collection into a single value by repeated application
     * of the function `f', along with an initial value `z'.
     *)
    | Aggregate

    (* GroupByAggregate(g:(T -> T''), f:((T', T) -> T'), z:T', c:TCollection(C,
     * T)) -> TCollection(C, (T'', T')): Group the elements of collection `c'
     * using the grouping function `g', and aggregate each group as in Aggregate
     * with `f' and `z'. Return a collection of (group-key, aggregate-value)
     * records.
     *)
    | GroupByAggregate

    (* Sort(c:TCollection(C, T), f:((x:T, y:T) -> TBool)) -> TCollection(TList,
     * T): Sort a collection, and return a list. The second argument is a
     * less-than operator for the element types.
     *)
    | Sort

    (* Peek(c:TCollection(C, T)) -> T: Get one element from collection `c'. *)
    | Peek

    (* Slice(c:TCollection(C, T), p) -> TCollection(C, T): Retrieve a
     * sub-collection from a given collection, according to a partially
     * specified element pattern.
     *
     * The pattern `p' is an underspecified element of type T, which forms a set
     * of equality predicates over the collection `c'. The slice operation
     * returns the sub-collection of `c' whose elements satisfy these equality
     * predicates.
     *)
    | Slice

    (* Insert(c, x) -> TUnit: Insert item with value `x' into collection `c'. *)
    | Insert

    (* Delete(c, x) -> TUnit: Delete item with value `x' from collection `c'. *)
    | Delete

    (* Update(c, x, y) -> TUnit: Update item with value `x' to have value `y' in
     * collection `c'.
     *)
    | Update

    | Assign
    | Deref

    (* Send(a:TTarget(T), b:TAddress, args:T): Send a message of type `T' to target `a'. *)
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
    = RandomStream of int
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
  | Code       of id_t * arg_t * (id_t * value_type_t * annotation_t) list * expr_t

type flow_statement_t =
  | Source      of flow_endpoint_t
  | Sink        of flow_endpoint_t
  | Bind        of id_t * id_t
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
(* Expressions to be tested may come from files or may be directly specified *)
type check_expr_t = FileExpr of string | InlineExpr of expr_t

(* Program, expected global values *)
type program_test_t =
    | ProgTest    of program_t * (expr_t * check_expr_t) list
    | NetworkTest of program_t * (expr_t * check_expr_t) list
      (* Declarations, expression to evaluate, expected value *)
    | ExprTest    of (program_t * expr_t * check_expr_t) list


end
