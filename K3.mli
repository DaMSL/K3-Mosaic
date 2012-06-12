(* The K3 Programming Language *)

open Tree

(* Identifiers *)
type id_t = string

(* Addresses *)
type address_t
    = Local     of id_t
    | Remote    of id_t * id_t * int

(* Collection Types *)
type collection_type_t
    = TSet
    | TBag
    | TList

(* Basic Types *)
type base_type_t
    = TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TFloat
    | TString
    | TTuple        of value_type_t list
    | TCollection   of collection_type_t * value_type_t
    | TTarget       of address_t * base_type_t
    | TMaybe        of base_type_t

and value_type_t
    = TRef  of base_type_t
    | BaseT of base_type_t

type type_t
    = TFunction of value_type_t * value_type_t
    | ValueT    of value_type_t

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

    (* Range(start:TInt, stride:TInt, steps:TInt) -> TCollection(C, TInt):
     * Creates a new collection of length (`steps' + 1) of integers starting at
     * `start' at intervals of `stride'.
     *)
    | Range     of collection_type_t

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

    (* Peek(c:TCollection(C, T)) -> T: Get one element from collection `c'. *)
    | Peek

    | Assign
    | Deref

    (* Send(a:TTarget(N, T), args:T): Send a message of type `T' to target `a'. *)
    | Send

(* Expression Tree *)
type 'a expr_t = ('a * expr_tag_t) tree_t

type stop_behavior_t
    = UntilCurrent
    | UntilEmpty
    | UntilEOF

type consumable_t
    = Source        of id_t * type_t
    | Loop          of id_t * consumable_t

    | Choice        of consumable_t list
    | Sequence      of consumable_t list
    | Optional      of consumable_t

    | Repeat        of consumable_t * stop_behavior_t

(* Top-Level Declarations *)
type 'a declaration_t
    = Global        of id_t * type_t
    | Foreign       of id_t * type_t
    | Trigger       of id_t * arg_t * (id_t * value_type_t) list * 'a expr_t
    | Bind          of id_t * id_t
    | Consumable    of consumable_t

(* Top-Level Instructions *)
type instruction_t
    = Consume of id_t

(* All Top-Level Statements *)
type 'a statement_t
    = Declaration   of 'a declaration_t
    | Instruction   of instruction_t

(* K3 Programs *)
type 'a program_t = 'a statement_t list

(* Utilities *)

val string_of_address: address_t -> string
val string_of_collection_type: collection_type_t -> string
val string_of_base_type: base_type_t -> string
val string_of_value_type: value_type_t -> string
val string_of_type: type_t -> string
val string_of_const: constant_t -> string
val string_of_arg: arg_t -> string
val string_of_expr_tag: expr_tag_t -> string list -> string
val string_of_expr: 'a expr_t -> string

val string_of_declaration: 'a declaration_t -> string
val string_of_instruction: instruction_t -> string
val string_of_statement: 'a statement_t -> string
val string_of_program: 'a program_t -> string
