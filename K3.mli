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
type type_t
    = TUnknown
    | TUnit
    | TBool
    | TInt
    | TFloat
    | TString
    | TTuple        of type_t list
    | TCollection   of collection_type_t * type_t
    | TFunction     of type_t * type_t
    | TTarget       of address_t * type_t
    | TMaybe        of type_t

(* Arguments *)
type arg_t
    = AVar      of id_t * type_t
    | ATuple    of (id_t * type_t) list

(* Constants *)
type constant_t
    = CBool     of bool
    | CInt      of int
    | CFloat    of float
    | CString   of string
    | CNothing

(* Expressions *)
type expr_tag_t

    = Const of constant_t
    | Var   of id_t * type_t
    | Tuple

    | Just

    | Empty     of type_t
    | Singleton of type_t
    | Combine

    (* Range(start:TInt, stride:TInt, steps:TInt) -> TCollection(C, TInt):
     * Creates a new collection of length (`steps' + 1) of integers starting at
     * `start' at intervals of `stride'.
     *)
    | Range     of type_t

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

    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda        of arg_t
    | AssocLambda   of arg_t * arg_t
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

    (* Sort(c:TCollection(C, T)) -> TCollection(TList, T): Sort a collection,
     * and return a list.
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
    | Slice of int list

    (* Insert(c, x) -> TUnit: Insert item with value `x' into collection `c'. *)
    | Insert

    (* Delete(c, x) -> TUnit: Delete item with value `x' from collection `c'. *)
    | Delete

    (* Update(c, x, y) -> TUnit: Update item with value `x' to have value `y' in
     * collection `c'.
     *)
    | Update

    (* Peek(c:TCollection(C, T)): Get one element from collection `c'. *)
    | Peek

    (* Send(a:TTarget(N, T), args:T): Send a message of type `T' to target `a'. *)
    | Send of address_t

(* Expression Tree *)
type 'a expr_t = ('a, expr_tag_t) tree_t

(* Top-Level Declarations *)
type 'a declaration_t
    = Global        of id_t * type_t
    | Foreign       of id_t * type_t
    | Source        of id_t * type_t
    | InputAdaptor  of id_t * type_t
    | OutputAdaptor of id_t * type_t
    | Trigger       of id_t * arg_t * (id_t * type_t * 'a expr_t option) list * 'a expr_t
    | Bind          of id_t * id_t list
    | Loop          of id_t * id_t list

(* Top-Level Directives *)
type directive_t
    = Consume of id_t list

(* All Top-Level Statements *)
type 'a statement_t
    = Declaration   of 'a declaration_t
    | Directive     of directive_t

(* K3 Programs *)
type 'a program_t = 'a statement_t list

(* Utilities *)

val string_of_address: address_t -> string
val string_of_collection_type: collection_type_t -> string
val string_of_type: type_t -> string
val string_of_const: constant_t -> string
val string_of_arg: arg_t -> string
val string_of_expr_tag: expr_tag_t -> string list -> string
val string_of_expr_meta: ('a -> string list -> string) -> 'a expr_t -> string
val string_of_expr: 'a expr_t -> string

val string_of_declaration: 'a declaration_t -> string
val string_of_directive: directive_t -> string
val string_of_statement: 'a statement_t -> string
val string_of_program: 'a program_t -> string
