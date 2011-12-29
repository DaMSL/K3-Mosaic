(* The K3 Programming Language *)

(* Generic Tree Structure *)
type ('a, 'tag) tree_t
    = Leaf of 'a * 'tag
    | Node of 'a * 'tag * ('a, 'tag) tree_t list

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

(* Identifiers *)
type id_t = string

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

(* Expressions *)
type expr_tag_t

    = Const of constant_t
    | Var   of id_t * type_t
    | Tuple

    | Empty     of type_t
    | Singleton of type_t
    | Combine
    | Range     of type_t

    | Add
    | Mult
    | Neg

    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda        of arg_t
    | AssocLambda   of arg_t * arg_t
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
    | Rank
    | Head
    | Tail

    | Member
    | Lookup
    | Update
    | Slice of int list

(* Expression Tree *)
type 'a expr_t = ('a, expr_tag_t) tree_t

(* Utilities *)

val string_of_collection_type: collection_type_t -> string
val string_of_type: type_t -> string
