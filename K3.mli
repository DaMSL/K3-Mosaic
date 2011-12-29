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

(* Utilities *)

val string_of_collection_type: collection_type_t -> string
val string_of_type: type_t -> string
