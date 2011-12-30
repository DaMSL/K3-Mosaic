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

type address_t
    = Local of id_t

(* Constants *)
type constant_t
    = CBool of bool
    | CInt    of int
    | CFloat  of float
    | CString of string

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

    | Send of address_t

(* Expression Tree *)
type 'a expr_t = ('a, expr_tag_t) tree_t

(* Trigger Effects *)
type 'a effect_t
    = Assign of id_t * 'a expr_t
    | Mutate of 'a expr_t

(* Top-Level Declarations *)
type declaration_t
    = Global        of id_t * type_t
    | Foreign       of id_t * type_t
    | Source        of id_t * type_t
    | InputAdaptor  of id_t * type_t
    | OutputAdaptor of id_t * type_t
    | Trigger       of id_t * arg_t * (id_t * type_t) list * int effect_t list
    | Bind          of id_t * id_t list
    | Loof          of id_t * id_t list

(* Top-Level Directives *)
type directive_t
    = Consume of id_t list

(* All Top-Level Statements *)
type statement_t
    = Declaration   of declaration_t
    | Directive     of directive_t

(* K3 Programs *)
type program_t = statement_t list

(* Utilities *)

let string_of_collection_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"

let rec string_of_type t = match t with
    | TUnknown  -> "TUnknown"
    | TUnit     -> "TUnit"
    | TBool     -> "TBool"
    | TInt      -> "TInt"
    | TFloat    -> "TFloat"
    | TString   -> "TString"

    | TTuple(t_l)
        -> "TTuple("^(String.concat ", " (List.map string_of_type t_l))^")"

    | TCollection(t_c, t_e)
        -> "TCollection("
            ^string_of_collection_type(t_c)^", "
            ^string_of_type(t_e)
        ^")"

    | TFunction(t_a, t_r)
        -> "TFunction("^string_of_type t_a ^", "^ string_of_type t_r ^")"
