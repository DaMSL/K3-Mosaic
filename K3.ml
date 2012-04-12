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

    | Slice of type_t

    | Insert
    | Delete
    | Update

    | Peek

    | AssignToRef

    | Send

(* Expression Tree *)
type 'a expr_t = ('a, expr_tag_t) tree_t

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
    | Trigger       of id_t * arg_t * (id_t * type_t * 'a expr_t option) list * 'a expr_t
    | Bind          of id_t * id_t
    | Consumable    of id_t

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

let string_of_address a = match a with
    | Local(i) -> "Local("^i^")"
    | Remote(i, h, p) -> "Remote("^i^"@"^h^":"^string_of_int p^")"

let string_of_collection_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"

let rec string_of_base_type t = match t with
    | TUnknown  -> "TUnknown"
    | TUnit     -> "TUnit"
    | TBool     -> "TBool"
    | TByte     -> "TByte"
    | TInt      -> "TInt"
    | TFloat    -> "TFloat"
    | TString   -> "TString"

    | TTuple(t_l)
        -> "TTuple("^(String.concat ", " (List.map string_of_value_type t_l))^")"

    | TCollection(t_c, t_e)
        -> "TCollection("
            ^string_of_collection_type(t_c)^", "
            ^string_of_value_type(t_e)
        ^")"

    | TTarget(a, t)
        -> "TTarget("^string_of_address(a)^", "^string_of_base_type(t)^")"

    | TMaybe(t)
        -> "TMaybe("^string_of_base_type(t)^")"

and string_of_value_type t = match t with
    | TRef(t)
        -> "TRef("^string_of_base_type(t)^")"
    | BaseT(t) -> string_of_base_type(t)

let string_of_type t = match t with
    | TFunction(t_a, t_r)
        -> "TFunction("^string_of_value_type t_a ^", "^ string_of_value_type t_r ^")"
    | ValueT(t) -> string_of_value_type(t)

let string_of_const c = match c with
    | CBool(b)   -> "CBool("^string_of_bool(b)^")"
    | CInt(i)    -> "CInt("^string_of_int(i)^")"
    | CFloat(f)  -> "CFloat("^string_of_float(f)^")"
    | CString(s) -> "CString(\""^s^"\")"
    | CNothing   -> "CNothing"

let string_of_arg a = match a with
    | AVar(i, t) -> "AVar("^i^": "^string_of_type(ValueT(t))^")"
    | ATuple(its)
        -> "ATuple("^(String.concat ", "
                (List.map (function (i, t) -> i^": "^string_of_type(ValueT(t))) its))
        ^")"

let string_of_expr_tag tag children = match tag with
    | Const(c)  -> "Const("^string_of_const(c)^")"
    | Var(i, t) -> "Var("^i^": "^string_of_type(t)^")"
    | Tuple     -> "Tuple("^(String.concat ", " children)^")"

    | Just -> "Just("^(List.hd children)^")"

    | Empty(t) -> "Empty("^string_of_type(t)^")"
    | Singleton(t)
        -> "Singleton("^string_of_type(t)^", "^(List.hd children)^")"
    | Combine
        -> "Combine("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)
        ^")"
    | Range(t)
        -> "Range("
            ^string_of_type(t)^", "
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)
        ^")"

    | Add   -> "Add("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | Mult  -> "Mult("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | Neg   -> "Neg("^(List.nth children 0)^")"
    | Eq    -> "Eq("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | Neq   -> "Neq("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | Lt    -> "Lt("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | Leq   -> "Leq("^(List.nth children 0)^", "^(List.nth children 1)^")"

    | Lambda(a)
        -> "Lambda("^string_of_arg(a)^", "^(List.nth children 0)^")"
    | AssocLambda(a, b)
        -> "AssocLambda("
            ^string_of_arg(a)^", "
            ^string_of_arg(b)^", "
            ^(List.nth children 0)
        ^")"
    | Apply
        -> "Apply("^(List.nth children 0)^", "^(List.nth children 1)^")"

    | Block
        -> "Block("^(String.concat ", " children)^")"
    | Iterate
        -> "Iterate("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | IfThenElse
        -> "IfThenElse("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)
        ^")"

    | Map
        -> "Map("^(List.nth children 0)^", "^(List.nth children 1)^")"
    | FilterMap
        -> "FilterMap("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)
        ^")"
    | Flatten
        -> "Flatten("^(List.nth children 0)^")"
    | Aggregate
        -> "Aggregate("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)
        ^")"
    | GroupByAggregate
        -> "GroupByAggregate("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)^", "
            ^(List.nth children 3)
        ^")"
    | Sort
        -> "Sort("^(List.nth children 0)^", "^(List.nth children 1)^")"

    | Slice(t)
        -> "Slice("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)
        ^")"

    | Insert
        -> "Insert("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)
        ^")"

    | Update
        -> "Update("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)^", "
            ^(List.nth children 2)
        ^")"

    | Delete
        -> "Delete("
            ^(List.nth children 0)^", "
            ^(List.nth children 1)
        ^")"

    | Peek -> "Peek("^List.nth children 0^")"

    | AssignToRef
        -> "AssignToRef("
            ^List.nth children 0^", "
            ^List.nth children 1
        ^")"

    | Send(a)
        -> "Send("
            ^string_of_address(a)^", "
            ^(List.nth children 0)
        ^")"

let string_of_expr_meta string_of_meta =
  string_of_tree string_of_meta string_of_expr_tag

let string_of_expr e = string_of_tree (fun _ _ -> "") string_of_expr_tag e

let string_of_stop_behavior_t s = match s with
    | UntilCurrent -> "UntilCurrent"
    | UntilEmpty -> "UntilEmpty"
    | UntilEOF -> "UntilEOF"

let rec string_of_consumable c = match c with
    | Source(i, t) -> "Source("^i^", "^string_of_type(t)^")"
    | Loop(id, c) -> "Loop("^id^", "^string_of_consumable(c)^")"
    | Choice(cs)
        -> "Choice("^String.concat ", "
            (List.map string_of_consumable cs)
        ^")"

    | Sequence(cs)
        -> "Sequence("^String.concat ", "
            (List.map string_of_consumable cs)
        ^")"

    | Optional(c) -> "Optional("^string_of_consumable c^")"
    | Repeat(c, s)
        -> "Repeat("^string_of_consumable c^", "^string_of_stop_behavior_t s^")"

let string_of_declaration d = match d with
    | Global(i, t)  -> "Global("^i^", "^string_of_type(t)^")"
    | Foreign(i, t) -> "Foreign("^i^", "^string_of_type(t)^")"

    | Trigger(i, arg, ds, e)
        -> "Trigger("^i^", "^string_of_arg(arg)^", ["
            ^(String.concat ", " (List.map
                (function (i', t', e') -> "("
                    ^i'^", " ^string_of_type(t') ^", "^(
                        match e' with
                            | Some e'' -> string_of_expr(e'')
                            | None -> ""
                        )
                    ^")"
                ) ds))^"], "
            ^string_of_expr(e)
        ^")"

    | Bind(i, i') -> "Bind("^i^", "^i'^")"
    | Consumable(c) -> "Consumable("^c^")"

let string_of_instruction i = match i with
    | Consume(id) -> "Consume("^id^")"

let string_of_statement s = match s with
    | Declaration(d) -> string_of_declaration(d)
    | Instruction(i)   -> string_of_instruction(i)

let string_of_program ss
    = "K3(["^(String.concat ", " (List.map string_of_statement ss))^"])"
