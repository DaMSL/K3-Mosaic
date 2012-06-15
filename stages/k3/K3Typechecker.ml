(* K3 Typechecker *)

open Tree
open K3

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree
exception TypeError

let check_tag_arity tag children =
    let length = List.length children in
    let correct_arity =
        match tag with
        | Const(_)  -> 0
        | Var(_) -> 0
        | Tuple -> length
        | Just -> 1

        | Empty(_)      -> 0
        | Singleton(_)  -> 1
        | Combine       -> 2
        | Range(_)      -> 3

        | Add   -> 2
        | Mult  -> 2
        | Neg   -> 1

        | Eq    -> 2
        | Lt    -> 2
        | Neq   -> 2
        | Leq   -> 2

        | Lambda(_) -> 1
        | Apply     -> 2

        | Block         -> length
        | Iterate       -> 2
        | IfThenElse    -> 3

        | Map               -> 2
        | FilterMap         -> 3
        | Flatten           -> 1
        | Aggregate         -> 3
        | GroupByAggregate  -> 4
        | Sort              -> 2

        | Slice(_)  -> 2
        | Insert    -> 2
        | Delete    -> 2
        | Update    -> 3
        | Peek      -> 1

        | Assign -> 2
        | Deref -> 1

        | Send -> 2
    in length = correct_arity

type 'a texpr_t = (((int * expr_tag_t) * type_t) * 'a) tree_t

let type_of texpr = let (((id, tag), t), _), children = decompose_tree texpr in t

let deduce_constant_type c =
    let constant_type =
        match c with
        | CUnit -> TUnit
        | CUnknown -> TUnknown
        | CBool(_) -> TBool
        | CInt(_) -> TInt
        | CFloat(_) -> TFloat
        | CString(_) -> TString
        | CNothing -> TMaybe(TIsolated(TImmutable(TUnknown)))
    in TIsolated(TImmutable(constant_type))

let deduce_arg_type a =
    match a with
    | AVar(i, t) -> t
    | ATuple(its) -> TIsolated(TImmutable(TTuple(snd(List.split its))))
