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

let (<|) x f = f x
and (|>) f y = f y

let is_value t x =
    match t with
    | TValue(vt) -> vt
    | _ -> raise x

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

let rec deduce_expr_type cur_env utexpr =
    let ((uuid, tag), aux), untyped_children = decompose_tree utexpr in

    (* Check Tag Arity *)
    if not (check_tag_arity tag untyped_children) then raise MalformedTree else

    (* Determine if the environment to be passed down to child typechecking needs to be augmented. *)
    let env =
        match tag with
        | Lambda(AVar(i, t)) -> (i, TValue(t)) :: cur_env
        | Lambda(ATuple(its)) -> (List.map (fun (i, t) -> (i, TValue(t))) its) @ cur_env
        | _ -> cur_env
    in

    let typed_children = List.map (deduce_expr_type env) untyped_children in
    let attach_type t = mk_tree ((((uuid, tag), t), aux), typed_children) in
    let bind n = type_of (List.nth typed_children n) in

    let current_type =
        match tag with
        | Const(c) -> TValue(deduce_constant_type c)
        | Var(id) -> (try List.assoc id env with Not_found -> raise TypeError)
        | Tuple ->
            let child_types = List.map (fun e -> type_of e <| is_value |> TypeError) typed_children
            in TValue(TIsolated(TImmutable(TTuple(child_types))))
        | Just ->
            let inner_type = bind 0 <| is_value |> TypeError in
            TValue(TIsolated(TImmutable(TMaybe(inner_type))))
        | _ -> TValue(deduce_constant_type CUnknown)
    in attach_type current_type
