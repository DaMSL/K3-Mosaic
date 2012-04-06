(* K3 Typechecker *)

open Tree
open K3

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree
exception TypeError

let check_tag_arity tag children = let length = List.length children in
    let correct_arity = match tag with
        | Const(_)  -> 0
        | Var(_, _) -> 0
        | Tuple     -> length
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

        | Lambda(_)         -> 1
        | AssocLambda(_)    -> 1
        | Apply             -> 2

        | Block         -> length
        | Iterate       -> length
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

        | AssignToRef   -> 2

        | Send(_)   -> 1
    in length = correct_arity

let type_of expr = let ((id, t), tag), children = decompose_tree expr in t

let deduce_constant_type c = let constant_type = match c with
        | CBool(_) -> TBool
        | CInt(_) -> TInt
        | CFloat(_) -> TFloat
        | CString(_) -> TString
        | CNothing -> TMaybe(TUnknown)
    in ValueT(BaseT(constant_type))

let get_base_type t = match t with
    | ValueT(TRef(bt)) -> bt
    | ValueT(BaseT(bt)) -> bt
    | _ -> raise TypeError

let rec deduce_type env expr =
    let (meta, tag), untyped_children = decompose_tree expr in

    (* Check Tag Arity *)
    if not (check_tag_arity tag untyped_children) then raise MalformedTree else

    (* Determine if we need to augment the type environment. *)
    let new_env = match tag with
            | Lambda(AVar(i, t)) -> (i, t) :: env
            | Lambda(ATuple(xs)) -> xs @ env
            | _ -> env
        in

    (* Deduce the types of all children nodes. *)
    let typed_children = List.map (deduce_type new_env) untyped_children in

    (* Define some helpers. *)
    let attach_type t = recompose_tree (((meta, t), tag), typed_children) in

    (* Deduce the type of the current node. *)
    let current_type = match tag with
            | Const(c) -> deduce_constant_type c
            | Var(id, t) -> t
            | Tuple -> let child_types = List.map (
                    fun e -> match type_of e with
                        | ValueT(BaseT(bt)) -> bt
                        | _ -> raise TypeError
                    ) typed_children in
                ValueT(BaseT(TTuple(child_types)))
            | Just -> let inner_type = type_of (List.hd typed_children) in (
                    match inner_type with
                        | ValueT(BaseT(bt)) -> ValueT(BaseT(TMaybe(bt)))
                        | _ -> raise TypeError
                )

            | Empty(t)      -> t
            | Singleton(t)  -> t

            | Combine -> let a, b =
                List.nth typed_children 0,
                List.nth typed_children 1 in (
                    match (type_of a, type_of b) with
                        | (ValueT(BaseT(TCollection(a_ct, a_et))),
                          (ValueT(BaseT(TCollection(b_ct, b_et))))) ->
                            if a_et <> b_et then raise TypeError else
                                let new_ct = (
                                    match (a_ct, b_ct) with
                                        | (TList, _)    -> TList
                                        | (_, TList)    -> TList
                                        | (TBag, _)     -> TBag
                                        | (_, TBag)     -> TBag
                                        | (TSet, TSet)  -> TSet
                                ) in ValueT(BaseT(TCollection(new_ct, a_et)))
                        | _ -> raise TypeError
                    )

            | Range(t) -> t

            | (Add|Mult) -> let a, b =
                List.nth typed_children 0,
                List.nth typed_children 1 in
                    let result_type = (
                        match (get_base_type (type_of a), get_base_type (type_of b)) with
                            | (TFloat, TFloat) -> TFloat
                            | (TInt, TFloat) -> TFloat
                            | (TFloat, TInt) -> TFloat
                            | (TInt, TInt) -> TInt
                            | (TBool, TBool) -> TBool
                            | _ -> raise TypeError
                    ) in ValueT(BaseT(result_type))

            | Neg -> let a = List.hd typed_children in
                let result_type = (
                    match get_base_type (type_of a) with
                        | (TBool|TInt|TFloat) as t -> t
                        | _ -> raise TypeError
                ) in ValueT(BaseT(result_type))

            (* TODO: Define comparable, equatable types. *)
            | (Eq|Lt|Neq|Leq) -> let a, b =
                List.nth typed_children 0,
                List.nth typed_children 1 in (
                    match (type_of a, type_of b) with
                        | (ValueT(a_vt), ValueT(b_vt)) ->
                             if a_vt <> b_vt then ValueT(BaseT(TBool)) else raise TypeError
                        | _ -> raise TypeError
                )

            | Lambda(a) -> let arg_type = (
                    match a with
                        | AVar(i, t) -> (
                            match t with
                                | ValueT(vt) -> vt
                                | _ -> raise TypeError
                            )
                        | ATuple(its) -> BaseT(TTuple(
                                List.fold_right (fun t -> fun ts -> (
                                        match t with
                                            | ValueT(BaseT(bt)) -> bt
                                            | _ -> raise TypeError
                                ) :: ts) (snd (List.split its)) []
                            ))
                ) in let return_type = (
                    match type_of (List.hd typed_children) with
                        | ValueT(vt) -> vt
                        | _ -> raise TypeError
                ) in TFunction(arg_type, return_type)
            | _ -> ValueT(BaseT(TUnknown))
        in attach_type current_type
