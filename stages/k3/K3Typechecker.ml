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

let (+++) f g = fun t x -> f (g t x) x
let (++%) f g = fun t x -> f (g t) x
let (%++) f g = fun t x -> f (g t x)

let value_of t x =
    match t with
    | TValue(vt) -> vt
    | _ -> raise x

let function_of t x =
    match t with
    | TFunction(t_a, t_r) -> (t_a, t_r)
    | _ -> raise x

let mutable_of vt =
    match vt with
    | TIsolated(mt)
    | TContained(mt) -> mt

let collection_of bt x =
    match bt with
    | TCollection(t_c, t_e) -> (t_c, t_e)
    | _ -> raise x

let dereft mt x =
    match mt with
    | TMutable(bt) -> bt
    | _ -> raise x

let base_of vt =
    match vt with
    | TIsolated(TImmutable(bt))
    | TIsolated(TMutable(bt))
    | TContained(TImmutable(bt))
    | TContained(TMutable(bt)) -> bt

let rec contained_of vt =
    let inner_base_type = base_of vt in
    let convertable_type = (
        match inner_base_type with
        | TTuple(ts) -> TTuple(List.map contained_of ts)
        | TCollection(t_c, t_e) -> TCollection(t_c, contained_of t_e)
        | TMaybe(t_m) -> TMaybe(contained_of t_m)
        | _ -> inner_base_type
    ) in TContained(TImmutable(convertable_type))

let canonical bt = TIsolated(TImmutable(bt))

let rec assignable t_l t_r =
    let t_lb = base_of t_l in
    let t_rb = base_of t_r in
    match (t_lb, t_rb) with
    | TMaybe(t_lm), TMaybe(t_rm) when assignable t_lm t_rm -> true
    | TTuple(t_ls), TTuple(t_rs) when List.for_all2 assignable t_ls t_rs -> true
    | TCollection(t_lc, t_le), TCollection(t_rc, t_re) when assignable t_le t_re -> true
    | _ when t_lb = t_rb -> true
    | _ -> false

let rec passable t_l t_r =
    match t_l, t_r with
    | TContained(TMutable(_)), TContained(TMutable(_)) -> assignable t_l t_r
    | TContained(TMutable(_)), _ -> false
    | _ -> assignable t_l t_r

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
    in canonical constant_type

let deduce_arg_type a =
    match a with
    | AVar(i, t) -> t
    | ATuple(its) -> canonical (TTuple(snd(List.split its)))

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
            let child_types = List.map (fun e -> type_of e <| value_of |> TypeError) typed_children
            in TValue(canonical (TTuple(child_types)))
        | Just ->
            let inner_type = bind 0 <| value_of |> TypeError in
            TValue(canonical (TMaybe(inner_type)))

        | Empty(t) -> TValue(t)
        | Singleton(t) ->
            let t_c, t_e = t <| collection_of ++% base_of |> TypeError in
            let t_ne = bind 0 <| value_of |> TypeError
            in TValue(canonical (TCollection(t_c, contained_of t_ne)))
        | Combine ->
            let t_c0, t_e0 = bind 0 <| collection_of +++ base_of %++ value_of |> TypeError in
            let t_c1, t_e1 = bind 1 <| collection_of +++ base_of %++ value_of |> TypeError in

            (* Only collections of matching element types can be combined. *)
            if t_e0 <> t_e1 then raise TypeError else

            (* Determine combined collection type. *)
            let t_cr = (
                match (t_c0, t_c1) with
                    | (TList, _)    -> TList
                    | (_, TList)    -> TList
                    | (TBag, _)     -> TBag
                    | (_, TBag)     -> TBag
                    | (TSet, TSet)  -> TSet
            ) in TValue(canonical (TCollection(t_cr, contained_of t_e0)))

        | Range(t_c) ->
            let start = bind 0 <| base_of %++ value_of |> TypeError in
            let stride = bind 1 <| base_of %++ value_of |> TypeError in
            let steps = bind 2 <| base_of %++ value_of |> TypeError in
            if not(steps = TInt) then raise TypeError else
            let t_e = (
                match (start, stride) with
                | (TInt, TInt) -> TInt
                | (TFloat, TInt)
                | (TInt, TFloat)
                | (TFloat, TFloat) -> TFloat
                | _ -> raise TypeError
            ) in TValue(canonical (TCollection(t_c, TContained(TImmutable(t_e)))))

        | (Add|Mult) ->
            let t_0 = bind 0 <| base_of %++ value_of |> TypeError in
            let t_1 = bind 1 <| base_of %++ value_of |> TypeError in
            let result_type = (
                match (t_0, t_1) with
                | (TFloat, TFloat) -> TFloat
                | (TInt, TFloat) -> TFloat
                | (TFloat, TInt) -> TFloat
                | (TInt, TInt) -> TInt
                | (TBool, TBool) -> TBool
                | _ -> raise TypeError
            ) in TValue(canonical (result_type))

        | Neg ->
            let t_0 = bind 0 <| base_of %++ value_of |> TypeError in (
                match t_0 with
                | (TBool|TInt|TFloat) as t -> TValue(canonical (t))
                | _ -> raise TypeError
            )

        | (Eq|Lt|Neq|Leq) ->
            let t_0 = bind 0 <| base_of %++ value_of |> TypeError in
            let t_1 = bind 1 <| base_of %++ value_of |> TypeError in

            (* We can compare any two values whose base types are the same, and *)
            (* are comparable, regardless of if either of them are refs. *)
            if t_0 = t_1 then TValue(canonical TBool) else raise TypeError

        | Insert ->
            let t_c, t_e = bind 0 <| collection_of +++ base_of %++ value_of |> TypeError in
            let t_n = bind 1 <| value_of |> TypeError in
            if assignable t_e t_n then TValue(canonical TUnit) else raise TypeError

        | Update ->
            let t_c, t_e = bind 0 <| collection_of +++ base_of %++ value_of |> TypeError in
            let t_o = bind 1 <| value_of |> TypeError in
            let t_n = bind 2 <| value_of |> TypeError in
            if assignable t_e t_o && assignable t_e t_n then TValue(canonical TUnit)
            else raise TypeError

        | Delete ->
            let t_c, t_e = bind 0 <| collection_of +++ base_of %++ value_of |> TypeError in
            let t_n = bind 1 <| value_of |> TypeError in
            if assignable t_e t_n then TValue(canonical TUnit) else raise TypeError

        | Peek ->
            let t_c, t_e = bind 0 <| collection_of +++ base_of %++ value_of |> TypeError in TValue(t_e)

        | Assign ->
            let t_l = bind 0 <| dereft +++ mutable_of %++ value_of |> TypeError in
            let t_r = bind 1 <| value_of |> TypeError in
            if assignable (canonical t_l) t_r then TValue(canonical TUnit) else raise TypeError

        | Deref ->
            let t_r = bind 0 <| value_of |> TypeError in
            let t_u = (
                match t_r with
                | TIsolated(mt) -> TIsolated(TImmutable(mt <| dereft |> TypeError))
                | TContained(mt) -> TContained(TImmutable(mt <| dereft |> TypeError))
            ) in TValue(t_u)

        | _ -> TValue(deduce_constant_type CUnknown)
    in attach_type current_type
