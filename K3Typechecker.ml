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

let deduce_arg_type a = match a with
    | AVar(i, t) -> t
    | ATuple(its) -> BaseT(TTuple(snd(List.split its)))

let get_base_type t =
    match t with
        | ValueT(TRef(bt)) -> bt
        | ValueT(BaseT(bt)) -> bt
        | _ -> raise TypeError

let base_of_value_type t =
    match t with
        | TRef(bt) -> bt
        | BaseT(bt) -> bt

let (!:) = base_of_value_type

let equivalent_upto_base_type t1 t2 = base_of_value_type t1 = base_of_value_type t2

let (=~) = equivalent_upto_base_type

let ref_value_coerceable t1 t2 =
  match t2 with
    | TRef(bt) -> t2 = t1
    | BaseT(bt) -> t2 =~ t1

let (=~>) = ref_value_coerceable

let (@:) f x = f x
let (>>) f g = function x -> f (g x)

let rec deduce_type env expr =
    let (meta, tag), untyped_children = decompose_tree expr in

    (* Check Tag Arity *)
    if not (check_tag_arity tag untyped_children) then raise MalformedTree else

    (* Determine if we need to augment the type environment. *)
    let new_env = match tag with
            | Lambda(AVar(i, t)) -> (i, ValueT(t)) :: env
            | Lambda(ATuple(xs)) -> (List.map (fun (i, t) -> (i, ValueT(t))) xs) @ env
            | _ -> env
        in

    (* Deduce the types of all children nodes. *)
    let typed_children = List.map (deduce_type new_env) untyped_children in

    (* Define some helpers. *)
    let attach_type t = recompose_tree (((meta, t), tag), typed_children) in
    let get_child n = List.nth typed_children n in

    (* An assertion that the given type is a function. *)
    let function_or_raise t x =
        match t with
            | TFunction(a_t, r_t) -> (a_t, r_t)
            | ValueT(_) -> raise x
    in let (/=>) = function_or_raise in

    (* An assertion that the given type is a value. *)
    let value_or_raise t x =
        match t with
            | ValueT(vt) -> vt
            | TFunction(_, _) -> raise x
    in let (/=.) = value_or_raise in

    let bind = (type_of >> get_child) in

    (* Deduce the type of the current node. *)
    let current_type = match tag with
        | Const(c) -> deduce_constant_type c
        | Var(id, t) ->
            if t = ValueT(BaseT(TUnknown)) then
                (try List.assoc id new_env with | Not_found -> raise TypeError)
            else raise TypeError

        | Tuple ->
            let child_types = List.map (fun e -> type_of e /=. TypeError) typed_children in
                ValueT(BaseT(TTuple(child_types)))

        | Just -> let inner_type = bind 0 /=. TypeError in
            ValueT(BaseT(TMaybe(!:inner_type)))

        | Empty(t)      -> t
        | Singleton(t)  -> t

        | Combine ->
            let a = bind 0 /=. TypeError in
            let b = bind 1 /=. TypeError in

            (* Ensure `a' is a collection. *)
            let (a_ct, a_et) = (
                match !:a with
                    | TCollection(a_ct, a_et) -> (a_ct, a_et)
                    | _ -> raise TypeError
            ) in

            (* Ensure `b' is a collection. *)
            let (b_ct, b_et) = (
                match !:b with
                    | TCollection(a_ct, a_et) -> (a_ct, a_et)
                    | _ -> raise TypeError
            ) in

            (* Only collections of matching element types can be combined. *)
            if a_et <> b_et then raise TypeError else

            (* Determine combined collection type. *)
            let new_ct = (
                match (a_ct, b_ct) with
                    | (TList, _)    -> TList
                    | (_, TList)    -> TList
                    | (TBag, _)     -> TBag
                    | (_, TBag)     -> TBag
                    | (TSet, TSet)  -> TSet
            ) in ValueT(BaseT(TCollection(new_ct, a_et)))

        | Range(t) -> t

        | (Add|Mult) ->
            let a = bind 0 /=. TypeError in
            let b = bind 1 /=. TypeError in
            let result_type = (
                match (!:a, !:b) with
                    | (TFloat, TFloat) -> TFloat
                    | (TInt, TFloat) -> TFloat
                    | (TFloat, TInt) -> TFloat
                    | (TInt, TInt) -> TInt
                    | (TBool, TBool) -> TBool
                    | _ -> raise TypeError
            ) in ValueT(BaseT(result_type))

        | Neg ->
            let a = bind 0 /=. TypeError in (
                match !:a with
                    | (TBool|TInt|TFloat) as t -> ValueT(BaseT(t))
                    | _ -> raise TypeError
            )

        (* TODO: Define comparable, equatable types. *)
        | (Eq|Lt|Neq|Leq) ->
            let t_a = bind 0 /=. TypeError in
            let t_b = bind 1 /=. TypeError in

            (* We can compare any two values whose base types are the same, and
             * are comparable, regardless of if either of them are refs.
             *)
            if t_a =~ t_b then ValueT(BaseT(TBool)) else raise TypeError

        | Lambda(a) ->
            let return_type = bind 0 /=. TypeError
            in TFunction(deduce_arg_type a, return_type)

        | AssocLambda(a1, a2) ->
            let return_type = bind 0 /=. TypeError
            in TFunction(
                BaseT(TTuple([deduce_arg_type a1; deduce_arg_type a2])),
                return_type
            )

        | Apply ->
            let a_t, r_t = bind 0 /=> TypeError in
            let arg_type = bind 1 /=. TypeError in
            if arg_type =~> a_t then ValueT(r_t) else raise TypeError

        | Block ->
            let rec validate_block_type components = (
                match components with
                    | e :: [] -> type_of e
                    | h :: t when (type_of h) = (ValueT(BaseT(TUnit)))
                        -> validate_block_type t
                    | _ -> raise TypeError
            ) in validate_block_type typed_children

        | Iterate ->
            if List.for_all (fun x -> type_of x = (ValueT(BaseT(TUnit)))) typed_children
            then (ValueT(BaseT(TUnit))) else raise TypeError

        | IfThenElse ->
            let predicate = bind 0 /=. TypeError in
            let then_clause = bind 1 in
            let else_clause = bind 2 in

            (* The predicate must be Boolean. *)
            if not (predicate =~ BaseT(TBool)) then raise TypeError else

            (* And both branches of the expression must be the same type. *)
            if not (then_clause = else_clause) then raise TypeError else

            (* Return the branch type, not necessarily a value type. *)
            then_clause

        | Map ->
            let a_t, r_t = bind 0 /=> TypeError in
            let collection = bind 1 /=. TypeError in

            (* Ensure that `collection' is actually a collection. *)
            let c_t, e_t = (
                match !: collection with
                    | TCollection(c_t, e_t) -> (c_t, e_t)
                    | _ -> raise TypeError
            ) in

            (* Element type should be coerceable to argument type. *)
            if not(e_t =~> a_t) then raise TypeError else

            (* Mapped collection. *)
            ValueT(BaseT(TCollection(c_t, r_t)))

        | FilterMap ->
            let pa_t, pr_t = bind 0 /=> TypeError in
            let a_t, r_t = bind 1 /=> TypeError in
            let collection = bind 2 /=. TypeError in

            (* Ensure that `collection' is actually a collection. *)
            let c_t, e_t = (
                match !: collection with
                    | TCollection(c_t, e_t) -> (c_t, e_t)
                    | _ -> raise TypeError
            ) in

            (* Element type should be coerceable to argument type. *)
            if not(e_t =~> a_t) then raise TypeError else

            (* Result of map should be coerceable to filter argument. *)
            if not(r_t =~> pa_t) then raise TypeError else

            (* Mapped, filtered collection. *)
            ValueT(BaseT(TCollection(c_t, r_t)))

        | Flatten ->
            let collection = bind 0 /=. TypeError in (

            (* Ensure `collection' is actually a collection of collections. *)
            (* TODO: What if the inner collection type is a ref'd collection? *)
            match !: collection with
                | TCollection(c_t1,
                      (BaseT(TCollection(c_t2, e_t2)))
                    ) -> ValueT(BaseT(TCollection(c_t1, e_t2)))
                | _ -> raise TypeError
            )

        | Aggregate ->
            let agg_a_t, r_t = bind 0 /=> TypeError in
            let z_t = bind 1 /=. TypeError in
            let collection = bind 2 /=. TypeError in

            (* Ensure that `collection' is actually a collection. *)
            let c_t, e_t = (
                match !: collection with
                    | TCollection(c_t, e_t) -> (c_t, e_t)
                    | _ -> raise TypeError
            ) in (

                (* Ensure that the aggregator has the correct form, function of
                 * tuple consisting of zero type and element type.
                 *)
                match !: agg_a_t with
                    | TTuple([a_t1; a_t2])
                        when
                            z_t =~> a_t1 &&
                            e_t =~> a_t2 &&
                            r_t =~> a_t1 ->
                            ValueT(z_t)

                    | _ -> raise TypeError
            )

        | GroupByAggregate ->
            let a_t, k_t = bind 0 /=> TypeError in
            let agg_a_t, r_t = bind 1 /=> TypeError in
            let z_t = bind 2 /=. TypeError in
            let collection = bind 3 /=. TypeError in

            (* Ensure that `collection' is actually a collection. *)
            let c_t, e_t = (
                match !: collection with
                    | TCollection(c_t, e_t) -> (c_t, e_t)
                    | _ -> raise TypeError
            ) in

            (* Ensure that the grouper acts on the element type. *)
            if not(e_t =~> a_t) then raise TypeError else (

                (* Ensure that the aggregator has the correct form, function of
                 * tuple consisting of zero type and element type.
                 *)
                match !: agg_a_t with
                    | TTuple([a_t1; a_t2])
                        when
                            z_t =~> a_t1 &&
                            e_t =~> a_t2 &&
                            r_t =~> a_t1 ->
                            ValueT(BaseT(
                                TCollection(c_t, (BaseT(TTuple([k_t; r_t]))))
                            ))

                    | _ -> raise TypeError
            )

        | Sort ->
            let collection = bind 0 /=. TypeError in
            let cmp_a, cmp_r = bind 1 /=> TypeError in

            (* Ensure that `collection' is actually a collection. *)
            let c_t, e_t = (
                match !: collection with
                    | TCollection(c_t, e_t) -> (c_t, e_t)
                    | _ -> raise TypeError
            ) in (

                (* Ensure that the comparator is well-formed. *)
                match cmp_a, cmp_r with
                    | BaseT(TTuple([arg_lt; arg_rt])), BaseT(TBool)
                        when e_t =~> arg_lt &&
                             e_t =~> arg_rt ->
                                 ValueT(BaseT(TCollection(TList, e_t)))
                    | _ -> raise TypeError
            )
        | Insert ->
            let collection = bind 0 /=. TypeError in
            let n_t = bind 1 /=. TypeError in (

                (* Insert is only possible into ref collections. *)
                match collection with
                    | TRef(TCollection(c_t, e_t)) when n_t =~ e_t ->
                        ValueT(BaseT(TUnit))
                    | _ -> raise TypeError
            )

        | Update ->
            let collection = bind 0 /=. TypeError in
            let o_t = bind 1 /=. TypeError in
            let n_t = bind 2 /=. TypeError in (

                (* Update is only possible into collections of ref elements *)
                match !: collection with
                    | TCollection(c_t, TRef(e_t)) when !: o_t = e_t && !: n_t = e_t ->
                        ValueT(BaseT(TUnit))
                    | _ -> raise TypeError
            )

        | Delete ->
            let collection = bind 0 /=. TypeError in
            let o_t = bind 1 /=. TypeError in (

                (* Delete is only possible from a ref collection. *)
                match collection with
                    | TRef(TCollection(c_t, e_t)) when o_t =~ e_t ->
                        ValueT(BaseT(TUnit))
                    | _ -> raise TypeError
            )

        | Peek ->
            let collection = bind 0 /=. TypeError in (
                match !: collection with
                    | TCollection(c_t, e_t) -> ValueT(e_t)
                    | _ -> raise TypeError
            )

        | AssignToRef ->
            let left = bind 0 /=. TypeError in
            let right = bind 1 /=. TypeError in (

            (* AssignToRef must assign to a ref. *)
                match left with
                    | TRef(_) when right =~> left -> ValueT(BaseT(TUnit))
                    | _ -> raise TypeError
            )

        | Send ->
            let target = bind 0 /=. TypeError in
            let arg = bind 1 /=. TypeError in

            let expected_arg = (
                match !: target with
                    | TTarget(_, t) -> t
                    | _ -> raise TypeError
            ) in

            (* If a ref is passed into a send, its values are copied. *)
            if !: arg = expected_arg then ValueT(BaseT(TUnit))
            else raise TypeError

        | _ -> ValueT(BaseT(TUnknown))
    in attach_type current_type
