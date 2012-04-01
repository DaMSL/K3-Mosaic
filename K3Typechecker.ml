(* K3 Typechecker *)

open Tree
open K3

exception MalformedTree
exception TypeError

let check_tag_arity tag children = true

let type_of expr = let ((id, t), tag), children = decompose_tree expr in t

let deduce_constant_type c = let constant_type = match c with
        | CBool(_) -> TBool
        | CInt(_) -> TInt
        | CFloat(_) -> TFloat
        | CString(_) -> TString
        | CNothing -> TMaybe(TUnknown)
    in ValueT(BaseT(constant_type))

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
            | _ -> ValueT(BaseT(TUnknown))
        in attach_type current_type
