(* K3 Typechecker *)

open Tree
open K3

exception TypeError

let type_of expr = let ((id, t), tag), children = decompose_tree expr in t

let deduce_constant_type c = match c with
    | CBool(_) -> TBool
    | CInt(_) -> TInt
    | CFloat(_) -> TFloat
    | CString(_) -> TString
    | CNothing -> TMaybe(TUnknown)

let rec deduce_type env expr =
    let (meta, tag), untyped_children = decompose_tree expr in
    let new_env = match tag with
            | Lambda(AVar(i, t)) -> (i, t) :: env
            | Lambda(ATuple(xs)) -> xs @ env
            | _ -> env
        in
    let children = List.map (deduce_type new_env) untyped_children in
    match tag with
        | Const(c) -> Leaf((meta, RefT(BaseT(deduce_constant_type c))), tag)
        | Var(id, t) -> Leaf((meta, t), tag)
        | Tuple -> let child_types = List.map (
                fun e -> match type_of e with 
                    | RefT(BaseT(bt)) -> bt
                    | _ -> raise TypeError
                ) children in
            Node((meta, RefT(BaseT(TTuple(child_types)))), tag, children)
        | _ -> Node((meta, RefT(BaseT(TUnknown))), tag, children)
