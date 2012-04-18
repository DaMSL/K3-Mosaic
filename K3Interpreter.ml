open Tree
open K3
open K3Typechecker

type value_t
    = VUnit
    | VBool of bool
    | VByte of int
    | VInt of int
    | VFloat of float
    | VString of string
    | VTuple of value_t list
    | VRef of value_t ref
    | VMaybe of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * (int * type_t) expr_t

exception RuntimeError

let deref = function
    | VRef(v) -> !v
    | v -> v

let rec eval env e = let ((_, t), tag), children = decompose_tree e in
    let eval' n = eval env (List.nth children n) in
    match tag with
        | Const(c) -> (
            match c with
                | CInt(n) -> VInt(n)
                | CFloat(n) -> VFloat(n)
                | CString(s) -> VString(s)
                | CBool(b) -> VBool(b)
                | CNothing -> VMaybe(None)
            )

        | Var(id) -> (try List.assoc id env with Not_found -> raise RuntimeError)

        | Tuple -> VTuple(List.map (eval env) children)
        | Just -> VMaybe(Some (eval' 0))


        | Add ->
            let a, b = deref (eval' 0), deref (eval' 1) in (
                match a, b with
                    | VFloat(f1), VFloat(f2) -> VFloat(f1 +. f2)
                    | VFloat(f1), VInt(i2) -> VFloat(f1 +. float_of_int i2)
                    | VInt(i1), VFloat(f2) -> VFloat(float_of_int i1 +. f2)
                    | VInt(i1), VInt(i2) -> VInt(i1 + i2)
                    | _ -> raise RuntimeError
                )
        | Mult ->
            let a, b = deref (eval' 0), deref (eval' 1) in (
                match a, b with
                    | VFloat(f1), VFloat(f2) -> VFloat(f1 *. f2)
                    | VFloat(f1), VInt(i2) -> VFloat(f1 *. float_of_int i2)
                    | VInt(i1), VFloat(f2) -> VFloat(float_of_int i1 *. f2)
                    | VInt(i1), VInt(i2) -> VInt(i1 * i2)
                    | _ -> raise RuntimeError
                )
        | Neg ->
            let a = deref (eval' 0) in (
                match a with
                    | VBool(b) -> VBool(not b)
                    | VInt(i) -> VInt(-i)
                    | VFloat(f) -> VFloat(-.f)
                    | _ -> raise RuntimeError
            )

        | (Eq|Neq) as cmp ->
            let a, b = deref (eval' 0), deref (eval' 1) in
            let (===) = if cmp = Eq then (=) else (<>) in (
                match a, b with
                    | VBool(b1), VBool(b2) -> VBool(b1 === b2)
                    | VByte(y1), VByte(y2) -> VBool(y1 === y2)
                    | VInt(i1), VInt(i2) -> VBool(i1 === i2)
                    | VFloat(f1), VFloat(f2) -> VBool(f1 === f2)
                    | VString(s1), VString(s2) -> VBool(s1 === s2)
                    | VTuple(t1), VTuple(t2) -> VBool(List.for_all2 (===) t1 t2)
                    | VMaybe(m1), VMaybe(m2) -> VBool(m1 === m2)
                    | _ -> raise RuntimeError
            )

        | (Lt|Leq) as cmp ->
            let a, b = deref (eval' 0), deref (eval' 1) in
            let (===) = if cmp = Lt then (<) else (<=) in (
                match a, b with
                    | VBool(b1), VBool(b2) -> VBool(b1 === b2)
                    | VByte(y1), VByte(y2) -> VBool(y1 === y2)
                    | VInt(i1), VInt(i2) -> VBool(i1 === i2)
                    | VFloat(f1), VFloat(f2) -> VBool(f1 === f2)
                    | VString(s1), VString(s2) -> VBool(s1 === s2)
                    | VTuple(t1), VTuple(t2) -> VBool(List.for_all2 (===) t1 t2)
                    | VMaybe(m1), VMaybe(m2) -> VBool(m1 === m2)
                    | _ -> raise RuntimeError
            )

        | IfThenElse ->
            let p = deref (eval' 0) in
            let condition = (
                match p with
                    | VBool(b) -> b
                    | _ -> raise RuntimeError
            ) in
            if condition then eval' 1 else eval' 2

        | AssignToRef ->
            let left = eval' 0 in
            let right = deref (eval' 1) in (
                match left with
                    | VRef(r) -> r := right; VUnit
                    | _ -> raise RuntimeError
            )
        | _ -> raise RuntimeError
