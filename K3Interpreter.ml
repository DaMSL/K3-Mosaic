open Tree
open K3
open K3Typechecker

type ocaml_K3_value_t
    = OUnit
    | OBool of bool
    | OByte of int
    | OInt of int
    | OFloat of float
    | OString of string
    | OTuple of ocaml_K3_value_t list
    | ORef of ocaml_K3_value_t
    | OMaybe of ocaml_K3_value_t option
    | OSet of ocaml_K3_value_t list
    | OBag of ocaml_K3_value_t list
    | OList of ocaml_K3_value_t list

exception RuntimeError

let deref = function
    | ORef(v) -> v
    | v -> v

let rec eval env e = let ((_, t), tag), children = decompose_tree e in
    let eval' n = eval env (List.nth children n) in
    match tag with
        | Const(c) -> (
            match c with
                | CInt(n) -> OInt(n)
                | CFloat(n) -> OFloat(n)
                | CString(s) -> OString(s)
                | CBool(b) -> OBool(b)
                | CNothing -> OMaybe(None)
            )

        | Add ->
            let a, b = deref (eval' 0), deref (eval' 1) in (
                match a, b with
                    | OFloat(f1), OFloat(f2) -> OFloat(f1 +. f2)
                    | OFloat(f1), OInt(i2) -> OFloat(f1 +. float_of_int i2)
                    | OInt(i1), OFloat(f2) -> OFloat(float_of_int i1 +. f2)
                    | OInt(i1), OInt(i2) -> OInt(i1 + i2)
                    | _ -> raise RuntimeError
                )
        | Mult ->
            let a, b = deref (eval' 0), deref (eval' 1) in (
                match a, b with
                    | OFloat(f1), OFloat(f2) -> OFloat(f1 *. f2)
                    | OFloat(f1), OInt(i2) -> OFloat(f1 *. float_of_int i2)
                    | OInt(i1), OFloat(f2) -> OFloat(float_of_int i1 *. f2)
                    | OInt(i1), OInt(i2) -> OInt(i1 * i2)
                    | _ -> raise RuntimeError
                )
        | Neg ->
            let a = deref (eval' 0) in (
                match a with
                    | OBool(b) -> OBool(not b)
                    | OInt(i) -> OInt(-i)
                    | OFloat(f) -> OFloat(-.f)
                    | _ -> raise RuntimeError
            )

        | (Eq|Neq) as cmp ->
            let a, b = deref (eval' 0), deref (eval' 1) in
            let (===) = if cmp = Eq then (=) else (<>) in (
                match a, b with
                    | OBool(b1), OBool(b2) -> OBool(b1 === b2)
                    | OByte(y1), OByte(y2) -> OBool(y1 === y2)
                    | OInt(i1), OInt(i2) -> OBool(i1 === i2)
                    | OFloat(f1), OFloat(f2) -> OBool(f1 === f2)
                    | OString(s1), OString(s2) -> OBool(s1 === s2)
                    | OTuple(t1), OTuple(t2) -> OBool(List.for_all2 (===) t1 t2)
                    | OMaybe(m1), OMaybe(m2) -> OBool(m1 === m2)
                    | _ -> raise RuntimeError
            )

        | (Lt|Leq) as cmp ->
            let a, b = deref (eval' 0), deref (eval' 1) in
            let (===) = if cmp = Lt then (<) else (<=) in (
                match a, b with
                    | OBool(b1), OBool(b2) -> OBool(b1 === b2)
                    | OByte(y1), OByte(y2) -> OBool(y1 === y2)
                    | OInt(i1), OInt(i2) -> OBool(i1 === i2)
                    | OFloat(f1), OFloat(f2) -> OBool(f1 === f2)
                    | OString(s1), OString(s2) -> OBool(s1 === s2)
                    | OTuple(t1), OTuple(t2) -> OBool(List.for_all2 (===) t1 t2)
                    | OMaybe(m1), OMaybe(m2) -> OBool(m1 === m2)
                    | _ -> raise RuntimeError
            )

        | _ -> raise RuntimeError
