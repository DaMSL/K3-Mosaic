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

let bind_args a v =
    match a with
    | AVar(i, t) -> [(i, v)]
    | ATuple(its) -> (
        match v with
        | VTuple(vs) when List.length vs = List.length its ->
            List.combine (fst (List.split its)) vs
        | _ -> raise RuntimeError
    )

let rec eval env e = let ((_, t), tag), children = decompose_tree e in
    let eval' cenv n = eval cenv (List.nth children n) in
    match tag with
        | Const(c) -> env, (
            match c with
                | CInt(n) -> VInt(n)
                | CFloat(n) -> VFloat(n)
                | CString(s) -> VString(s)
                | CBool(b) -> VBool(b)
                | CNothing -> VMaybe(None)
            )

        | Var(id) -> env, (try List.assoc id env with Not_found -> raise RuntimeError)

        | Tuple -> let env', res' = eval_chain env children in env', VTuple(res')
        | Just -> let env', res' = eval' env 0 in env', VMaybe(Some (res'))

        | Add ->
            (* let a, b = deref (eval' 0), deref (eval' 1) in ( *)
            let enva, a' = eval' env 0 in
            let envb, b' = eval' enva 1 in
            let a, b = deref a', deref b' in envb, (
                match a, b with
                    | VFloat(f1), VFloat(f2) -> VFloat(f1 +. f2)
                    | VFloat(f1), VInt(i2) -> VFloat(f1 +. float_of_int i2)
                    | VInt(i1), VFloat(f2) -> VFloat(float_of_int i1 +. f2)
                    | VInt(i1), VInt(i2) -> VInt(i1 + i2)
                    | _ -> raise RuntimeError
            )
        | Mult ->
            let enva, a' = eval' env 0 in
            let envb, b' = eval' enva 1 in
            let a, b = deref a', deref b' in envb, (
                match a, b with
                    | VFloat(f1), VFloat(f2) -> VFloat(f1 *. f2)
                    | VFloat(f1), VInt(i2) -> VFloat(f1 *. float_of_int i2)
                    | VInt(i1), VFloat(f2) -> VFloat(float_of_int i1 *. f2)
                    | VInt(i1), VInt(i2) -> VInt(i1 * i2)
                    | _ -> raise RuntimeError
            )
        | Neg ->
            let enva, a' = eval' env 0 in
            let a = deref a' in enva, (
                match a with
                    | VBool(b) -> VBool(not b)
                    | VInt(i) -> VInt(-i)
                    | VFloat(f) -> VFloat(-.f)
                    | _ -> raise RuntimeError
            )

        | (Eq|Neq) as cmp ->
            let enva, a' = eval' env 0 in
            let envb, b' = eval' enva 1 in
            let a, b = deref a', deref b' in envb, (
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
            )

        | (Lt|Leq) as cmp ->
            let enva, a' = eval' env 0 in
            let envb, b' = eval' enva 1 in
            let a, b = deref a', deref b' in envb, (
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
            )

        | AssignToRef ->
            let envl, left = eval' env 0 in
            let envr, right' = eval' envl 1 in
            let right = deref right' in envr, (
                match left with
                    | VRef(r) -> r := right; VUnit
                    | _ -> raise RuntimeError
            )

        | Lambda(arg) -> env, VFunction(arg, List.hd children)

        | Apply ->
            let envf, f = eval' env 0 in
            let enva, a = eval' envf 1 in
            let args, body = (
                match f with
                    | VFunction(args, body) -> (args, body)
                    | _ -> raise RuntimeError
            ) in
            let bindings = bind_args args a
            in eval (bindings @ enva) body

        | Block ->
            let env', res' = eval_chain env children in env', List.hd (List.rev res')

        | IfThenElse ->
            let envp, p = eval' env 0 in
            let condition = (
                match deref p with
                    | VBool(b) -> b
                    | _ -> raise RuntimeError
            ) in
            if condition then eval' envp 1 else eval' envp 2

        | _ -> raise RuntimeError

and eval_chain env exprs =
    match exprs with
        | [] -> env, []
        | h :: t ->
            let new_env, now_val = eval env h in
            let last_env, last_vals = eval_chain new_env t in
            last_env, now_val :: last_vals
