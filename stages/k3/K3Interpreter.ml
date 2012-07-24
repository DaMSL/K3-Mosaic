open Tree
open K3
open K3Typechecker
open K3Util

exception RuntimeError of int

(* TODO: remaining constant types, byte, string, addresses and targets *)
type value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VTuple of value_t list
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * int texpr_t

(* Value stringification *)
let rec string_of_value v =
    match v with
    | VUnknown -> "VUnknown"
    | VUnit -> "VUnit"
    | VBool(b) -> "VBool("^ string_of_bool b^")"
    | VInt(i) -> "VInt("^ string_of_int i^")"
    | VFloat(f) -> "VFloat("^ string_of_float f^")"
    | VTuple(vs) -> "VTuple("^ String.concat ", " (List.map string_of_value vs)^")"
    | VSet(vs) -> "VSet("^ String.concat ", " (List.map string_of_value vs)^")"
    | VBag(vs) -> "VBag("^ String.concat ", " (List.map string_of_value vs)^")"
    | VList(vs) -> "VList("^ String.concat ", " (List.map string_of_value vs)^")"
    | VFunction(a, b) -> "VFunction("^ string_of_arg a ^" -> "^ string_of_expr b^")"

(* Value environment helpers *)

type frame_t = (id_t * value_t) list
type env_t = frame_t list

let rec lookup id env =
    match env with
    | [] -> raise Not_found
    | h :: t -> (
        try List.assoc id h
        with Not_found -> lookup id t
    )

let nub xs =
    let blank = Hashtbl.create (List.length xs) in
        List.iter (fun x -> Hashtbl.replace blank x ()) xs;
        Hashtbl.fold (fun h () t -> h :: t) blank []

let value_of_const c =
    match c with
    | CUnknown -> VUnknown
    | CUnit -> VUnit
    | CBool(b) -> VBool(b)
    | CInt(i) -> VInt(i)
    | CFloat(f) -> VFloat(f)
    | _ -> VUnknown

let rec eval_expr cenv texpr =
    let ((uuid, tag), (t, _)), children = decompose_tree texpr in
    let extract_value_list x = (
        match x with
        | VSet(cl)
        | VBag(cl)
        | VList(cl) -> cl
        | _ -> raise (RuntimeError uuid)
    ) in
    let mkvfunc f = (
        match f with
        | VFunction(arg, body) -> (
            match arg with
            | AVar(i, t) ->
                    fun env -> fun a ->
                        let renv, result = eval_expr ([(i, a)] :: env) body in
                        (List.tl renv, result)
            | ATuple(its) ->
                fun env -> fun a ->
                let bindings = (
                    match a with
                    | VTuple(vs) -> List.combine (fst (List.split its)) vs
                    | _ -> raise (RuntimeError uuid)
                ) in
                let renv, result = eval_expr (bindings :: env) body in
                (List.tl renv, result)
        )
        | _ -> raise (RuntimeError uuid)
    ) in
    let withVCType f v = (
        match v with
        | VSet(cl) -> VSet(List.sort compare (nub (f cl)))
        | VBag(cl) -> VBag(List.sort compare (f cl))
        | VList(cl) -> VList(f cl)
        | _ -> raise (RuntimeError uuid)
    ) in
    match tag with
    | Const(c) -> (cenv, value_of_const c)
    | Var(id) -> (try cenv, lookup id cenv with Not_found -> raise (RuntimeError uuid))
    | Tuple -> let fenv, vals = threaded_eval cenv children in (fenv, VTuple(vals))
    | Empty(ct) ->
        let ctype, _ = ct <| collection_of ++% base_of |> TypeError in cenv, (
            match ctype with
            | TSet -> VSet([])
            | TBag -> VBag([])
            | TList -> VList([])
        )
    | Singleton(ct) ->
        let nenv, element = eval_expr cenv (List.nth children 0) in
        let ctype, _ = ct <| collection_of ++% base_of |> TypeError in cenv, (
            match ctype with
            | TSet -> VSet([element])
            | TBag -> VBag([element])
            | TList -> VList([element])
        )
    | Combine ->
        let nenv, components = threaded_eval cenv children in
        let left, right = (
            match components with
            | [x; y] -> (x, y)
            | _ -> raise (RuntimeError uuid)
        ) in nenv, (
            match left, right with
            | VSet(vs1), VSet(vs2) -> VSet(List.sort compare (nub (vs1 @ vs2)))
            | VBag(vb1), VBag(vb2) -> VBag(List.sort compare (vb1 @ vb2))
            | VList(vl1), VList(vl2) -> VList(vl1 @ vl2)
            | _ -> raise (RuntimeError uuid)
        )
    | Add ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [VBool(b1); VBool(b2)] -> VBool(b1 || b2)
            | [VInt(i1); VInt(i2)] -> VInt(i1 + i2)
            | [VInt(i1); VFloat(f2)] -> VFloat(float_of_int i1 +. f2)
            | [VFloat(f1); VInt(i2)] -> VFloat(f1 +. float_of_int i2)
            | [VFloat(f1); VFloat(f2)] -> VFloat(f1 +. f2)
            | _ -> raise (RuntimeError uuid)
        )
    | Mult ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [VBool(b1); VBool(b2)] -> VBool(b1 && b2)
            | [VInt(i1); VInt(i2)] -> VInt(i1 * i2)
            | [VInt(i1); VFloat(f2)] -> VFloat(float_of_int i1 *. f2)
            | [VFloat(f1); VInt(i2)] -> VFloat(f1 *. float_of_int i2)
            | [VFloat(f1); VFloat(f2)] -> VFloat(f1 *. f2)
            | _ -> raise (RuntimeError uuid)
        )
    | Neg ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [VBool(b)] -> VBool(not b)
            | [VInt(i)] -> VInt(-i)
            | [VFloat(f)] -> VFloat(-. f)
            | _ -> raise (RuntimeError uuid)
        )
    | Eq ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [v1; v2] -> VBool(v1 = v2)
            | _ -> raise (RuntimeError uuid)
        )
    | Lt ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [v1; v2] -> VBool(v1 < v2)
            | _ -> raise (RuntimeError uuid)
        )
    | Neq ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [v1; v2] -> VBool(v1 <> v2)
            | _ -> raise (RuntimeError uuid)
        )
    | Leq ->
        let fenv, vals = threaded_eval cenv children in fenv, (
            match vals with
            | [v1; v2] -> VBool(v1 <= v2)
            | _ -> raise (RuntimeError uuid)
        )
    | Lambda(a) -> let body = List.nth children 0 in cenv, VFunction(a, body)
    | Apply ->
        let fenv, f = eval_expr cenv (List.nth children 0) in
        let aenv, a = eval_expr fenv (List.nth children 1) in
        (mkvfunc f) aenv a
    | Block ->
        let fenv, vals = threaded_eval cenv children in fenv, (List.nth vals (List.length vals - 1))
    | IfThenElse ->
        let penv, pred = eval_expr cenv (List.nth children 0) in (
            match pred with
            | VBool(b) when b -> eval_expr penv (List.nth children 1)
            | VBool(b) when not b -> eval_expr penv (List.nth children 2)
            | _ -> raise (RuntimeError uuid)
        )
    | Map ->
        let fenv, f = eval_expr cenv (List.nth children 0) in
        let nenv, c = eval_expr fenv (List.nth children 1) in
        let g = mkvfunc f in
        let folder = fun cl -> List.fold_left (
            fun (e, r) -> fun x ->
                let ienv, i = g e x in
                (ienv, r @ [i])
        ) (nenv, []) cl in (
            match c with
            | VSet(cl)
            | VBag(cl)
            | VList(cl) -> let renv, r =  folder cl in renv, (withVCType (fun _ -> r) c)
            | _ -> raise (RuntimeError uuid)
        )
    | FilterMap ->
        let penv, p = eval_expr cenv (List.nth children 0) in
        let fenv, f = eval_expr penv (List.nth children 1) in
        let nenv, c = eval_expr fenv (List.nth children 2) in
        let p' = mkvfunc p in
        let f' = mkvfunc f in
        let folder = fun cl -> List.fold_left (
            fun (e, r) -> fun x ->
                let p'env, inc = p' e x in
                match inc with
                | VBool(true) ->
                    let ienv, i = f' e x in
                    (ienv, r @ [i])
                | VBool(false) -> (p'env, r)
                | _ -> raise (RuntimeError uuid)
        ) (nenv, []) cl in (
            match c with
            | VSet(cl)
            | VBag(cl)
            | VList(cl) -> let renv, r =  folder cl in renv, (withVCType (fun _ -> r) c)
            | _ -> raise (RuntimeError uuid)
        )
    | Flatten ->
        let nenv, c = eval_expr cenv (List.nth children 0) in
        nenv, (withVCType (fun vs -> List.concat (List.map extract_value_list vs)) c)
    | Aggregate ->
        let fenv, f = eval_expr cenv (List.nth children 0) in
        let zenv, z = eval_expr fenv (List.nth children 1) in
        let nenv, c = eval_expr zenv (List.nth children 2) in
        let f' = mkvfunc f in
        List.fold_left (
            fun (e, v) a -> f' e (VTuple([v; a]))
        ) (nenv, z) (extract_value_list c)
    | GroupByAggregate ->
        let genv, g = eval_expr cenv (List.nth children 0) in
        let fenv, f = eval_expr genv (List.nth children 1) in
        let zenv, z = eval_expr fenv (List.nth children 2) in
        let nenv, c = eval_expr zenv (List.nth children 3) in
        let g' = mkvfunc g in
        let f' = mkvfunc f in
        let cl = extract_value_list c in
        let h = Hashtbl.create (List.length cl) in
        let renv = List.fold_left (
            fun e a ->
                let kenv, key = g' e a in
                let v = (try Hashtbl.find h key with Not_found -> z) in
                let aenv, agg = f' kenv (VTuple([v; a])) in
                Hashtbl.replace h key agg; aenv
        ) nenv (extract_value_list c)
        in renv, withVCType (fun _ -> (Hashtbl.fold (fun k v kvs -> (VTuple([k; v]) :: kvs)) h [])) c
        
    (* TODO: range, collection modifiers, send *)

    | _ -> raise (RuntimeError uuid)

and threaded_eval ienv texprs =
    match texprs with
    | [] -> (ienv, [])
    | h :: t ->
        let nenv, nval = eval_expr ienv h in
        let lenv, vals = threaded_eval nenv t in (lenv, nval :: vals)

