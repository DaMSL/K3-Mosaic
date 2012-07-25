open Util
open Tree
open K3
open K3Typechecker
open K3Util

exception RuntimeError of int

type value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VByte of char
    | VString of string
    | VTuple of value_t list
    | VOption of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * int texpr_t
    | VAddress of string * int (* ip, port *)
    | VTarget of id_t

type eval_t = VDeclared of value_t ref | VTemp of value_t

(* Generic helpers *)
let unwrap opt = match opt with Some v -> v | _ -> failwith "invalid option unwrap"

let (<|) x f = f x
and (|>) f y = f y

let (+++) f g = fun t x -> f (g t x) x
let (++%) f g = fun t x -> f (g t) x
let (%++) f g = fun t x -> f (g t x)

let nub xs =
    let blank = Hashtbl.create (List.length xs) in
        List.iter (fun x -> Hashtbl.replace blank x ()) xs;
        Hashtbl.fold (fun h () t -> h :: t) blank []


(* Value stringification *)
let rec string_of_value v =
    match v with
    | VUnknown  -> "VUnknown"
    | VUnit     -> "VUnit"
    | VBool b   -> "VBool("^ string_of_bool b^")"
    | VInt i    -> "VInt("^ string_of_int i^")"
    | VFloat f  -> "VFloat("^ string_of_float f^")"
    | VByte c   -> "VByte("^ string_of_int (Char.code c)^")"
    | VString s -> "VString("^s^")"
    | VTuple vs -> "VTuple("^ String.concat ", " (List.map string_of_value vs)^")"
    
    | VOption vopt ->
      "VOption("^(if vopt = None then "None" else string_of_value (unwrap vopt))^")"
    
    | VSet vs  -> "VSet("^ String.concat ", " (List.map string_of_value vs)^")"
    | VBag vs  -> "VBag("^ String.concat ", " (List.map string_of_value vs)^")"
    | VList vs -> "VList("^ String.concat ", " (List.map string_of_value vs)^")"
    
    | VFunction (a, b) -> "VFunction("^ string_of_arg a ^" -> "^ string_of_expr b^")"
    | VAddress (ip,port) -> "VAddress("^ip^":"^ string_of_int port^")"
    | VTarget id -> "VTarget("^id^")"

(* Prettified error handling *)
let interpreter_error s = failwith ("interpreter: "^s)


(* Environment helpers *)
type frame_t = (id_t * value_t) list
type env_t = (id_t * value_t ref) list * frame_t list

type trigger_env_t = id_t * (env_t -> value_t -> unit) list

let lookup id (mutable_env, frame_env) =
  let rec match_in_frames frame_env =
    match frame_env with
    | [] -> raise Not_found
    | h :: t ->
        try List.assoc id h
        with Not_found -> match_in_frames t
  in
  try VTemp(match_in_frames frame_env)
  with Not_found ->
    (if List.mem_assoc id mutable_env
     then VDeclared(List.assoc id mutable_env)
     else raise Not_found)


(* Scheduling helpers *)

(* TODO: add a trigger to the global scheduling data structure, according
 * to a policy *)
let schedule target addr args = () 

(* Expression interpretation *)

let value_of_eval ev = match ev with VDeclared v_ref -> !v_ref | VTemp v -> v

let value_of_const c =
    match c with
    | CUnknown -> VUnknown
    | CUnit -> VUnit
    | CBool b -> VBool b
    | CInt i -> VInt i
    | CFloat f -> VFloat f
    | CString s -> VString s
    | CAddress (ip,port) -> VAddress (ip,port)
    | CTarget id -> VTarget id
    | CNothing -> VOption None

let rec eval_fun uuid f = 
  let strip_frame (m_env, f_env) = (m_env, List.tl f_env) in
  match f with
    | VFunction(arg, body) -> (
        match arg with
        | AVar(i, t) ->
            fun (m_env, f_env) -> fun a ->
              let new_env = m_env, ([(i, a)] :: f_env) in
              let renv, result = eval_expr new_env body in
              (strip_frame renv, result)

        | ATuple(its) ->
            fun (m_env,f_env) -> fun a ->
            let bindings = (
                match a with
                | VTuple(vs) -> List.combine (fst (List.split its)) vs
                | _ -> raise (RuntimeError uuid)
            ) in
            let new_env = m_env, (bindings :: f_env) in
            let renv, result = eval_expr new_env body in
            (strip_frame renv, result)
    )
    | _ -> raise (RuntimeError uuid)
    
and eval_expr cenv texpr =
    let ((uuid, tag), (t, _)), children = decompose_tree texpr in
    let t_erroru = t_error uuid in (* pre-curry the type error *)
    let eval_fn = eval_fun uuid in
    
    let child_value env i = 
      let renv, reval = eval_expr env (List.nth children i)
      in renv, value_of_eval reval
    in

    let child_values env = 
      let renv, revals = threaded_eval env children
      in renv, List.map value_of_eval revals
    in

    let extract_value_list x = match x with
        | VSet cl
        | VBag cl
        | VList cl -> cl
        | _ -> raise (RuntimeError uuid)
    in

    let preserve_collection f v = VTemp(match v with
        | VSet(cl) -> VSet(List.sort compare (nub (f cl)))
        | VBag(cl) -> VBag(List.sort compare (f cl))
        | VList(cl) -> VList(f cl)
        | _ -> raise (RuntimeError uuid)
        )
    in

    (* Slices use simple single-level tuple matching, rather than
     * generalized nested match for now. *)
    let match_pattern pat_v v =
      let match_or_unknown v1 v2 = match v1, v2 with
        | VUnknown, _ -> true
        | _, _ -> v1 = v2
      in 
      match pat_v, v with
      | VTuple pat_f, VTuple v_f -> 
        (try List.for_all2 match_or_unknown pat_f v_f
         with Invalid_argument _ -> false)  
      | _, _ -> pat_v = v
    in

    (* Collection modifications are all side-effects, and must replace any
     * existing binding of the named entity in the environment *)
    let modify_collection modify_f =
      let renv, parts = threaded_eval cenv children in
      begin match modify_f renv parts with
        | Some (v_ref,v) -> (v_ref := value_of_eval v; renv, VTemp VUnit)
        | None -> raise (RuntimeError uuid)
      end
    in

    let eval_binop bool_op int_op float_op = 
      let fenv, vals = child_values cenv in fenv, VTemp(
        match vals with
        | [VBool(b1); VBool(b2)] -> VBool(bool_op b1 b2)
        | [VInt(i1); VInt(i2)] -> VInt(int_op i1 i2)
        | [VInt(i1); VFloat(f2)] -> VFloat(float_op (float_of_int i1) f2)
        | [VFloat(f1); VInt(i2)] -> VFloat(float_op f1 (float_of_int i2))
        | [VFloat(f1); VFloat(f2)] -> VFloat(float_op f1 f2)
        | _ -> raise (RuntimeError uuid)
        )
    in

    let eval_cmpop cmp_op =
        let fenv, vals = child_values cenv in fenv, VTemp(
            match vals with
            | [v1; v2] -> VBool(cmp_op v1 v2)
            | _ -> raise (RuntimeError uuid)
        )
    in

    match tag with
    | Const(c) -> (cenv, VTemp(value_of_const c))
    | Var(id) -> (try cenv, lookup id cenv with Not_found -> raise (RuntimeError uuid))
    | Tuple -> let fenv, vals = child_values cenv in (fenv, VTemp(VTuple(vals)))
    | Just  ->
      let renv, rval = child_value cenv 0
      in (renv, VTemp(VOption (Some rval)))

    | Empty(ct) ->
        let name = "Empty" in
        let ctype, _ = ct <| collection_of ++% base_of |> t_erroru name @: VTBad(ct) in
        cenv, VTemp(
            match ctype with
            | TSet -> VSet([])
            | TBag -> VBag([])
            | TList -> VList([])
        )
    | Singleton(ct) ->
        let nenv, element = child_value cenv 0 in
        let name = "Singleton" in
        let ctype, _ = ct <| collection_of ++% base_of |> t_erroru name @: VTBad(ct) in
        cenv, VTemp(
            match ctype with
            | TSet -> VSet([element])
            | TBag -> VBag([element])
            | TList -> VList([element])
        )
    | Combine ->
        let nenv, components = child_values cenv in
        let left, right = (
            match components with
            | [x; y] -> (x, y)
            | _ -> raise (RuntimeError uuid)
        ) in nenv, VTemp(
            match left, right with
            | VSet(vs1), VSet(vs2) -> VSet(List.sort compare (nub (vs1 @ vs2)))
            | VBag(vb1), VBag(vb2) -> VBag(List.sort compare (vb1 @ vb2))
            | VList(vl1), VList(vl2) -> VList(vl1 @ vl2)
            | _ -> raise (RuntimeError uuid)
        )

    (* TODO: byte and string types for binary operations *)
    | Add  -> eval_binop (||) (+) (+.)
    | Mult -> eval_binop (&&) ( * ) ( *. ) 
    
    | Neg ->
        let fenv, vals = child_values cenv in fenv, VTemp(
            match vals with
            | [VBool(b)] -> VBool(not b)
            | [VInt(i)] -> VInt(-i)
            | [VFloat(f)] -> VFloat(-. f)
            | _ -> raise (RuntimeError uuid)
        )

    | Eq -> eval_cmpop (=)
    | Lt -> eval_cmpop (<)
    | Neq -> eval_cmpop (<>)
    | Leq -> eval_cmpop (<=)

    | Lambda(a) ->
      let body = List.nth children 0
      in cenv, VTemp(VFunction(a, body))

    | Apply ->
        let fenv, f = child_value cenv 0 in
        let aenv, a = child_value fenv 1 in
        let renv, reval = (eval_fn f) aenv a
        in renv, VTemp(value_of_eval reval)

    | Block ->
        let fenv, vals = threaded_eval cenv children
        in fenv, (List.nth vals (List.length vals - 1))

    | IfThenElse ->
        let penv, pred = child_value cenv 0 in (
            match pred with
            | VBool(b) when b -> eval_expr penv (List.nth children 1)
            | VBool(b) when not b -> eval_expr penv (List.nth children 2)
            | _ -> raise (RuntimeError uuid)
        )
    | Map ->
        let fenv, f = child_value cenv 0 in
        let nenv, c = child_value cenv 1 in
        let g = eval_fn f in
        let folder = fun cl -> List.fold_left (
            fun (e, r) x -> let ienv, i = g e x in (ienv, r @ [value_of_eval i])
        ) (nenv, []) cl in (
            match c with
            | VSet(cl)
            | VBag(cl)
            | VList(cl) ->
              let renv, r =  folder cl
              in renv, (preserve_collection (fun _ -> r) c)
            | _ -> raise (RuntimeError uuid)
        )
    | FilterMap ->
        let penv, p = child_value cenv 0 in
        let fenv, f = child_value penv 1 in
        let nenv, c = child_value fenv 2 in
        let p' = eval_fn p in
        let f' = eval_fn f in
        let folder = fun cl -> List.fold_left (
            fun (e, r) -> fun x ->
                let p'env, inc = p' e x in
                match value_of_eval inc with
                | VBool(true) ->
                    let ienv, i = f' e x in
                    (ienv, r @ [value_of_eval i])
                | VBool(false) -> (p'env, r)
                | _ -> raise (RuntimeError uuid)
        ) (nenv, []) cl in (
            match c with
            | VSet(cl)
            | VBag(cl)
            | VList(cl) ->
              let renv, r =  folder cl
              in renv, (preserve_collection (fun _ -> r) c)
            | _ -> raise (RuntimeError uuid)
        )
    | Flatten ->
        let nenv, c = child_value cenv 0 in
        nenv, (preserve_collection (fun vs -> List.concat (List.map extract_value_list vs)) c)

    | Aggregate ->
        let fenv, f = child_value cenv 0 in
        let zenv, z = child_value fenv 1 in
        let nenv, c = child_value zenv 2 in
        let f' = eval_fn f in
        let renv, rval = List.fold_left (
            fun (e, v) a -> 
              let renv, reval = f' e (VTuple([v; a])) in renv, value_of_eval reval
          ) (nenv, z) (extract_value_list c)
        in renv, VTemp rval

    | GroupByAggregate ->
        let genv, g = child_value cenv 0 in
        let fenv, f = child_value genv 1 in
        let zenv, z = child_value fenv 2 in
        let nenv, c = child_value zenv 3 in
        let g' = eval_fn g in
        let f' = eval_fn f in
        let cl = extract_value_list c in
        let h = Hashtbl.create (List.length cl) in
        let renv = List.fold_left (
            fun e a ->
                let kenv, key = 
                  let e,k = g' e a in e, value_of_eval k
                in
                let v = (try Hashtbl.find h key with Not_found -> z) in
                let aenv, agg = f' kenv (VTuple([v; a])) in
                Hashtbl.replace h key (value_of_eval agg); aenv
        ) nenv (extract_value_list c)
        in renv, preserve_collection (fun _ -> (Hashtbl.fold (fun k v kvs -> (VTuple([k; v]) :: kvs)) h [])) c
        
    (* TODO: range, collection modifiers, send *)
    
    | Slice ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [c;pat] -> renv, (preserve_collection (fun els -> List.filter (match_pattern pat) els) c)
        | _ -> raise (RuntimeError uuid)
      end
      
    | Peek -> 
      let renv, c = child_value cenv 0
      in renv, VTemp(List.hd (extract_value_list c))

    | Insert ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared(c_ref); v] ->
          Some(c_ref, preserve_collection (fun els -> (value_of_eval v)::els) !c_ref)
        | _ -> None)

    | Update ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared(c_ref);oldv;newv] ->
          Some(c_ref, preserve_collection (fun l ->
            (value_of_eval newv)::(List.filter ((=) (value_of_eval oldv)) l)) !c_ref)
        | _ -> None)

    | Delete ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared(c_ref);oldv] ->
          Some(c_ref, preserve_collection (fun l ->
            List.filter ((=) (value_of_eval oldv)) l) !c_ref)
        | _ -> None)
      
    | Send ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [target;addr;arg] -> (schedule target addr arg; renv, VTemp VUnit)
        | _ -> raise (RuntimeError uuid)
      end

    | _ -> raise (RuntimeError uuid)

and threaded_eval ienv texprs =
    match texprs with
    | [] -> (ienv, [])
    | h :: t ->
        let nenv, nval = eval_expr ienv h in
        let lenv, vals = threaded_eval nenv t in (lenv, nval :: vals)

(* Declaration interpretation *)

(* Returns a default value for every type in the language *)
let rec default_value ty = match ty with
    | TFunction _ -> interpreter_error "no default value avaialble for function types"
    | TValue vt -> default_isolated_value vt

and default_collection_value ct et = match ct with
    | TSet -> VSet []
    | TBag -> VBag []
    | TList -> VList []

and default_base_value bt = 
  let ierror = interpreter_error in
  match bt with
      TUnknown -> VUnknown
    | TUnit    -> VUnit 
    | TBool    -> VBool false
    | TByte    -> ierror "bytes are not implemented"
    | TInt     -> VInt 0
    | TFloat   -> VFloat 0.0
    | TString  -> ierror "strings are not implemented"

    | TMaybe   vt -> ierror "options are not implemented"
    | TTuple   ft -> VTuple (List.map default_isolated_value ft)
    
    | TCollection (ct,et) -> default_collection_value ct et
    | TAddress            -> ierror "addresses are not implemented"
    | TTarget bt          -> ierror "targets are not implemented" 

and default_isolated_value vt = match vt with
    | TIsolated (TMutable bt) -> default_base_value bt
    | TIsolated (TImmutable bt) -> default_base_value bt
    | TContained _ -> interpreter_error "invalid default contained value"


(* Returns a foreign function evaluator *)
let dispatch_foreign id = interpreter_error "foreign functions not implemented"

(* Returns a trigger evaluator *)
let trigger_eval id arg local_decls body =
  fun (m_env, f_env) -> fun a ->
    let default (id,t) = id, ref (default_isolated_value t) in
    let local_env = (List.map default local_decls)@m_env, f_env
    in (eval_fun (-1) (VFunction(arg,body))) local_env a

let env_of_program k3_program =
  let ierror = interpreter_error in
  let env_of_declaration (trig_env, (m_env, f_env)) d = match d with
      Global     (id,t,init_opt) ->
        let (rm_env, rf_env), init_val = match init_opt with
          | Some e ->
            let renv, reval = eval_expr (m_env, f_env) e
            in renv, value_of_eval reval

          | None -> (m_env, f_env), default_value t 
        in trig_env, (((id, ref init_val) :: rm_env), rf_env)

    | Foreign    (id,t) -> trig_env, (m_env, [id, dispatch_foreign id] :: f_env)

    | Trigger    (id,arg,local_decls,body) ->
      (id, trigger_eval id arg local_decls body) :: trig_env, (m_env, f_env)

    | Bind       (src_id, trig_id) -> ierror "not yet implemented"
    | Consumable c -> ierror "not yet implemented"
  in
  let env_of_stmt (trig_env, (m_env, f_env)) k3_stmt = match k3_stmt with
    | Declaration d -> env_of_declaration (trig_env, (m_env, f_env)) d 
    | _ -> trig_env, (m_env, f_env)
  in List.fold_left env_of_stmt ([],([],[])) k3_program


(* Instruction interpretation *)

(* TODO: Shyam's sources and loop code *)
let eval_instructions (trig_env, prog_env) k3_program = ()


(* Program interpretation *)

(* TODO: simulate scheduling, peers, and state per peer *)
let eval_program k3_program =
  let trig_env, env = env_of_program k3_program
  in eval_instructions (trig_env, env) k3_program