open Util
open Tree
open K3
open K3Values
open K3Typechecker
open K3Util
open K3Consumption
open K3Runtime

type eval_t = VDeclared of value_t ref | VTemp of value_t

type stream_program_t = stream_env_t * fsm_env_t * source_bindings_t


(* Generic helpers *)

let (<|) x f = f x
and (|>) f y = f y

let (+++) f g = fun t x -> f (g t x) x
let (++%) f g = fun t x -> f (g t) x
let (%++) f g = fun t x -> f (g t x)

let nub xs =
    let blank = Hashtbl.create (List.length xs) in
        List.iter (fun x -> Hashtbl.replace blank x ()) xs;
        Hashtbl.fold (fun h () t -> h :: t) blank []


(* Prettified error handling *)
let interpreter_error s = failwith ("interpreter: "^s)


(* Environment helpers *)

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

    (* TODO: byte and string types for binary and comparison operations *)
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

    (* Start of evaluator *)
    match tag with
    | Const(c) -> (cenv, VTemp(value_of_const c))
    | Var(id) -> (try cenv, lookup id cenv with Not_found -> raise (RuntimeError uuid))
    | Tuple -> let fenv, vals = child_values cenv in (fenv, VTemp(VTuple(vals)))
    | Just  ->
      let renv, rval = child_value cenv 0
      in (renv, VTemp(VOption (Some rval)))

    (* Collection constructors *)
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

    | Range c_t ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [start; stride; VInt(steps)] ->
          let init_fn =
            let f = float_of_int in
            match start, stride with
            | VInt(x),   VInt(y)   -> (fun i -> VInt(x+(i*y)))
            | VInt(x),   VFloat(y) -> (fun i -> VFloat((f x) +. ((f i) *. y)))
            | VFloat(x), VInt(y)   -> (fun i -> VFloat(x +. ((f i) *. (f y))))
            | VFloat(x), VFloat(y) -> (fun i -> VFloat(x +. ((f i) *. y)))
            | _, _ -> raise (RuntimeError uuid)
          in 
          let l = Array.to_list (Array.init steps init_fn) in
          let reval = VTemp(match c_t with
                | TSet -> VSet(l) 
                | TBag -> VBag(l)
                | TList -> VList(l))
          in renv, reval
        | _ -> raise (RuntimeError uuid)
      end

    (* Arithmetic and comparators *)
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

    (* Control flow *)
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
        
    (* Collection transformers *)  
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
        let gb_agg_fn find_fn replace_fn = fun e a ->
            let kenv, key = 
              let e,k = g' e a in e, value_of_eval k
            in
            let v = (try find_fn key with Not_found -> z) in
            let aenv, agg = f' kenv (VTuple([v; a])) in
            replace_fn key (value_of_eval agg); aenv
        in
        
		    (* We use two different group by aggregation methods to preserve the
         * order of group=by entries in the result collection based on their
         * order in the input collection *)
		    let hash_gb_agg_method = lazy(
		      let h = Hashtbl.create 10 in
		      let agg_fn = gb_agg_fn (Hashtbl.find h) (Hashtbl.replace h) in
		      let build_fn () = Hashtbl.fold (fun k v kvs -> (VTuple([k; v]) :: kvs)) h []
		      in agg_fn, build_fn)
		    in
		
		    let order_preserving_gb_agg_method = lazy(
		      let l = ref [] in
		      let agg_fn =
		        gb_agg_fn (fun k -> List.assoc k !l)
		          (fun k v ->
		            let found,nl = List.fold_left (fun (f_acc,l_acc) (k2,v2) ->
		                if k = k2 then (true, l_acc@[k,v]) else (f_acc, l_acc@[k2,v2])
		              ) (false,[]) !l
		            in l := if found then nl else nl@[k,v])
		      in
		      let build_fn () = List.map (fun (k,v) -> VTuple([k;v])) !l
		      in agg_fn, build_fn)
		    in

        let agg_fn, build_fn = Lazy.force (match c with
          | VSet _ | VBag _ -> hash_gb_agg_method
          | VList _ -> order_preserving_gb_agg_method
          | _ ->  raise (RuntimeError uuid))
        in
        let renv = List.fold_left agg_fn nenv cl
        in renv, preserve_collection (fun _ -> build_fn ()) c

    | Sort ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [c;f] ->
          let env, l, f_val = (ref renv), (extract_value_list c), (eval_fn f) in
          let sort_fn v1 v2 =
            (* Comparator application propagates an environment since it could be stateful *) 
            let nenv, r = f_val !env (VTuple([v1; v2])) in
              env := nenv;
              match v1 = v2, value_of_eval r with
              | true, _ -> 0
              | false, VBool(true) -> -1
              | false, VBool(false) -> 1
              | _, _ -> raise (RuntimeError uuid) 
          in !env, VTemp(VList(List.sort sort_fn l))
        | _ -> raise (RuntimeError uuid)
      end      

    (* Collection accessors and modifiers *)
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
      
    (* Messaging *)
    | Send ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [target;addr;arg] -> (schedule_trigger target addr arg; renv, VTemp VUnit)
        | _ -> raise (RuntimeError uuid)
      end

    (* TODO: mutation and deref *)

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
let prepare_trigger id arg local_decls body =
  fun (m_env, f_env) -> fun a ->
    let default (id,t) = id, ref (default_isolated_value t) in
    let local_env = (List.map default local_decls)@m_env, f_env in 
    let _, reval = (eval_fun (-1) (VFunction(arg,body))) local_env a in
    match value_of_eval reval with
      | VUnit -> ()
      | _ -> raise (RuntimeError (-1))

(* Builds a trigger, global value and function environment *)
let env_of_program k3_program =
  let env_of_declaration ((trig_env, (m_env, f_env)) as env) d
  = match d with
      K3.Global (id,t,init_opt) ->
        let (rm_env, rf_env), init_val = match init_opt with
          | Some e ->
            let renv, reval = eval_expr (m_env, f_env) e
            in renv, value_of_eval reval

          | None -> (m_env, f_env), default_value t 
        in
        trig_env, (((id, ref init_val) :: rm_env), rf_env)

    | Foreign (id,t) -> 
      trig_env, (m_env, [id, dispatch_foreign id] :: f_env)

    | Trigger (id,arg,local_decls,body) ->
      (id, prepare_trigger id arg local_decls body) :: trig_env, (m_env, f_env)

    | _ -> env
  in
  let env_of_stmt env k3_stmt = match k3_stmt with
    | Declaration d -> env_of_declaration env d 
    | _ -> env
  in 
  let init_env = ([], ([],[])) in
  List.fold_left env_of_stmt init_env k3_program


(* Instruction interpretation *)
let eval_instructions env address (stream_env, fsm_env, src_bindings) k3_program =
  let run_instruction stmt = match stmt with
    | Declaration _ -> ()
    | Instruction (Consume id) ->
      try
        let fsm = List.assoc id fsm_env in
        let first, next_state = ref true, (ref None) in
        while !first || !next_state <> None do
          first := false;
          (* Per-event scheduling, that is, we run the scheduling policy
           * for each individual external event *)
          match run fsm_env fsm !next_state with
            | Some(v), ns -> 
              (schedule_event src_bindings id address [v];
               run_scheduler address env;
               next_state := ns)
            | None, ns -> next_state := ns
        done
      with Not_found -> interpreter_error ("no stream program found for "^id)
  in List.iter run_instruction k3_program


(* Program interpretation *)
let interpreter_event_loop k3_program = 
	let (s,f,b) = event_loop_of_program k3_program in
	let nf = List.map (fun (id,fsm) -> id, (initialize fsm)) f
	in (s,nf,b)

let eval_program address k3_program =
  let env = env_of_program k3_program in
  let event_loop = interpreter_event_loop k3_program in 
		initialize_scheduler address env;
		eval_instructions env address event_loop k3_program;
		print_program_env env


(* Distributed program interpretation *)

(* TODO: peer multiplexing *)
let eval_networked_program peer_list k3_program =
  let env = env_of_program k3_program in
  let event_loop = interpreter_event_loop k3_program in
  let peer_envs = List.map (fun addr -> addr, env) peer_list in
  List.iter (fun (addr,env) ->
      initialize_scheduler addr env;
      eval_instructions env addr event_loop k3_program
    ) peer_envs
