open Util
open Tree

open K3.AST
open K3Util
open K3Helpers
open K3Printing
open K3Values
open K3Values.Value
open K3Typechecker
open K3Streams
open K3Consumption
open K3Runtime

module KP = K3Printing
module R = K3Runtime
module C = K3Consumption
module SR = K3Streams.ResourceFSM
module D = K3Dist

(* Generic helpers *)

(* set to true to debug *)
let sov = string_of_value

let debug = ref false
let debug_env = ref false
(* debug *)
let num_iters = 100000
let iters = ref 0

(* Prettified error handling *)
let int_erroru uuid ?extra fn_name s =
  let msg = fn_name^": "^s in
  let rs = lazy ("interpreter: "^msg) in
  Log.log rs `Error;
  (match extra with
  | Some (address, env) ->
    Log.log (lazy (sp ">>>> Peer %s\n" @@ string_of_address address)) `Error;
    Log.log (lazy (sp "%s\n" @@ string_of_env ~skip_empty:false ~accessed_only:false env)) `Error
  | _ -> ());
  raise @@ RuntimeError(uuid, msg)

let int_error = int_erroru (-1)

(* Environment helpers *)

let lookup id env =
  try
    VTemp(hd @@ IdMap.find id env.locals)
  with Not_found ->
    (env.accessed) := StrSet.add id !(env.accessed);
    VDeclared(IdMap.find id env.globals)

let env_modify id env f =
  try
    begin match IdMap.find id env.locals with
    | v::vs -> {env with locals=IdMap.add id (f v::vs) env.locals}
    | []    -> failwith "unexpected"
    end
    with Not_found -> (* look in globals *)
      try
        let rv = IdMap.find id env.globals in
        (env.accessed) := StrSet.add id !(env.accessed);
        rv := f !rv;
        env
      with Not_found ->
        failwith @@ id^" not found in any environment"

(* Expression interpretation *)

let value_of_eval ev = match ev with VDeclared v_ref -> !v_ref | VTemp v -> v

let env_add i v env =
    try
      let vs = IdMap.find i env in
      IdMap.add i (v::vs) env
    with Not_found -> IdMap.add i [v] env

let env_remove i env =
  try match IdMap.find i env with
    | [v]   -> IdMap.remove i env
    | _::vs -> IdMap.add i vs env
    | []    -> failwith @@ "unexpectedly can't find id "^i^" in env"
  with Not_found -> env


(* Given an arg_t and a value_t list, bind the values to their corresponding argument names. *)
(* l: level *)
let rec bind_args l uuid arg vs env =
  let error = int_erroru uuid "bind_args" in
  try
    begin match arg, vs with
    | AIgnored, _                 -> env
    | AVar(i, _), [v]             -> env_add i v env
    | AMaybe a', [VOption(Some v')] -> bind_args (l+1) uuid a' [v'] env
    | AMaybe _,  [VOption None]     -> error "missing VOption value"
    | ATuple args, _ when l=0     -> list_fold2 (fun acc a v ->
                                    bind_args (l+1) uuid a [v] acc) env args vs
    | ATuple args, [VTuple vs]    -> list_fold2 (fun acc a v ->
                                      bind_args (l+1) uuid a [v] acc) env args vs
    | _                           -> error @@ sp "bind args: bad values.\n Args:%s\n Values:%s\n"
                                       (KP.flat_string_of_arg arg) (strcatmap string_of_value vs)
    end
  with Invalid_argument _ -> error @@ sp "bind args: values length mismatch.\n Args:%s Values:%s\n"
                               (KP.flat_string_of_arg arg) (strcatmap string_of_value vs)

let rec unbind_args uuid arg env =
  match arg with
  | AIgnored    -> env
  | AVar(i, _)  -> env_remove i env
  | AMaybe a    -> unbind_args uuid a env
  | ATuple args -> List.fold_left (fun acc a ->
                     unbind_args uuid a acc) env args

let rec eval_fun uuid f =
  let error = int_erroru uuid "eval_fun" in
  match f with
    | VFunction(arg, closure, body) ->
        fun addr sched env al ->
          let new_env = {env with locals=bind_args 0 uuid arg al closure} in
          let env', result = eval_expr addr sched new_env body in
          {env' with locals=env.locals}, result

    | VForeignFunction(id, arg, f) ->
        fun addr sched env al ->
          begin match id, sched, al with
          (* override the default function for sleep *)
          | "sleep", Some s, [VInt t] -> R.sleep s addr (foi t /. 1000.) ;
                                       env, VTemp VUnit
          | "haltEngine", Some s, _ ->
              Log.log (lazy ("shutting down "^string_of_address addr)) ();
              R.halt s addr; env, VTemp VUnit
          | _ ->
            let new_env = {env with locals=bind_args 0 uuid arg al env.locals} in
            begin try
              let env', result = f new_env in
              {env' with locals=unbind_args uuid arg env'.locals}, result
            with Failure x ->
              raise @@ RuntimeError(uuid, x^"\n"^
                string_of_env ~skip_empty:false ~accessed_only:false env) end
          end

   | _ -> error "eval_fun: Non-function value"

and eval_expr (address:address) sched_st cenv texpr =
    let ((uuid, tag), _), children = decompose_tree texpr in
    let error = int_erroru uuid ~extra:(address, cenv) in
    let eval_fn = eval_fun uuid in

    let get_id () = match tag_of_expr @@ hd children with
      | Var id -> id
      | _      -> failwith "bad lvar"
    in

    let rec threaded_eval address sched_st ienv texprs =
        match texprs with
        | [] -> (ienv, [])
        | h :: t ->
            let nenv, nval = eval_expr address sched_st ienv h in
            let lenv, vals = threaded_eval address sched_st nenv t in
            (lenv, nval :: vals)
    in

    let child_value env i =
      let renv, reval = eval_expr address sched_st env @@ List.nth children i
      in renv, value_of_eval reval
    in

    let child_values env =
      let renv, revals = threaded_eval address sched_st env children
      in renv, List.map value_of_eval revals
    in

    let temp x = VTemp x in

    let eval_binop s l r  bool_op int_op float_op =
      let error = int_erroru uuid "eval_binop" in
      temp @@ match l, r with
        | VBool b1,  VBool b2  -> VBool(bool_op b1 b2)
        | VInt i1,   VInt i2   -> VInt(int_op i1 i2)
        | VInt i1,   VFloat f2 -> VFloat(float_op (foi i1) f2)
        | VFloat f1, VInt i2   -> VFloat(float_op f1 (foi i2))
        | VFloat f1, VFloat f2 -> VFloat(float_op f1 f2)
        | x, y -> error @@
            Printf.sprintf "non-matching values for %s: %s and %s" s (sov x) (sov y)
    in

    let eval_eq_op l r ~neq =
      let f = if neq then not else id_fn in
      temp @@ VBool(f @@ equal_values l r)
    in

    let eval_cmpop l r cmp_op =
      temp @@ VBool(compare_values cmp_op l r)
    in

    let name = KP.string_of_tag_type tag in

    (* Start of evaluator *)
    let envout, valout = match tag with

    (* Control flow *)
    (* First we list expressions that don't need the environment *)
    | Lambda a ->
      let body = List.nth children 0
      in cenv, VTemp(VFunction(a, cenv.locals, body))

    | Const c -> cenv, temp @@ value_of_const c

    | Var id  ->
        begin try cenv, lookup id cenv
        with Not_found -> error "Var" @@ "id "^id^" not found" end

    | Nothing _ -> cenv, temp @@ VOption None

    | Empty ct ->
        let ctype, _ = unwrap_tcol ct in
        cenv, temp @@ v_empty_of_t ctype

    (* Conditional execution *)
    | IfThenElse ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VBool true  ->
            eval_expr address sched_st penv @@ List.nth children 1
        | VBool false ->
            eval_expr address sched_st penv @@ List.nth children 2
        | _ -> error name "non-boolean predicate"
        end

    (* Conditional execution *)
    | CaseOf x ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VOption(Some v) ->
            let penv = {penv with locals=env_add x v penv.locals} in
            let penv, v = eval_expr address sched_st penv @@
              List.nth children 1 in
            {penv with locals=env_remove x penv.locals}, v
        | VOption None    ->
            eval_expr address sched_st penv @@ List.nth children 2
        | _ -> error name "non-maybe predicate"
        end

    (* Special ordered execution required *)
    | BindAs x ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VIndirect rv ->
          let penv = {penv with locals=env_add x !rv penv.locals} in
          let penv, ret = eval_expr address sched_st penv @@ List.nth children 1
          in
          (* update bound value *)
          let v = value_of_eval @@ lookup x penv in
          rv := v;
          {penv with locals=env_remove x penv.locals}, ret
        | _ -> error name "non-indirect predicate"
        end

    (* Special ordered execution required *)
    | Let ids ->
        let env, bound = child_value cenv 0 in
        begin match ids, bound with
        | [id], _  ->
            let env = if id = "_" then env
                      else {env with locals=env_add id bound env.locals} in
            eval_expr address sched_st env @@ List.nth children 1
        | _, VTuple vs ->
            let env = List.fold_left2 (fun acc_env id v ->
              if id = "_" then acc_env
              else {acc_env with locals=env_add id v acc_env.locals})
              env ids vs
            in
            eval_expr address sched_st env @@ List.nth children 1
        | _ -> error name "bad let destruction"
        end

    (* Then we deal with standard environment modifiers *)
    | _ ->
    let nenv, res = child_values cenv in

    match tag, res with
    | Tuple,[v]  -> nenv, temp v
    | Tuple,(_::_ as vals) -> nenv, temp @@ VTuple vals
    | Ignore, _     -> nenv, temp VUnit
    | Just, [rval]  -> nenv, temp @@ VOption (Some rval)

    | Singleton ct, [elem] ->
        let ctype, _ = unwrap_tcol ct in
        nenv, temp @@ v_singleton error elem ctype

    | Combine, [left; right] ->
        nenv, temp @@ v_combine error left right

    | Range c_t, [start; stride; VInt steps] ->
        let init_fn i =
          let f = float_of_int in
          match start, stride with
          | VInt x,   VInt y   -> VInt(x + i * y)
          | VInt x,   VFloat y -> VFloat((f x) +. ((f i) *. y))
          | VFloat x, VInt y   -> VFloat(x +. ((f i) *. (f y)))
          | VFloat x, VFloat y -> VFloat(x +. ((f i) *. y))
          | _, _ -> error name "mismatching start and stride"
        in
        let l = Array.to_list (Array.init steps init_fn) in
        let reval = temp @@ match c_t with
          | TSet  -> VSet(ValueSet.of_list l)
          | TBag  -> VBag(ValueBag.of_list l)
          | TList -> VList(IList.of_list l)
          | TVector -> VVector(IList.of_list l)
          | _ -> error name "range: unsupported type"
        in nenv, reval

    (* Arithmetic and comparators *)
    | Add, [l;r] -> nenv, eval_binop "add" l r (||) (+) (+.)
    | Mult,[l;r] -> nenv, eval_binop "mult" l r (&&) ( * ) ( *. )

    | Neg, [x] ->
        nenv, temp @@ begin match x with
          | VBool b  -> VBool(not b)
          | VInt i   -> VInt(-i)
          | VFloat f -> VFloat(-. f)
          | _        -> error "Neg" "invalid value"
          end

    | Eq,  [l;r] -> nenv, eval_eq_op l r ~neq:false
    | Lt,  [l;r] -> nenv, eval_cmpop l r (<)
    | Neq, [l;r] -> nenv, eval_eq_op l r ~neq:true
    | Leq, [l;r] -> nenv, eval_cmpop l r (<=)

    | Apply, (f::args) ->
        let renv, reval = (eval_fn f) address sched_st nenv args
        in renv, temp @@ value_of_eval reval

    | Block, vals -> nenv, temp @@ list_last vals

    | Iterate, [f; c] ->
        let f' = eval_fn f address sched_st in
        v_fold error (fun env x -> fst @@ f' env [x]) nenv c, temp VUnit

    | Map, [f; col] ->
        let f' = eval_fn f address sched_st in
        let zero = v_empty error ~no_map:true ~no_multimap:true col in
        let env, c' = v_fold error (fun (env, acc) x ->
          let env', y = f' env [x] in
          env', v_insert error (value_of_eval y) acc
        ) (nenv, zero) col
        in
        env, temp c'

    | Filter, [p; col] ->
        let p' = eval_fn p address sched_st in
        let zero = v_empty error col in
        let env, c' = v_fold error (fun (env, acc) x ->
          let env', filter = p' env [x] in
          match value_of_eval filter with
          | VBool true  -> env', v_insert error x acc
          | VBool false -> env', acc
          | _           -> error name "non boolean predicate"
        ) (nenv, zero) col
        in
        env, temp c'

    | Flatten, [c] ->
        let zero = match v_peek error c with
          | Some m -> v_empty error m
          (* the container is empty, so we must use the types *)
          | _ -> let t = type_of_expr texpr in
                 let tcol, _ = unwrap_tcol t in
                 v_empty_of_t tcol
        in
        let new_col = v_fold error (fun acc x -> v_combine error x acc) zero c in
        nenv, VTemp new_col

    | Aggregate, [f; zero; col] ->
        let f' = eval_fn f address sched_st in
        let renv, rval = v_fold error (fun (env, acc) x ->
            let renv, reval = f' env [acc; x] in
            renv, value_of_eval reval
          )
          (nenv, zero)
          col
        in renv, VTemp rval

    | AggregateV, [f; zero; col] ->
        let f' = eval_fn f address sched_st in
        let renv, rval = v_foldv error (fun (env, acc) vid x ->
            let renv, reval = f' env [acc; vid; x] in
            renv, value_of_eval reval
          )
          (nenv, zero)
          col
        in renv, VTemp rval

    | GroupByAggregate, [g; f; zero; col] ->
        let g' = eval_fn g address sched_st in
        let f' = eval_fn f address sched_st in
        (* result type *)
        let empty = v_empty error ~no_multimap:true col in

        (* use hashtable for maximum performance *)
        let r_env = ref nenv in
        let h = Hashtbl.create 100 in
        v_iter error (fun x ->
          let env, key = second value_of_eval @@ g' !r_env [x] in
            (* common to both cases below *)
            let apply_and_update acc =
              let env', acc' = f' env [acc; x] in
              Hashtbl.replace h key @@ value_of_eval acc';
              r_env := env'
            in
            try
              let acc = Hashtbl.find h key in
              apply_and_update acc
            with Not_found ->
              apply_and_update zero
          ) col;
        (* insert into result collection *)
        !r_env, temp @@ Hashtbl.fold (fun k v acc ->
          v_insert error (VTuple [k;v]) acc
        ) h empty

    | Sort, [f; c] -> (* only applies to list *)
        let env = ref nenv in
        let f' = eval_fn f address sched_st in
        let sort_fn v1 v2 =
          (* Comparator application propagates an environment *)
          let nenv, r = f' !env [v1; v2] in
          env := nenv;
          match value_of_eval r with
          | VBool true  -> -1
          | VBool false -> 1
          | _ -> error "Sort" "non-boolean sort result"
        in
        !env, temp @@ v_sort error sort_fn c

    | Size, [c] -> nenv, temp @@ v_size error c

    | Subscript i, [VTuple l] -> nenv, temp @@ at l (i-1)

    (* Collection accessors and modifiers *)
    | Slice, [c; pat] -> nenv, temp @@ v_slice error pat c

    | SliceFrontier, [c; pat]->
         nenv, temp @@ v_slice_frontier error pat c

    | Peek, [c] -> nenv, temp @@ VOption(v_peek error c)

    | PeekWithVid, [c; lam_none; lam_some] ->
        begin match v_peek ~vid:true error c with
          | Some(VTuple(t::xs)) ->
              eval_fn lam_some address sched_st nenv [t; VTuple xs]
          | None ->
              eval_fn lam_none address sched_st nenv [VUnit]
          | _ -> error name "peekwithvid: bad value"
        end

    | AtWith, [c; idx; lam_none; lam_some] ->
        begin match v_at error c idx with
          | Some x ->
              eval_fn lam_some address sched_st nenv [x]
          | None ->
              eval_fn lam_none address sched_st nenv [VUnit]
        end

    | MinWith, [c; lam_none; lam_some] ->
        begin match v_min error c with
          | Some x ->
              eval_fn lam_some address sched_st nenv [x]
          | None ->
              eval_fn lam_none address sched_st nenv [VUnit]
        end

    (* Messaging *)
    | Send, [target; addr; arg] ->
      begin match sched_st with
        | Some s ->
            let send_str =
              lazy (Printf.sprintf "send(%s, %s, %s)\n" (sov target) (sov addr) (sov arg)) in
            Log.log send_str ~name:"K3Interpreter.Msg" `Debug;

            (* create a new level on the queues *)
            schedule_trigger s target addr arg;
            nenv, temp VUnit
        | None -> error name "missing scheduler"
      end

    | Indirect, [v] -> nenv, temp @@ VIndirect(ref v)

    (* envronmental modifiers *)
    | Insert, [_; v] ->
        (env_modify (get_id ()) nenv @@ fun col -> v_insert error v col), temp VUnit

    | Update, [_; oldv; newv]->
        (env_modify (get_id ()) nenv @@
          fun col -> v_update error oldv newv col), temp VUnit

    (* we can't modify the environment within the lambda *)
    (* semantics for upsertwithbefore:
      * if none results from lookup, update with key's vid
      * if some x, delete the x and update with the new vid *)
    | (UpsertWith | UpsertWithBefore), [_; key; lam_none; lam_some] ->
        let col_id = get_id () in
        let col = value_of_eval @@ lookup col_id nenv in
        let key = match key with
          | VTuple xs -> VTuple(list_drop_end 1 xs @ [VUnknown])
          | _ -> error "upsert_with" "not a tuple"
        in
        let slice_fn =
          if tag = UpsertWith then v_slice else v_slice_frontier in
        let slice = slice_fn error key col in
        begin match v_peek ~vid:true error slice with
          | None   ->
              let env, v = eval_fn lam_none address sched_st nenv [VUnit] in
              (env_modify col_id env @@
                fun col -> v_insert ~vidkey:key error (value_of_eval v) col), temp VUnit
          | Some v ->
              (* strip the vid for the lambda *)
              let v_no_vid = if is_vmap col then strip_vid v else v in
              let env, v' = eval_fn lam_some address sched_st nenv [v_no_vid] in
              let v' = value_of_eval v' in
              (env_modify (get_id ()) env @@
                fun col -> v_insert ~vidkey:key error v' col), temp VUnit
        end

    (* we can't modify the environment within the lambda *)
    | UpdateSuffix, [_; key; lam_update] ->
        let f x = value_of_eval @@ snd @@ eval_fn lam_update address sched_st nenv (unwrap_vtuple x) in
        (env_modify (get_id ()) nenv @@
          fun col -> v_update_suffix error key f col), VTemp VUnit

    | Delete, [_; v] ->
        (env_modify (get_id ()) nenv @@
          fun col -> v_delete error v col), VTemp VUnit

    | DeletePrefix, [_; key] ->
        (env_modify (get_id ()) nenv @@
          fun col -> v_delete_prefix error key col), temp VUnit

    | FilterGEQ, _ -> error name "FilterGEQ not supported"

    | Assign, [_; v] -> env_modify (get_id ()) nenv @@ const v, temp VUnit


    | _, args -> error name @@
      "incorrect arguments: "^String.concat "," @@ List.map repr_of_value args

    in
    if !debug then
      Printf.printf "tag: %s, uuid: %d, val: %s\n" name uuid (sov @@ value_of_eval valout);
    if !debug_env then
      Printf.printf "env: %s\n" (string_of_env ~skip_functions:true envout);

    envout, valout

(* Declaration interpretation *)

(* Returns a default value for every type in the language *)
let rec default_value id t = default_base_value id t.typ

and default_collection_value ct et = v_empty_of_t ct

and default_base_value id bt =
  let error = int_error "default_base_value" in
  match bt with
  | TTop | TUnknown     -> VUnknown
  | TUnit               -> VUnit
  | TBool               -> VBool false
  | TByte               -> error "bytes are not implemented"
  | TInt
  | TDate               -> VInt 0
  | TFloat              -> VFloat 0.0
  | TString             -> VString ""
  | TMaybe   vt         -> VOption None
  | TTuple   ft         -> VTuple(List.map (default_value id) ft)
  | TCollection (ct,et) -> default_collection_value ct et
  | TIndirect vt        -> VIndirect(ref @@ default_base_value id vt.typ)
  | TAddress            -> VAddress("0.0.0.0", 0)
  | TTarget bt          -> error @@ "no default value for a target "^id
  | TFunction _         -> error @@ "no default value for a function "^id

(* Returns a foreign function evaluator *)
let dispatch_foreign id = K3StdLib.lookup_value id

(* Returns a trigger evaluator function to serve as a simulation
 * of the trigger *)
let prepare_trigger sched_st id arg local_decls body =
  let default (id,t,_) = id, ref @@ default_value id t in
  let new_vals  = List.map default local_decls in
  (* add a level of wrapping to the argument binder so that when we do the first layer
   * which handles lists of arguments, it only takes one argument *)
  let vfun = VFunction(arg, IdMap.empty, body) in

  fun address env args ->
    let local_env = {env with globals=add_from_list env.globals new_vals} in
    let _, reval  = eval_fun (-1) vfun address (Some sched_st) local_env args in
    match value_of_eval reval with
      | VUnit -> ()
      | _ -> int_error "prepare_trigger" @@ "trigger "^id^" returns non-unit"

(* add code sinks to the trigger environment *)
let prepare_sinks sched_st env fp =
  List.fold_left
    (fun e (fs,a) -> match fs with
    | Sink(Resource _) -> failwith "sink resource interpretation not supported"
    | Sink(Code(id, arg, locals, body)) ->
        {e with triggers=IdMap.add id (prepare_trigger sched_st id arg locals body) e.triggers}
    | _ -> e)
    env fp

let convert_json = function
  | `Bool x   -> VBool x
  | `Float x  -> VFloat x
  | `Int x    -> VInt x
  | `String x -> VString x
  | _         -> failwith "advanced json structure unsupported"

let lookup_json json id = match json with
  | Some (`Assoc l) -> begin try some @@ convert_json @@ List.assoc id l
                       with Not_found -> None end
  | Some _          -> failwith "invalid json format"
  | _               -> None

(* Builds a trigger, global value and function environment (ie frames) *)
let env_of_program ?address ?json ~role ~peers sched_st k3_program =
  let me_addr = match address with
    | None   -> Constants.default_address
    | Some a -> a in
  let env_of_declaration env (d,_) = match d with
    | K3.AST.Global (id, t, init_opt) ->
        let rf_env, init_val = match id, init_opt with

          (* substitute the proper address expression for 'me' *)
          | id, _ when id = K3Global.me.id    -> env, VAddress me_addr

          (* substitute for peers *)
          | id, _ when id = K3Global.peers.id ->
              env, VSet (ValueSet.of_list @@ List.map (fun x -> VAddress x) @@ fst_many peers)

          | id, _ when id = K3Global.role.id  -> env, VString role

          | id, Some e ->
              begin match lookup_json json id with
              | Some v -> env, v
              | _      -> second value_of_eval @@ eval_expr me_addr (Some sched_st) env e
              end

          | id, None  -> env, maybe (default_value id t) id_fn @@ lookup_json json id
        in
        {env with globals=IdMap.add id (ref init_val) env.globals; locals=rf_env.locals}
    | Foreign (id,_) ->
        {env with globals=IdMap.add id (ref @@ dispatch_foreign id) env.globals}
    | Flow fp -> prepare_sinks sched_st env fp
    | _ -> env
  in
  (* triggers, (variables, arg frames) *)
  List.fold_left env_of_declaration default_env k3_program


(* Instruction interpretation *)

type environments = {
  res_env : resource_env_t;
  fsm_env : SR.fsm_env_t;  (* really the dispatcher's fsm *)
  prog_env : env_t;
  mutable instrs : instruction_t list; (* role instructions *)
  mutable disp_env : dispatcher_t;
}

(* preserve the state of the interpreter *)
type interpreter_t = {
  scheduler : R.scheduler_state;
  (* metadata including remaining resources and instructions *)
  peer_list : K3Global.peer_t list;
  envs : (address, environments) Hashtbl.t;

  (* how often (in sec) to consume a source *)
  src_interval : float;
  mutable last_s_time : float;
}

(* consume sources ie. evaluate instructions *)
let consume_sources sched_st address env =
  let log_node s = Log.log (lazy (sp "Node %s: %s\n" (string_of_address address) s)) `Trace in
  (* function to pass to consumer *)
  let schedule_fn src_bindings src_id events =
    schedule_event sched_st src_bindings src_id address events
  in
  match env.instrs with
  | []                 -> failwith "consume_sources: missing instructions"
  | Consume id :: rest ->
    let fsm = List.assoc id env.fsm_env in
    let rec loop () =
      log_node @@ sp "consuming from event loop: %s\n" id;
      (* check if we're in the middle of running the dispatcher *)
      if C.is_running env.disp_env then
        (* take another step in this stream, and check if we're done *)
        if C.run_step schedule_fn env.disp_env env.res_env fsm then ()
        else env.instrs <- rest
      else
        (* create a new stream *)
        try
          let disp_env = C.init_dispatcher env.res_env env.disp_env fsm in
          env.disp_env <- disp_env;
          loop ()
        with Not_found ->
          int_error "consume_sources" @@ "no event loop found for "^id
    in loop ()

(* Program interpretation *)
let interpreter_event_loop role k3_program =
  let error s =
    int_error "interpreter_event_loop" s in
 let roles, default_role = extended_roles_of_program k3_program in
 let get_role role fail_f = try List.assoc role roles with Not_found ->
    fail_f @@ "No role "^role^" found in k3 program"
 in match default_role with
   | Some (_, y) -> get_role role @@ const y
   | None        -> get_role role error

(* returns address, (event_loop_t, environment) *)
let initialize_peer sched_st ~address ~role ~peers ?json k3_program =
  let prog_env = env_of_program sched_st ~address ~role ~peers ?json k3_program in
  address, (interpreter_event_loop role k3_program, prog_env)

let interpret_k3_program i =
  (* Continue running until all peers have finished their instructions,
   * and all messages have been processed
   *  @last_src_peers: how many peers had active sources
   *)
  let rec loop last_src_peers =
    incr iters;
    (* debug *)
    (* Log.log (Printf.sprintf "%d iterations\n" !iters) ();
       if !iters >= num_iters then begin
         Log.log ("finished "^soi !iters^" iterations. stop.\n") () end else *)
    (* number of peers with messages *)
    let msg_peers =
      if R.network_has_work i.scheduler then begin
        (* func to return program env *)
        let prog_env_fn addr =
          Log.log (lazy (sp "Node %s: consuming messages\n" @@ string_of_address addr)) `Trace;
          (Hashtbl.find i.envs addr).prog_env
        in
        run_scheduler ~slice:1 i.scheduler prog_env_fn
      end else 0
    in
    let t = Sys.time () in
    let src_peers =
      (* is it time to scan for sources again ? *)
      if t -. i.last_s_time >= i.src_interval then begin
        i.last_s_time <- t;
        Hashtbl.fold (fun addr env count ->
          if env.instrs <> [] then begin
            consume_sources i.scheduler addr env;
            count + 1
          end else count)
        i.envs 0
      (* if it's not time for a source, use 1 to get to next time *)
      end else 1
    in
    (* check if we should continue *)
    if msg_peers > 0 || src_peers > 0 then loop src_peers
    else
      Log.log (lazy (sp "Interpreter shutting down. msg_peers[%d], src_peers[%d]\n" msg_peers src_peers)) `Trace
  in
  Log.log (lazy (sp "Starting up interpreter\n")) `Trace;
  loop 1;
  Log.log (lazy (sp "Finished execution\n")) `Trace;
  let prog_state = List.map (fun (i,x) -> i, x.prog_env) @@ list_of_hashtbl i.envs in
  (* Log program state *)
  List.iter (fun (addr, e) ->
    Log.log (lazy (sp ">>>> Peer %s\n" @@ string_of_address addr)) `Trace;
    Log.log (lazy (sp "%s\n" @@ string_of_env e ~accessed_only:false)) `Trace;
  ) prog_state;
  prog_state

(* Initialize an interpreter given the parameters *)
let init_k3_interpreter ?queue_type ?(src_interval=0.002) ~peers ~load_path ~interp_file typed_prog =
  Log.log (lazy (sp "Initializing scheduler\n")) `Trace;
  let scheduler = init_scheduler_state ?queue_type ~peers in
  let json =
    if interp_file <> "" then
      some @@ Yojson.Safe.from_file interp_file
    else None
  in
  match peers with
  | []  -> failwith "init_k3_program: Peers list is empty!"
  | _   ->
      Log.log (lazy (sp "Initializing interpreter\n")) `Trace;
      (* Initialize an environment for each peer *)
      K3StdLib.g_load_path := load_path;
      let len = List.length peers in
      let envs = Hashtbl.create len in
      List.iter (fun (address, role) ->
                 (* event_loop_t * program_env_t *)
          let _, ((res_env, fsm_env, instrs), prog_env) =
            initialize_peer scheduler ~address ~role ~peers ?json typed_prog in
          Hashtbl.replace envs address
            {res_env; fsm_env; prog_env; instrs; disp_env=C.def_dispatcher}
      ) peers;
      {scheduler; peer_list=peers; envs; last_s_time=0.; src_interval}


