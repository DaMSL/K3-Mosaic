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
let sp = Printf.sprintf

let debug = ref false
let debug_env = ref false

(* Prettified error handling *)
let int_erroru uuid ?extra fn_name s =
  let msg = fn_name^": "^s in
  let rs = "interpreter: "^msg in
  Log.log rs `Error;
  (match extra with
  | Some (address, env) ->
    Log.log (sp ">>>> Peer %s\n" @@ string_of_address address) `Error;
    Log.log (sp "%s\n" @@ string_of_env env) `Error
  | _ -> ());
  raise @@ RuntimeError(uuid, msg)

let int_error = int_erroru (-1)

(* Environment helpers *)

let lookup id (mutable_env, frame_env) =
  try
    VTemp(hd @@ IdMap.find id frame_env)
  with Not_found ->
    VDeclared(IdMap.find id mutable_env)

let env_modify id (env:env_t) f =
  try
    begin match IdMap.find id @@ snd env with
    | v::vs -> second (IdMap.add id @@ f v::vs) env
    | []    -> failwith "unexpected"
    end
    with Not_found -> (* look in globals *)
      try
        let rv = IdMap.find id @@ fst env in
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


(* Given an arg_t and a value_t, bind the values to their corresponding argument names. *)
let rec bind_args uuid arg v env =
  let error = int_erroru uuid "bind_args" in
  match arg, v with
  | AIgnored, _                 -> env
  | AVar(i, _), _               -> env_add i v env
  | AMaybe a', VOption(Some v') -> bind_args uuid a' v' env
  | AMaybe _,  VOption None     -> error "missing VOption value"
  | AMaybe _, _                 -> error "improper maybe value"
  | ATuple args, VTuple vs      -> list_fold2 (fun acc a v ->
                                     bind_args uuid a v acc) env args vs
  | ATuple _, _                 -> error "bad tuple value"

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
        fun addr sched (m_env, f_env) a ->
          let new_env = m_env, bind_args uuid arg a closure in
          let (m_env', _), result = eval_expr addr sched new_env body in
          (m_env', f_env), result

    | VForeignFunction(id, arg, f) ->
        fun addr sched (m_env, f_env) a ->
          (* override the default function for sleep *)
          begin match id, sched, a with
          | "sleep", Some s, VInt t -> R.sleep s addr (foi t /. 1000.) ;
                                       (m_env, f_env), VTemp VUnit
          | _ ->
            let new_env = m_env, (bind_args uuid arg a f_env) in
            begin try
              let (m_env', f_env'), result = f new_env in
              (m_env', unbind_args uuid arg f_env'), result
            with Failure x ->
              raise @@ RuntimeError(uuid, x^"\n"^string_of_env (m_env, f_env)) end
          end

   | _ -> error "eval_fun: Non-function value"

and eval_expr (address:address) sched_st cenv texpr =
    let ((uuid, tag), _), children = decompose_tree texpr in
    let error = int_erroru uuid ~extra:(address, cenv) in
    let eval_fn = eval_fun uuid in

    let child_value env i =
      let renv, reval = eval_expr address sched_st env @@ List.nth children i
      in renv, value_of_eval reval
    in

    let child_values env =
      let renv, revals = threaded_eval address sched_st env children
      in renv, List.map value_of_eval revals
    in

    (* TODO: byte and string types for binary and comparison operations *)
    let eval_binop bool_op int_op float_op =
      let error = int_erroru uuid "eval_binop" in
      let fenv, vals = child_values cenv in fenv, VTemp(
        match vals with
        | [VBool b1;  VBool b2]  -> VBool(bool_op b1 b2)
        | [VInt i1;   VInt i2]   -> VInt(int_op i1 i2)
        | [VInt i1;   VFloat f2] -> VFloat(float_op (foi i1) f2)
        | [VFloat f1; VInt i2]   -> VFloat(float_op f1 (foi i2))
        | [VFloat f1; VFloat f2] -> VFloat(float_op f1 f2)
        | _ -> error "non-matching values"
        )
    in

    let eval_eq_op ~neq =
      let f = if neq then not else id_fn in
      match child_values cenv with
      | fenv, [l; r] -> fenv, VTemp(VBool(f @@ equal_values l r))
      | _ -> error "eval_eq_op" "bad input"
    in

    let eval_cmpop cmp_op =
      match child_values cenv with
        | fenv, [v1; v2] -> fenv, VTemp(VBool(compare_values cmp_op v1 v2))
        | _ -> error "eval_cmpop" "missing values"
    in

    let name = KP.string_of_tag_type tag in

    (* Start of evaluator *)
    let envout, valout = match tag with
    | Const c   -> cenv, VTemp(value_of_const c)
    | Var id    -> begin
        try cenv, lookup id cenv
        with Not_found -> error "Var" @@ "id "^id^" not found"
        end
    | Tuple     -> let fenv, vals = child_values cenv in
                   fenv, VTemp(VTuple vals)
    | Just      ->
      let renv, rval = child_value cenv 0
      in (renv, VTemp(VOption (Some rval)))

    | Nothing _ -> cenv, VTemp(VOption None)

    (* Collection constructors *)
    | Empty ct ->
        let ctype, _ = unwrap_tcol ct in
        cenv, VTemp(
            match ctype with
            | TSet           -> VSet(ISet.empty)
            | TBag           -> VBag(ValueBag.empty)
            | TList          -> VList(IList.empty)
            | TMap           -> VMap(ValueMap.empty)
            | TMultimap idxs -> VMultimap(ValueMMap.init idxs)
        )

    | Singleton ct ->
        let nenv, element = child_value cenv 0 in
        let ctype, _ = unwrap_tcol ct in
        cenv, VTemp(v_singleton error element ctype)

    | Combine ->
        begin match child_values cenv with
          | nenv, [left; right] -> nenv, VTemp(
            begin match left, right with
            | VList v1, VList v2         -> VList(IList.combine v1 v2)
            | VSet v1,  VSet v2          -> VSet(ISet.combine v1 v2)
            | VBag v1,  VBag v2          -> VBag(ValueBag.combine v1 v2)
            | VMap v1,  VMap v2          -> VMap(ValueMap.combine v1 v2)
            | VMultimap v1, VMultimap v2 -> VMultimap(ValueMMap.combine v1 v2)
            | _ -> error name "mismatching or non-collections"
            end)
          | _      -> error name "missing sub-components"
        end

    | Range c_t ->
      begin match child_values cenv with
        | renv, [start; stride; VInt steps] ->
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
          let reval = VTemp(match c_t with
            | TSet  -> VSet(ISet.of_list l)
            | TBag  -> VBag(ValueBag.of_list l)
            | TList -> VList(IList.of_list l)
            | TMap
            | TMultimap _ -> error name "cannot have map")
          in renv, reval
        | _ -> error name "invalid format"
      end

    (* Arithmetic and comparators *)
    | Add  -> eval_binop (||) (+) (+.)
    | Mult -> eval_binop (&&) ( * ) ( *. )

    | Neg ->
        let fenv, vals = child_values cenv in fenv, VTemp(
          match vals with
          | [VBool b]  -> VBool(not b)
          | [VInt i]   -> VInt(-i)
          | [VFloat f] -> VFloat(-. f)
          | _ -> error "Neg" "invalid value"
        )

    | Eq  -> eval_eq_op ~neq:false
    | Lt  -> eval_cmpop (<)
    | Neq -> eval_eq_op ~neq:true
    | Leq -> eval_cmpop (<=)

    (* Control flow *)
    | Lambda a ->
      let body = List.nth children 0
      in cenv, VTemp(VFunction(a, snd cenv, body))

    | Apply ->
        begin match child_values cenv with
          | aenv, [f; a] ->
            let renv, reval = (eval_fn f) address sched_st aenv a
            in renv, VTemp(value_of_eval reval)
          | _ -> error "Apply" "bad format"
        end

    | Block ->
        let fenv, vals = threaded_eval address sched_st cenv children
        in fenv, list_last vals

    | Iterate ->
        begin match child_values cenv with
          | nenv, [f;c] ->
              let f' = eval_fn f address sched_st in
              v_fold error (fun env x -> fst @@ f' env x) nenv c, VTemp VUnit
          | _ -> error name "bad format"
        end

    | IfThenElse ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VBool true  ->
            eval_expr address sched_st penv @@ List.nth children 1
        | VBool false ->
            eval_expr address sched_st penv @@ List.nth children 2
        | _ -> error name "non-boolean predicate"
        end

    | CaseOf x ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VOption(Some v) ->
            let penv = second (env_add x v) penv in
            let penv, v = eval_expr address sched_st penv @@
              List.nth children 1 in
            second (env_remove x) penv, v
        | VOption None    ->
            eval_expr address sched_st penv @@ List.nth children 2
        | _ -> error name "non-maybe predicate"
        end

    | BindAs x ->
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VIndirect rv ->
          let penv = second (env_add x !rv) penv in
          let penv, ret = eval_expr address sched_st penv @@ List.nth children 1
          in
          (* update bound value *)
          let v = value_of_eval @@ lookup x penv in
          rv := v;
          second (env_remove x) penv, ret
        | _ -> error name "non-indirect predicate"
        end

    | Let ids ->
        let env, bound = child_value cenv 0 in
        begin match ids, bound with
        | [id], _  ->
            let env = if id = "_" then env
                      else second (env_add id bound) env in
            eval_expr address sched_st env @@ List.nth children 1
        | _, VTuple vs ->
            let env = List.fold_left2 (fun acc_env id v ->
              if id = "_" then acc_env
              else second (env_add id v) acc_env)
              env ids vs
            in
            eval_expr address sched_st env @@ List.nth children 1
        | _ -> error name "bad let destruction"
        end

    (* Collection transformers *)

    (* Keeps the same type *)
    | Map ->
        begin match child_values cenv with
          | nenv, [f; col] ->
            let f' = eval_fn f address sched_st in
            let zero = v_empty error ~no_multimap:true col in
            let env, c' = v_fold error (fun (env, acc) x ->
              let env', y = f' env x in
              env', v_insert error (value_of_eval y) acc
            ) (nenv, zero) col
            in
            env, VTemp c'
          | _ -> error name "bad format"
        end

    | Filter ->
        begin match child_values cenv with
          | nenv, [p; col] ->
            let p' = eval_fn p address sched_st in
            let zero = v_empty error col in
            let env, c' = v_fold error (fun (env, acc) x ->
              let env', filter = p' env x in
              match value_of_eval filter with
              | VBool true  -> env', v_insert error x acc
              | VBool false -> env', acc
              | _           -> error name "non boolean predicate"
            ) (nenv, zero) col
            in
            env, VTemp c'
          | _ -> error name "bad format"
        end

    | Flatten ->
        let nenv, c = child_value cenv 0 in
        let zero = match v_peek error c with
          | Some m -> v_empty error m
          (* the container is empty, so we must use the types *)
          | _ -> let t = type_of_expr texpr in
                 let tcol, _ = unwrap_tcol t in
                 v_empty_of_t tcol
        in
        let new_col = v_fold error (fun acc x -> v_combine error x acc) zero c in
        nenv, VTemp new_col

    | Aggregate ->
        begin match child_values cenv with
          | nenv, [f; zero; col] ->
            let f' = eval_fn f address sched_st in
            let renv, rval = v_fold error (fun (env, acc) x ->
                let renv, reval = f' env (VTuple [acc; x]) in
                renv, value_of_eval reval
              )
              (nenv, zero)
              col
            in renv, VTemp rval
          | _ -> error "Aggregate" "bad format"
        end

    | GroupByAggregate ->
        begin match child_values cenv with
          | nenv, [g; f; zero; col] ->
            let g' = eval_fn g address sched_st in
            let f' = eval_fn f address sched_st in
            (* result type *)
            let empty = v_empty error ~no_multimap:true col in

            (* use hashtable for maximum performance *)
            let r_env = ref nenv in
            let h = Hashtbl.create 100 in
            v_iter error (fun x ->
                let env, key = second value_of_eval @@ g' !r_env x in
                (* common to both cases below *)
                let apply_and_update acc =
                  let env', acc' = f' env @@ VTuple[acc; x] in
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
            !r_env, VTemp(Hashtbl.fold (fun k v acc ->
                v_insert error (VTuple [k;v]) acc
              ) h empty)
          | _ -> error "GroupByAggregate" "bad format"
        end

    | Sort -> (* only applies to list *)
      begin match child_values cenv with
      | renv, [f; c] ->
        let env = ref renv in
        let f' = eval_fn f address sched_st in
        let sort_fn v1 v2 =
          (* Comparator application propagates an environment *)
          let nenv, r = f' !env (VTuple([v1; v2])) in
          env := nenv;
          match v1 = v2, value_of_eval r with
          | true, _             -> 0
          | false, VBool(true)  -> -1
          | false, VBool(false) -> 1
          | _, _ -> error "Sort" "non-boolean sort result"
        in
        !env, VTemp(v_sort error sort_fn c)
      | _ -> error name "bad values"
      end

    | Subscript i ->
      begin match child_values cenv with
      | renv, [VTuple l] -> renv, VTemp(at l (i-1))
      | _                -> error "Subscript" "bad tuple"
      end

    (* Collection accessors and modifiers *)
    | Slice ->
      begin match child_values cenv with
        | renv, [c; pat] -> renv, VTemp(v_slice error pat c)
        | _       -> error "Slice" "bad values"
      end

    | SliceIdx(idx, comp) ->
      begin match child_values cenv with
        | renv, [c; pat] -> renv, VTemp(v_slice_idx error idx comp pat c)
        | _       -> error "SliceIdx" "bad values"
      end

    | Peek ->
      let renv, c = child_value cenv 0 in
      (* a hack while peek still uses unsafe semantics *)
      renv, VTemp(VOption(v_peek error c))

    | Insert col_id ->
        let renv, v = child_value cenv 0 in
        (env_modify col_id renv @@ fun col -> v_insert error v col), VTemp VUnit

    | Update col_id ->
        begin match child_values cenv with
          | renv, [oldv; newv] ->
              (env_modify col_id renv @@
                fun col -> v_update error oldv newv col), VTemp VUnit
          | _ -> error "Update" "bad format"
        end

    | Delete col_id ->
        let renv, v = child_value cenv 0 in
        (env_modify col_id renv @@
          fun col -> v_delete error v col), VTemp VUnit

    (* Messaging *)
    | Send ->
      let renv, parts = child_values cenv in
      (* get the sender's address from global variable "me" *)
      begin match sched_st, parts with
        | Some s, [target; addr; arg] ->
            let e = expr_of_value uuid in
            (* log our send *)
            let send_code = K3Helpers.mk_send_raw (e target) (e addr) @@ e arg in
            let send_str  = K3PrintSyntax.string_of_expr send_code in
            Log.log (send_str^"\n") ~name:"K3Interpreter.Msg" `Debug;

            (* create a new level on the queues *)
            schedule_trigger s target addr arg;
            renv, VTemp VUnit
        | None, _ -> error "Send" "missing scheduler"
        | _, _    -> error "Send" "bad values"
      end

    | Indirect -> let fenv, v = child_value cenv 0 in
      fenv, VTemp(VIndirect(ref v))

    | Assign x -> let fenv, v = child_value cenv 0 in
      env_modify x fenv @@ const v, VTemp VUnit

    in
    if !debug then
      Printf.printf "tag: %s, uuid: %d, val: %s\n" name uuid (string_of_value @@ value_of_eval valout);
    if !debug_env then
      Printf.printf "env: %s\n" (string_of_env ~skip_functions:true envout);

    envout, valout

and threaded_eval address sched_st ienv texprs =
    match texprs with
    | [] -> (ienv, [])
    | h :: t ->
        let nenv, nval = eval_expr address sched_st ienv h in
        let lenv, vals = threaded_eval address sched_st nenv t in
        (lenv, nval :: vals)

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
  fun address (m_env, f_env) args ->
    let default (id,t,_) = id, ref @@ default_value id t in
    let new_vals = List.map default local_decls in
    let local_env = add_from_list m_env new_vals, f_env in
    let _, reval = (eval_fun (-1) @@ VFunction(arg, IdMap.empty, body)) address
                   (Some sched_st) local_env args in
    match value_of_eval reval with
      | VUnit -> ()
      | _ -> int_error "prepare_trigger" @@ "trigger "^id^" returns non-unit"

(* add code sinks to the trigger environment *)
let prepare_sinks sched_st env fp =
  List.fold_left
    (fun ((trig_env, (m_env, f_env)) as env) (fs,a) -> match fs with
    | Sink(Resource _) ->
      failwith "sink resource interpretation not supported"

    | Sink(Code(id, arg, locals, body)) ->
      IdMap.add id (prepare_trigger sched_st id arg locals body) trig_env, (m_env, f_env)

    | _ -> env
    ) env fp

(* Builds a trigger, global value and function environment (ie frames) *)
let env_of_program ?address ~role ~peers sched_st k3_program =
  let me_addr = match address with
    | None   -> Constants.default_address
    | Some a -> a in
  let env_of_declaration (trig_env, (m_env, f_env as penv) as env) (d,_) = match d with
    | K3.AST.Global (id, t, init_opt) ->
        let (rm_env, rf_env), init_val = match id, init_opt with

          (* substitute the proper address expression for 'me' *)
          | id, _ when id = K3Global.me.id ->
              penv, VAddress me_addr

          (* substitute for peers *)
          | id, _ when id = K3Global.peers.id ->
              penv, VSet (List.map (fun x -> VAddress x) @@ fst_many peers)

          | id, _ when id = K3Global.role.id ->
              penv, VString role

          (* hack to support dist *)
          | id, _ when id = D.jobs.id ->
              let m = ValueMap.of_list @@
                List.map (fun (k,v) -> VAddress k, VInt(D.job_of_str v)) peers in
              penv, VMap m

          | _, Some e -> second value_of_eval @@
              eval_expr me_addr (Some sched_st) penv e

          | id, None -> penv, default_value id t
        in
        trig_env, ((IdMap.add id (ref init_val) rm_env), rf_env)

    | Foreign (id,_) -> trig_env, (IdMap.add id (ref @@ dispatch_foreign id) m_env, f_env)

    | Flow fp -> prepare_sinks sched_st env fp

    | _ -> env
  in
  (* triggers, (variables, arg frames) *)
  let init_env = IdMap.empty, (IdMap.empty, IdMap.empty) in
  List.fold_left env_of_declaration init_env k3_program


(* Instruction interpretation *)

type environments = {
  res_env : resource_env_t;
  fsm_env : SR.fsm_env_t;  (* really the dispatcher's fsm *)
  prog_env : program_env_t;
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
  let log_node s = Log.log (sp "Node %s: %s\n" (string_of_address address) s) `Trace in
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
let initialize_peer sched_st ~address ~role ~peers k3_program =
  let prog_env = env_of_program sched_st ~address ~role ~peers k3_program in
  address, (interpreter_event_loop role k3_program, prog_env)

let interpret_k3_program i =
  (* Continue running until all peers have finished their instructions,
   * and all messages have been processed
   *  @last_src_peers: how many peers had active sources
   *)
  let rec loop last_src_peers =
    (* number of peers with messages *)
    let msg_peers =
      if R.network_has_work i.scheduler then
        (* func to return program env *)
        let prog_env_fn addr =
          Log.log (sp "Node %s: consuming messages\n" @@ string_of_address addr) `Trace;
          (Hashtbl.find i.envs addr).prog_env
        in
        run_scheduler ~slice:1 i.scheduler prog_env_fn; 1
      else 0
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
      (* if it's not time for a source, use last count *)
      end else last_src_peers
    in
    (* check if we should continue *)
    if msg_peers > 0 || src_peers > 0 then loop src_peers else ()
  in
  Log.log (sp "Starting up interpreter\n") `Trace;
  loop 1;
  let prog_state = List.map (fun (i,x) -> i, x.prog_env) @@ list_of_hashtbl i.envs in
  (* Log program state *)
  List.iter (fun (addr, e) ->
    Log.log (sp ">>>> Peer %s\n" (string_of_address addr)) `Trace;
    Log.log (sp "%s\n" (string_of_program_env e)) `Trace;
  ) prog_state;
  prog_state

(* Initialize an interpreter given the parameters *)
let init_k3_interpreter ?queue_type ~peers ~load_path ?(src_interval=0.002) typed_prog =
  let scheduler = init_scheduler_state ?queue_type ~peers in
  match peers with
  | []  -> failwith "interpret_k3_program: Peers list is empty!"
  | _   ->
      Log.log (sp "Initializing interpreter\n") `Trace;
      (* Initialize an environment for each peer *)
      K3StdLib.g_load_path := load_path;
      let len = List.length peers in
      let envs = Hashtbl.create len in
      List.iter (fun (address, role) ->
                 (* event_loop_t * program_env_t *)
          let _, ((res_env, fsm_env, instrs), prog_env) =
            initialize_peer scheduler ~address ~role ~peers typed_prog in
          Hashtbl.replace envs address {res_env; fsm_env; prog_env; instrs; disp_env=C.def_dispatcher}
      ) peers;
      {scheduler; peer_list=peers; envs; last_s_time=0.; src_interval}

