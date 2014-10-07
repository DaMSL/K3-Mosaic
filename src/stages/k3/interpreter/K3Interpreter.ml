open Lazy
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

(* Generic helpers *)

type breakpoint_t = K3Runtime.breakpoint_t
let sp = Printf.sprintf

(* Prettified error handling *)
let int_erroru uuid ?extra fn_name s =
  let msg = fn_name^": "^s in
  let rs = "interpreter: "^msg in
  Log.log rs `Error;
  (match extra with
  | Some (address, env) ->
    Log.log (sp ">>>> Peer %s" @: string_of_address address) `Error;
    Log.log (sp "%s" @@ string_of_env env) `Error
  | _ -> ());
  raise @: RuntimeError(uuid, msg)

let int_error = int_erroru (-1)

(* Environment helpers *)

let lookup id (mutable_env, frame_env) =
  try
    VTemp(hd @: IdMap.find id frame_env)
  with Not_found ->
    VDeclared(IdMap.find id mutable_env)

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
    | []    -> failwith @: "unexpectedly can't find id "^i^" in env"
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
    | VFunction(arg, body) ->
        fun address sched_st (m_env, f_env) a ->
          let new_env = m_env, bind_args uuid arg a f_env in
          let (m_env', f_env'), result = eval_expr address sched_st new_env body in
          (m_env', unbind_args uuid arg f_env'), result

    | VForeignFunction(arg, f) ->
        fun _ _ (m_env, f_env) a ->
          let new_env = m_env, (bind_args uuid arg a f_env) in
          let (m_env', f_env'), result = f new_env in
          (m_env', unbind_args uuid arg f_env'), result

   | _ -> error "eval_fun: Non-function value"

and eval_expr (address:address) sched_st cenv texpr =
    let ((uuid, tag), _), children = decompose_tree texpr in
    let error = int_erroru uuid ~extra:(address, cenv) in
    let t_erroru = t_error uuid in (* pre-curry the type error *)
    let eval_fn = eval_fun uuid in

    let child_value env i =
      let renv, reval = eval_expr address sched_st env @: List.nth children i
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
        | fenv, [v1; v2] -> fenv, VTemp(VBool(cmp_op v1 v2))
        | _ -> error "eval_cmpop" "missing values"
    in

    (* Start of evaluator *)
    match tag with
    | Const c   -> cenv, VTemp(value_of_const c)
    | Var id    -> begin
        try cenv, lookup id cenv
        with Not_found -> error "Var" @: "id "^id^" not found"
        end
    | Tuple     -> let fenv, vals = child_values cenv in
                   fenv, VTemp(VTuple vals)
    | Just      ->
      let renv, rval = child_value cenv 0
      in (renv, VTemp(VOption (Some rval)))

    | Nothing _ -> cenv, VTemp(VOption None)

    (* Collection constructors *)
    | Empty ct ->
        let name = "Empty" in
        let ctype, _ = ct <| collection_of +++ base_of |> t_erroru name @:
                                                          VTBad(ct, "not a collection") in
        cenv, VTemp(
            match ctype with
            | TSet           -> VSet(ISet.empty)
            | TBag           -> VBag(ValueBag.empty)
            | TList          -> VList(IList.empty)
            | TMap           -> VMap(ValueMap.empty)
            | TMultimap idxs -> VMultimap(ValueMMap.init idxs)
        )

    | Singleton ct ->
        let name = "Singleton" in
        let nenv, element = child_value cenv 0 in
        let ctype, _ =
          ct <| collection_of +++ base_of |> t_erroru name @:
                                             VTBad(ct, "not a collection") in
        cenv, VTemp(v_singleton error element ctype)

    | Combine ->
        let name = "Combine" in
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
      let name = "range" in
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
      in cenv, VTemp(VFunction(a, body))

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
        let name = "Iterate" in
        begin match child_values cenv with
          | nenv, [f;c] ->
              let f' = eval_fn f address sched_st in
              v_fold error (fun env x -> fst @: f' env x) nenv c, VTemp VUnit
          | _ -> error name "bad format"
        end

    | IfThenElse ->
        let name = "IfThenElse" in
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VBool true  ->
            eval_expr address sched_st penv @: List.nth children 1
        | VBool false ->
            eval_expr address sched_st penv @: List.nth children 2
        | _ -> error name "non-boolean predicate"
        end

    | CaseOf x ->
        let name = "CaseOf" in
        let penv, pred = child_value cenv 0 in
        begin match pred with
        | VOption(Some v) ->
            let penv' = second (env_add x v) penv in
            eval_expr address sched_st penv' @: List.nth children 1
        | VOption None    ->
            eval_expr address sched_st penv @: List.nth children 2
        | _ -> error name "non-maybe predicate"
        end

    (* Collection transformers *)

    (* Keeps the same type *)
    | Map ->
        let name = "Map" in
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
        let name = "Filter" in
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
                 let _, (tcol, _)  = unwrap_tcol t in
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
                let env, key = second value_of_eval @: g' !r_env x in
                (* common to both cases below *)
                let apply_and_update acc =
                  let env', acc' = f' env @: VTuple[acc; x] in
                  Hashtbl.replace h key @: value_of_eval acc';
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
      let name = "Sort" in
      begin match child_values cenv with
      | renv, [c; f] ->
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

    (* Collection accessors and modifiers *)
    | Slice ->
      begin match child_values cenv with
        | renv, [c; pat] -> renv, VTemp(v_slice error pat c)
        | _       -> error "Slice" "bad values"
      end

    | SliceIdx idx ->
      begin match child_values cenv with
        | renv, [comps; c; pat] -> renv, VTemp(v_slice_idx error idx comps pat c)
        | _       -> error "SliceIdx" "bad values"
      end

    | Peek ->
      let renv, c = child_value cenv 0 in
      (* a hack while peek still uses unsafe semantics *)
      renv, VTemp(VOption(v_peek error c))

    | Insert ->
        begin match threaded_eval address sched_st cenv children with
          | renv, [VDeclared c_ref; v] ->
              c_ref := v_insert error (value_of_eval v) !c_ref;
              renv, VTemp VUnit
          | _ -> error "Insert" "bad format"
        end

    | Update ->
        begin match threaded_eval address sched_st cenv children with
          | renv, [VDeclared c_ref; oldv; newv] ->
              c_ref := v_update error (value_of_eval oldv) (value_of_eval newv) !c_ref;
              renv, VTemp VUnit
          | _ -> error "Update" "bad format"
        end

    | Delete ->
        begin match threaded_eval address sched_st cenv children with
          | renv, [VDeclared c_ref; v] ->
              c_ref := v_delete error (value_of_eval v) !c_ref;
              renv, VTemp VUnit
          | _ -> error "Update" "bad format"
        end

    (* Messaging *)
    | Send ->
      let renv, parts = child_values cenv in
      (* get the sender's address from global variable "me" *)
      begin match sched_st, parts with
        | Some s, [target;addr;arg] ->
            let e = expr_of_value uuid in
            (* log our send *)
            let send_code = K3Helpers.mk_send (e target) (e addr) (e arg) in
            let send_str = K3PrintSyntax.string_of_expr send_code in
            Log.log send_str ~name:"K3Interpreter.Msg" `Debug;

            (* check if we need to buffer our triggers for shuffling *)
            if K3Runtime.use_shuffle_tasks s then
              (* buffer trigger for later shuffle *)
              (buffer_trigger s target addr arg address;
              renv, VTemp VUnit)
            else
              (* create a new level on the queues *)
              (schedule_trigger s target addr arg;
              renv, VTemp VUnit)
        | None, _ -> error "Send" "missing scheduler"
        | _, _    -> error "Send" "bad values"
      end

    | Indirect -> let fenv, v = child_value cenv 0 in
      fenv, VTemp(VIndirect(ref v))

    | Deref    -> let fenv, v = child_value cenv 0 in
      begin match v with
      | VIndirect x -> fenv, VDeclared x
      | _           -> error "Deref" "not an indirection"
      end

    | Assign   -> let fenv, vs = child_values cenv in
      begin match vs with
      | [VIndirect ind; v] ->
          ind := v;
          fenv, VTemp(VUnit)
      | _                  -> error "Assign" "incorrect units"
      end

and threaded_eval address sched_st ienv texprs =
    match texprs with
    | [] -> (ienv, [])
    | h :: t ->
        let nenv, nval = eval_expr address sched_st ienv h in
        let lenv, vals = threaded_eval address sched_st nenv t in
        (lenv, nval :: vals)

(* Declaration interpretation *)

(* Returns a default value for every type in the language *)
let rec default_value = function
  | TFunction _ ->
      int_error "default_value" "no default value available for function types"
  | TValue vt -> default_isolated_value vt

and default_collection_value ct et = v_empty_of_t ct

and default_base_value bt =
  let error = int_error "default_base_value" in
  match bt with
  | TUnknown -> VUnknown
  | TUnit    -> VUnit
  | TBool    -> VBool false
  | TByte    -> error "bytes are not implemented"
  | TInt
  | TDate    -> VInt 0
  | TFloat   -> VFloat 0.0
  | TString  -> VString ""

  | TMaybe   vt -> error "options are not implemented"
  | TTuple   ft -> VTuple (List.map default_isolated_value ft)

  | TCollection (ct,et) -> default_collection_value ct et
  | TIndirect vt       -> let bt = base_of vt () in
                           VIndirect(ref (default_base_value bt))
  | TAddress            -> error "addresses are not implemented"
  | TTarget bt          -> error "targets are not implemented"

and default_isolated_value = function
  | TIsolated (TMutable (bt,_))   -> default_base_value bt
  | TIsolated (TImmutable (bt,_)) -> default_base_value bt
  | TContained _ ->
      int_error "default_isolated_value" "invalid default contained value"

(* Returns a foreign function evaluator *)
let dispatch_foreign id = K3StdLib.lookup_value id

(* Returns a trigger evaluator function to serve as a simulation
 * of the trigger *)
let prepare_trigger sched_st id arg local_decls body =
  fun address (m_env, f_env) args ->
    let default (id,t,_) = id, ref (default_isolated_value t) in
    let new_vals = List.map default local_decls in
    let local_env = add_from_list m_env new_vals, f_env in
    let _, reval = (eval_fun (-1) @: VFunction(arg,body)) address (Some sched_st) local_env args in
    match value_of_eval reval with
      | VUnit -> ()
      | _ -> int_error "prepare_trigger" @: "trigger "^id^" returns non-unit"

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
let env_of_program ?address sched_st k3_program =
  let me_addr = match address with
    | None   -> Constants.default_address
    | Some a -> a in
  let env_of_declaration ((trig_env, (m_env, f_env)) as env) (d,_) =
    match d with
    | K3.AST.Global (id, t, init_opt) ->
        let (rm_env, rf_env), init_val = match id, init_opt with
          | _, Some e ->
            let renv, reval = eval_expr me_addr (Some sched_st) (m_env, f_env) e
            in renv, value_of_eval reval

          (* substitute the proper address expression for 'me' *)
          | id, _ when id = K3Global.me_name ->
              let renv, reval = eval_expr me_addr (Some sched_st) (m_env, f_env) @: mk_caddress me_addr
              in renv, value_of_eval reval

          | _, None -> (m_env, f_env), default_value t
        in
        trig_env, ((IdMap.add id (ref init_val) rm_env), rf_env)

    | Foreign (id,t) -> trig_env, (m_env, env_add id (dispatch_foreign id) f_env)

    | Flow fp -> prepare_sinks sched_st env fp

    | _ -> env
  in
  (* triggers, (variables, arg frames) *)
  let init_env = IdMap.empty, (IdMap.empty, IdMap.empty) in
  List.fold_left env_of_declaration init_env k3_program


(* Instruction interpretation *)

(* consume messages to a specific node *)
let consume_msgs ?(slice = max_int) sched_st env address =
  let log_node s = Log.log (sp "Node %s: %s" (string_of_address address) s) `Trace in
  log_node "consuming messages";
  let status = run_scheduler ~slice sched_st address env in
  status

(* consume sources ie. evaluate instructions *)
let consume_sources sched_st env address (res_env, d_env) (ri_env, instrs) =
  let log_node s = Log.log (sp "Node %s: %s" (string_of_address address) s) `Trace in
  match instrs with
  | []              -> NormalExec, (ri_env, [])
  | (Consume id)::t ->
    log_node @: "consuming from event loop: "^id;
    try let i = List.assoc id d_env in
        let r = run_dispatcher sched_st address res_env ri_env i in
        (*let status = run_scheduler sched_st address env in*)
        NormalExec, (r, t)
    with Not_found ->
      int_error "consume_sources" @: "no event loop found for "^id

(* Program interpretation *)
let interpreter_event_loop role_opt k3_program =
  let error s =
    int_error "interpreter_event_loop" s in
 let roles, default_role = extended_roles_of_program k3_program in
  let get_role role fail_f = try List.assoc role roles with Not_found ->
    fail_f @: "No role "^role^" found in k3 program"
  in match role_opt, default_role with
   | Some x, Some (_,y) -> get_role x (fun _ -> y)
   | Some x, None       -> get_role x error
   | None, Some (_,y)   -> y
   | None, None         -> error "No roles specified or found for K3 program"

(* returns address, (event_loop_t, environment) *)
let initialize_peer sched_st address role_opt k3_program =
  let prog_env = env_of_program sched_st k3_program ~address in
    initialize_scheduler sched_st address prog_env;
    address, (interpreter_event_loop role_opt k3_program, prog_env)

(* preserve the state of the interpreter *)
type interpreter_t = {
  scheduler : scheduler_state;
  (* metadata including remaining resources and instructions *)
  peer_meta : (address, resource_impl_env_t * instruction_t list) Hashtbl.t;
  peer_list : K3Global.peer_t list;
  envs : (address * (resource_env_t * dispatcher_env_t * program_env_t)) list;
}

type status_t = K3Runtime.status_t

let interpret_k3_program {scheduler; peer_meta; peer_list; envs} =
  (* Continue running until all peers have finished their instructions,
   * and all messages have been processed *)
  let rec loop (status:status_t) =
    if status = NormalExec then
      (* find and process every peer that has a message to process. This way we make sure we
       * don't inject new sources until all messages are processed *)
      let message_peers, status' =
        (* for global queueing, we don't loop. Instead, we run for only one
         * iteration, since each trigger needs a different environment *)
        if K3Runtime.use_global_queueing scheduler then
          if network_has_work scheduler then
            let addr = next_global_address scheduler in
            let _,_,prog_env = List.assoc addr envs in
            match consume_msgs ~slice:1 scheduler prog_env addr with
            | BreakPoint bp -> 1, BreakPoint bp
            | NormalExec    -> 1, status
          else 0, status
        else
          (* do one cycle over all the nodes *)
          List.fold_left (fun (count, stat) (addr,_,_) ->
            if node_has_work scheduler addr then
              let _,_,prog_env = List.assoc addr envs in
              match consume_msgs scheduler prog_env addr with
              | BreakPoint bp -> count + 1, BreakPoint bp
              | NormalExec    -> count + 1, stat
            else count, stat
          ) (0, status) peer_list
      in
      (*Printf.printf "msgs: %d\n" message_peers;*)
      if message_peers > 0  && not (K3Runtime.use_shuffle_tasks scheduler) then loop status'
      else
        (* now deal with sources *)
        let source_peers, (status':status_t) =
          Hashtbl.fold (fun addr (ri_env, insts) (count, stat) ->
            if insts <> [] then
              let res_env, de, env = List.assoc addr envs in
              let status, eval =
                consume_sources scheduler env addr (res_env, de) (ri_env, insts)
              in
              Hashtbl.replace peer_meta addr eval;
              match status with
              | BreakPoint bp -> count + 1, BreakPoint bp
              | NormalExec    -> count + 1, stat
            else count, stat
          ) peer_meta (0, status')
        in
        (*Printf.printf "sources: %d, msgs: %d\n" source_peers message_peers;*)
        if source_peers > 0 || message_peers > 0 then loop status'
        else status'
    else status
  in
  let (result:status_t) = loop NormalExec in
  let prog_state = List.map (fun (addr, (_,_,e)) -> addr, e) envs in
  (* We only print if we finished execution *)
  if result = NormalExec then
    (* Log program state *)
    List.iter (fun (addr, e) ->
      Log.log (sp ">>>> Peer %s" (string_of_address addr)) `Trace;
      Log.log (sp "%s" (string_of_program_env e)) `Trace;
    ) prog_state;
  result, prog_state

(* Initialize an interpreter given the parameters *)
let init_k3_interpreter ?shuffle_tasks
                        ?breakpoints
                        ?(queue_type=GlobalQ)
                        ~run_length
                        ~peers
                        typed_prog =
  let s = init_scheduler_state ?shuffle_tasks ?breakpoints ~queue_type ~run_length () in
  match peers with
  | []  -> failwith "interpret_k3_program: Peers list is empty!"
  | _   ->
      (* Initialize an environment for each peer *)
      let peer_meta = Hashtbl.create @: List.length peers in
      let envs = List.map (fun (addr, role_opt, _) ->
                 (* event_loop_t * program_env_t *)
          let _, ((res_env, d_env, instrs), prog_env) =
            initialize_peer s addr role_opt typed_prog
          in Hashtbl.replace peer_meta addr ([], instrs);
          addr, (res_env, d_env, prog_env)
        ) peers
      in
      {scheduler=s; peer_meta=peer_meta; peer_list=peers; envs=envs}

