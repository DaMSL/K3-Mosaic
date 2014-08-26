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

(* Prettified error handling *)
let int_erroru uuid ?extra fn_name s =
  let msg = fn_name^": "^s in
  let rs = "interpreter: "^msg in
  LOG rs LEVEL ERROR;
  (match extra with
  | Some (address, env) ->
    LOG ">>>> Peer %s" (string_of_address address) LEVEL ERROR;
    LOG "%s" (string_of_env env) LEVEL ERROR
  | _ -> ());
  raise @: RuntimeError(uuid, msg)

let int_error = int_erroru (-1)

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
  with Not_found -> VDeclared(IdMap.find id mutable_env)


(* Expression interpretation *)

let value_of_eval ev = match ev with VDeclared v_ref -> !v_ref | VTemp v -> v

(* Given an arg_t and a value_t, bind the values to their corresponding argument names. *)
let rec bind_args uuid a v =
  let error = int_erroru uuid "bind_args" in
  match a with
  | AIgnored -> []
  | AVar(i, _) -> [(i, v)]
  | AMaybe(a') -> (
      match v with
      | VOption(Some v') -> bind_args uuid a' v'
      | VOption(None) ->
          error "bind_args: missing VOption value"
      | _ -> error "bind_args: improper maybe value"
  )
  | ATuple(args) -> (
      match v with
      | VTuple(vs) -> List.concat (List.map2 (bind_args uuid) args vs)
      | _ -> error "bind_args: bad tuple value"
  )

let rec eval_fun uuid f =
  let error = int_erroru uuid "eval_fun" in
  let strip_frame (m_env, f_env) = m_env, List.tl f_env in
  match f with
    | VFunction(arg, body) ->
        fun address sched_st (m_env, f_env) a ->
            let new_env = m_env, (bind_args uuid arg a :: f_env) in
            let renv, result = eval_expr address sched_st new_env body in
            (strip_frame renv, result)

    | VForeignFunction(arg, f) ->
        fun _ _ (m_env, f_env) a ->
            let new_env = m_env, (bind_args uuid arg a :: f_env) in
            let renv, result = f new_env in
            (strip_frame renv, result)

   | _ -> error "eval_fun: Non-function value"

and eval_expr (address:address) sched_st cenv texpr =
    (*LOG "%s" *)
      (*(string_of_env cenv) NAME "K3Interpreter.DetailedState" LEVEL DEBUG;*)

    let ((uuid, tag), _), children = decompose_tree texpr in
    let error = int_erroru uuid "eval_expr" ~extra:(address, cenv) in
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

    let extract_value_list x =
      let error = int_erroru uuid "extract_value_list" in
      match x with
        | VSet cl | VBag cl | VList cl -> cl
        | VMap cn -> list_of_valuemap cn
        | _       -> error "non-collection"
    in

    let preserve_collection f v = VTemp(
      let error = int_erroru uuid "preserve_collection" in
      match v with
        | VSet cl  -> VSet(nub @: f cl)
        | VBag cl  -> VBag(f cl)
        | VList cl -> VList(f cl)
        | VMap cm  -> VMap(valuemap_of_list @: f @: list_of_valuemap cm)
        | _        -> error "non-collection"
      )
    in

    (* Slices use simple single-level tuple matching, rather than
     * generalized nested match for now. *)
    let match_pattern pat_v v =
      let match_or_unknown v1 v2 = match v1, v2 with
        | VUnknown, _ -> true
        | _, _        -> v1 = v2
      in
      match pat_v, v with
      | VTuple pat_f, VTuple v_f ->
        (try List.for_all2 match_or_unknown pat_f v_f
         with Invalid_argument _ -> false)
      | _, _ -> match_or_unknown pat_v v
    in

    (* Collection modifications are all side-effects, and must replace any
     * existing binding of the named entity in the environment *)
    let modify_collection modify_f =
      let error = int_erroru uuid "modify_collection" in
      let renv, parts = threaded_eval address sched_st cenv children in
      begin match modify_f renv parts with
        | Some (v_ref,v) -> (v_ref := value_of_eval v; renv, VTemp VUnit)
        | None -> error "no ref found"
      end
    in

    (* removes only one instance *)
    let remove_from_collection v l = list_remove v l in

    (* TODO: byte and string types for binary and comparison operations *)
    let eval_binop bool_op int_op float_op =
      let error = int_erroru uuid "eval_binop" in
      let fenv, vals = child_values cenv in fenv, VTemp(
        match vals with
        | [VBool b1;  VBool b2]  -> VBool(bool_op b1 b2)
        | [VInt i1;   VInt i2]   -> VInt(int_op i1 i2)
        | [VInt i1;   VFloat f2] -> VFloat(float_op (float_of_int i1) f2)
        | [VFloat f1; VInt i2]   -> VFloat(float_op f1 (float_of_int i2))
        | [VFloat f1; VFloat f2] -> VFloat(float_op f1 f2)
        | _ -> error "non-matching values"
        )
    in

    let eval_eq_op ~neq =
      let error = int_erroru uuid "eval_cmpop" in
      let fenv, vals = child_values cenv in fenv, VTemp(
      let rec check2 left right = match left, right with
        | VList xs, VList ys ->
            (try List.for_all2 (fun x y -> check2 x y) xs ys
            with Invalid_argument _ -> false)
        | (VList xs | VBag xs | VSet xs), (VList ys | VBag ys | VSet ys) ->
            let dx = ListAsSet.diff xs ys in
            let dy = ListAsSet.diff ys xs in
            if dx = [] && dy = [] then true
            else (* do a careful match *)
              (* check for all possible combinations: yacc should be empty
                * when we're done, and we check all x's for existance *)
              let ok, dy' =
                  foldl_until (fun (ok, yacc) x ->
                    let found, ys' =
                      List.fold_left (fun (found, yacc) y ->
                        if check2 x y then (true, yacc)
                        else (found, y::yacc)
                      ) (false, []) dy
                    in
                    if not found then Left (false, [])
                    else Right (true, ys')
                  ) (true, []) dx in
              ok && dy' = []
        | v1, v2 -> v1 = v2
      in
      match vals with
      | [v1; v2] ->
          let res = check2 v1 v2 in
          let res = if neq then not res else res in
          VBool res
      | _        -> error "eval_eq: missing values"

    )
    in

    let eval_cmpop cmp_op =
      let error = int_erroru uuid "eval_cmpop" in
      let fenv, vals = child_values cenv in fenv, VTemp(
          match vals with
          | [v1; v2] -> VBool(cmp_op v1 v2)
          | _ -> error "eval_cmpop: missing values"
      )
    in
    (* Common for both Map and MapSelf *)
    let handle_map colF =
      let fenv, f = child_value cenv 0 in
      let nenv, c = child_value fenv 1 in
      let folder cl = mapfold (fun env x ->
        let ienv, i = eval_fn f address sched_st env x in
        ienv, value_of_eval i
      ) nenv cl
      in
      begin match c with
      | VSet cl | VBag cl | VList cl ->
        let renv, r = folder cl
        in renv, colF c r
      | VMap cm ->
        let cl = list_of_valuemap cm in
        let renv, r = folder cl
        in renv, colF c r
      | _ -> error "(Map): non-collection value"
      end
    in

    (* Start of evaluator *)
    match tag with
    | Const(c) -> (cenv, VTemp(value_of_const c))
    | Var(id)  -> begin
        try cenv, lookup id cenv
        with Not_found -> error @: "(Var): id "^id^" not found"
        end
    | Tuple    -> let fenv, vals = child_values cenv in (fenv, VTemp(VTuple(vals)))
    | Just     ->
      let renv, rval = child_value cenv 0
      in (renv, VTemp(VOption (Some rval)))
    | Nothing _ -> cenv, VTemp(VOption(None))

    (* Collection constructors *)
    | Empty(ct) ->
        let name = "Empty" in
        let ctype, _ = ct <| collection_of +++ base_of |> t_erroru name @: VTBad(ct) in
        cenv, VTemp(
            match ctype with
            | TSet  -> VSet([])
            | TBag  -> VBag([])
            | TList -> VList([])
            | TMap  -> VMap(ValueMap.empty)
        )

    | Singleton(ct) ->
        let nenv, element = child_value cenv 0 in
        let name = "Singleton" in
        let ctype, _ =
          ct <| collection_of +++ base_of |> t_erroru name @: VTBad(ct) in
        cenv, VTemp(
            match ctype with
            | TSet  -> VSet  [element]
            | TBag  -> VBag  [element]
            | TList -> VList [element]
            | TMap  -> VMap  (valuemap_of_list [element])
        )

    | Combine ->
        let nenv, components = child_values cenv in
        let left, right = (
          match components with
          | [x; y] -> (x, y)
          | _ -> error "(combine): missing sub-components"
        ) in nenv, VTemp(
            match left, right with
            | VList v1, VList v2 -> VList(v1 @ v2)
            | VSet v1, VSet v2   -> VSet(nub @: v1 @ v2)
            | VBag v1, VBag v2   -> VBag(v1 @ v2)
            | VMap v1, VMap v2   -> VMap(ValueMap.merge (fun k mv1 mv2 ->
                                      match mv1, mv2 with
                                      | _, Some _ -> mv2
                                      | Some _, _ -> mv1
                                      | _         -> None)
                                      v1 v2)
            | _ -> error "(combine): mismatching or non-collections"
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
            | _, _ -> error "(Range): mismatching start and stride"
          in
          let l = Array.to_list (Array.init steps init_fn) in
          let reval = VTemp(match c_t with
            | TSet  -> VSet l
            | TBag  -> VBag l
            | TList -> VList l
            | TMap  -> failwith "(Range) cannot have map")
          in renv, reval
        | _ -> error "(Range): invalid format"
      end

    (* Arithmetic and comparators *)
    | Add  -> eval_binop (||) (+) (+.)
    | Mult -> eval_binop (&&) ( * ) ( *. )

    | Neg ->
        let fenv, vals = child_values cenv in fenv, VTemp(
          match vals with
          | [VBool(b)]  -> VBool(not b)
          | [VInt(i)]   -> VInt(-i)
          | [VFloat(f)] -> VFloat(-. f)
          | _ -> error "(Neg): invalid value"
        )

    | Eq -> eval_eq_op ~neq:false
    | Lt -> eval_cmpop (<)
    | Neq -> eval_eq_op ~neq:true
    | Leq -> eval_cmpop (<=)

    (* Control flow *)
    | Lambda(a) ->
      let body = List.nth children 0
      in cenv, VTemp(VFunction(a, body))

    | Apply ->
        let fenv, f = child_value cenv 0 in
        let aenv, a = child_value fenv 1 in
        let renv, reval = (eval_fn f) address sched_st aenv a
        in renv, VTemp(value_of_eval reval)

    | Block ->
        let fenv, vals = threaded_eval address sched_st cenv children
        in fenv, (List.nth vals (List.length vals - 1))

    | Iterate ->
        let fenv, f = child_value cenv 0 in
        let nenv, c = child_value fenv 1 in
        let g = eval_fn f address sched_st in
        let folder l = List.fold_left (
            fun e x -> let ienv, _ = g e x in ienv
        ) nenv l in
          begin match c with
            | VSet cl
            | VBag cl
            | VList cl -> folder cl, VTemp(VUnit)
            | VMap cm  -> folder @: list_of_valuemap cm, VTemp(VUnit)
            | _ -> error "(Iterate): non-collection value"
          end

    | IfThenElse ->
        let penv, pred = child_value cenv 0 in (
            match pred with
            | VBool(b) when b     ->
                eval_expr address sched_st penv @: List.nth children 1
            | VBool(b) when not b ->
                eval_expr address sched_st penv @: List.nth children 2
            | _ -> error "(IfThenElse): non-boolean predicate"
        )

    (* Collection transformers *)
    (* Map by default transforms to a bag *)
    | Map     -> handle_map (fun _ x -> VTemp(VBag x))

    (* Keeps the same type *)
    | MapSelf -> handle_map @: (fun c r -> preserve_collection (fun _ -> r) c)

    | Filter ->
        let penv, p = child_value cenv 0 in
        let nenv, c = child_value penv 1 in
        let p' = eval_fn p address sched_st in
        let folder cl = List.fold_left (fun (env, r) x ->
          let p'env, filter = p' env x in
          match value_of_eval filter with
          | VBool true  -> p'env, x::r
          | VBool false -> p'env, r
          | _           -> error "(FilterMap): non boolean predicate"
        ) (nenv, []) cl
        in
        begin match c with
        | VSet(cl) | VBag(cl) | VList(cl) ->
          let renv, r = folder cl in
          let r = List.rev r in (* reverse because of cons *)
          renv, (preserve_collection (fun _ -> r) c)
        | VMap cm ->
          let cl = list_of_valuemap cm in
          let renv, r = folder cl in
          let r = List.rev r in (* reverse because of cons *)
          renv, VTemp(VMap(valuemap_of_list r))
        | _ -> error "(FilterMap): non-collection value"
        end

    | Flatten ->
        let nenv, c = child_value cenv 0 in
        nenv, preserve_collection (fun vs -> List.concat (List.map extract_value_list vs)) c

    | Aggregate ->
        let fenv, f = child_value cenv 0 in
        let zenv, zero = child_value fenv 1 in
        let nenv, col = child_value zenv 2 in
        let f' = eval_fn f address sched_st in
        let renv, rval = List.fold_left (
            fun (e, v) a ->
              let renv, reval = f' e (VTuple [v; a]) in
              renv, value_of_eval reval
          )
          (nenv, zero)
          (extract_value_list col)
        in renv, VTemp rval

    | GroupByAggregate ->
        let genv, g = child_value cenv 0 in
        let fenv, f = child_value genv 1 in
        let zenv, zero = child_value fenv 2 in
        let nenv, c = child_value zenv 3 in
        let g' = eval_fn g address sched_st in
        let f' = eval_fn f address sched_st in
        let cl = extract_value_list c in

        let gb_agg_fn find_fn replace_fn = fun (env, data) a ->
          let kenv, key =
            let env, k = g' env a in
            env, value_of_eval k
          in
          let v = (try find_fn key data with Not_found -> zero) in
          let aenv, agg = f' kenv (VTuple [v; a]) in
          let data' = replace_fn key (value_of_eval agg) data in
          aenv, data'
        in

        let hash_gb_agg_method = lazy(
          let agg_fn = gb_agg_fn ValueMap.find ValueMap.add in
          let build_fn b = VMap b in
          let data = ValueMap.empty in
          agg_fn, build_fn, data)
        in

        let (agg_fn, build_fn, data0) = Lazy.force @: match c with
          | VSet _ | VBag _ | VList _ | VMap _ -> hash_gb_agg_method
          | _       -> error "(GroupBy): non-collection value"
        in
        let renv, data = List.fold_left agg_fn (nenv, data0) cl in
        renv, VTemp(build_fn data)

    | Sort ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [c;f] ->
          let env, l, f_val =
            ref renv, extract_value_list c, eval_fn f address sched_st in
          let sort_fn v1 v2 =
            (* Comparator application propagates an environment since it could
             * be stateful *)
            let nenv, r = f_val !env (VTuple([v1; v2])) in
              env := nenv;
              match v1 = v2, value_of_eval r with
              | true, _             -> 0
              | false, VBool(true)  -> -1
              | false, VBool(false) -> 1
              | _, _ -> error "(Sort): non-boolean sort result"
          in !env, VTemp(VList(List.sort sort_fn l))
        | _ -> error "(Sort): bad values"
      end

    (* Collection accessors and modifiers *)
    (* TODO: convert to lookup for maps *)
    | Slice ->
      let renv, parts = child_values cenv in
      begin match parts with
        | [c; pat] ->
            renv, (preserve_collection (fun els ->
                List.filter (match_pattern pat) els) c)
        | _       -> error "(Slice): bad values"
      end

    | Peek ->
      let renv, c = child_value cenv 0 in
      begin match extract_value_list c with
        | x::_ -> renv, VTemp x
        | _    -> error "(Peek): empty container"
      end

    | Insert ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared(c_ref); v] ->
            begin match !c_ref, value_of_eval v with
            | VMap cm, VTuple [k; value] -> Some(c_ref, VTemp(VMap(ValueMap.add k value cm)))
            | VMap _, _ -> error "Insert(Map): no key-value tuple found"
            | _ -> Some(c_ref, preserve_collection
                     (fun els -> (value_of_eval v)::els) !c_ref)
            end
       | _ -> None)

    | Update ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared(c_ref); oldv; newv] ->
            begin match !c_ref, value_of_eval oldv, value_of_eval newv with
            | VMap cm, VTuple [oldk; _], VTuple [newk; newv] ->
                Some(c_ref, VTemp(VMap(ValueMap.add newk newv @:
                                         ValueMap.remove oldk cm)))
            | VMap _, _, _ -> error "Insert(Map): no key-value tuple found"
            | _ -> Some(c_ref, preserve_collection (fun l ->
                    value_of_eval newv ::
                      remove_from_collection (value_of_eval oldv) l)
                    !c_ref)
            end
        | _ -> None)

    | Delete ->
      modify_collection (fun env parts -> match parts with
        | [VDeclared c_ref; oldv] ->
            begin match !c_ref, value_of_eval oldv with
            | VMap cm, VTuple [oldk; _] ->
                Some(c_ref, VTemp(VMap(ValueMap.remove oldk cm)))
            | VMap _, _ -> error "Insert(Map): no key-value tuple found"
            | _ -> Some(c_ref, preserve_collection (fun l ->
                    remove_from_collection (value_of_eval oldv) l) !c_ref)
            end
        | _ -> None)

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
            LOG send_str NAME "K3Interpreter.Msg" LEVEL DEBUG;

            (* check if we need to buffer our triggers for shuffling *)
            if K3Runtime.use_shuffle_tasks s then
              (* buffer trigger for later shuffle *)
              (buffer_trigger s target addr arg address;
              renv, VTemp VUnit)
            else
              (* create a new level on the queues *)
              (schedule_trigger s target addr arg;
              renv, VTemp VUnit)
        | None, _ -> error "Send: missing scheduler"
        | _, _    -> error "Send: bad values"
      end

    | Indirect -> let fenv, v = child_value cenv 0 in
      fenv, VTemp(VIndirect(ref v))

    | Deref    -> let fenv, v = child_value cenv 0 in
      begin match v with
      | VIndirect x -> fenv, VDeclared(x)
      | _           -> error "Deref: not an indirection"
      end

    | Assign   -> let fenv, vs = child_values cenv in
      begin match vs with
      | [VIndirect ind; v] ->
          ind := v;
          fenv, VTemp(VUnit)
      | _                  -> error "Assign: incorrect units"
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

and default_collection_value ct et =
  match ct with
  | TSet  -> VSet []
  | TBag  -> VBag []
  | TList -> VList []
  | TMap  -> VMap (ValueMap.empty)

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
  | TString  -> error "strings are not implemented"

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

    | Foreign (id,t) -> trig_env, (m_env, [id, dispatch_foreign id]::f_env)

    | Flow fp -> prepare_sinks sched_st env fp

    | _ -> env
  in
  (* triggers, (variables, arg frames) *)
  let init_env = IdMap.empty, (IdMap.empty,[]) in
  List.fold_left env_of_declaration init_env k3_program


(* Instruction interpretation *)

(* consume messages to a specific node *)
let consume_msgs ?(slice = max_int) sched_st env address =
  let log_node s = LOG "Node %s: %s" (string_of_address address) s LEVEL TRACE in
  log_node "consuming messages";
  let status = run_scheduler ~slice sched_st address env in
  status

(* consume sources ie. evaluate instructions *)
let consume_sources sched_st env address (res_env, d_env) (ri_env, instrs) =
  let log_node s = LOG "Node %s: %s" (string_of_address address) s LEVEL TRACE in
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
  let error () =
    int_error "interpreter_event_loop" "No role found for K3 program" in
 let roles, default_role = extended_roles_of_program k3_program in
  let get_role role fail_f = try List.assoc role roles with Not_found ->
    fail_f ()
  in match role_opt, default_role with
   | Some x, Some (_,y) -> get_role x (fun () -> y)
   | Some x, None       -> get_role x error
   | None, Some (_,y)   -> y
   | None, None         -> error ()

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
      LOG ">>>> Peer %s" (string_of_address addr) LEVEL TRACE;
      LOG "%s" (string_of_program_env e) LEVEL TRACE;
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

