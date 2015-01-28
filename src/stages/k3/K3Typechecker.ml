open Util
open Tree

open K3.AST
open K3.Annotation

open K3Util
open K3Printing
open K3Helpers

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree

type type_bindings_t = (id_t * type_t) list
type event_type_bindings_t = (id_t * (id_t * (type_t list)) list) list

(* Internal type declarations *)
(* has_type, expected_type, msg *)
type error_type =
  | TMismatch of type_t * type_t * string
  | BTMismatch of base_type_t * base_type_t * string
  | TBad of type_t * string
  | BTBad of base_type_t * string
  | InvalidTypeAnnotation
  | MultiplePossibleTypes of string
  | UntypedExpression
  | TMsg of string

(* uuid, location in typechecker, compared types *)
exception TypeError of int * string * error_type

let not_maybe t      = TBad(t, "not a maybe")
let not_value t      = TBad(t, "not a value")
let not_tuple t      = TBad(t, "not a tuple")
let not_ind t        = TBad(t, "not an indirection")
let not_function t   = TBad(t, "not a function")
let not_collection t = TBad(t, "not a collection")
let not_collection_bt t = BTBad(t, "not a collection")
let error_tuple_small n tl t = TBad(t, Printf.sprintf "tuple has size %d but subscript %d" tl n)

let t_error uuid name msg () = raise @@ TypeError(uuid, name, msg)

let check_tag_arity tag children =
  let length = List.length children in
  let correct_arity = match tag with
    | Const(_)  -> 0
    | Var(_)    -> 0
    | Tuple     -> length
    | Just      -> 1
    | Nothing _ -> 0

    | Empty(_)      -> 0
    | Singleton(_)  -> 1
    | Combine       -> 2
    | Range(_)      -> 3

    | Add   -> 2
    | Mult  -> 2
    | Neg   -> 1

    | Eq    -> 2
    | Lt    -> 2
    | Neq   -> 2
    | Leq   -> 2

    | Lambda _    -> 1
    | Apply       -> 2
    | Subscript _ -> 1

    | Block         -> length
    | Iterate       -> 2
    | IfThenElse    -> 3
    | CaseOf _      -> 3
    | BindAs _      -> 2

    | Map               -> 2
    | Filter            -> 2
    | Flatten           -> 1
    | Aggregate         -> 3
    | GroupByAggregate  -> 4
    | Sort              -> 2

    | Slice      -> 2
    | SliceIdx _ -> 2
    | Insert _  -> 1
    | Delete _  -> 1
    | Update _  -> 2
    | Peek      -> 1

    | Assign _ -> 1
    | Indirect -> 1

    | Send -> 3
  in length = correct_arity

(* similar to haskell's infix `function` *)
let (<|) x f = f x
and (|>) f y = f y

(* thread exceptions through function applications *)
let (+++) f g = fun t x -> f (g t x) x

(* Type extraction primitives *)
let type_of_expr e =
  let error s err = t_error (id_of_expr e) s err () in
  let is_type_annotation a = match a with Type _ -> true | _ -> false in
  let extract_type = function
    Type t -> t | _ -> error "Invalid type annotation" InvalidTypeAnnotation in
  let type_annotations = List.filter is_type_annotation (meta_of_expr e) in
  match type_annotations with
    | []  -> error "Untyped expression" UntypedExpression
    | [x] -> extract_type x
    | l   -> error "Multiple possible types" @@ MultiplePossibleTypes(
        List.fold_left (fun acc x ->
          acc^" "^string_of_type @@ extract_type x) "" l)

(* Type composition/decomposition primitives *)

(* get a default value for a type *)
let canonical_value_of_type vt =
  let rec loop vt =
    match vt.typ with
    | TUnknown              -> mk_cint 0
    | TUnit                 -> mk_cunit
    | TBool                 -> mk_cbool false
    | TInt                  -> mk_cint 0
    | TFloat                -> mk_cfloat 0.
    | TString               -> mk_cstring ""
    | TMaybe t              -> mk_nothing vt
    | TTuple ts             -> mk_tuple @@ List.map loop ts
    | TCollection(ctype, t) -> mk_empty vt
    | TAddress              -> mk_caddress Constants.default_address
    | _                     -> failwith "unhandled default"
  in loop vt

(* Type comparison primitives *)

let rec assignable t_l t_r = match t_l.typ, t_r.typ with
  | TMaybe t_lm, TMaybe t_rm -> assignable t_lm t_rm
  | TTuple t_ls, TTuple t_rs ->
      List.length t_ls = List.length t_rs && List.for_all2 assignable t_ls t_rs
  | TCollection(t_lc, t_le), TCollection(t_rc, t_re) ->
      t_lc = t_rc && assignable t_le t_re
  | TFunction(it, ot), TFunction(it', ot') ->
      assignable it it' && assignable ot ot' && it.mut = it'.mut && ot.mut = ot'.mut
  | TDate, TInt              -> true
  | TInt, TDate              -> true
  (* ints and floats can be assigned. They'll just be concatentated *)
  | TInt, TFloat             -> true
  | TFloat, TInt             -> true
  (* handle lambdas with _ arguments *)
  | TUnknown, _              -> true
  | TTop, _ | _, TTop        -> true
  | _ when t_l.typ = t_r.typ -> true
  | _ -> false

(* takes mutability into account *)
and passable t_l t_r =
  if t_l.mut && not t_r.mut then false
  else assignable t_l t_r

let (===) = assignable


let (<~) = passable


(* Whether a type contains TUnknown somewhere ie. it's not a fully known type *)
let rec is_unknown_t t = match t.typ with
  | TMaybe t_m          -> is_unknown_t t_m
  | TTuple(t_s)         -> List.exists is_unknown_t t_s
  | TCollection(_, t_e) -> is_unknown_t t_e
  | TFunction(it, ot)   -> is_unknown_t it || is_unknown_t ot
  | TIndirect(t_e)      -> is_unknown_t t_e
  | TUnknown            -> true
  | _                   -> false

(* Type deduction *)

let deduce_constant_type id trig_env c =
  (* pre-curry the type error *)
  let t_erroru = t_error id in
  let constant_type = match c with
    | CUnit       -> TUnit
    | CUnknown    -> TUnknown
    | CBool _     -> TBool
    | CInt _      -> TInt
    | CFloat _    -> TFloat
    | CString _   -> TString
    | CAddress _  -> TAddress
    | CTarget id  -> (* retrieve type from trig env *)
        let name = "CTarget" in
        begin try
          let typ = List.assoc id trig_env in
          typ.typ
        with Not_found -> t_erroru name (TMsg("Trigger "^id^" not found")) () end
  in canonical constant_type

let rec gen_arg_bindings = function
  | AIgnored    -> []
  | AVar(i, t)  -> [i, t]
  | AMaybe a'   -> gen_arg_bindings a'
  | ATuple args -> List.concat @@ List.map gen_arg_bindings args

(* fill_in: check at each node whether we already have a type annotation.
 * If so, don't go any further down *)
let rec deduce_expr_type ?(override=true) trig_env env utexpr : expr_t =
  let ((uuid, tag), aux), untyped_children = decompose_tree utexpr in
  let name = K3Printing.string_of_tag_type tag in
  let t_erroru = t_error uuid name in (* pre-curry the type error *)

  (* Check Tag Arity *)
  if not @@ check_tag_arity tag untyped_children then raise MalformedTree else

  (* Augment environments for children *)
  let env_proc_fn (last_ch:expr_t option) i = match tag, last_ch, i with
    | Lambda a, None, _    -> gen_arg_bindings a @ env
    | CaseOf x, Some ch, 1 ->
        let t = type_of_expr ch in
        let t_e = match t.typ with
                  | TMaybe mt -> mt
                  | _         -> t_erroru (not_maybe t) () in
        (x, t_e) :: env
    | BindAs x, Some ch, 1 ->
        let t = type_of_expr ch in
        let t_e = match t.typ with
                  | TIndirect it -> it
                  | _            -> t_erroru (not_ind t) () in
        (x, t_e) :: env
    | _ -> env
  in
  (* If not overriding, find those children for which we have no type already *)
  let has_type ch =
    try ignore @@ type_of_expr ch; true
    with TypeError(_, _, UntypedExpression) -> false
          | _                                -> true
  in
  let typed_children = List.rev @@ fst @@ List.fold_left (fun (acc, i) ch ->
      if override || not @@ has_type ch
      then
        let ch' = deduce_expr_type ~override trig_env (env_proc_fn (hd' acc) i) ch
        in ch'::acc, i+1
      else ch::acc,  i+1)
    ([], 0)
    untyped_children
  in
  let attach_type t = mk_tree (((uuid, tag), Type t::aux), typed_children) in
  let bind n = type_of_expr @@ List.nth typed_children n in

  let common_ops () =
    let tfun, tcol' = bind 0, bind 1 in
    let targ, tret =
      try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) () in
    let tcol, telem =
      try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') ()  in
    tfun, tcol', targ, tret, tcol, telem
  in

  let current_type =
      match tag with
      | Const c -> deduce_constant_type uuid trig_env c
      | Var id  -> begin
          try List.assoc id env
          with Not_found -> t_erroru (TMsg(id^" not found")) ()
        end
      | Tuple       ->
          let child_types = List.map type_of_expr typed_children in
          wrap_ttuple child_types
      | Just        -> wrap_tmaybe @@ bind 0
      | Nothing t   -> t
      | Empty t     -> t
      | Singleton t ->
          let t_c, t_e = try unwrap_tcol t with Failure _ -> t_erroru (not_collection t) () in
          let t_ne = bind 0 in
          if not (t_e === t_ne) then t_erroru (TMismatch(t_ne, t_e, "Collection inner type")) ()
          else 
            let t_e' = if is_unknown_t t_ne then t_e else t_ne in
            canonical @@ TCollection(t_c, t_e')

      | Combine ->
          let t0, t1 = bind 0, bind 1 in
          let _ = try unwrap_tcol t0 with Failure _ -> t_erroru (not_collection t0) () in
          let _ = try unwrap_tcol t1 with Failure _ -> t_erroru (not_collection t1) () in
          if not (t0 === t1) then t_erroru (TMismatch(t0, t1,"")) ()
          else (* Avoid unknowns *)
            if is_unknown_t t0 then t1 else t0

      | Range t_c ->
          let start, stride, steps = bind 0, bind 1, bind 2 in
          if not (steps.typ = TInt) then t_erroru (BTMismatch(TInt, steps.typ,"steps:")) () else
          let t_e = match start.typ, stride.typ with
            | TInt, TInt     -> TInt
            | TFloat, TInt
            | TInt, TFloat
            | TFloat, TFloat -> TFloat
            | _ -> t_erroru (TMsg("start/stride types are bad")) ()
          in canonical @@ TCollection(t_c, canonical t_e)

      | Add | Mult ->
          let t_l, t_r = bind 0, bind 1 in
          let result_type = match t_l.typ, t_r.typ with
            | TFloat, TFloat -> TFloat
            | TInt, TFloat   -> TFloat
            | TFloat, TInt   -> TFloat
            | TInt, TInt     -> TInt
            | TBool, TBool   -> TBool
            | _ -> t_erroru (TMismatch(t_l, t_r, "")) ()
          in canonical result_type

      | Neg ->
          let t0 = bind 0 in
          begin match t0.typ with
          | TBool | TInt | TFloat -> t0
          | _ -> t_erroru (not_collection t0) ()
          end

      | Eq | Lt | Neq | Leq ->
          let t_l, t_r = bind 0, bind 1 in
          if t_l === t_r then t_bool
          else t_erroru (TMismatch(t_l, t_r, "")) ()

      | IfThenElse ->
          let t_p, t_t, t_e = bind 0, bind 1, bind 2 in
          if canonical TBool === t_p then
              if t_t === t_e then t_t
              else t_erroru (TMismatch(t_t, t_e,"")) ()
          else t_erroru (TMismatch(canonical TBool, t_p,"")) ()

      | CaseOf id ->
          (* the expression was handled in the prelude *)
          let t_s, t_n = bind 1, bind 2 in
          if t_n === t_s then t_s
          else t_erroru (TMismatch(t_n, t_s, "case branches")) ()

      | BindAs id ->
          (* handled in the prelude *)
          bind 1

      | Block ->
          let rec validate_block components = match components with
            | e :: [] -> type_of_expr e
            | h :: t when type_of_expr h  === canonical TUnit -> validate_block t
            | _       -> t_erroru (TMsg("Bad or non-TUnit expression")) ()
          in validate_block typed_children

      | Lambda t_a ->
          let t_r = bind 0 in
          canonical @@ TFunction(type_of_arg t_a, t_r)

      | Apply ->
          let t_f, t_a = bind 0, bind 1 in
          let t_e, t_r = try unwrap_tfun t_f with Failure _ -> t_erroru (not_function t_f) ()
          in
          if t_e <~ t_a then t_r
          else t_erroru (TMismatch(t_e, t_a, "")) ()

      | Subscript n ->
          let t_e = bind 0 in
          begin match t_e.typ with
          | TTuple l when n <= List.length l -> at l (n - 1)
          | TTuple l -> t_erroru (error_tuple_small n (List.length l) t_e) ()
          | _ -> t_erroru (not_tuple t_e) ()
          end

      | Iterate ->
          let _, _, targ, tret, _, telem = common_ops () in
          if not (tret === canonical TUnit)
              then t_erroru (TMismatch(tret, canonical TUnit, "return val:")) () else
          if targ <~ telem then canonical TUnit
          else t_erroru (TMismatch(targ, telem, "element:")) ()

      | Map ->
          let _, tcol', targ, tret, tcol, telem = common_ops () in
          if targ <~ telem then match tcol with
            | TMap | TMultimap _ -> wrap_tbag tret
            | _ -> canonical @@ TCollection(tcol, tret)
          else t_erroru (TMismatch(targ, telem, "element:")) ()

      | Filter ->
          let _, tcol', targ, tret, tcol, telem = common_ops () in
          if not (targ <~ telem) then
            t_erroru (TMismatch(targ, telem, "predicate:")) () else
          if not (canonical TBool === tret) then
            t_erroru (TMismatch(canonical TBool, tret, "")) () else
          tcol'

      | Flatten ->
          let tcol'' = bind 0 in
          let tcol, telem =
            try unwrap_tcol tcol'' with Failure _ -> t_erroru (not_collection tcol'') () in
          let _ =
            try unwrap_tcol telem with Failure _ -> t_erroru (not_collection telem) () in
          begin match tcol with
          | TMap | TMultimap _ -> t_erroru (TBad (tcol'', "can't flatten a Map")) ()
          | _ -> telem
          end

      | Aggregate ->
          let tfun, tzero, tcol' = bind 0, bind 1, bind 2 in
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) () in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') ()  in
          let expected1 = wrap_ttuple [tzero; telem] in
          if not (targ <~ expected1)
              then t_erroru (TMismatch(targ, expected1, "")) () else
          let expected2 = canonical @@ TTuple[tret; telem] in
          if not (targ <~ expected2)
              then t_erroru (TMismatch(targ, expected2, "")) () else
          tzero

      | GroupByAggregate ->
          let tgrp, tagg, tzero, tcol' = bind 0, bind 1, bind 2, bind 3 in
          let tgarg, tgret =
            try unwrap_tfun tgrp with Failure _ -> t_erroru (not_function tgrp) () in
          let taarg, taret =
            try unwrap_tfun tagg with Failure _ -> t_erroru (not_function tagg) () in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') ()  in
          if not (tgarg <~ telem) then
            t_erroru (TMismatch(tgarg, telem, "grouping func:")) () else
          let expected1 = wrap_ttuple [tzero; telem] in
          if not (taarg <~ expected1) then
            t_erroru (TMismatch(taarg, expected1, "agg func:")) () else
          if not (tzero <~ taret) then
            t_erroru (TMismatch(taret, tzero, "agg func:")) ()
          else canonical @@
            TCollection(tcol, wrap_ttuple [tgret; taret])

      | Sort ->
          let tfun, tcol', targ, tret, tcol, telem = common_ops () in

          let expected1 = wrap_ttuple [telem; telem] in
          if not (targ <~ expected1) then
            t_erroru (TMismatch(targ, expected1, "Sort function arg")) () else
          if not (canonical TBool === tret) then
            t_erroru (TMismatch(canonical TBool, tret, "Sort function result")) () else
          canonical @@ TCollection(TList, telem)

      | Slice ->
          let tcol', tpat = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') ()  in
          (* take care of possible unknowns in pattern *)
          if tpat === telem then tcol'
          else t_erroru (TMismatch(tpat, telem, "pattern")) ()

      | SliceIdx(idx, comp) ->
          let tcol', tpat = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') ()  in
          begin match tcol with
          | TMultimap mmidx when IndexSet.mem idx mmidx -> ()
          | TMultimap _ -> t_erroru (TMsg "slice mismatch on multimap") ()
          | _ -> t_erroru (TBad(tcol', "not a multimap")) ()
          end;
          (* indexing returns a bag *)
          (* take care of possible unknowns in pattern *)
          if tpat === telem then wrap_tbag telem
          else t_erroru (TMismatch(tpat, telem, "pattern")) ()

      | Insert id ->
          let tcol' = try List.assoc id env
                    with Not_found -> t_erroru (TMsg(id^" not found")) () in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') () in
          let telem' = bind 0 in
          if telem === telem' then t_unit
          else t_erroru (TMismatch(telem, telem', "")) ()

      | Update id ->
          let tcol' = try List.assoc id env
                    with Not_found -> t_erroru (TMsg(id^" not found")) () in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') () in
          let told, tnew = bind 0, bind 1 in
          if not (telem === told) then t_erroru (TMismatch(telem, told, "old value")) () else
          if not (telem === tnew) then t_erroru (TMismatch(telem, tnew, "new value")) () else
          t_unit

      | Delete id ->
          let tcol' = try List.assoc id env
                    with Not_found -> t_erroru (TMsg(id^" not found")) () in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') () in
          let told = bind 0 in
          if telem === told then t_unit
          else t_erroru (TMismatch(telem, told, "")) ()

      | Peek ->
          let tcol' = bind 0 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') () in
          wrap_tmaybe telem

      | Assign id ->
          let tl = try List.assoc id env
                        with Not_found -> t_erroru (TMsg(id^" not found")) () in
          let tr = bind 0 in
          if not (tl === tr) then t_erroru (TMismatch(tl, tr, "")) () else
          if not tl.mut then t_erroru (TMsg(id^" is not mutable")) () else
          t_unit

      (* Add a layer of indirection *)
      | Indirect ->
          let tr = bind 0 in
          wrap_tind tr

      | Send ->
          let target, taddr, targs = bind 0, bind 1, bind 2 in
          let ttarget = match target.typ with
              | TTarget t -> t
              | _         -> t_erroru (TBad(target, "not a target")) ()
          in
          match taddr.typ with
          | TAddress ->
              if ttarget === targs then t_unit
              else t_erroru (TMismatch(target, targs, "")) ()
          | _ -> t_erroru (TBad(taddr, "not an address")) ()

  in attach_type current_type

let check_trigger_type trig_env env id args locals body rebuild_f =
  let name           = "Trigger("^id^")" in
  let self_bindings  = id, canonical @@ TTarget(type_of_arg args) in
  let arg_bindings   = gen_arg_bindings args in
  let local_bindings = List.map (fun x -> fst3 x, snd3 x) locals in
  let inner_env = self_bindings :: arg_bindings @ local_bindings @ env in
  let typed_body = deduce_expr_type trig_env inner_env body in
  let t_b = type_of_expr typed_body in
  match t_b.typ with
  | TUnit ->
      (* add annotations *)
      let new_locals = List.map (fun (i,vt,meta) -> (i, vt, (Type(vt)::meta))) locals
      in ((rebuild_f id args new_locals typed_body), self_bindings :: env)
  | _     -> t_error (-1) name (TMismatch(t_unit, t_b, "trigger return type")) ()


(* Flow program type deduction *)

let rec types_of_pattern (env:(id_t * type_t list) list) p : type_t list=
  let rcr = types_of_pattern env in
  let rcr_list l = ListAsSet.no_duplicates (List.flatten (List.map rcr l)) in
  match p with
  | Terminal id  ->
     (try List.assoc id env
      with Not_found ->
        raise @@ TypeError(-1, "", TMsg("No resource "^id^" found in pattern")))

  | Choice l     -> rcr_list l
  | Sequence l   -> rcr_list l
  | Optional p   -> rcr p
  | Repeat (p,_) -> rcr p

let type_of_resource (env:(id_t * type_t list) list) r = match r with
  | Handle(t, _, _) -> [t]
  | Stream(t, ConstStream e) ->
    let uuid = id_of_expr e in
    let tcol' = type_of_expr @@ deduce_expr_type [] [] e in
    let tcol, telem =
      try unwrap_tcol tcol' with Failure _ -> t_error uuid "Stream" (not_collection tcol') () in
    if not (t === telem)
      then t_error uuid "stream" (TMismatch(t, telem, "resource type")) ()
      else [t]
  | Stream(t, _) -> [t]
  | Pattern p -> types_of_pattern env p

let typecheck_bind src_types trig_arg_types =
  match trig_arg_types, src_types with
  | [], [] ->
    Some(TMsg "Neither source event nor trigger argument has valid types.")

  | [x], [y] when not (x === y) ->
    Some(TMismatch(x, y, "Resource binding type mismatch."))

  | _, x when List.length x > 1 ->
    Some(TMsg "Multiple resource event types found for dispatch to trigger.")

  | x, _ when List.length x > 1 ->
    Some(TMsg "Multiple trigger arg types found during bind.")

  | [_], [_] -> None

  | _, _ -> Some(TMsg "Invalid types.")

let bound_resource_type error_prefix resource_env src_id =
  try List.assoc src_id resource_env
  with Not_found ->
    t_error (-1) error_prefix (TMsg("Could not find resource named "^src_id)) ()

let arg_type_of_trigger error_prefix trig_env trig_id =
  try
    let t = List.assoc trig_id trig_env in
    begin match t.typ with
      | TTarget arg_t -> [arg_t]
      | _ -> t_error (-1) error_prefix (TMsg "Invalid trigger argument type") ()
    end
  with Not_found ->
    t_error (-1) error_prefix (TMsg("Could not find trigger named "^trig_id)) ()

let typecheck_flow env trig_env resource_env fp =
  let check_code_type name id args locals body rebuild_f =
    try check_trigger_type trig_env env id args locals body rebuild_f
    with TypeError(ast_id, inner_name, msg) ->
      raise @@ TypeError(ast_id, name^":"^inner_name, msg)
  in
  List.fold_left (fun (nfp, env) (fs,a) ->
      let nfs, nenv = match fs with
      | Source(Code(id, args, locals, body)) ->
        check_code_type "Generator" id args locals body
          (fun id args locals body -> Source(Code(id, args, locals, body)))

      | Sink(Code(id, args, locals, body)) ->
        check_code_type "Trigger" id args locals body
          (fun id args locals body -> Sink(Code(id, args, locals, body)))

      | BindFlow (src_id, trig_id) ->
        let error_preamble = "Invalid binding of "^src_id^" -> "^trig_id in
        let src_types = bound_resource_type error_preamble resource_env src_id in
        let trig_arg_type = arg_type_of_trigger error_preamble trig_env trig_id in
        let error_msg = typecheck_bind src_types trig_arg_type in
        begin match error_msg with
          | None -> fs, env
          | Some(msg) -> t_error (-1) error_preamble msg ()
        end

      | _ -> fs, env
      in (nfp@[nfs,a]), nenv
    ) ([], env) fp


(* Environment constructors *)
let types_of_endpoints endpoint_l =
  let error_if_dup k v l =
    if not(List.mem_assoc k l) then (k,v)::l
    else t_error (-1) ("Endpoint("^k^")") (TMsg("Found duplicate endpoint named "^k)) ()
  in
  List.fold_left (fun env ep -> match ep with
      | Resource(id,r) -> error_if_dup id (type_of_resource env r) env
      | Code(id, args, locals, body) ->
          let t = canonical @@ TTarget(type_of_arg args) in
          error_if_dup id [t] env
    ) [] endpoint_l

let source_types_of_program p = types_of_endpoints (sources_of_program p)
let sink_types_of_program p   = types_of_endpoints (sinks_of_program p)

(* Returns a trigger environment for the program *)
let trigger_types_of_program p =
  let env = types_of_endpoints (triggers_of_program p) in
  List.map (fun (id, tl) -> match tl with
      | [x] -> (id,x)
      | _ -> t_error (-1) ("Endpoint("^id^")") (TMsg("Multiple types resolved")) ()
    ) env

(* Returns a list of role ids, and source resources defined in that role.
 * For each resource, we track a list of possible types to address patterns.
 * Each role is prepended with resources defined in top-level flows. *)
let source_types_of_roles prog =
  let init = source_types_of_program prog in
  List.fold_left (fun env (d,_) -> match d with
    | Role(id,fp) ->
      let resources = List.filter
        (function Resource _ -> true | Code _ -> false) (sources_of_flow fp)
      in (id, init@(types_of_endpoints resources))::env
    | _ -> env
    ) [] prog


(* Typechecking API *)

let type_bindings_of_program prog =
  (* do a first pass, collecting trigger types and resources *)
  let trig_env = trigger_types_of_program prog in
  let resource_env = source_types_of_program prog in
  let rresource_env = source_types_of_roles prog in
  let prog, env =
    List.fold_left (fun (nprog, env) (d, meta) ->
      let nd, nenv = match d with
        | Global(i, t, Some init) ->
          let typed_init =
            try deduce_expr_type trig_env env init
            with TypeError(ast_id, inner, msg) ->
              raise (TypeError(ast_id, "Global "^i^":"^inner, msg))
          in
          let expr_type = type_of_expr typed_init in
          if not (t === expr_type) then t_error (-1) i
              (TMismatch(t, expr_type,
                  "Mismatch in global type declaration.")) ()
          else
          Global(i, t, Some typed_init), (i, t) :: env

        | Global(i, t, None) -> (Global(i, t, None), (i, t) :: env)

        | Foreign(i, t) ->
            begin try let t_f = K3StdLib.lookup_type i in
              if not (t = t_f) then t_error (-1) i
                (TMismatch(t, t_f, "Mismatch in foreign function type.")) ()
              else
                (Foreign(i, t), (i, t) :: env)
            with Not_found ->
              t_error (-1) i (TMsg "Foreign function not found") () end

        | Flow fp ->
          let nfp, nenv = typecheck_flow env trig_env resource_env fp
          in (Flow nfp), nenv

        | Role(id,fp) ->
          let role_resource_env =
            try List.assoc id rresource_env with Not_found ->
              t_error (-1) "Invalid role" (TMsg("No role named "^id^" found")) ()
          in
          let nfp,nenv = typecheck_flow env trig_env role_resource_env fp
          in (Role(id, nfp), nenv)

        | DefaultRole id ->
          if List.mem_assoc id rresource_env then (DefaultRole(id), env)
          else t_error (-1) "Invalid default role" (TMsg("No role named "^id^" found")) ()

      in (nprog@[nd, (Type(t_unit)::meta)]), nenv
    ) ([], []) prog
 in prog, env, trig_env, rresource_env

let deduce_program_type program =
  let prog,_,_,_ = type_bindings_of_program program
  in prog

let deduce_program_test_type prog_test =
  let proc p testl =
    let p', env, trig_env, _ = type_bindings_of_program p in
    let testl' = list_map (fun (expr, check_expr) ->
      match check_expr with
      | FileExpr s ->
          (* can't check if it's a file *)
          let expr_t = deduce_expr_type trig_env env expr in
          expr_t, check_expr
      | InlineExpr e ->
          (* create a dummy equals to check both expressions *)
          let e_test = mk_eq expr e in
          let e_test_t = deduce_expr_type trig_env env e_test in
          let e_l, e_r = decompose_eq e_test_t in
          e_l, InlineExpr e_r
      ) testl
    in p', testl'
  in
  match prog_test with
  | ProgTest(p, testl) ->
      let p', tl' = proc p testl in
      ProgTest(p', tl')
  | NetworkTest(p, testl) ->
      let p', tl' = proc p testl in
      NetworkTest(p', tl')
  | ExprTest p_t_l ->
      (* change format so we can still use proc *)
      let p_t_l' = list_map (fun (p, e, check) ->
        let p', testl = proc p [e, check] in
        let e', check' = hd testl in
        p', e', check'
      ) p_t_l
      in
      ExprTest p_t_l'

