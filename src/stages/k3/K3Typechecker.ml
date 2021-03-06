open Util
open Tree

open K3.AST
open K3.Annotation

open K3Util
open K3Printing
open K3Helpers

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree of string

type type_bindings_t = (id_t * type_t) list
type event_type_bindings_t = (id_t * (id_t * (type_t list)) list) list

let lookup_type = ref (fun _ -> failwith "uninitialized")

(* Internal type declarations *)
(* has_type, expected_type, msg *)
type error_type =
  | TMismatch of type_t * type_t * string
  | FunMismatch of type_t * type_t list * string
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
let not_sorted_collection t = TBad(t, "not a sorted collection")
let not_collection_bt t = BTBad(t, "not a collection")
let not_vector t = BTBad(t, "not a vector")
let error_tuple_small n tl t = TBad(t, Printf.sprintf "tuple has size %d but subscript %d" tl n)
let wrong_let_size t = TBad(t, "wrong size for let")

let t_error uuid name msg = raise @@ TypeError(uuid, name, msg)

let check_tag_arity tag children =
  let length = List.length children in
  let correct_arity = match tag with
    | Const(_)  -> 0
    | Var(_)    -> 0
    | Tuple     -> length
    | Ignore    -> 1
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
    | Apply       -> length
    | Subscript _ -> 1

    | Block         -> length
    | Iterate       -> 2
    | IfThenElse    -> 3
    | CaseOf _      -> 3
    | BindAs _      -> 2
    | Let _         -> 2

    | Map               -> 2
    | Filter            -> 2
    | Flatten           -> 1
    | Aggregate         -> 3
    | AggregateV        -> 3
    | GroupByAggregate  -> 4
    | Sort              -> 2
    | Size              -> 1
    | Equijoin          -> 6

    | Peek          -> 1
    | PeekWithVid   -> 3
    | Slice         -> 2
    | SliceOp _     -> 2
    | At            -> 2
    | AtWith        -> 4
    | MinWith       -> 3
    | IsMember      -> 2
    | Insert        -> 2
    | InsertAt      -> 3
    | SetAll        -> 2
    | Extend        -> 2
    | Update        -> 3
    | UpsertWith
    | UpsertWithBefore -> 4
    | UpdateSuffix -> 3
    | UpdateAtWith -> 3
    | Delete       -> 2
    | DeletePrefix -> 2
    | DeleteAllPrefix -> 2
    | DeleteAt     -> 2
    | DeleteWith   -> 4
    | Pop -> 1
    | ClearAll     -> 1
    | FilterOp _   -> 2

    | Assign -> 2
    | Indirect -> 1

    | Send -> 3

    | PolyIter -> 4
    | PolyIterTag _ -> 4
    | PolyFold -> 3
    | PolyFoldTag _ -> 5
    | PolyAt _ -> 3
    | PolyAtWith _ -> 5
    | PolyInsert _ -> 2
    | PolyTagAt -> 2
    | PolySkip _ -> 3
    | PolyUnpack -> 1
    | PolyReserve -> 4

  in length = correct_arity

(* similar to haskell's infix `function` *)
let (<|) x f = f x
and (|>) f y = f y

(* thread exceptions through function applications *)
let (+++) f g = fun t x -> f (g t x) x

(* Type extraction primitives *)
let type_of_expr e =
  let error s err = t_error (id_of_expr e) s err in
  let is_type_annotation a = match a with Type _ -> true | _ -> false in
  let extract_type = function
    Type t -> t | _ -> error "Invalid type annotation" InvalidTypeAnnotation in
  let type_annotations = List.filter is_type_annotation (meta_of_expr e) in
  match nub type_annotations with
    | []  -> error "Untyped expression" UntypedExpression
    | [x] -> extract_type x
    | l   -> error "Multiple possible types" @@ MultiplePossibleTypes(
        List.fold_left (fun acc x ->
          acc^" "^string_of_type @@ extract_type x) "" l)

(* Type composition/decomposition primitives *)

let lookup_alias tenv = function
  | TAlias id -> (try some @@ List.assoc id tenv with Not_found -> None)
  | t -> Some t

(* get a default value for a type *)
let canonical_value_of_type vt =
  let rec loop vt =
    match vt.typ with
    | TUnknown              -> mk_cint 0
    | TUnit                 -> mk_cunit
    | TBool                 -> mk_cbool false
    | TDate
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

let rec assignable ?(unknown_ok=false) t_l t_r =
  let (===) = assignable in
  match t_l.typ, t_r.typ with
  | TMaybe t_lm, TMaybe t_rm -> t_lm === t_rm
  | TTuple t_ls, TTuple t_rs ->
      list_forall2 (===) t_ls t_rs
  | TCollection(TVMap(Some s), t_le), TCollection(TVMap(Some s'), t_re) ->
      IntSetSet.equal s s' && t_le === t_re
  | TCollection(t_lc, t_le), TCollection(t_rc, t_re) ->
      t_lc = t_rc && t_le === t_re
  | TFunction(it, ot), TFunction(it', ot') ->
      list_forall2 (fun t t' -> t === t' && t.mut = t'.mut) it it' &&
      ot === ot' && ot.mut = ot'.mut
  | TIndirect t, TIndirect t' -> t === t'
  | TDate, TInt               -> true
  | TInt, TDate               -> true
  (* ints and floats can be assigned. They'll just be concatentated *)
  | TInt, TFloat              -> true
  | TFloat, TInt              -> true
  (* handle lambdas with _ arguments *)
  | TUnknown, _               -> true
  | _, TUnknown when unknown_ok -> true
  | TTop, _
  | _, TTop                   -> true
  | _ when t_l.typ = t_r.typ  -> true
  | _                         -> false

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
  | TFunction(it, ot)   -> List.exists is_unknown_t it || is_unknown_t ot
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
        with Not_found -> t_erroru name (TMsg("Trigger "^id^" not found")) end
  in canonical constant_type

let rec gen_arg_bindings = function
  | AIgnored    -> []
  | AVar(i, t)  -> [i, t]
  | AMaybe a'   -> gen_arg_bindings a'
  | ATuple args -> List.concat @@ List.map gen_arg_bindings args

let rec descend_type f t = match t.typ with
  | TMaybe t -> f t
  | TTuple l -> List.iter f l
  | TCollection(_,t) -> f t
  | TTarget t -> f t
  | TFunction(l, t) -> List.iter f l; f t
  | TIndirect t -> f t
  | _ -> ()

let rec map f t =
  let map = map f in
  let typ = match t.typ with
    | TMaybe t -> TMaybe (map t)
    | TTuple l -> TTuple (List.map map l)
    | TCollection(c,t) -> TCollection(c, map t)
    | TTarget t -> TTarget(map t)
    | TFunction(l, t) -> TFunction(List.map map l, map t)
    | TIndirect t -> TIndirect(map t)
    | x -> x
  in f {t with typ}

let repr tenv t = match t.typ with
  | TAlias id ->
      begin try
        List.assoc id tenv
      with Not_found ->
        raise @@ TypeError(-1, "", TBad(t, sp "Type alias %s not found" id))
      end
  | _ -> t

let drepr tenv t = map (repr tenv) t

(* fill_in: check at each node whether we already have a type annotation.
 * If so, don't go any further down *)
let rec deduce_expr_type ?(override=true) trig_env env tenv utexpr : expr_t =
  (* If not overriding, see if we've already got a type *)
  let has_type e =
    try ignore @@ type_of_expr e; true
    with TypeError(_, _, UntypedExpression) -> false
         | _                                -> true
  in
  if has_type utexpr && not override then utexpr else

  let ((uuid, tag), aux), untyped_children = decompose_tree utexpr in
  let name = K3Printing.string_of_tag_type tag in
  let t_erroru = t_error uuid name in (* pre-curry the type error *)

  let malformed_tree () = raise @@ MalformedTree (K3Printing.string_of_tag_type tag) in

  (* convert_aliases to types *)
  (* let rec subst_aliases t =
    let rec loop t = match t.typ with
      | TAlias id ->
          begin try
            let t' = List.assoc id tenv in
            t.typ <- t'.typ
          with Not_found -> t_erroru @@ TBad(t, sp "Type alias %s not found" id)
          end
      | _ -> descend_type loop t
    in loop t; t
  in *)

  (* convert_aliases to types *)
  let repr t = try repr tenv t with TypeError(_,_,t) -> t_erroru t in
  (* make versions of functions that automatically lookup aliases in env *)
  let get_typ t = (repr t).typ in
  (* deep replacement *)
  let drepr t = drepr tenv t in

  let assignable ?unknown_ok t_l t_r = assignable ?unknown_ok (drepr t_l) (drepr t_r) in
  let (===) x y = (drepr x) === (drepr y) in
  let (<~) x y = (drepr x) <~ (drepr y) in
  let unwrap_tcol c = unwrap_tcol (repr c) in
  let unwrap_tfun f = unwrap_tfun (repr f) in
  let unwrap_ttuple t = unwrap_ttuple (repr t) in

  (* Check Tag Arity *)
  if not @@ check_tag_arity tag untyped_children then malformed_tree () else

  (* Augment environments for children *)
  let env_proc_fn (last_ch:expr_t option) i = match tag, last_ch, i with
    | Lambda a, None, _    -> gen_arg_bindings a @ env
    | CaseOf x, Some ch, 1 ->
        let t = type_of_expr ch in
        let t_e = match get_typ t with
          | TMaybe mt -> mt
          | _         -> t_erroru (not_maybe t) in
        (x, t_e) :: env
    | BindAs x, Some ch, 1 ->
        let t = type_of_expr ch in
        let t_e = match get_typ t with
          | TIndirect it -> it
          | _            -> t_erroru (not_ind t) in
        (x, t_e) :: env
    | Let xs, Some ch, 1 ->
        let t = type_of_expr ch in
        let ts = match get_typ t with
          | TTuple ts when List.length ts = List.length xs -> ts
          | _         when List.length xs = 1              -> [t]
          | _                                              -> t_erroru (wrong_let_size t)
        in
        (* remove unknowns from binding *)
        (List.filter ((<>) "_" |- fst) @@ list_zip xs ts) @ env
    | _ -> env
  in
  let typed_children = List.rev @@ fst @@ List.fold_left (fun (acc, i) ch ->
      let ch' = deduce_expr_type ~override trig_env (env_proc_fn (hd' acc) i) tenv ch
      in ch'::acc, i+1)
    ([], 0)
    untyped_children
  in
  let attach_type t = mk_tree (((uuid, tag), Type t::aux), typed_children) in

  let bind n = type_of_expr @@ List.nth typed_children n in

  let common_ops () =
    let tfun, tcol' = bind 0, bind 1 in
    let targ, tret =
      try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
    let tcol, telem =
      try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
    if is_tvmap tcol then t_erroru @@ TBad(tcol', "cannot be a VMap") else
    tfun, tcol', targ, tret, tcol, telem
  in

  let check_vmap col_t tup_t =
    if is_tvmap col_t then
      match unwrap_ttuple tup_t with
      | [] | [_] -> t_erroru @@ TBad(tup_t, "improper element types for vmap")
      | t::_ when t === t_vid -> ()
      | t::_ -> t_erroru @@ TMismatch(t, t_vid, "vmap element")
    else ()
  in
  let check_vmap_pat ?(msg="pattern") col_t elem_t pat =
    if is_tvmap col_t then
      let elem_v = wrap_ttuple @@ t_vid :: unwrap_ttuple elem_t in
      if not (pat === elem_v) then t_erroru @@ TMismatch(pat, elem_v, "vmap "^msg) else ()
    else
      if not (pat === elem_t) then t_erroru @@ TMismatch(pat, elem_t, msg) else ()
  in

  let current_type = match tag with
      | Const c -> deduce_constant_type uuid trig_env c
      | Var id  -> begin
          try List.assoc id env
          with Not_found -> t_erroru (TMsg(id^" not found"))
        end
      | Tuple       ->
          let child_types = List.map type_of_expr typed_children in
          wrap_ttuple child_types
      | Ignore      -> t_unit
      | Just        -> wrap_tmaybe @@ bind 0
      | Nothing t   ->
          let _ = match t.typ with
            | TMaybe _ -> ()
            | _        -> t_erroru (TBad(t, "not a maybe type")) in
          t
      | Empty t     -> t
      | Singleton t ->
          let t_c, t_e = try unwrap_tcol t with Failure _ -> t_erroru (not_collection t) in
          let t_ne = bind 0 in
          check_vmap t_c t_ne;
          (* adjust type for vmap *)
          let t_ne' =
            if is_tvmap t_c then wrap_ttuple @@ tl @@ unwrap_ttuple t_ne else t_ne in
          (* we disregard the element part of the singleton ast because it's not needed *)
          canonical @@ TCollection(t_c, t_ne')

      | Combine ->
          let t0, t1 = bind 0, bind 1 in
          let _ = try unwrap_tcol t0 with Failure _ -> t_erroru (not_collection t0) in
          let _ = try unwrap_tcol t1 with Failure _ -> t_erroru (not_collection t1) in
          if not (t0 === t1) then t_erroru (TMismatch(t0, t1,"")) else
          if is_unknown_t t0 then t1 else t0

      | Range t_c ->
          let start, stride, steps = bind 0, bind 1, bind 2 in
          if not (get_typ steps = TInt) then t_erroru (BTMismatch(TInt, steps.typ,"steps:")) else
          let t_e = match get_typ start, get_typ stride with
            | TInt, TInt     -> TInt
            | TFloat, TInt
            | TInt, TFloat
            | TFloat, TFloat -> TFloat
            | _ -> t_erroru (TMsg("start/stride types are bad"))
          in canonical @@ TCollection(t_c, canonical t_e)

      | Add | Mult ->
          let t_l, t_r = bind 0, bind 1 in
          let result_type = match get_typ t_l, get_typ t_r with
            | (TFloat | TInt), TFloat -> TFloat
            | TFloat, TInt   -> TFloat
            | TInt, TInt     -> TInt
            | TBool, TBool   -> TBool
            | _ -> t_erroru (TMismatch(t_l, t_r, ""))
          in canonical result_type

      | Neg ->
          let t0 = bind 0 in
          begin match get_typ t0 with
          | TBool | TInt | TFloat -> t0
          | _ -> t_erroru (not_collection t0)
          end

      | Eq | Lt | Neq | Leq ->
          let t_l, t_r = bind 0, bind 1 in
          if t_l === t_r then t_bool
          else t_erroru (TMismatch(t_l, t_r, ""))

      | IfThenElse ->
          let t_p, t_t, t_e = bind 0, bind 1, bind 2 in
          if canonical TBool === t_p then
              if assignable ~unknown_ok:true t_t t_e then t_t
              else t_erroru @@ TMismatch(t_t, t_e,"")
          else t_erroru @@ TMismatch(canonical TBool, t_p, "")

      | CaseOf id ->
          (* the expression was handled in the prelude *)
          let t_s, t_n = bind 1, bind 2 in
          if t_n === t_s then t_s
          else t_erroru @@ TMismatch(t_n, t_s, "case branches")

        (* handled in the prelude *)
      | BindAs _ -> bind 1

        (* handled in the prelude *)
      | Let _ -> bind 1

      | Block ->
          let rec validate_block i = function
            | e :: [] -> type_of_expr e
            | h :: t when type_of_expr h === canonical TUnit -> validate_block (i+1) t
            | _       -> t_erroru @@ TMsg(sp "Bad or non-TUnit expression at stmt %d" i)
          in validate_block 1 typed_children

      | Lambda t_a ->
          let t_r = bind 0 in
          canonical @@ TFunction(type_of_arg t_a, immut t_r)

      | Apply ->
          begin match List.map type_of_expr typed_children with
          | t_f::t_args ->
              let t_es, t_r =
                try unwrap_tfun t_f with Failure _ -> t_erroru (not_function t_f)
              in
              if list_forall2 (<~) t_es t_args then t_r
              else t_erroru (FunMismatch(t_f, t_args, ""))

          | _ -> t_erroru (TMsg("Bad arguments to apply"))
          end

      | Subscript n ->
          let t_e = bind 0 in
          begin match get_typ t_e with
          | TTuple l when n <= List.length l -> at l (n - 1)
          | TTuple l -> t_erroru (error_tuple_small n (List.length l) t_e)
          | TTop -> canonical TTop
          | _ -> t_erroru (not_tuple t_e)
          end

      | Iterate ->
          let _, _, targ, tret, _, telem = common_ops () in
          if not (tret === canonical TUnit)
            then t_erroru (TMismatch(tret, canonical TUnit, "return val:")) else
          if not (hd targ <~ telem) then
            t_erroru (TMismatch(hd targ, telem, "element:")) else
          canonical TUnit

      | Map ->
          let _, tcol', targ, tret, tcol, telem = common_ops () in
          if not (hd targ <~ telem) then
            t_erroru (TMismatch(hd targ, telem, "element:")) else
          canonical @@ TCollection(tcol, tret)

      | Filter ->
          let _, tcol', targ, tret, tcol, telem = common_ops () in
          if not (hd targ <~ telem) then
            t_erroru (TMismatch(hd targ, telem, "predicate:")) else
          if not (canonical TBool === tret) then
            t_erroru (TMismatch(canonical TBool, tret, "")) else
          tcol'

      | Flatten ->
          let tcol'' = bind 0 in
          let tcol, telem =
            try unwrap_tcol tcol'' with Failure _ -> t_erroru (not_collection tcol'') in
          let _ =
            try unwrap_tcol telem with Failure _ -> t_erroru (not_collection telem) in
          begin match tcol with
          | TSortedMap | TMap | TVMap _ -> t_erroru (TBad (tcol'', "can't flatten a Map"))
          | _ -> telem
          end

      | Aggregate ->
          let tfun, tzero, tcol' = bind 0, bind 1, bind 2 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          (* vmap fold puts the vid in the zero *)
          let tzero =
            if is_tvmap tcol then
              match unwrap_ttuple tzero with
              | [t_vid'; t_z] ->
                  if not (t_vid' === t_vid) then
                    t_erroru @@ TMismatch(t_vid', t_vid, "vid for vmap")
                  else t_z
              | _ -> t_erroru @@ TBad(tzero, "no vid for vmap")
            else tzero in
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let expected1 = [tzero; telem] in
          if not (tzero === tret) then
            t_erroru (TMismatch(tzero, tret, "lambda return and agg")) else
          if not (list_forall2 (<~) targ expected1) then
            t_erroru (TMismatch(wrap_ttuple targ, wrap_ttuple expected1, "lambda arg")) else
          tzero

      | AggregateV ->
          let tfun, tzero, tcol' = bind 0, bind 1, bind 2 in
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          let expected1 =
            match tcol with
            | TVMap _ -> [tzero; t_vid; telem]
            | _       -> t_erroru @@ TBad(tcol', "must have a vmap")
          in
          if not (tzero === tret) then
            t_erroru (TMismatch(tzero, tret, "lambda return and agg")) else
          if not (list_forall2 (<~) targ expected1) then
            t_erroru (TMismatch(wrap_ttuple targ, wrap_ttuple expected1, "lambda arg")) else
          tzero


      | GroupByAggregate ->
          let tgrp, tagg, tzero, tcol' = bind 0, bind 1, bind 2, bind 3 in
          let tgarg, tgret =
            try unwrap_tfun tgrp with Failure _ -> t_erroru (not_function tgrp) in
          let taarg, taret =
            try unwrap_tfun tagg with Failure _ -> t_erroru (not_function tagg) in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if is_tvmap tcol then t_erroru @@ TBad(tcol', "cannot run on vmap") else
          if not (hd tgarg <~ telem) then
            t_erroru (TMismatch(hd tgarg, telem, "grouping func:")) else
          let expected1 = [tzero; telem] in
          if not (list_forall2 (<~) taarg expected1) then
            t_erroru (TMismatch(wrap_ttuple taarg, wrap_ttuple expected1, "agg func:")) else
          if not (tzero <~ taret) then
            t_erroru (TMismatch(taret, tzero, "agg func:"))
          else canonical @@
            TCollection(tcol, wrap_ttuple [tgret; taret])

      | Sort ->
          let tfun, tcol', targ, tret, tcol, telem = common_ops () in

          let expected1 = [telem; telem] in
          if not (list_forall2 (<~) targ expected1) then
            t_erroru (TMismatch(wrap_ttuple targ, wrap_ttuple expected1, "Sort function arg")) else
          if not (canonical TBool === tret) then
            t_erroru (TMismatch(canonical TBool, tret, "Sort function result")) else
          if not (tcol = TList || tcol = TVector) then
            t_erroru (TMsg "can only sort on a list or vector") else
          canonical @@ TCollection(tcol, telem)

      | Equijoin ->
        begin match List.map type_of_expr typed_children with
          | [tcol1; tcol2; tprj1; tprj2; tagg_fn; tzero] ->
            let _, telem1 =
              try unwrap_tcol tcol1 with Failure _ -> t_erroru @@ not_collection tcol1 in
            let _, telem2 =
              try unwrap_tcol tcol2 with Failure _ -> t_erroru @@ not_collection tcol2 in
            let targ1, tret1 =
              try unwrap_tfun tprj1 with Failure _ -> t_erroru @@ not_function tprj1 in
            let targ2, tret2 =
              try unwrap_tfun tprj2 with Failure _ -> t_erroru @@ not_function tprj2 in
            if not (tret1 === tret2) then
              t_erroru @@ TMismatch(tret1, tret2, "Projection return functions") else
            if not (hd targ1 <~ telem1) then
              t_erroru @@ TMismatch(hd targ1, telem1, "First projection function") else
            if not (hd targ2 <~ telem2) then
              t_erroru @@ TMismatch(hd targ2, telem1, "Second projection function") else
            let targ3, tret3 =
              try unwrap_tfun tagg_fn with Failure _ -> t_erroru @@ not_function tagg_fn in
            let _ =
              try unwrap_tcol tzero with Failure _ -> t_erroru @@ not_collection tzero in
            let expected_targ3 = [tzero; wrap_ttuple targ1; wrap_ttuple targ2] in
            if not (list_forall2 (<~) targ3 expected_targ3) then
              t_erroru @@ TMismatch(wrap_ttuple targ3, wrap_ttuple expected_targ3, "agg func args") else
            if not (tret3 === tzero) then
              t_erroru @@ TMismatch(tret3, tzero, "agg func ret") else
            tzero

          | _ -> malformed_tree ()
        end

      | Size ->
          let tcol = bind 0 in
          ignore(try unwrap_tcol tcol with Failure _ -> t_erroru (not_collection tcol));
          t_int

      | Peek ->
          let tcol' = bind 0 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          wrap_tmaybe telem

      | PeekWithVid ->
          let tcol', tlam_none, tlam_some = bind 0, bind 1, bind 2 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          let tn_arg, tn_ret =
            try unwrap_tfun tlam_none with Failure _ -> t_erroru (not_function tlam_none) in
          let ts_arg, ts_ret =
            try unwrap_tfun tlam_some with Failure _ -> t_erroru (not_function tlam_some) in
          if not (is_tvmap tcol) then
            t_erroru (TMismatch(tcol', wrap_tvmap telem, "collection type")) else
          if not (tn_ret === ts_ret) then
            t_erroru (TMismatch(tn_ret, ts_ret, "function return types")) else
          if not (list_forall2 (<~) tn_arg [t_unit]) then
            t_erroru (TMismatch(wrap_ttuple tn_arg, t_unit, "none lambda")) else
          if not (list_forall2 (<~) ts_arg [t_vid; telem]) then
            t_erroru (TMismatch(wrap_ttuple [t_vid; telem], wrap_ttuple ts_arg, "some lambda")) else
          tn_ret

      | Slice ->
          let tcol', tpat = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          (* order to take care of possible unknowns in pattern *)
          if is_tvmap tcol then
            let telem_v = wrap_ttuple @@ t_vid :: unwrap_ttuple telem in
            if not (tpat === telem_v) then t_erroru (TMismatch(tpat, telem_v, "vmap pattern"))
            else tcol'
          else (* any data structure *)
            if not (tpat === telem) then t_erroru (TMismatch(tpat, telem, "pattern"))
            else tcol'

      | SliceOp _ ->
          let tcol', tpat = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tsorted tcol then t_erroru @@ not_sorted_collection tcol' else
          check_vmap_pat tcol telem tpat;
          tcol'

      | At ->
          let tcol', tidx = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (is_tvector tcol) then
            t_erroru (TMismatch(tcol', wrap_tvector telem, "collection type")) else
          telem

      | IsMember ->
          let tcol', tkey = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tkey === telem) then t_erroru @@ TMismatch(tkey, telem, "elem") else
          if not (is_tset tcol) then t_erroru @@ TBad(tcol', "not a set") else
          t_bool

      | AtWith ->
          let tcol', tidx, tlam_none, tlam_some = bind 0, bind 1, bind 2, bind 3 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          let tn_arg, tn_ret =
            try unwrap_tfun tlam_none with Failure _ -> t_erroru (not_function tlam_none) in
          let ts_arg, ts_ret =
            try unwrap_tfun tlam_some with Failure _ -> t_erroru (not_function tlam_some) in
          if not (is_tvector tcol) then
            t_erroru (TMismatch(tcol', wrap_tvector telem, "collection type")) else
          if not (tn_ret === ts_ret) then
            t_erroru (TMismatch(tn_ret, ts_ret, "function return types")) else
          if not (list_forall2 (<~) tn_arg [t_unit]) then
            t_erroru (TMismatch(wrap_ttuple tn_arg, t_unit, "none lambda")) else
          if not (list_forall2 (<~) ts_arg [telem]) then
            t_erroru (TMismatch(wrap_ttuple [telem], wrap_ttuple ts_arg, "some lambda")) else
          ts_ret

      | MinWith ->
          let tcol', tlam_none, tlam_some = bind 0, bind 1, bind 2 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          let tn_arg, tn_ret =
            try unwrap_tfun tlam_none with Failure _ -> t_erroru (not_function tlam_none) in
          let ts_arg, ts_ret =
            try unwrap_tfun tlam_some with Failure _ -> t_erroru (not_function tlam_some) in
          if not @@ is_tsorted tcol then t_erroru @@ not_sorted_collection tcol' else
          if not (tn_ret === ts_ret) then
            t_erroru (TMismatch(tn_ret, ts_ret, "function return types")) else
          if not (list_forall2 (<~) tn_arg [t_unit]) then
            t_erroru (TMismatch(wrap_ttuple tn_arg, t_unit, "none lambda")) else
          check_vmap_pat ~msg:"some lambda" tcol telem (wrap_ttuple ts_arg);
          tn_ret

      | Insert ->
          let tcol', telem' = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          check_vmap_pat tcol telem telem';
          t_unit

      | InsertAt ->
          let tcol', tidx, telem' = bind 0, bind 1, bind 2 in
          if not (tidx === t_int) then t_erroru (TMismatch(tidx, t_int, "index")) else
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tvector tcol) then
            t_erroru (TMismatch(tcol', wrap_tvector telem, "not a vector")) else
          check_vmap_pat tcol telem telem';
          t_unit

      | SetAll ->
          let tcol', telem' = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tvector tcol) then
            t_erroru (TMismatch(tcol', wrap_tvector telem, "not a vector")) else
          check_vmap_pat tcol telem telem';
          t_unit

      | Extend ->
          let tcol1, tcol2 = bind 0, bind 1 in
          let _ = try unwrap_tcol tcol1 with Failure _ -> t_erroru @@ not_collection tcol1 in
          let _ = try ignore(unwrap_tcol tcol2) with Failure _ -> t_erroru @@ not_collection tcol2 in
          if not (tcol1 === tcol2) then t_erroru @@ TMismatch(tcol1, tcol2, "") else
          t_unit

      | Update ->
          let tcol', told, tnew = bind 0, bind 1, bind 2 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          check_vmap_pat tcol telem tnew;
          if not (told === telem) then t_erroru @@ TMismatch(told, telem, "old value") else
          t_unit

      | UpdateSuffix ->
          let tcol', tnew, tlam_update = bind 0, bind 1, bind 2 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tvmap tcol) then
            t_erroru (TMismatch(tcol', wrap_tvmap telem, "collection type")) else
          check_vmap_pat tcol telem tnew;
          let tlam_update' = wrap_tfunc (t_vid :: [telem]) telem in
          if not (tlam_update === tlam_update') then
            t_erroru (TMismatch(tlam_update, tlam_update', "update lambda")) else
          t_unit

      | UpsertWith | UpsertWithBefore ->
          let tcol', tkey, tlam_insert, tlam_update = bind 0, bind 1, bind 2, bind 3 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          check_vmap_pat tcol telem tkey;
          let tlam_insert' = wrap_tfunc [t_unit] telem in
          if not (tlam_insert === tlam_insert') then
            t_erroru (TMismatch(tlam_insert, tlam_insert', "insert lambda")) else
          let tlam_update' = wrap_tfunc [telem] telem in
          if not (tlam_update === tlam_update') then
            t_erroru (TMismatch(tlam_update, tlam_update', "update lambda")) else
          t_unit

      (* upsertwith for vector *)
      | UpdateAtWith ->
          let tcol', tkey, tlambda = bind 0, bind 1, bind 2 in
          if not (tkey === t_int) then t_erroru @@ TMismatch(tkey, t_int, "key") else
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if tcol <> TVector then t_erroru @@ TBad(tcol', "not a vector") else
          let tlambda' = wrap_tfunc [telem] telem in
          if not (tlambda === tlambda') then
            t_erroru (TMismatch(tlambda, tlambda', "lambda")) else
          t_unit

      | FilterOp _ ->
          let tcol', telem' = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tsorted tcol then t_erroru @@ not_sorted_collection tcol' else
          check_vmap_pat tcol telem telem';
          tcol'

      | Delete ->
          let tcol', told = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          check_vmap_pat tcol telem told;
          t_unit

      | DeletePrefix ->
          let tcol', told = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tsorted tcol then t_erroru @@ not_sorted_collection tcol' else
          check_vmap_pat tcol telem told;
          t_unit

      | DeleteAllPrefix ->
          let tcol', tvid = bind 0, bind 1 in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tvmap tcol then t_erroru @@ TBad(tcol', "not a vmap") else
          if not (tvid === t_vid) then t_erroru @@ TMismatch(tvid, t_vid, "vid") else
          t_unit

      | DeleteAt ->
          let tcol', tidx = bind 0, bind 1 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tcol = TVector) then t_erroru @@ not_vector tcol'.typ else
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          telem

      | Pop ->
          let tcol' = bind 0 in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tcol = TList) then t_erroru @@ TBad(tcol', "not a list") else
          t_unit

      | DeleteWith ->
          let tcol', tkey, tlam_none, tlam_some = bind 0, bind 1, bind 2, bind 3 in
          let tcol, telem =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (tkey <~ telem) then t_erroru @@ TMismatch(tkey, telem, "key") else
          let tn_arg, tn_ret =
            try unwrap_tfun tlam_none with Failure _ -> t_erroru (not_function tlam_none) in
          let ts_arg, ts_ret =
            try unwrap_tfun tlam_some with Failure _ -> t_erroru (not_function tlam_some) in
          if not (tn_ret === ts_ret) then
            t_erroru (TMismatch(tn_ret, ts_ret, "function return types")) else
          if not (list_forall2 (<~) tn_arg [t_unit]) then
            t_erroru (TMismatch(wrap_ttuple tn_arg, t_unit, "none lambda")) else
          if not (list_forall2 (<~) ts_arg [telem]) then
            t_erroru (TMismatch(wrap_ttuple [telem], wrap_ttuple ts_arg, "some lambda")) else
          ts_ret

      | ClearAll ->
        let tcol = bind 0 in
        let _ =
          try unwrap_tcol tcol with Failure _ -> t_erroru (not_collection tcol) in
        t_unit

      | Assign ->
          let tl, tr = bind 0, bind 1 in
          if not (tl === tr) then t_erroru @@ TMismatch(tl, tr, "") else
          if not tl.mut then t_erroru @@ TBad(tl, "not mutable") else
          t_unit

      (* Add a layer of indirection *)
      | Indirect ->
          let tr = bind 0 in
          wrap_tind tr

      | Send ->
          let target, taddr, targs = bind 0, bind 1, bind 2 in
          let ttarget = match get_typ target with
              | TTarget t -> t
              | _         -> t_erroru (TBad(target, "not a target"))
          in
          begin match get_typ taddr with
          | TAddress ->
              if ttarget === targs then t_unit
              else t_erroru @@ TMismatch(targs, ttarget, "")
          | _ -> t_erroru @@ TBad(taddr, "not an address")
          end

      | PolyIter ->
          let tidx, toffset, tfun, tcol' = bind 0, bind 1, bind 2, bind 3 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tpolyq tcol then t_erroru @@ TBad(tcol', "not a polyqueue") else
          let targ' = [t_int; t_int; t_int] in
          if not @@ list_forall2 (<~) targ targ' then
            t_erroru @@ TMismatch(wrap_ttuple targ, wrap_ttuple targ', "args") else
          let tret' = wrap_ttuple [t_int; t_int] in
          if not (tret === tret')
            then t_erroru (TMismatch(tret, tret', "return val")) else
          tret'

      | PolyFold ->
          let tfun, tacc, tcol' = bind 0, bind 1, bind 2 in
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not @@ is_tpolyq tcol then t_erroru @@ TBad(tcol', "not a polyqueue") else
          let targ' = [tacc; t_int; t_int; t_int] in
          if not @@ list_forall2 (<~) targ targ' then
            t_erroru @@ TMismatch(wrap_ttuple targ, wrap_ttuple targ', "args") else
          if not (tret === tacc)
            then t_erroru (TMismatch(tret, tacc, "return val")) else
          tret

      | PolyIterTag tag ->
          let tidx, toffset, tfun, tcol' = bind 0, bind 1, bind 2, bind 3 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
              let t_tag =
                try
                  thd3 @@ List.find (fun (_,s,_) -> (s:string) = tag) tags
                with Not_found -> t_erroru @@ TBad(tcol', "no tag named "^tag)
              in
              let targ' = [t_int; t_int; t_tag] in
              if not @@ list_forall2 (<~) targ targ' then
                t_erroru @@ TMismatch(wrap_ttuple targ, wrap_ttuple targ', "args") else
              if not (tret === t_unit)
                then t_erroru (TMismatch(tret, t_unit, "return val")) else
              t_unit
          end

      | PolyFoldTag tag ->
          let tidx, toffset, tfun, tacc, tcol' = bind 0, bind 1, bind 2, bind 3, bind 4 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let targ, tret =
            try unwrap_tfun tfun with Failure _ -> t_erroru (not_function tfun) in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
              let t_tag =
                try
                  thd3 @@ List.find (fun (_,s,_) -> (s:string) = tag) tags
                with Not_found -> t_erroru @@ TBad(tcol', "no tag named "^tag)
              in
              let targ' = [tacc; t_int; t_int; t_tag] in
              if not @@ list_forall2 (<~) targ targ' then
                t_erroru @@ TMismatch(wrap_ttuple targ, wrap_ttuple targ', "args") else
              if not (tret === tacc)
                then t_erroru (TMismatch(tret, tacc, "return val")) else
              tacc
          end

      | PolyAt tag ->
          let tcol', tidx, toffset = bind 0, bind 1, bind 2 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
              try
                thd3 @@ List.find (fun (_,s,_) -> (s:string) = tag) tags
              with Not_found -> t_erroru @@ TBad(tcol', "no tag named "^tag)
          end

      | PolyTagAt ->
          let tcol', tidx = bind 0, bind 1 in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tpolyq tcol) then t_erroru @@ TBad(tcol', "not a polyqueue") else
          t_int

      | PolyUnpack ->
          let tcol' = bind 0 in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tpolyq tcol) then t_erroru @@ TBad(tcol', "not a polyqueue") else
          t_unit

      | PolyReserve ->
          let tcol', x, y, z = bind 0, bind 1, bind 2, bind 3 in
          if not (x === t_int) then t_erroru @@ TMismatch(x, t_int, "number of elements") else
          if not (y === t_int) then t_erroru @@ TMismatch(y, t_int, "fixed size") else
          if not (z === t_int) then t_erroru @@ TMismatch(z, t_int, "variable size") else
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          if not (is_tpolyq tcol) then t_erroru @@ TBad(tcol', "not a polyqueue") else
          t_unit

      | PolyAtWith tag ->
          let tcol', tidx, toffset, tlam_none, tlam_some = bind 0, bind 1, bind 2, bind 3, bind 4 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          let tn_arg, tn_ret =
            try unwrap_tfun tlam_none with Failure _ -> t_erroru (not_function tlam_none) in
          let ts_arg, ts_ret =
            try unwrap_tfun tlam_some with Failure _ -> t_erroru (not_function tlam_some) in
          if not (tn_ret === ts_ret) then
            t_erroru (TMismatch(tn_ret, ts_ret, "function return types")) else
          if not (list_forall2 (<~) tn_arg [t_unit]) then
            t_erroru (TMismatch(wrap_ttuple tn_arg, t_unit, "none lambda")) else
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
            let t_tag =
              try
                thd3 @@ List.find (fun (_,s,_) -> (s:string) = tag) tags
              with Not_found -> t_erroru @@ TBad(tcol', "no tag named "^tag)
            in
            if not (list_forall2 (<~) ts_arg [t_tag]) then
              t_erroru (TMismatch(wrap_ttuple ts_arg, t_tag, "some lambda")) else
            ts_ret
          end

      | PolyInsert tag ->
          let tcol', telem = bind 0, bind 1 in
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
            let t_tag =
              try
                thd3 @@ List.find (fun (_,s,_) -> (s:string) = tag) tags
              with Not_found -> t_erroru @@ TBad(tcol', "no tag named "^tag)
            in
            if not (telem === t_tag) then t_erroru @@ TMismatch(telem, t_tag, "element") else
            t_unit
          end

      | PolySkip(_, tag) ->
          let tcol', tidx, toffset = bind 0, bind 1, bind 2 in
          if not (tidx === t_int) then t_erroru @@ TMismatch(tidx, t_int, "index") else
          if not (toffset === t_int) then t_erroru @@ TMismatch(toffset, t_int, "offset") else
          let tcol, _ =
            try unwrap_tcol tcol' with Failure _ -> t_erroru (not_collection tcol') in
          begin match get_tpolyq_tags tcol with
          | None      -> t_erroru @@ TBad(tcol', "not a polyqueue")
          | Some tags ->
              if not (List.exists (fun (_,s,_) -> (s:string) = tag) tags) then
                t_erroru @@ TBad(tcol', sp "no tag named %s" tag) else
              wrap_ttuple [t_int; t_int]
          end

  in
  begin try
    let t_c, t_e = unwrap_tcol current_type in
    if is_tmap t_c && List.length @@ unwrap_ttuple t_e <> 2 then
      t_erroru @@ TBad(current_type, "Malformed map type")
  with Failure _ -> () end;
  attach_type current_type

let check_trigger_type trig_env env tenv id args locals body rebuild_f =
  let name           = "Trigger("^id^")" in
  let self_bindings  = id, canonical @@ TTarget(hd @@ type_of_arg args) in
  let arg_bindings   = gen_arg_bindings args in
  let local_bindings = List.map (fun x -> fst3 x, snd3 x) locals in
  let inner_env = self_bindings :: arg_bindings @ local_bindings @ env in
  let typed_body = deduce_expr_type trig_env inner_env tenv body in
  let t_b = type_of_expr typed_body in
  match t_b.typ with
  | TUnit ->
      (* add annotations *)
      let new_locals = List.map (fun (i,vt,meta) -> (i, vt, (Type(vt)::meta))) locals
      in ((rebuild_f id args new_locals typed_body), self_bindings :: env)
  | _     -> t_error (-1) name (TMismatch(t_unit, t_b, "trigger return type"))


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
    let tcol' = type_of_expr @@ deduce_expr_type [] [] [] e in
    let tcol, telem =
      try unwrap_tcol tcol' with Failure _ -> t_error uuid "Stream" (not_collection tcol') in
    if not (t === telem)
      then t_error uuid "stream" (TMismatch(t, telem, "resource type"))
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
    t_error (-1) error_prefix (TMsg("Could not find resource named "^src_id))

let arg_type_of_trigger error_prefix trig_env trig_id =
  try
    let t = List.assoc trig_id trig_env in
    begin match t.typ with
      | TTarget arg_t -> [arg_t]
      | _ -> t_error (-1) error_prefix (TMsg "Invalid trigger argument type")
    end
  with Not_found ->
    t_error (-1) error_prefix (TMsg("Could not find trigger named "^trig_id))

let typecheck_flow env tenv trig_env resource_env fp =
  let check_code_type name id args locals body rebuild_f =
    try check_trigger_type trig_env env tenv id args locals body rebuild_f
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
          | Some(msg) -> t_error (-1) error_preamble msg
        end

      | _ -> fs, env
      in (nfp@[nfs,a]), nenv
    ) ([], env) fp


(* Environment constructors *)
let types_of_endpoints endpoint_l =
  let error_if_dup k v l =
    if not(List.mem_assoc k l) then (k,v)::l
    else t_error (-1) ("Endpoint("^k^")") (TMsg("Found duplicate endpoint named "^k))
  in
  List.fold_left (fun env ep -> match ep with
      | Resource(id,r) -> error_if_dup id (type_of_resource env r) env
      | Code(id, args, locals, body) ->
          let t = canonical @@ TTarget(wrap_ttuple @@ type_of_arg args) in
          error_if_dup id [t] env
    ) [] endpoint_l

let source_types_of_program p = types_of_endpoints (sources_of_program p)
let sink_types_of_program p   = types_of_endpoints (sinks_of_program p)

(* Returns a trigger environment for the program *)
let trigger_types_of_program p =
  let env = types_of_endpoints (triggers_of_program p) in
  List.map (fun (id, tl) -> match tl with
      | [x] -> (id,x)
      | _ -> t_error (-1) ("Endpoint("^id^")") (TMsg("Multiple types resolved"))
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
  (* tenv = alias env *)
  let trig_env = trigger_types_of_program prog in
  let resource_env = source_types_of_program prog in
  let rresource_env = source_types_of_roles prog in
  let prog, env, tenv =
    List.fold_left (fun (nprog, env, tenv) (d, meta) ->
      let nd, nenv, talias_env = match d with
        | Global(i, t, Some init) ->
          let typed_init =
            try deduce_expr_type trig_env env tenv init
            with TypeError(ast_id, inner, msg) ->
              raise @@ TypeError(ast_id, "Global "^i^":"^inner, msg)
          in
          let expr_type = type_of_expr typed_init in
          if not (expr_type === t) then t_error (-1) i @@
            TMismatch(expr_type, t, "Mismatch in global type declaration.")
          else
          Global(i, t, Some typed_init), (i, t) :: env, tenv

        | Global(i, t, None) as x -> x, (i, t) :: env, tenv

        | Foreign(i, t) as x ->
            begin try let t_f = !lookup_type i in
              if not (t <~ t_f) then t_error (-1) i @@
                TMismatch(t, t_f, "Mismatch in foreign function type.")
              else
                x, (i, t) :: env, tenv
            with Not_found ->
              t_error (-1) i @@ TMsg "Foreign function not found"
            end

        | Flow fp ->
          let nfp, nenv = typecheck_flow env tenv trig_env resource_env fp in
          Flow nfp, nenv, tenv

        | Role(id, fp) ->
          let role_resource_env =
            try List.assoc id rresource_env with Not_found ->
              t_error (-1) "Invalid role" @@ TMsg("No role named "^id^" found")
          in
          let nfp, nenv = typecheck_flow env tenv trig_env role_resource_env fp in
          Role(id, nfp), nenv, tenv

        | DefaultRole id as x->
          if List.mem_assoc id rresource_env then
            x, env, tenv
          else
            t_error (-1) "Invalid default role" @@ TMsg("No role named "^id^" found")

        | Typedef(id, t) as x -> x, env, (id, t)::tenv

      in (nprog@[nd, (Type(t_unit)::meta)]), nenv, talias_env
    ) ([], [], []) prog
 in prog, env, tenv, trig_env, rresource_env

let deduce_program_type program =
  let prog,_,_,_,_ = type_bindings_of_program program
  in prog

let deduce_program_test_type prog_test =
  let proc p testl =
    let p', env, tenv, trig_env, _ = type_bindings_of_program p in
    let testl' = list_map (fun (expr, check_expr) ->
      match check_expr with
      | FileExpr s ->
          (* can't check if it's a file *)
          let expr_t = deduce_expr_type trig_env env tenv expr in
          expr_t, check_expr
      | InlineExpr (nm, e) ->
          (* create a dummy equals to check both expressions *)
          let e_test = mk_eq expr e in
          let e_test_t = deduce_expr_type trig_env env tenv e_test in
          let e_l, e_r = decompose_eq e_test_t in
          e_l, InlineExpr (nm, e_r)
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

