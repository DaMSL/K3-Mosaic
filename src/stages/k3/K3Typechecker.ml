(* K3 Typechecker *)

open Util
open Tree

open K3.AST
open K3.Annotation

open K3Util
open K3Printing

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree

(* uuid, location in typechecker, compared types *)
exception TypeError of int * string

type type_bindings_t = (id_t * type_t) list
type event_type_bindings_t = (id_t * (id_t * (type_t list)) list) list

(* Internal type declarations *)
type error_type =
  | TMismatch of type_t * type_t * string
  | VTMismatch of value_type_t * value_type_t * string
  | BTMismatch of base_type_t * base_type_t * string
  | TBad of type_t
  | VTBad of value_type_t
  | BTBad of base_type_t
  | MTBad of mutable_type_t
  | TMsg of string

let t_error uuid name msg () = 
  let extra = match msg with
    | TMismatch(t1,t2,s)  -> s^" This expression has type "^string_of_type t1^
        "\nBut an expression was expected of type "^string_of_type t2
    
    | VTMismatch(t1,t2,s) -> s^" This expression has type "^
        string_of_value_type t1^"\nBut an expression was expected of type "^
        string_of_value_type t2
    
    | BTMismatch(t1,t2,s) -> s^" This expression has type "^
        string_of_base_type t1^"\nBut an expression was expected of type "^
        string_of_base_type t2
    
    | TBad(t)           -> "Bad type "^string_of_type t
    | VTBad(t)          -> "Bad type "^string_of_value_type t
    | BTBad(t)          -> "Bad type "^string_of_base_type t
    | MTBad(t)          -> "Bad type "^flat_string_of_mutable_type t
    | TMsg(s)           -> s
  in
  raise (TypeError(uuid, name^": "^extra^")" ))

let check_tag_arity tag children =
  let length = List.length children in
  let correct_arity = match tag with
    | Const(_)  -> 0
    | Var(_) -> 0
    | Tuple -> length
    | Just -> 1

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

    | Lambda(_) -> 1
    | Apply     -> 2

    | Block         -> length
    | Iterate       -> 2
    | IfThenElse    -> 3

    | Map               -> 2
    | FilterMap         -> 3
    | Flatten           -> 1
    | Aggregate         -> 3
    | GroupByAggregate  -> 4
    | Sort              -> 2

    | Slice     -> 2
    | Insert    -> 2
    | Delete    -> 2
    | Update    -> 3
    | Peek      -> 1

    | Assign -> 2
    | Deref -> 1

    | Send -> 3
  in length = correct_arity

let (<|) x f = f x
and (|>) f y = f y

let (+++) f g = fun t x -> f (g t x) x
let (++%) f g = fun t x -> f (g t) x
let (%++) f g = fun t x -> f (g t x)

(* Type extraction primitives *)
let type_of_expr e =
  let error s = t_error (id_of_expr e) "ExprType" (TMsg s) () in
  let is_type_annotation a = match a with Type _ -> true | _ -> false in
  let extract_type = function
    Type t -> t | _ -> error "invalid type annotation" in
  let type_annotations = List.filter is_type_annotation (meta_of_expr e) in
  match type_annotations with
    | []  -> error "found untyped expression"
    | [x] -> extract_type x
    | l   -> error @: "multiple possible types found: "^
      List.fold_left (fun acc x -> 
        acc^" "^string_of_type @: extract_type x) "" l

(* Type composition/decomposition primitives *)

let canonical bt = TIsolated(TImmutable(bt,[]))

let value_of t x =
  let dummy = canonical TUnknown in
    match t with
    | TValue(vt) -> vt
    | _ -> x (); dummy (* raise exception *)

let function_of t x =
    let dummy = (canonical TUnknown, canonical TUnknown) 
    in match t with
    | TFunction(t_a, t_r) -> (t_a, t_r)
    | _ -> x (); dummy (* raise exception *)

let mutable_of vt =
    match vt with
    | TIsolated(mt)
    | TContained(mt) -> mt

let collection_of bt x =
    let dummy = (TList, canonical TUnknown) in
    match bt with
    | TCollection(t_c, t_e) -> (t_c, t_e)
    | _ -> x (); dummy (* raise exception *)

let dereft mt x =
    let dummy = TUnknown in 
    match mt with
    | TMutable(bt,_) -> bt
    | _ -> x (); dummy

let annotation_of vt = 
    match vt with
    | TIsolated(TImmutable(_,a))
    | TIsolated(TMutable(_,a))
    | TContained(TImmutable(_,a))
    | TContained(TMutable(_,a)) -> a

let apply_to_base_of f vt =
    match vt with
    | TIsolated(TImmutable(bt,a))
    | TIsolated(TMutable(bt,a))
    | TContained(TImmutable(bt,a))
    | TContained(TMutable(bt,a)) -> f (bt, a)

let base_of vt           = apply_to_base_of fst vt 
let annotated_base_of vt = apply_to_base_of (fun x -> x) vt
let annotation_of vt     = apply_to_base_of snd vt


(* This copies all annotations from the base type *)
let rec contained_of vt =
    let inner_base_type, inner_ann = annotated_base_of vt in
    let convertable_type = (
        match inner_base_type with
        | TTuple(ts) -> TTuple(List.map contained_of ts)
        | TCollection(t_c, t_e) -> TCollection(t_c, contained_of t_e)
        | TMaybe(t_m) -> TMaybe(contained_of t_m)
        | _ -> inner_base_type
    ) in TContained(TImmutable(convertable_type, inner_ann))


(* Type comparison primitives *)

let rec assignable t_l t_r =
    let t_lb = base_of t_l in let t_rb = base_of t_r in
    match (t_lb, t_rb) with
    | TMaybe(t_lm), TMaybe(t_rm) -> 
      begin match base_of t_lm, base_of t_rm with
      | _, TUnknown -> true
      | TUnknown, _ -> true
      | _ -> assignable t_lm t_rm
      end
    | TTuple(t_ls), TTuple(t_rs) -> List.length t_ls = List.length t_rs && 
        List.for_all2 assignable t_ls t_rs
    | TCollection(t_lc, _), TCollection(t_rc, TContained(TImmutable(TUnknown,_)))
      when t_lc = t_rc -> true
    | TCollection(t_lc, TContained(TImmutable(TUnknown,_))), TCollection(t_rc, _)
      when t_lc = t_rc -> true
    | TCollection(t_lc, t_le), TCollection(t_rc, t_re) -> assignable t_le t_re
      (* For the case of an empty collection *)
    | TUnknown, _ -> true
    | _ when t_lb = t_rb -> true
    | _ -> false

let (===) = assignable

(* Whether a type contains TUnknown somewhere ie. it's not a fully known type *)
let rec is_unknown_t t = match base_of t with
    | TMaybe t_m -> is_unknown_t t_m
    | TTuple(t_s) -> if List.for_all (not |- is_unknown_t) t_s then false else true
    | TCollection(_, t_e) -> is_unknown_t t_e
    | TUnknown -> true
    | _ -> false

let compare_type_ts t_l t_r = match t_l, t_r with
    | TFunction(in_l,out_l), TFunction(in_r, out_r) -> 
            in_l === in_r && out_l === out_r
    | TValue(v_l), TValue(v_r) -> v_l === v_r
    | _ -> false

let rec passable t_l t_r =
    match t_l, t_r with
    | TContained(TMutable(_)), TContained(TMutable(_)) -> assignable t_l t_r
    | TContained(TMutable(_)), _ -> false
    | _ -> assignable t_l t_r

let (<~) = passable


(* Type deduction *)

let deduce_constant_type id trig_env c =
  (* pre-curry the type error *)
  let t_erroru = t_error id in 
  let constant_type =
      match c with
      | CUnit -> TUnit
      | CUnknown -> TUnknown
      | CBool(_) -> TBool
      | CInt(_) -> TInt
      | CFloat(_) -> TFloat
      | CString(_) -> TString
      | CAddress(_) -> TAddress
      | CTarget(id) -> (* retrieve type from trig env *)
          let name = "CTarget" in
          begin try let typ = List.assoc id trig_env in
              typ <| base_of %++ value_of |> t_erroru name @: TBad(typ)
          with Not_found -> t_erroru name (TMsg("Trigger "^id^" not found")) () end
      | CNothing -> TMaybe(canonical TUnknown)
  in canonical constant_type

let rec gen_arg_bindings a =
    match a with
    | AIgnored -> []
    | AVar(i, t) -> [(i, TValue(t))]
    | AMaybe(a') -> gen_arg_bindings a'
    | ATuple(args) -> List.concat (List.map gen_arg_bindings args)

let rec deduce_expr_type trig_env cur_env utexpr =
    let ((uuid, tag), aux), untyped_children = decompose_tree utexpr in
    let t_erroru = t_error uuid in (* pre-curry the type error *)

    (* Check Tag Arity *)
    if not (check_tag_arity tag untyped_children) then raise MalformedTree else

    (* Determine if the environment to be passed down to child typechecking needs to be augmented. *)
    let env =
        match tag with
        | Lambda(a) -> gen_arg_bindings a @ cur_env
        | _ -> cur_env
    in

    let typed_children = List.map (deduce_expr_type trig_env env) untyped_children in
    let attach_type t = mk_tree (((uuid, tag), ((Type t)::aux)), typed_children) in
    let bind n = type_of_expr (List.nth typed_children n) in

    let current_type =
        match tag with
        | Const(c) -> TValue(deduce_constant_type uuid trig_env c)
        | Var(id) -> (
            try List.assoc id env
            with Not_found -> t_erroru "Var" (TMsg(id^" not found")) ()
        )
        | Tuple ->
            let child_types = List.map 
            (fun e -> 
                type_of_expr e <| value_of |> t_erroru "Tuple" (TBad(type_of_expr e)))
                typed_children 
            in 
            TValue(canonical (TTuple(child_types)))
        | Just ->
            let inner = bind 0 in
            let inner_type = inner <| value_of |> t_erroru "Just" (TBad(inner)) in
            TValue(canonical (TMaybe(inner_type)))

        | Empty(t) -> TValue(t)
        | Singleton(t) ->
            let name = "Singleton" in
            let t_c, t_e = t <| collection_of ++% base_of |> 
                t_erroru name @: VTBad(t) in
            let t0 = bind 0 in
            let t_ne = t0 <| value_of |> t_erroru name @: TBad(t0)
            in TValue(canonical (TCollection(t_c, contained_of t_ne)))
        | Combine ->
            let name = "Combine" in
            let t0 = bind 0 in
            let t_c0, t_e0 = t0 <| collection_of +++ base_of %++ value_of |> 
                t_erroru name @: TBad(t0) in
            let t1 = bind 1 in
            let t_c1, t_e1 = t1 <| collection_of +++ base_of %++ value_of |> 
                t_erroru name @: TBad(t1) in

            (* Only collections of matching element types can be combined. *)
            (* Note: strictly speaking this isn't true, e.g. [nothing]++[Just 1]
             * we'll use assignable here even though it's not a perfect match *)
            if not (t_e0 === t_e1) then t_erroru name (VTMismatch(t_e0, t_e1,"")) () else

            (* Determine combined collection type. *)
            let t_cr = begin match (t_c0, t_c1) with
              | (TList, _)    -> TList
              | (_, TList)    -> TList
              | (TBag, _)     -> TBag
              | (_, TBag)     -> TBag
              | (TSet, TSet)  -> TSet
            end in let t_e = begin match is_unknown_t t_e0, is_unknown_t t_e1 with
              | false, _ -> t_e0
              | _, false -> t_e1
              | _        -> t_e0
            end in TValue(canonical (TCollection(t_cr, contained_of t_e)))

        | Range(t_c) ->
            let name = "Range" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let start = t0 <| base_of %++ value_of |> t_erroru name @: TBad(t0) in
            let stride = t1 <| base_of %++ value_of |> t_erroru name @: TBad(t1) in
            let steps = t2 <| base_of %++ value_of |> t_erroru name @: TBad(t2) in
            if not(steps = TInt) 
                then t_erroru name (BTMismatch(TInt, steps,"steps:")) () else
            let t_e = begin
                match (start, stride) with
                | (TInt, TInt) -> TInt
                | (TFloat, TInt)
                | (TInt, TFloat)
                | (TFloat, TFloat) -> TFloat
                | _ -> t_erroru name (TMsg("start and stride types are bad")) ()
            end in TValue(canonical @: TCollection(t_c, TContained(TImmutable (t_e,[]))))

        | (Add|Mult) ->
            let name = match tag with Add -> "Add" | Mult -> "Mult" | _ -> "" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_l = t0 <| base_of %++ value_of |> t_erroru name @: TBad(t0) in
            let t_r = t1 <| base_of %++ value_of |> t_erroru name @: TBad(t1) in
            let result_type = (
                match (t_l, t_r) with
                | (TFloat, TFloat) -> TFloat
                | (TInt, TFloat) -> TFloat
                | (TFloat, TInt) -> TFloat
                | (TInt, TInt) -> TInt
                | (TBool, TBool) -> TBool
                | _ -> t_erroru name (TMsg "Types do not match") ()
            ) in TValue(canonical (result_type))

        | Neg ->
            let name = "Neg" in
            let t0 = bind 0 in
            let t_0 = t0 <| base_of %++ value_of |> t_erroru name @: TBad(t0) in (
                match t_0 with
                | (TBool|TInt|TFloat) as t -> TValue(canonical (t))
                | t-> t_erroru name (BTBad(t)) ()
            )

        | (Eq|Lt|Neq|Leq) ->
            let name = match tag with Eq -> "Eq" | Lt -> "Lt" | Neq -> "Neq" 
                | Leq -> "Leq" | _ -> "" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_l = t0 <| value_of |> t_erroru name @: TBad(t0) in
            let t_r = t1 <| value_of |> t_erroru name @: TBad(t1) in

            (* We can compare any two values whose base types are the same, and *)
            (* are comparable, regardless of if either of them are refs. *)
            if t_l === t_r then TValue(canonical TBool) 
            else t_erroru name (VTMismatch(t_l, t_r, "")) ()

        | IfThenElse ->
            let name = "IfThenElse" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_p = t0 <| value_of |> t_erroru name @: TBad(t0) in
            let t_t = t1 <| value_of |> t_erroru name @: TBad(t1) in
            let t_e = t2 <| value_of |> t_erroru name @: TBad(t2) in
            if canonical TBool === t_p then 
                if t_t === t_e then TValue(t_t) 
                else t_erroru name (VTMismatch(t_t, t_e,"")) ()
            else t_erroru name (VTMismatch(canonical TBool, t_p,"")) ()

        | Block ->
            let name = "Block" in
            let rec validate_block components = (
                match components with
                | e :: [] -> type_of_expr e
                | h :: t when type_of_expr h <| value_of |> 
                    t_erroru name @: TBad(type_of_expr h) === canonical TUnit -> validate_block t
                | _ -> t_erroru name (TMsg("Bad or non-TUnit expression")) ()
            ) in validate_block typed_children

        | Lambda(t_a) ->
            let t0 = bind 0 in
            let t_r = t0 <| value_of |> t_erroru "Lambda" @: TBad(t0)
            in TFunction(value_type_of_arg t_a, t_r)

        | Apply ->
            let name = "Apply" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_e, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_a = t1 <| value_of |> t_erroru name @: TBad(t0) in
            if t_e <~ t_a then TValue(t_r) 
            else t_erroru name (VTMismatch(t_e, t_a,"")) ()

        | Iterate ->
            let name = "Iterate" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_a, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_c, t_e = 
              t1 <| collection_of +++ base_of %++ value_of |> t_erroru name @:
                  TBad(t1) in
            if not (t_r === canonical TUnit) 
                then t_erroru name (VTMismatch(t_r, canonical TUnit, "return val:")) () else
            if t_a <~ t_e then TValue(canonical TUnit) 
            else t_erroru name (VTMismatch(t_a, t_e, "element:")) ()

        | Map ->
            let name = "Map" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_a, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_c, t_e = 
              t1 <| collection_of +++ base_of %++ value_of |> t_erroru name @:
                  TBad(t1) in
            if t_a <~ t_e then TValue(canonical (TCollection(t_c, contained_of t_r)))
            else t_erroru name (VTMismatch(t_a, t_e, "element:")) ()

        | FilterMap ->
            let name = "FilterMap" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_pa, t_pr = t0 <| function_of |> t_erroru name @: TBad t0 in
            let t_ma, t_mr = t1 <| function_of |> t_erroru name @: TBad t0 in
            let t_c, t_e = t2 <| collection_of +++ base_of %++ value_of |>
              t_erroru name @: TBad t2 in

            if not (t_pa <~ t_e) 
                then t_erroru name (VTMismatch(t_pa, t_e,"predicate:")) () else
            if not (canonical TBool === t_pr) 
                then t_erroru name (VTMismatch(canonical TBool, t_pr, "")) () else
            if not (t_ma <~ t_e) then t_erroru name (VTMismatch(t_ma, t_e, "map:")) () else
            TValue(canonical @: TCollection(t_c, contained_of t_mr))

        | Flatten ->
            let name = "Flatten" in
            let t0 = bind 0 in
            let t_c0, t_e0 = 
              t0 <| collection_of +++ base_of %++ value_of |> t_erroru name @:
                  TBad(t0) in
            let t_c1, t_e1 = t_e0 <| collection_of ++% base_of |> t_erroru name
                @: VTBad(t_e0) in
            TValue(canonical (TCollection(t_c0, t_e1)))

        | Aggregate ->
            let name = "Aggregate" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_a, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_z = t1 <| value_of |> t_erroru name @: TBad(t1) in
            let t_c, t_e = t2 <| collection_of +++ base_of %++ value_of |>
                t_erroru name @: TBad(t2) in
            let expected1 = canonical @: TTuple[t_z; t_e] in
            if not (t_a <~ expected1)
                then t_erroru name (VTMismatch(t_a, expected1, "")) () else
            let expected2 = canonical @: TTuple[t_r; t_e] in
            if not (t_a <~ expected2) 
                then t_erroru name (VTMismatch(t_a, expected2, "")) () else 
            TValue(t_z)

        | GroupByAggregate ->
            let name = "GroupByAggregate" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in 
            let t3 = bind 3 in
            let t_ga, t_gr = t0 <| function_of |> t_erroru name @: TBad t0 in
            let t_aa, t_ar = t1 <| function_of |> t_erroru name @: TBad t1 in
            let t_z = t2 <| value_of |> t_erroru name @: TBad t2 in
            let t_c, t_e = t3 <| collection_of +++ base_of %++ value_of |> 
                t_erroru name @: TBad t3 in
            if not (t_ga <~ t_e) then t_erroru name (VTMismatch(t_ga, t_e,
                "grouping func:")) () else
            let expected1 = canonical @: TTuple[t_z; t_e] in
            if not (t_aa <~ expected1) 
                then t_erroru name (VTMismatch(t_aa, expected1, "agg func:")) () else 
            let expected2 = canonical @: TTuple[t_ar; t_e] in
            if not (t_aa <~ expected2) 
                then t_erroru name (VTMismatch(t_aa, expected2, "agg func:")) () 
            else TValue(canonical @: 
                TCollection(t_c, contained_of @: canonical @: TTuple[t_gr; t_ar]))

        | Sort ->
            let name = "Sort" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_c, t_e = t0 <| collection_of +++ base_of %++ value_of |>
                t_erroru name @: TBad t0 in
            let t_ca, t_cr = t1 <| function_of |> t_erroru name @: TBad t1 in

            let expected1 = canonical @: TTuple[t_e; t_e] in
            if not (t_ca <~ expected1) then 
                t_erroru name (VTMismatch(t_ca, expected1, "")) () else
            if not (canonical TBool === t_cr) 
                then t_erroru name (VTMismatch(canonical TBool, t_cr, "")) () else
            TValue(canonical @: TCollection(TList, t_e))

        | Slice ->
            let name = "Slice" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_c, t_e = 
              t0 <| collection_of +++ base_of %++ value_of |> 
                  t_erroru name @: TBad(t0) in
            let t_p = t1 <| value_of |> t_erroru name @: TBad(t1) in
            if t_e === t_p then TValue t_e else begin
                match base_of t_p, base_of t_e with
                | TTuple(t_ps), TTuple(t_es) ->
                    if List.length t_ps = List.length t_es && List.for_all2 
                        (fun tp te -> (canonical TUnknown) === tp || te === tp) 
                        t_ps t_es
                    then t0
                    else t_erroru name (BTMismatch(TTuple(t_ps), TTuple(t_es), "")) ()
                | TUnknown, _ -> TValue (canonical @: TCollection(t_c, t_e))
                | _ -> t_erroru name (VTBad(t_p)) ()
            end

        | Insert ->
            let name = "Insert" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_c, t_e = 
              t0 <| collection_of +++ base_of %++ value_of |> 
                  t_erroru name @: TBad t0 in
            let t_n = t1 <| value_of |> t_erroru name @: TBad t1 in
            if t_e === t_n then TValue (canonical TUnit) 
            else t_erroru name (VTMismatch(t_e, t_n, "")) ()

        | Update ->
            let name = "Update" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_c, t_e = t0 <| collection_of +++ base_of %++ value_of |> 
                t_erroru name @: TBad t0 in
            let t_o = t1 <| value_of |> t_erroru name @: TBad t1 in
            let t_n = t2 <| value_of |> t_erroru name @: TBad t2 in
            if t_e === t_o then 
                if t_e === t_n then TValue(canonical TUnit)
                else t_erroru name (VTMismatch(t_n, t_e, "new value:")) ()
            else t_erroru name (VTMismatch(t_o, t_e, "old value:")) ()

        | Delete ->
            let name = "Delete" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_c, t_e = 
              t0 <| collection_of +++ base_of %++ value_of |> 
              t_erroru name @: TBad t0 in
            let t_n = t1 <| value_of |> t_erroru name @: TBad t1 in
            if t_e === t_n then TValue(canonical TUnit) 
            else t_erroru name (VTMismatch(t_e, t_n, "")) ()

        | Peek ->
            let name = "Peek" in
            let t0 = bind 0 in
            let t_c, t_e = 
              t0 <| collection_of +++ base_of %++ value_of |> 
                  t_erroru name @: TBad t0 in 
            TValue(t_e)

        | Assign ->
            let name = "Assign" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_l = 
              t0 <| dereft +++ mutable_of %++ value_of |> 
                  t_erroru name @: TBad t0 in
            let t_r = t1 <| value_of |> t_erroru name @: TBad t1 in
            if canonical t_l === t_r then TValue(canonical TUnit) else
              t_erroru name (VTMismatch(canonical t_l, t_r, "")) ()

        | Deref ->
            let name = "Deref" in
            let t0 = bind 0 in
            let t_r = t0 <| value_of |> t_erroru name @: TBad t0 in
            let t_u = 
              let d_t mt = mt <| dereft |> t_erroru name @: MTBad mt in
              begin match t_r with
                | TIsolated mt -> TIsolated(TImmutable(d_t mt, []))
                | TContained mt -> TContained(TImmutable(d_t mt, []))
            end in TValue t_u

        | Send ->
            let name = "Send" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_target  = t0 <| base_of %++ value_of |> 
                t_erroru name @: TBad t0 in
            let t_address = t1 <| base_of %++ value_of |> 
                t_erroru name @: TBad t1 in
            let t_args = t2 <| value_of |> 
                t_erroru name @: TBad t2 in
            let t_target_args = begin match t_target with
                | TTarget(t_arg) -> t_arg
                | _ -> t_erroru name (BTBad(t_target)) ()
            end in 
            match t_address with TAddress -> 
                let expected = canonical t_target_args in
                if expected === t_args then TValue(canonical TUnit) 
                else t_erroru name (VTMismatch(t_args, expected, "")) ()
            | _ -> t_erroru name (BTBad(t_address)) ()

    in attach_type current_type

let check_trigger_type trig_env env id args locals body rebuild_f =
  let name = "Trigger("^id^")" in
  let self_bindings = (id, TValue(canonical @: TTarget(base_of @: value_type_of_arg args))) in
  let arg_bindings = gen_arg_bindings args in
  let local_bindings = List.map (fun (i, vt, _) -> (i, TValue(vt))) locals in
  let inner_env = self_bindings :: arg_bindings @ local_bindings @ env in
  let typed_body = deduce_expr_type trig_env inner_env body in
  let t_b =
    type_of_expr typed_body <| value_of |>
      t_error (-1) name @: TBad(type_of_expr typed_body)
  in
  if not (t_b === canonical TUnit) then
    t_error (-1) name (VTMismatch(canonical TUnit, t_b,"")) () 
  else 
    let new_locals = List.map (fun (i,vt,meta) -> (i, vt, (Type(TValue(vt))::meta))) locals
    in ((rebuild_f id args new_locals typed_body), self_bindings :: env)


(* Flow program type deduction *)

let rec types_of_pattern (env:(id_t * type_t list) list) p : type_t list= 
  let rcr = types_of_pattern env in
  let rcr_list l = ListAsSet.no_duplicates (List.flatten (List.map rcr l)) in
  match p with
  | Terminal id  ->
     (try List.assoc id env
      with Not_found -> 
        raise (TypeError(-1, "No resource "^id^" found in pattern")))

  | Choice l     -> rcr_list l
  | Sequence l   -> rcr_list l
  | Optional p   -> rcr p
  | Repeat (p,_) -> rcr p 

let type_of_resource (env:(id_t * type_t list) list) r = match r with
  | Handle(t,_,_) -> [t]
  | Stream(t, ConstStream e) -> 
    let t' = t <| value_of |> t_error (-1) "stream" @: TBad(t) in
    let tcol = type_of_expr @: deduce_expr_type [] [] e in
    let t_c, t_e = tcol <| collection_of +++ base_of %++ value_of |> t_error
      (-1) "stream" @: TBad(t) in
    if not (t' === t_e) 
      then t_error (-1) "stream" (VTMismatch(t', t_e, "resource type")) ()
      else [t]
  | Stream(t, _) -> [t]
  | Pattern p -> types_of_pattern env p

let typecheck_bind src_types trig_arg_types =
  match src_types, trig_arg_types with
  | [], [] -> 
    Some(TMsg("Neither source event nor trigger argument has valid types."))
  
  | [x], [y] when x <> y ->
    Some(TMismatch(x,y,"Resource binding type mismatch."))
    
  | x, _ when List.length x > 1 ->
    Some(TMsg("Multiple resource event types found for dispatch to trigger."))
  
  | _, x when List.length x > 1 ->
    Some(TMsg("Multiple trigger arg types found during bind."))
  
  | [x], [y] when x = y -> None
  
  | x, y -> Some(TMsg("Invalid types."))

let bound_resource_type error_prefix resource_env src_id =
  try List.assoc src_id resource_env
  with Not_found ->
    t_error (-1) error_prefix (TMsg("Could not find resource named "^src_id)) () 

let arg_type_of_trigger error_prefix trig_env trig_id =
  try
    let t = (List.assoc trig_id trig_env) <| base_of %++ value_of |>
      t_error (-1) error_prefix @: (TMsg("Invalid trigger argument type"))
    in
    begin match t with
      | TTarget(arg_t) -> [TValue(canonical arg_t)]
      | _ -> t_error (-1) error_prefix (TMsg("Invalid trigger argument type")) ()
    end
  with Not_found ->
    t_error (-1) error_prefix (TMsg("Could not find trigger named "^trig_id)) ()

let typecheck_flow env trig_env resource_env fp =
  let check_code_type name id args locals body rebuild_f =
    try check_trigger_type trig_env env id args locals body rebuild_f
    with TypeError(ast_id, msg) -> 
      raise (TypeError(ast_id, "In "^name^" "^id^": "^msg))
  in
  List.fold_left (fun (nfp, env) (fs,a) ->
      let nfs, nenv = match fs with
      | Source(Code(id, args, locals, body)) ->
        check_code_type "Generator" id args locals body
          (fun id args locals body -> Source(Code(id, args, locals, body)))

      | Sink(Code(id, args, locals, body)) ->
        check_code_type "Trigger" id args locals body
          (fun id args locals body -> Sink(Code(id, args, locals, body)))
  
      | Bind (src_id, trig_id) ->
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
        let t = TValue(canonical @: TTarget(base_of @: value_type_of_arg args))
        in error_if_dup id [t] env
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
            with TypeError(ast_id, msg) -> raise (TypeError(ast_id, "In Global "^i^": "^msg))
          in
          let expr_type = type_of_expr typed_init in
          if not (compare_type_ts expr_type t) then t_error (-1) i
              (TMismatch(t, expr_type,
                  "Mismatch in global type declaration.")) ()
          else 
          Global(i, t, Some typed_init), (i, expr_type) :: env

        | Global(i, t, None) -> (Global(i, t, None), (i, t) :: env)
        
        | Foreign(i, t) -> (Foreign(i, t), (i, t) :: env)
            
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
  
      in (nprog@[nd, (Type(TValue(canonical TUnit))::meta)]), nenv
    ) ([], []) prog
 in prog, env, trig_env, rresource_env

let deduce_program_type program = 
  ((fun (prog,_,_,_) -> prog) (type_bindings_of_program program))
