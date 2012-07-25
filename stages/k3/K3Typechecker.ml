(* K3 Typechecker *)

open Util
open Tree
open K3
open K3Util

(* TODO: Make exceptions more informative for error reporting. *)
exception MalformedTree
(* uuid, location in typechecker, compared types *)
exception TypeError of int * string

type error_type =
    | TMismatch of type_t * type_t
    | VTMismatch of value_type_t * value_type_t
    | BTMismatch of base_type_t * base_type_t
    | TBad of type_t
    | VTBad of value_type_t
    | BTBad of base_type_t
    | MTBad of mutable_type_t
    | TMsg of string

let t_error uuid name msg () = 
    let extra = match msg with
        | TMismatch(t1,t2)  -> "This expression has type "^string_of_type t1^
            "\nBut an expression was expected of type "^string_of_type t2
        | VTMismatch(t1, t2) -> "This expression has type "^
            string_of_value_type t1^"\nBut an expression was expected of type "^
            string_of_value_type t2
        | BTMismatch(t1, t2) -> "This expression has type "^
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
    let correct_arity =
        match tag with
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

        | Slice(_)  -> 2
        | Insert    -> 2
        | Delete    -> 2
        | Update    -> 3
        | Peek      -> 1

        | Assign -> 2
        | Deref -> 1

        | Send -> 3
    in length = correct_arity

type 'a texpr_t = (type_t * 'a) expr_t
type 'a tprogram_t = (type_t * 'a) program_t

let type_of texpr = let (_, (t, _)), _ = decompose_tree texpr in t

let (<|) x f = f x
and (|>) f y = f y

let (+++) f g = fun t x -> f (g t x) x
let (++%) f g = fun t x -> f (g t) x
let (%++) f g = fun t x -> f (g t x)

let canonical bt = TIsolated(TImmutable(bt))

let value_of t x =
    match t with
    | TValue(vt) -> vt
    | _ -> x ()

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
    | TMutable(bt) -> bt
    | _ -> x (); dummy

let base_of vt =
    match vt with
    | TIsolated(TImmutable(bt))
    | TIsolated(TMutable(bt))
    | TContained(TImmutable(bt))
    | TContained(TMutable(bt)) -> bt

let rec contained_of vt =
    let inner_base_type = base_of vt in
    let convertable_type = (
        match inner_base_type with
        | TTuple(ts) -> TTuple(List.map contained_of ts)
        | TCollection(t_c, t_e) -> TCollection(t_c, contained_of t_e)
        | TMaybe(t_m) -> TMaybe(contained_of t_m)
        | _ -> inner_base_type
    ) in TContained(TImmutable(convertable_type))

let rec assignable t_l t_r =
    let t_lb = base_of t_l in
    let t_rb = base_of t_r in
    match (t_lb, t_rb) with
    | TMaybe(_), TMaybe(TIsolated(TImmutable(TUnknown)))-> true
    | TMaybe(t_lm), TMaybe(t_rm) -> assignable t_lm t_rm
    | TTuple(t_ls), TTuple(t_rs) -> List.length t_ls = List.length t_rs && 
        List.for_all2 assignable t_ls t_rs
    | TCollection(t_lc, t_le), TCollection(t_rc, t_re) -> assignable t_le t_re
    | _ when t_lb = t_rb -> true
    | _ -> false

let (===) = assignable

let rec passable t_l t_r =
    match t_l, t_r with
    | TContained(TMutable(_)), TContained(TMutable(_)) -> assignable t_l t_r
    | TContained(TMutable(_)), _ -> false
    | _ -> assignable t_l t_r

let (<~) = passable

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

let deduce_arg_type a =
    match a with
    | AVar(i, t) -> t
    | ATuple(its) -> canonical (TTuple(snd(List.split its)))

let rec deduce_expr_type trig_env cur_env utexpr =
    let ((uuid, tag), aux), untyped_children = decompose_tree utexpr in
    let t_erroru = t_error uuid in (* pre-curry the type error *)

    (* Check Tag Arity *)
    if not (check_tag_arity tag untyped_children) then raise MalformedTree else

    (* Determine if the environment to be passed down to child typechecking needs to be augmented. *)
    let env =
        match tag with
        | Lambda(AVar(i, t)) -> (i, TValue(t)) :: cur_env
        | Lambda(ATuple(its)) -> (List.map (fun (i, t) -> (i, TValue(t))) its) @ cur_env
        | _ -> cur_env
    in

    let typed_children = List.map (deduce_expr_type trig_env env) untyped_children in
    let attach_type t = mk_tree (((uuid, tag), (t, aux)), typed_children) in
    let bind n = type_of (List.nth typed_children n) in

    let current_type =
        match tag with
        | Const(c) -> TValue(deduce_constant_type uuid trig_env c)
        | Var(id) -> begin try List.assoc id env 
            with Not_found -> t_erroru "Var" (TMsg(id^" not found")) () end
        | Tuple ->
            let child_types = List.map 
            (fun e -> 
                type_of e <| value_of |> t_erroru "Tuple" (TBad(type_of e)))
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
            if t_e0 <> t_e1 then t_erroru name (VTMismatch(t_e0, t_e1)) () else

            (* Determine combined collection type. *)
            let t_cr = (
                match (t_c0, t_c1) with
                    | (TList, _)    -> TList
                    | (_, TList)    -> TList
                    | (TBag, _)     -> TBag
                    | (_, TBag)     -> TBag
                    | (TSet, TSet)  -> TSet
            ) in TValue(canonical (TCollection(t_cr, contained_of t_e0)))

        | Range(t_c) ->
            let name = "Range" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let start = t0 <| base_of %++ value_of |> t_erroru name @: TBad(t0) in
            let stride = t1 <| base_of %++ value_of |> t_erroru name @: TBad(t1) in
            let steps = t2 <| base_of %++ value_of |> t_erroru name @: TBad(t2) in
            if not(steps = TInt) 
                then t_erroru name (BTMismatch(TInt, steps)) () else
            let t_e = begin
                match (start, stride) with
                | (TInt, TInt) -> TInt
                | (TFloat, TInt)
                | (TInt, TFloat)
                | (TFloat, TFloat) -> TFloat
                | _ -> t_erroru name (TMsg("start and stride types are bad")) ()
            end in TValue(canonical @: TCollection(t_c, TContained(TImmutable t_e)))

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
            let t_l = t0 <| base_of %++ value_of |> t_erroru name @: TBad(t0) in
            let t_r = t1 <| base_of %++ value_of |> t_erroru name @: TBad(t1) in

            (* We can compare any two values whose base types are the same, and *)
            (* are comparable, regardless of if either of them are refs. *)
            if t_l = t_r then TValue(canonical TBool) 
            else t_erroru name (BTMismatch(t_l, t_r)) ()

        | IfThenElse ->
            let name = "IfThenElse" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_p = t0 <| value_of |> t_erroru name @: TBad(t0) in
            let t_t = t1 <| value_of |> t_erroru name @: TBad(t1) in
            let t_e = t2 <| value_of |> t_erroru name @: TBad(t2) in
            if canonical TBool === t_p then 
                if t_t === t_e then TValue(t_t) 
                else t_erroru name (VTMismatch(t_t, t_e)) ()
            else t_erroru name (VTMismatch(canonical TBool, t_p)) ()

        | Block ->
            let name = "Block" in
            let rec validate_block components = (
                match components with
                | e :: [] -> type_of e
                | h :: t when type_of h <| value_of |> 
                    t_erroru name @: TBad(type_of h) === canonical TUnit -> validate_block t
                | _ -> t_erroru name (TMsg("Bad or non-TUnit expression")) ()
            ) in validate_block typed_children

        | Lambda(t_a) ->
            let t0 = bind 0 in
            let t_r = t0 <| value_of |> t_erroru "Lambda" @: TBad(t0)
            in TFunction(deduce_arg_type t_a, t_r)

        | Apply ->
            let name = "Apply" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_e, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_a = t1 <| value_of |> t_erroru name @: TBad(t0) in
            if t_e <~ t_a then TValue(t_r) 
            else t_erroru name (VTMismatch(t_e, t_a)) ()

        | Iterate ->
            let name = "Iterate" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_a, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_c, t_e = 
              t1 <| collection_of +++ base_of %++ value_of |> t_erroru name @:
                  TBad(t1) in
            if not (t_r === canonical TUnit) 
                then t_erroru name (VTMismatch(canonical TUnit, t_r)) () else
            if t_a <~ t_e then TValue(canonical TUnit) 
            else t_erroru name (VTMismatch(t_a, t_e)) ()

        | Map ->
            let name = "Map" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_a, t_r = t0 <| function_of |> t_erroru name @: TBad(t0) in
            let t_c, t_e = 
              t1 <| collection_of +++ base_of %++ value_of |> t_erroru name @:
                  TBad(t1) in
            if t_a <~ t_e then TValue(canonical (TCollection(t_c, contained_of t_r)))
            else t_erroru name (VTMismatch(t_a, t_e)) ()

        | FilterMap ->
            let name = "FilterMap" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_pa, t_pr = t0 <| function_of |> t_erroru name @: TBad t0 in
            let t_ma, t_mr = t1 <| function_of |> t_erroru name @: TBad t0 in
            let t_c, t_e = t2 <| collection_of +++ base_of %++ value_of |>
              t_erroru name @: TBad t2 in

            if not (t_pa <~ t_e) then t_erroru name (VTMismatch(t_pa, t_e)) () else
            if not (canonical TBool === t_pr) 
                then t_erroru name (VTMismatch(canonical TBool, t_pr)) () else
            if not (t_ma <~ t_e) then t_erroru name (VTMismatch(t_ma, t_e)) () else
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
                then t_erroru name (VTMismatch(t_a, expected1)) () else
            let expected2 = canonical @: TTuple[t_r; t_e] in
            if not (t_a <~ expected2) 
                then t_erroru name (VTMismatch(t_a, expected2)) () else 
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
            if not (t_ga <~ t_e) then t_erroru name (VTMismatch(t_ga, t_e)) () else
            let expected1 = canonical @: TTuple[t_z; t_e] in
            if not (t_aa <~ expected1) 
                then t_erroru name (VTMismatch(t_aa, expected1)) () else 
            let expected2 = canonical @: TTuple[t_ar; t_e] in
            if not (t_aa <~ expected2) 
                then t_erroru name (VTMismatch(t_aa, expected2)) () 
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
                t_erroru name (VTMismatch(t_ca, expected1)) () else
            if not (canonical TBool === t_cr) 
                then t_erroru name (VTMismatch(canonical TBool, t_cr)) () else
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
                    if List.for_all2 (
                        fun tp te -> (canonical TUnknown) === tp || te === tp
                        ) t_ps t_es
                    then t0
                    else t_erroru name (BTMismatch(TTuple(t_ps), TTuple(t_es))) ()
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
            else t_erroru name (VTMismatch(t_e, t_n)) ()

        | Update ->
            let name = "Update" in
            let t0 = bind 0 in let t1 = bind 1 in let t2 = bind 2 in
            let t_c, t_e = t0 <| collection_of +++ base_of %++ value_of |> 
                t_erroru name @: TBad t0 in
            let t_o = t1 <| value_of |> t_erroru name @: TBad t1 in
            let t_n = t2 <| value_of |> t_erroru name @: TBad t2 in
            if t_e === t_o then 
                if t_e === t_n then TValue(canonical TUnit)
                else t_erroru name (VTMismatch(t_e, t_n)) ()
            else t_erroru name (VTMismatch(t_e, t_o)) ()

        | Delete ->
            let name = "Delete" in
            let t0 = bind 0 in let t1 = bind 1 in
            let t_c, t_e = 
              t0 <| collection_of +++ base_of %++ value_of |> 
              t_erroru name @: TBad t0 in
            let t_n = t1 <| value_of |> t_erroru name @: TBad t1 in
            if t_e === t_n then TValue(canonical TUnit) 
            else t_erroru name (VTMismatch(t_e, t_n)) ()

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
              t_erroru name (VTMismatch(canonical t_l, t_r)) ()

        | Deref ->
            let name = "Deref" in
            let t0 = bind 0 in
            let t_r = t0 <| value_of |> t_erroru name @: TBad t0 in
            let t_u = begin match t_r with
                | TIsolated mt -> 
                    TIsolated(TImmutable(mt <| dereft |> 
                    t_erroru name @: MTBad mt))
                | TContained mt -> 
                    TContained(TImmutable(mt <| dereft |> 
                    t_erroru name @: MTBad mt))
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
                else t_erroru name (VTMismatch(expected, t_args)) ()
            | _ -> t_erroru name (BTBad(t_address)) ()

    in attach_type current_type

let deduce_program_type program = 
  let rec build_trig_env trig_env prog = match prog with
    [] -> trig_env
    | Declaration (Trigger(id, args, locals, body)) :: ss ->
        let t = TValue(canonical @: TTarget(base_of @: deduce_arg_type args))
        in build_trig_env ((id, t)::trig_env) ss
    | _ :: ss -> build_trig_env trig_env ss
  in
  let rec deduce_prog_t trig_env env prog = match prog with 
    [] -> [] 
    | Instruction(i) :: ss -> deduce_prog_t trig_env env ss
    | Declaration(d) :: ss -> 
        let nd, nenv = begin match d with
        | Global(i, t, Some init) ->
            let typed_init = deduce_expr_type trig_env env init
            in (Global(i, t, Some typed_init), (i, type_of typed_init) :: env)
        | Global(i, t, None) -> (Global(i, t, None), (i, t) :: env)
        | Foreign(i, t) -> (Foreign(i, t), (i, t) :: env)
        | Trigger(id, args, locals, body) ->
            let name = "Trigger("^id^")" in
            let self_bindings = (id, 
            TValue(canonical @: TTarget(base_of @: deduce_arg_type args))) in
            let arg_bindings = (
                match args with
                | AVar(i, t) -> [(i, TValue(t))]
                | ATuple(its) -> List.map (fun (i, t) -> (i, TValue(t))) its
            ) in
            let local_bindings = List.map (fun (i, vt) -> (i, TValue(vt))) locals in
            let inner_env = self_bindings :: arg_bindings @ local_bindings @ env in
            let typed_body = deduce_expr_type trig_env inner_env body in
            let t_b = type_of typed_body <| value_of |> t_error (-1) name @:
                TBad(type_of typed_body) in
            if not (t_b === canonical TUnit)
                then t_error (-1) name (VTMismatch(canonical TUnit, t_b)) () 
            else (Trigger(id, args, locals, typed_body), self_bindings :: env)
        end in
        Declaration(nd) :: deduce_prog_t trig_env nenv ss
  in
  (* do a first pass, collecting trigger types *)
  let trig_env = build_trig_env [] program in
  deduce_prog_t trig_env [] program

