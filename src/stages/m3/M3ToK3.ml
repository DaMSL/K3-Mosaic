(** Module for translating a M3 program into a K3 program. *)

(**/**)
open Util
open Arithmetic
open Calculus

module T = M3Type
module Const = M3Constants
module V = Arithmetic.ValueRing
module C = Calculus.CalcRing
module K = K3.AST
module KP = K3Printing
module KS = K3PrintSyntax
module KH = K3Helpers
module KU = K3Util
module KT = K3Typechecker
module K3N = K3NewPrint

let wrap_map  = KH.wrap_tbag
let wrap_map' = KH.wrap_tbag'

exception M3ToK3Failure of
  Calculus.expr_t option *
  K.expr_t option *
  string

let error ?(m3 = None) ?(k3 = None) msg =
  raise (M3ToK3Failure(m3, k3, msg))

let m3_type_to_k3_base_type = function
  | T.TBool            -> K.TBool
  | T.TInt             -> K.TInt
  | T.TFloat           -> K.TFloat
  | T.TString          -> K.TString
  | T.TDate            -> K.TDate
  | T.TAny             -> failwith "Expecting M3 to be fully typed"
  | T.TExternal(ext_t) -> failwith "External types unsupported"

let mvar_btypes = List.map (m3_type_to_k3_base_type |- snd)

let m3_type_to_k3_type (t:T.type_t) =
  KH.canonical (m3_type_to_k3_base_type t)

let init_val_from_type t =
begin match t.K.typ with
  | K.TInt   -> KH.mk_const @@ K.CInt 0
  | K.TFloat -> KH.mk_const @@ K.CFloat(0.)
  | _        -> failwith "Map type with no initial value"
end

let mk_k3_collection (base_ivars:K.base_type_t list)
                     (base_ovars:K.base_type_t list)
                     (base_v:K.base_type_t) =
  let canon = KH.canonical in
  let ivars = List.map canon base_ivars in
  let ovars = List.map canon base_ovars in
  let v = canon base_v in
  let wrap = wrap_map' in
  wrap @@
    match ivars, ovars with
    | [], [] -> [v]
    | [], _  -> ovars @ [v]
    | _,  [] -> ivars @ [v]
    | _,  _  -> ivars @ [wrap @@ ovars@[v]]

(* we create a 0 initial value only if we have no ivars or ovars -- why? *)
let m3_map_to_k3_map (m3_map: M3.map_t) : K.declaration_t =
  let coll_name, coll_type, coll_ivc = match m3_map with
    | M3.DSView(ds) ->
        let map_name, input_vars, output_vars, map_type, _ =
          Plan.expand_ds_name ds.Plan.ds_name
        in
        let element_type = m3_type_to_k3_base_type map_type in
        let ivar_types = mvar_btypes input_vars in
        let ovar_types = mvar_btypes output_vars in
        (* initial value *)
        let ivc = if null ivar_types && null ovar_types then
          Some(KH.mk_singleton
            (wrap_map @@ KH.canonical element_type) @@
              [init_val_from_type @@ KH.canonical element_type])
          else None
        in
        map_name,
        mk_k3_collection ivar_types ovar_types element_type,
        ivc

    | M3.DSTable(rel_name, rel_schema, _) -> (* static base relation *)
        rel_name,
        mk_k3_collection [] (mvar_btypes rel_schema) K.TInt,
        None
  in
  K.Global(coll_name, coll_type, coll_ivc)

(**/**)

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**/**)

let init_val_from_full_type t = init_val_from_type @@ t

let base_of_any t = t.K.typ

let compatible_types t1 t2 = match base_of_any t1, base_of_any t2 with
| K.TInt,   K.TBool
| K.TBool,  K.TInt
| K.TFloat, K.TBool
| K.TBool,  K.TFloat
| K.TInt,   K.TFloat
| K.TFloat, K.TInt   -> true
| _,        _        -> t1 = t2

let numerical_type t = match base_of_any t with
| K.TBool
| K.TInt
| K.TFloat -> true
| _        -> false

let arithmetic_return_types ?(expr=None) t1 t2 =
match base_of_any t1, base_of_any t2 with
| K.TBool,  K.TBool
| K.TBool,  K.TInt
| K.TInt,   K.TInt
| K.TInt,   K.TBool  -> KH.t_int
| K.TBool,  K.TFloat
| K.TInt,   K.TFloat
| K.TFloat, K.TFloat
| K.TFloat, K.TBool
| K.TFloat, K.TInt   -> KH.t_float
| _, _ -> error ~m3:expr "M3ToK3: arguments must be of numerical type."

let escalate_type ?(expr=None) from_t to_t =
match base_of_any from_t, base_of_any to_t with
| K.TBool, K.TInt
| K.TInt,  K.TInt   -> KH.t_int
| K.TBool, K.TFloat
| K.TInt,  K.TFloat
| K.TFloat, K.TFloat -> KH.t_float
| _, _              ->
    if from_t = to_t then to_t
    else error ~m3:expr ("M3ToK3: Unable to escalate type " ^
                        KP.string_of_type from_t ^
                        " to type " ^ KP.string_of_type to_t)

let escalate_expr to_t expr = expr
(*
print_endline ("Escalating "^(KP.string_of_expr expr)^" to "^(KP.string_of_type to_t));
begin match ((K3Typechecker.type_of_expr expr), to_t) with
  | (K.TValue(a), K.TValue(b))
        when   ((a = KH.t_int)   || (a = KH.t_int_mut))
            && ((b = KH.t_float) || (b = KH.t_float_mut)) ->
      KH.mk_mult (KH.mk_const (K.CFloat(1.0))) expr
  | (K.TValue(a), K.TValue(b)) when a = b -> expr
  | _ -> failwith ("Can not escalate "^(KP.string_of_expr expr)^" to "^
                    (KP.string_of_type to_t))
end
*)

let m3_const_to_k3_const = function
| Const.CInt i       -> KH.mk_const @@ K.CInt i
| Const.CFloat f     -> KH.mk_const @@ K.CFloat f
| Const.CString s    -> KH.mk_const @@ K.CString s
| Const.CBool b      -> KH.mk_const @@ K.CBool b
| Const.CDate(y, m, d) -> KH.mk_const @@ K.CInt(y * 10000 + m * 100 + d)

let zero_of_type = function
| K.TInt   -> K.CInt(0)
| K.TFloat -> K.CFloat(0.)
| t        -> failwith @@ "Can not generate zero for type: "^
                KP.string_of_base_type t

(**/**)

(**********************************************************************)
(**/**)

let keys_from_kvars kvars  = List.map (fun kv -> KU.id_of_var kv, kv) kvars

(**/**)

(**[assoc_lambda v1_el v2_el body]

  Constructs an associative lambda expression from the expression [body]
  with the variables in [v1_el] and [v1_el].
  @param v1_el The first list of K3 variables of the [body].
  @param v2_el The second list of K3 variables of the [body].
  @param body  The body of the associative lambda expression.
  @return      The K3 associative lambda expression expecting [v1_el] and
              [v2_el] as arguments and computing [body]. *)
let assoc_lambda v1_el v2_el body =
  KH.mk_assoc_lambda (KH.wrap_args v1_el) (KH.wrap_args v2_el) body

(**[apply_lambda v_el el body]

  Evaluates the K3 expression [body] by binding the list of expressions
  in [el] to the list of K3 variables in [v_el]. *)
let apply_lambda v_el el body =
  if List.length v_el != List.length el then
    error ("M3ToK3: Applying lambda to expression with " ^
            "different size schema!");
  KH.mk_let (fst_many v_el) (KH.mk_tuple el) body

(**[apply_lambda_to_expr lambda_e expr]

  Applies the lambda expression [lambda_e] to the contents of [expr].
  [expr] must be either a singleton expression or a K3 collection
  (ie. a mapping from keys to values, no collection with just values
  are allowed). If [expr] is singleton and lambda_e outputs a tuple the
  result is wrapped in a Singleton collection. *)
let apply_lambda_to_expr lambda_e lambda_t expr =
  let _, lambda_body = KU.decompose_lambda lambda_e in
  match KU.arg_of_lambda lambda_e, KU.tag_of_expr lambda_body with
  | Some(K.AVar(id,_)), K.Tuple ->
        KH.mk_singleton (wrap_map lambda_t) @@
          [KH.mk_let [id] expr lambda_body]
  | Some(K.AVar(id,_)), _       -> KH.mk_let [id] expr lambda_body
  | Some(K.ATuple _), _         -> KH.mk_map lambda_e expr
  | _ -> error "M3ToK3: Invalid arguments to apply_lambda_to_expr."

(**[project_fn from_v_el to_v_el]

  Projects the list of variables [from_v_el] to the list of variables
  [to_v_el]. When maped over a collection it will project away keys that
  are not present in [to_v_el]. *)
let project_fn (from_v_el:(K.id_t * K.type_t) list)
              (to_v_el:(K.id_t * K.type_t) list) =
  KH.mk_lambda' from_v_el @@ KH.mk_tuple @@ List.map (KH.mk_var |- fst) to_v_el

let gen_accum_var = FreshVariable.declare_class "functional/M3ToK3" "accv"

(**[aggregate_fn v_el multpl_e]

  Constructs an aggregation function.
  @param v_el     The list of K3 variables of the collection being aggregated.
  @param multpl_e Variable expression used for accessing the values associated
                  with each tuple in the aggregated collection. *)
let aggregate_fn v_el multpl_e =
  let accv = gen_accum_var () in
  let multpl_n, multpl_t = multpl_e in
  assoc_lambda [accv, multpl_t] (v_el@[multpl_n, multpl_t]) @@
              KH.mk_add (KH.mk_var accv) (KH.mk_var multpl_n)

(**[external_KH.wrap_args fn t_l]

  Converts a list of K3 types into K3 external lambda arguments.
  @param fn   The name of the external function, used when naming
              the arguments.
  @param t_l  The list of K3 types to be converted.
  @return     The K3 lambda argument corresponding to [t_l] *)
let external_lambda_args fn t_l =
  KH.wrap_args @@ KH.types_to_ids_types ~first:1 (fn^"_arg_") t_l

(**[external_lambda fn t_l ftype]

  Constructs an external lambda expression for the external function [fn]
  with arguments of types corresponding to [t_l] and return type ftype.
  @param fn    The name of the external function.
  @param t_l   The list of K3 types used for typing the arguments.
  @param ftype The return type of the function. *)
let external_lambda fn t_l ftype =
  failwith "TODO: Add support for external lambdas"

(**[apply_external_lambda fn te_l ftype]

  Evaluates the external function [fn] by binding its arguments to
  the expressions in [te_l]. *)
let apply_external_lambda fn in_x_ts out_t =
  let fname = match fn, List.map (fun (x,_) -> x.K.typ) in_x_ts, out_t.K.typ with
    | "/", [K.TFloat; K.TFloat], K.TFloat -> "divf"
    | "/", [K.TInt], K.TFloat             -> "reciprocali"
    | "/", [K.TFloat], K.TFloat           -> "reciprocal"
    | "listmax", [K.TInt; K.TInt], K.TInt -> "maxi"
    | "listmax", [K.TInt; K.TFloat], K.TFloat -> "maxif"
    | "regexp_match", [K.TString; K.TString], K.TInt -> "regex_match_int"
    | "substring", [K.TString; K.TInt; K.TInt], K.TString -> "substring"
    | "date_part", [K.TString; K.TDate], K.TInt -> "date_part"
    | _ -> failwith @@ sp "Unsupported extern lambda: %s: %s -> %s"
             fn (KS.string_of_type @@ KH.wrap_ttuple @@ fst_many @@ in_x_ts)
                (KS.string_of_type out_t)
  in
  KH.mk_apply (KH.mk_var fname) (snd_many in_x_ts)

let mk_project ?(id="projected_field") width idx ret_t expr =
  let rec build_tuple w =
    if w >= width then []
    else
      (if w = idx then id else "_") :: (build_tuple @@ w+1)
  in
  KH.mk_let (build_tuple 0) expr @@ KH.mk_var id

(* translate a slice using the names of the bound keys and the names
 * of all keys, to figure out which should be 'unknown' *)
let mk_slice collection all_keys bound_keys =
  KH.mk_slice collection @@
    List.map (fun x ->
        if List.mem x bound_keys then KH.mk_var x
        else KH.mk_cunknown)
      all_keys
    @ [KH.mk_cunknown]

let mk_lookup collection bag_t keys key_types =
  KH.mk_case_ns
    (KH.mk_peek @@ mk_slice collection keys keys)
    "x"
    (KH.mk_const @@ zero_of_type bag_t) @@
    KH.mk_subscript (List.length keys + 1) (KH.mk_var "x")

let mk_test_member collection keys key_types val_type =
  KH.mk_has_member collection
    (KH.ids_to_vars @@ keys @ ["_"])
    (KH.wrap_ttuple_mut (key_types @ [val_type]))

let mk_arg x xt = K.AVar(x, KH.canonical xt)

let mk_tuple_arg keys keys_tl v vt = K.ATuple(List.map2 mk_arg keys keys_tl @ [mk_arg v vt])

let mk_lambda keys keys_tl v vt body = KH.mk_lambda (mk_tuple_arg keys keys_tl v vt) body;;

let mk_var_tuple keys v = KH.ids_to_vars (keys@[v])

let mk_val_tuple keys v = (KH.ids_to_vars keys)@[v]

let mk_update col bag_t ivars ivar_t ovars ovar_t new_val =
(* new_val might (and in fact, usually will) depend on the col, so
    we need to evaluate it and save it to a variable before clearing the
    existing elements out of the col *)
  let new_val_var = KH.mk_var "update_value" in
  let update_block = match ivars, ovars with
    | [], [] ->
        KH.mk_block [
          KH.mk_iter
            (KH.mk_lambda' ["value", KH.canonical bag_t] @@
              KH.mk_delete col [KH.mk_var "value"]) @@
            KH.mk_slice' col [KH.mk_cunknown];
          KH.mk_insert col (mk_val_tuple [] new_val_var)
        ]
    | [], _  ->
        KH.mk_block [
          KH.mk_iter
            (KH.mk_lambda' ((list_zip ovars @@ List.map KH.canonical ovar_t) @ ["value", KH.canonical bag_t]) @@
              KH.mk_delete col (mk_var_tuple ovars "value")) @@
            mk_slice (KH.mk_var col) ovars ovars;
          KH.mk_insert col (mk_val_tuple ovars new_val_var)
        ]
    | _, []  ->
        KH.mk_block [
          KH.mk_iter
            (KH.mk_lambda' ((list_zip ivars @@ List.map KH.canonical ivar_t) @ ["value", KH.canonical bag_t]) @@
              KH.mk_delete col (mk_var_tuple ivars "value")) @@
            mk_slice (KH.mk_var col) ivars ivars;
          KH.mk_insert col (mk_val_tuple ivars new_val_var)
        ]
    | _      -> failwith "FullPC unsupported"
  in
  KH.mk_let [KU.id_of_var new_val_var] new_val update_block

(**********************************************************************)
(**/**)
(* Utility function for generating map access expression. *)
let gen_map_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "map_ret_"

let gen_slice_sym =
  FreshVariable.declare_class "functional/M3ToK3" "slice_"

let gen_init_val_sym =
  FreshVariable.declare_class "functional/M3ToK3" "init_val_"

let gen_val_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "val_ret_"

let gen_cmp_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "cmp_ret_"

let gen_lift_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "lift_ret_"

let gen_exists_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "exists_ret_"

let gen_sum_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "sum_ret_"

let gen_prod_ret_sym =
  FreshVariable.declare_class "functional/M3ToK3" "prod_ret_"



(**/**)
(**********************************************************************)

let abc_str = "abcdefghijklmnopqrstuvwxyz"
(**[map_access_to_expr mapn resultn ins outs map_ret_t theta_vars_el init_expr_opt]

  Generates a K3 expression for accessing a map.
  @param mapn          The name of the map being accessed.
  @param resultn       The name of the result map for which this map is accessed.
  @param ins           The input variables of the map access.
  @param outs          The output variables of the map access.
  @param map_ret_t     The type of the value associated with each tuple of
                      the map.
  @param theta_vars_el The variables that are in scope when accessing the map.
  @param init_expr_opt The expression that should be used for initialization.
                      It is [None] if no expression is provided.
  @return              A tuple consisting of :
                      - the schema of the collection resulting from
                        the map access,
                      - a variable that can be used for binding against
                        the value
                        associated with each element of the map,
                      - the K3 expression for accessing the map. *)
let map_access_to_expr mapn resultn ins outs map_ret_t theta_vars_k init_expr_opt =

    let ins_k:K.id_t list  = fst_many ins in
    let outs_k:K.id_t list = fst_many outs in
    let free_vars_k    = ListAsSet.diff outs_k theta_vars_k in
    let bound_vars_k   = ListAsSet.diff outs_k free_vars_k in

    let ins_tl         = List.map m3_type_to_k3_base_type @@ snd_many ins in
    let outs_tl        = List.map m3_type_to_k3_base_type @@ snd_many outs in
    (* K3 type of the value associated with each tuple in the map *)
    let map_ret_kt = m3_type_to_k3_type map_ret_t in
    (* K3 type of the out tier of the map *)
    let map_out_t:K.type_t =
        if outs_tl = [] then m3_type_to_k3_type map_ret_t
        else
          mk_k3_collection [] outs_tl @@ m3_type_to_k3_base_type map_ret_t
    in

    let map_ret_ve = KH.mk_var @@ gen_map_ret_sym () in
    let map_out_ve = KH.mk_var @@ gen_slice_sym () in

    let type_map = List.combine ins_k ins_tl @
                    List.combine outs_k outs_tl in
    let typed_var_pair v = v, KH.canonical @@ List.assoc v type_map in

    (* Given a collection it slices it according to the bound variables *)
    (* and projects only the free variables *)
    let slice_and_project coll_ve : K.expr_t =
        if free_vars_k = [] then
          error ("M3ToK3: We shouldn't slice if all variables are bound."^
                  " We should Lookup instead.");
        if free_vars_k = outs_k
        then KU.add_property "BaseRelation" coll_ve
        else
          let record_id_of_num i =
            let rec s_of_i acc i =
              if i <= String.length abc_str then
                sp "%s%c" acc (abc_str.[i-1])
              else
                s_of_i (acc ^ "z") (i-String.length abc_str)
            in
            "r" ^ (s_of_i "" i)
          in
          let add_record_ids l =
            match l with
            | []    -> failwith "No list to add record ids to"
            | [x]   -> ["elem", x]
            | [x;y] -> ["key", x; "value", y]
            | _     ->
              let i_l = insert_index_fst ~first:1 l in
              List.map (fun (i, x) -> record_id_of_num i, x) i_l
          in
          let stringify_record l = "{" ^ (String.concat "," @@ List.map (fun (s,t) -> s^": "^t) l) ^ "}" in
          let singleton_record l =
            match l with
            | [] -> failwith "Invalid record wrapping"
            | [x,y] -> y
            | _ -> stringify_record l
          in
          let mk_slice_key all_vars bound_vars =
            singleton_record
              @@ List.fold_left (fun acc (s, (t,u)) -> if t then acc @ [s, "t."^s] else acc) []
                    @@ add_record_ids @@ List.map (fun x -> List.mem x bound_vars, x) all_vars
          in
          let mk_slice_prj all_vars free_vars extra_vars =
            singleton_record @@ add_record_ids
              @@ List.fold_left (fun acc (s, (t,u)) -> if t then acc @ ["t."^s] else acc) []
                  @@ add_record_ids ((List.map (fun x -> List.mem x free_vars, x) all_vars) @ extra_vars)
          in
          let new_key = mk_slice_key outs_k bound_vars_k in
          let lkp_probe_key = stringify_record ["key", singleton_record @@ add_record_ids bound_vars_k] in
          let idx_probe_key = stringify_record ["key", new_key] in
          let prj_expr = List.map typed_var_pair free_vars_k @ [KU.id_of_var map_ret_ve, map_ret_kt] in
          let prj_types = (List.map (fun v -> K3N.string_of_base_type @@ KH.canonical @@ List.assoc v type_map) free_vars_k) @
                            [K3N.string_of_base_type map_ret_kt]
          in
          let prj_vars = mk_slice_prj outs_k free_vars_k [true, KU.id_of_var map_ret_ve] in
          let idx_key_type =
            add_record_ids @@
              List.map (fun v -> K3N.string_of_base_type @@ KH.canonical @@ List.assoc v type_map) bound_vars_k
          in
          let key_type = singleton_record idx_key_type in
          let val_type = singleton_record @@ add_record_ids prj_types in
          KU.add_annotation
            (sp ("MosaicIndex(lbl=[# %s], relation=[# %s], key_type=[: %s], lookup_probe=[$ %s], index_probe=[$ %s], missing_fn=[$ (\\_ -> {key: %s, value: empty %s @Collection})], present_fn=[$ (\\acc -> ((acc.value.insert %s); acc)) ], index_key=[:> key=>%s], index_value=[:> value=>collection %s @Collection])")
                  ((KU.id_of_var coll_ve)^"_"^resultn) mapn key_type lkp_probe_key idx_probe_key new_key val_type prj_vars key_type val_type)
            (KH.mk_map
               (project_fn
                 (List.map typed_var_pair outs_k @ [KU.id_of_var map_ret_ve, map_ret_kt]) @@ prj_expr) @@
               mk_slice (KU.add_property "BaseRelation" coll_ve) outs_k bound_vars_k)
    in
    let map_expr = KH.mk_var mapn in

    let expr = match ins, outs with
      | [], [] ->
          (* No need to perform initial value computation. This should have *)
          (* already been initialized at system start-up. *)
          let zero = KH.default_value_of_t map_ret_kt in
          KH.mk_peek_or_zero ~zero @@
            KH.mk_var mapn

      | [], y -> begin match free_vars_k, init_expr_opt with
          | [], None ->
              mk_lookup map_expr (map_ret_kt.K.typ) outs_k outs_tl
          | [], Some (_, init_expr) ->
                KH.mk_if
                  (mk_test_member map_expr outs_k
                    (List.map KH.canonical outs_tl)
                    map_ret_kt)
                  (mk_lookup map_expr (map_ret_kt.K.typ) outs_k outs_tl)
                  init_expr
          | _ -> slice_and_project map_expr
          end

      | x, [] -> begin match init_expr_opt with
          | None -> mk_lookup map_expr (map_ret_kt.K.typ) outs_k outs_tl
          | Some (_, init_expr) ->
              let iv_e = KH.mk_var (gen_init_val_sym ()) in
              KH.mk_if (mk_test_member map_expr ins_k
                                      (List.map KH.canonical
                                                ins_tl)
                                      map_ret_kt)
                      (mk_lookup map_expr map_out_t.K.typ ins_k ins_tl)
                      (apply_lambda [KU.id_of_var iv_e, map_ret_kt]
                                    [init_expr] iv_e)
          end

      | x, y ->
          let out_access_expr coll_ve =
              if null free_vars_k then
                mk_lookup coll_ve map_ret_kt.K.typ outs_k outs_tl
              else
                slice_and_project coll_ve
          in

          match init_expr_opt with
          | None -> apply_lambda [KU.id_of_var map_out_ve, map_out_t]
                      [mk_lookup map_expr map_out_t.K.typ ins_k ins_tl] @@
                      out_access_expr map_out_ve

          | Some (init_outs_el, ie) ->
              let init_outs_k = List.map fst init_outs_el in
              let iv_e = KH.mk_var (gen_init_val_sym ()) in
              let init_expr, init_block =
                if ListAsSet.seteq init_outs_k outs_k then
                  let ie' () =
                    (* Use projection to change the order of output
                    variables from 'init_outs_el' to 'outs_el' *)
                    KH.mk_map
                      (project_fn
                        (List.map typed_var_pair init_outs_k @
                          [KU.id_of_var map_ret_ve, map_ret_kt])
                        (List.map typed_var_pair outs_k @
                          [KU.id_of_var map_ret_ve, map_ret_kt]))
                      ie
                  in
                  let ib' =
                    KH.mk_block [
                      mk_update mapn map_ret_kt.K.typ ins_k ins_tl [] [] iv_e;
                      out_access_expr iv_e]
                  in
                  if init_outs_k = outs_k then ie, ib'
                  else ie' (), ib'

                else if not (ListAsSet.seteq init_outs_k free_vars_k) then
                  error ("Initialization expressions that span more " ^
                          "than the free vars of the expression are not "^
                          "supported for full pcs.")
                else
                  error ("Initialization expressions that span the " ^
                          "free vars of the expression are not supported " ^
                          "for full pcs.")
              in
              KH.mk_if (mk_test_member map_expr ins_k
                         (List.map KH.canonical ins_tl) @@ map_out_t)
                       (apply_lambda [KU.id_of_var map_out_ve, map_out_t]
                          [mk_lookup map_expr map_out_t.K.typ ins_k ins_tl]
                          (out_access_expr map_out_ve))
                       (apply_lambda [KU.id_of_var iv_e, map_out_t]
                          [init_expr]
                          init_block)

    in List.map (fun (name, vtype) -> name, vtype) @@
         List.map typed_var_pair free_vars_k,
       map_ret_ve,
       expr


(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(**[value_to_k3_expr value_calc]

   Converts a value ring expression into a K3 expression.
   @param value_calc The value ring expression being translated.
   @return           The [value_calc] translated into K3 as a tuple
                     containing :
                     - the K3 type of the expression,
                     - the K3 expression itself. *)
let rec value_to_k3_expr (value_calc : V.expr_t) : K.type_t * K.expr_t =
  let bin_fn op_fn r1 c2 =
    let ret1_t, e1 = r1 in
    let ret2_t, e2 = value_to_k3_expr c2 in
    if not @@ numerical_type ret1_t && numerical_type ret2_t then
      error ("M3ToK3: ValueRing.Sum or Prod arguments should be of "^
              "numerical type.");
    let ret_t = arithmetic_return_types ret1_t ret2_t in
    ret_t, op_fn e1 e2
  in
  match value_calc with
  | V.Val(AConst i)  -> m3_type_to_k3_type @@ Const.type_of_const i,
                        m3_const_to_k3_const i
  | V.Val(AVar(v, t)) -> m3_type_to_k3_type t,
                        KH.mk_var v
  | V.Val(AFn(fn, fargs, ftype)) ->
        let ret_t = m3_type_to_k3_type ftype in
        let te_l = List.map value_to_k3_expr fargs in
        ret_t, apply_external_lambda fn te_l ret_t
  | V.Neg neg_arg  ->
        let neg_t, neg_e = value_to_k3_expr neg_arg in
        let neg_cst = match base_of_any neg_t with
            | K.TInt
            | K.TFloat -> KH.mk_const @@ K.CInt(-1)
            | _        -> error "M3ToK3: Negation type should be int or float."
        in
        neg_t, KH.mk_mult neg_cst neg_e
  | V.Sum(c1::sum_args_tl)   ->
      List.fold_left (bin_fn KH.mk_add) (value_to_k3_expr c1) sum_args_tl
  | V.Prod(c1::prod_args_tl) ->
      List.fold_left (bin_fn KH.mk_mult) (value_to_k3_expr c1) prod_args_tl
  | _  -> error "M3ToK3: Empty V.Sum or V.Prod."



(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(** Along the translation process we keep two sets of lists encapsulated
in the meta_t type.

The elements of the first list contain initialized keys, indicating
for a specific map under which variables it has been initialized. This is
used as to not generate member tests and initialization expressions for map
accesses that we know have been encountered before, and thus are guaranteed
to be initialized. We only keep initialized keys for input maps or full maps,
 as these are the only maps that get updated when initialization has to be
performed. For the other maps the initialization expression is returned without
modifying the map itself. Besides the name of the map, we also keep a list of
K3 variables representing the input keys for which the map has been
initialized. When performing initialization on a full map we create an entire
output tier at once. This is why we only record input keys and no output keys.

The elements of the second list contain the description of temporary maps
needed for performing sum operations. These maps will get added to the set
of maps that need to be created for this K3 program.

If one is not interested in the metadata information one can supply the result
of [empty_meta] as argument to functions that require metadata.
 *)
type meta_t = (string * string list) list
let empty_meta = ([], [])

(**/**)
let meta_has_init_keys meta init_keys = List.mem init_keys meta

let meta_append_init_keys meta init_keys = meta@[init_keys]
(**/**)

(**********************************************************************)


(**[calc_to_k3_expr meta generate_init theta_vars calc]

   Converts a calculus ring expression into a K3 expression.
   @param meta       Metadata associated with the translation process. See
                     [meta_t] definition above.
   @param generate_init Flag specifying whether to generate initial value
                        computation or not. By default is set to false.
   @param resultn An optional name for the result map which this calculus
                  expression is maintaining.
   @param theta_vars The variables that are in scope when evaluating the
                     expression.
   @param calc       The calculus expression being translated.
   @return           The [calc] translated into K3 and the updated meta.
                     For the K3 result we return a tuple containing :
                     - the schema of the K3 expression as a list of K3
                       variables. This can be used for binding against
                       the keys of the collection represented by the
                       expression. This schema will contain only unbound
                       variables, all bound variables get projected away.
                     - a variable that can be used for binding against
                       the value associated with each tuple in the collection
                       represented by the expression.
                     - the K3 expression itself *)
let rec calc_to_k3_expr meta ?(generate_init = false) ?(resultn="") theta_vars_k calc :
  ((K.id_t * K.type_t) list * (K.expr_t * K.type_t) * K.expr_t) * meta_t =
  let rcr = calc_to_k3_expr meta  ~generate_init ~resultn theta_vars_k in
  let rcr2 meta2 =
    calc_to_k3_expr meta2 ~generate_init ~resultn theta_vars_k in

  let ins, outs = schema_of_expr calc in
  let ins_k = fst_many ins in
  if ListAsSet.diff ins_k theta_vars_k != [] then
    (print_endline @@ "Expr: \n"^CalculusPrinter.string_of_expr calc;
    print_endline  @@ "\ninput_vars: "^String.concat ", " ins_k;
    print_endline  @@ "scope_vars: "^String.concat ", " theta_vars_k;
    error ("M3ToK3: Error: All inputs of calculus expression " ^
          "should be bound!"));

  let cmp_fn op_fn c1 c2 =
    let ret1_t, e1 = value_to_k3_expr c1 in
    let ret2_t, e2 = value_to_k3_expr c2 in
    let out_type = KH.canonical K.TInt in
    let correct_type = escalate_expr out_type in
    if not @@ compatible_types ret1_t ret2_t then
      error ("M3ToK3: Error: Incompatible argument types for " ^
            "comparison operation: "^
            KP.string_of_type ret1_t ^ " <> " ^ KP.string_of_type ret2_t);
    ([],
      (KH.mk_var @@ gen_cmp_ret_sym (), out_type),
      KH.mk_if (op_fn (correct_type e1) @@ correct_type e2)
        (KH.mk_const @@ K.CInt 1)
        (KH.mk_const @@ K.CInt 0)),
    meta
  in
  let (k3_out_el, k3_ret_v, k3_expr), k3_meta = match calc with
    | C.Val(calc_val) ->
        begin match calc_val with
        | Value(calc_val_value) ->
            let ret_t, expr = value_to_k3_expr calc_val_value in
            ([], (KH.mk_var (gen_val_ret_sym ()), ret_t), expr), meta
        | Cmp(T.Eq,  c1, c2) -> cmp_fn KH.mk_eq  c1 c2
        | Cmp(T.Lt,  c1, c2) -> cmp_fn KH.mk_lt  c1 c2
        | Cmp(T.Lte, c1, c2) -> cmp_fn KH.mk_leq c1 c2
        | Cmp(T.Gt,  c1, c2) -> cmp_fn KH.mk_lt  c2 c1
        | Cmp(T.Gte, c1, c2) -> cmp_fn KH.mk_leq c2 c1
        | Cmp(T.Neq, c1, c2) -> cmp_fn KH.mk_neq c1 c2

        | Rel(reln, rel_schema) ->
          let rel_outs_el, rel_ret_ve, expr =
                map_access_to_expr reln resultn [] rel_schema T.TInt theta_vars_k None
          in
          (rel_outs_el, (rel_ret_ve, KH.t_int), expr), meta

        | External(mapn, eins, eouts, ext_type, einit_calc_opt) ->
          (* init_expr_opt will contain if required the schema of the
             initialization expression and the expression itself. *)
          let eouts_k = fst_many eouts in
          let free_eouts_el = ListAsSet.diff eouts_k theta_vars_k in

          let get_init_expr init_calc_opt =
            let init_calc = unwrap_some init_calc_opt in

            if eins = [] && (eouts = [] || free_eouts_el <> []) then
              (print_endline @@ "External: "^
                             CalculusPrinter.string_of_expr calc;
              error ("M3ToK3: No initialization should be required for "^
                      "singleton maps or slice accesses of out maps."));

            let init_theta_vars_k =
                if eins <> [] && eouts <> [] then
                    ListAsSet.diff theta_vars_k eouts_k
                else
                    theta_vars_k
              in
              let (init_outs_el, (init_ret_ve, init_ret_vt), init_expr), nm_1 =
                calc_to_k3_expr meta ~generate_init ~resultn init_theta_vars_k init_calc
              in
              let init_outs_k = fst_many init_outs_el in

              if null eins || null eouts then
                if not @@ ListAsSet.seteq free_eouts_el init_outs_k then
                  begin
                    print_endline @@ "External: "^
                      CalculusPrinter.string_of_expr calc;
                    error
                      ("M3ToK3: Schema of intialization expression should " ^
                        "coincide with the free out vars of the map access.")
                  end
              else (* eins <> [] && eouts <> [] *)
                if ListAsSet.diff free_eouts_el init_outs_k <> [] then
                  begin
                    print_endline @@ "External: " ^
                      (CalculusPrinter.string_of_expr calc);
                    error
                      ("M3ToK3: Schema of intialization expression should " ^
                        "include the free out vars of the map access.")
                  end
                else if ListAsSet.diff init_outs_k eouts_k <> [] then
                  begin
                    print_endline @@ "External: " ^
                      (CalculusPrinter.string_of_expr calc);
                    error
                      ("M3ToK3: Schema of intialization expression should " ^
                        "be included in the out vars of the map access.")
                  end;

              let _ = escalate_type ~expr:(Some calc)
                (init_ret_vt) @@
                m3_type_to_k3_type ext_type
              in
              nm_1, Some(init_outs_el, init_expr)
          in
          let nm, init_expr_opt =
            if not generate_init then meta, None
            else
              if einit_calc_opt != None then
                get_init_expr einit_calc_opt
              else
                let default_init =
                  if null eins && null free_eouts_el then
                    Some([], init_val_from_type @@ m3_type_to_k3_type ext_type)
                  else
                    if Debug.active "DEBUG-DM" ||
                       Debug.active "M3TOK3-INIT-FOR-INS" then
                      Some([], init_val_from_type @@ m3_type_to_k3_type ext_type)
                    else None
                in
                meta, default_init
          in
          let map_outs_el, map_ret_ve, expr =
            map_access_to_expr
              mapn resultn eins eouts ext_type theta_vars_k init_expr_opt
          in
          (map_outs_el,
            (map_ret_ve, m3_type_to_k3_type ext_type), expr), nm

        | AggSum(agg_vars0, aggsum_calc) ->
          (* Convert group by variables to K3 variables and eliminate
              those that are bound. *)
          let agg_vars0_el =
              ListAsSet.diff (fst_many agg_vars0) theta_vars_k
          in

          let (aggsum_outs_el, ret_ve, aggsum_e), nm = rcr aggsum_calc in
          let aggsum_outs_k = fst_many aggsum_outs_el in

          (* Make sure that all the group by variables are included in
              the output schema of the "aggsum" expression. *)
          if ListAsSet.diff agg_vars0_el aggsum_outs_k <> [] then
            (print_endline @@ CalculusPrinter.string_of_expr calc;
            print_endline @@ "GB vars: "^String.concat ", " agg_vars0_el;
            print_endline @@ "AggSum outs: "^
                          String.concat ", " aggsum_outs_k;
            error
              ("M3ToK3: The group by variables of aggsum should " ^
                "be included in the schema of the aggsum expression."));

          let agg_vars_k = ListAsSet.inter agg_vars0_el aggsum_outs_k in
          let agg_vars_el = List.map (fun elem ->
              (elem, List.assoc elem aggsum_outs_el)
            ) agg_vars_k
          in

          let agg_fn = aggregate_fn aggsum_outs_el
            (KU.id_of_var @@ fst ret_ve, snd ret_ve)
          in
          let expr =
            if null aggsum_outs_el then aggsum_e
            else if null agg_vars_k then
              KH.mk_agg agg_fn (init_val_from_full_type @@ snd ret_ve) aggsum_e
            else
              let gb_fn = project_fn
                (aggsum_outs_el @ [KU.id_of_var @@ fst ret_ve, snd ret_ve]) @@
                agg_vars_el
              in
              let gb_aggsum_e = aggsum_e in
              let agg_expr =
                KH.mk_gbagg gb_fn
                  agg_fn
                  (init_val_from_full_type @@ snd ret_ve)
                  gb_aggsum_e
              in
              let flatten_fn =
                KH.mk_lambda (K.ATuple [K.ATuple [
                  KH.wrap_args agg_vars_el;
                  KH.wrap_args [KU.id_of_var @@ fst ret_ve, snd ret_ve]]])
                (KH.mk_tuple @@
                  List.map (KH.mk_var |- fst) agg_vars_el @ [fst ret_ve])
              in
              KH.mk_map flatten_fn agg_expr
          in
          (agg_vars_el, ret_ve, expr), nm

        | Lift(lift_v, lift_calc) ->
          let (lift_outs_el, lift_ret_ve, lift_e), nm = rcr lift_calc in
          let is_bound = List.mem (fst lift_v) theta_vars_k in
          (* If the variable is already bound, this lift should be treated as
             an equality predicate and we can leave its type in place.  If it
             isn't bound, we escalate the variable's type based on the lifted
             expression. *)
          let lift_vt =
            if is_bound then m3_type_to_k3_type @@ snd lift_v
            else escalate_type ~expr:(Some calc)
                  (snd lift_ret_ve) @@
                  m3_type_to_k3_type @@ snd lift_v
          in
          let lift_ve = KH.mk_var @@ fst lift_v in
          let extra_ve, ret_ve, lift_body =
            if is_bound then
              [],
              KH.mk_var @@ gen_lift_ret_sym (),
              KH.mk_tuple @@ (List.map (KH.mk_var |- fst) lift_outs_el) @
                [ KH.mk_if
                    (KH.mk_eq (fst lift_ret_ve) lift_ve)
                    (KH.mk_cint 1)
                    (KH.mk_cint 0)
                ]
            else
              [KU.id_of_var lift_ve, lift_vt],
              KH.mk_var @@ gen_lift_ret_sym (),
              KH.mk_tuple @@ (List.map (KH.mk_var |- fst) lift_outs_el) @
                [fst lift_ret_ve; KH.mk_cint 1]
          in
          let lift_lambda =
            KH.mk_lambda'
              (lift_outs_el @ [KU.id_of_var @@ fst lift_ret_ve, snd lift_ret_ve])
              lift_body
          in
          let expr = apply_lambda_to_expr lift_lambda KH.t_int lift_e
          in
          (lift_outs_el @ extra_ve, (ret_ve, KH.t_int), expr), nm

        (***** BEGIN EXISTS HACK *****)

        | Exists exists_calc ->
          let (exists_outs_el, exists_ret_ve, exists_e), nm = rcr exists_calc in
          let ret_ve = KH.mk_var @@ gen_exists_ret_sym () in
          let exists_body =
            KH.mk_tuple @@
              (KH.ids_to_vars @@ fst_many exists_outs_el) @
                [KH.mk_if
                  (KH.mk_neq (fst exists_ret_ve) @@
                    KH.mk_const @@ zero_of_type @@
                      (snd exists_ret_ve).K.typ)
                  (KH.mk_cint 1) @@
                  KH.mk_cint 0]
          in
          let exists_lambda =
            KH.mk_lambda'
              (exists_outs_el @
                  [KU.id_of_var @@ fst exists_ret_ve, snd exists_ret_ve])
              exists_body
          in
          let expr = apply_lambda_to_expr exists_lambda KH.t_int exists_e in
          (exists_outs_el, (ret_ve, KH.t_int), expr), nm

        (***** END EXISTS HACK *****)
        end

  | C.Sum sum_args ->
        (* Compute the schema of the resulting sum expression *)
        let outs_k = ListAsSet.diff (fst_many outs) theta_vars_k in
        let outs_el = List.map (fun x ->
            x, m3_type_to_k3_type @@ List.assoc x outs)
          outs_k
        in
        (* Translate all terms of the sum into K3 and make sure their schema *)
        (* corresponds to the sum expression's schema and that their return *)
        (* values have numerical types *)
        let prepare_fn old_meta old_ret_t c =
          let (e_outs_el, (e_ret_ve, e_ret_vt), e), new_meta = rcr2 old_meta c in
          let e_outs_k = fst_many e_outs_el in
          if not @@ ListAsSet.seteq e_outs_k outs_k then
            begin
              print_endline @@ "Expression:\n" ^
                            CalculusPrinter.string_of_expr calc;
              print_endline @@ "Scope: "^String.concat ", " theta_vars_k;
              print_endline @@ "Output vars: "^String.concat ", " e_outs_k;
              print_endline @@ "Sum output vars: "^String.concat ", " outs_k;
              error
                ("M3ToK3: The schema of a sum term should " ^
                  "be the same as the schema of the entire sum.")
            end;
          let new_outs_el, new_e =
            if e_outs_k <> outs_k then
              (* If using combine, the sum terms must have the exact
                same schema (same vars in the same order) *)
              outs_el,
              KH.mk_map
                (project_fn
                  (e_outs_el @ [KU.id_of_var e_ret_ve, e_ret_vt]) @@
                   outs_el @ [KU.id_of_var e_ret_ve, e_ret_vt])
                e
            else
              e_outs_el, e
          in
          let new_ret_t = arithmetic_return_types old_ret_t e_ret_vt in
          let e_txt = CalculusPrinter.string_of_expr c in
          (new_outs_el, e_ret_ve, new_e, e_txt), new_meta, new_ret_t
        in

        let nm, ret_t, sum_exprs =
          List.fold_left (fun (old_meta, old_ret_t, old_el) c ->
              let result, new_meta, new_ret_t = prepare_fn old_meta old_ret_t c in
              new_meta, new_ret_t, old_el @ [result])
            (meta, KH.t_int, [])
            sum_args
        in

        let ret_ve = KH.mk_var @@ gen_sum_ret_sym () in
        let hd_outs_el, hd_ret_ve, hd_s, hd_txt = List.hd sum_exprs in
        let sum_exprs_tl = List.tl sum_exprs in
        let sum_expr_terms = List.map (fun (_, _, x, _) -> x) sum_exprs in

        let sum_result =
          if null outs_el then
            let sum_fn sum_e (s_outs_el, s_ret_ve, s, _) =
                KH.mk_add sum_e s
            in
            List.fold_left sum_fn hd_s sum_exprs_tl
          else
            match sum_expr_terms with
            | []       -> failwith "empty combine"
            | hd::rest -> List.fold_left KH.mk_combine hd rest
        in
        (outs_el, (ret_ve, ret_t), sum_result), nm

  | C.Prod(prod_args) ->
      (* Translate all terms of the product into K3 and make sure *)
      (* their return values have numerical types *)
      let prepare_fn (old_meta, old_scope) c =
        let (e_outs_el, e_ret_ve, e), new_meta =
          calc_to_k3_expr old_meta ~generate_init ~resultn old_scope c
        in
        let new_scope = ListAsSet.union old_scope @@ fst_many e_outs_el in
        (e_outs_el, e_ret_ve, e), (new_meta, new_scope)
      in
      let (nm, _), prod_exprs =
        List.fold_left (fun (old_extra, old_el) c ->
            let result, new_extra = prepare_fn old_extra c in
            new_extra, old_el @ [result])
          ((meta, theta_vars_k), [])
          prod_args
      in

      let prod_expr_hd = List.hd prod_exprs in
      let prod_exprs_tl = List.tl prod_exprs in

      let prod_fn (p1_outs_el, ((p1_ret_ve:K.expr_t), p1_ret_t), p1)
                  (p2_outs_el, ((p2_ret_ve:K.expr_t), p2_ret_t), p2) =
        let ret_ve = KH.mk_var @@ gen_prod_ret_sym () in
        let ret_vt = arithmetic_return_types p1_ret_t p2_ret_t in
        let p_outs_el, p =
          match p1_outs_el, p2_outs_el with
          | [], [] -> [], KH.mk_mult p1 p2
          |  _, [] ->
              p1_outs_el,
              KH.mk_map
                (KH.mk_lambda'
                  (p1_outs_el @ [KU.id_of_var p1_ret_ve, p1_ret_t])
                  (KH.mk_tuple @@ List.map (KH.mk_var |- fst) p1_outs_el @
                    [KH.mk_mult p1_ret_ve p2]))
                p1
          | [], _ ->
              p2_outs_el,
              KH.mk_map
                (KH.mk_lambda'
                  (p2_outs_el @ [KU.id_of_var p2_ret_ve, p2_ret_t]) @@
                  KH.mk_tuple @@ List.map (KH.mk_var |- fst) p2_outs_el @
                    [KH.mk_mult p2_ret_ve p1])
                p2
          |  _, _ ->
            let union_el = ListAsSet.union p1_outs_el p2_outs_el in
            let prod_e = KH.mk_tuple @@
              List.map (KH.mk_var |- fst) union_el @
                [KH.mk_mult p1_ret_ve p2_ret_ve]
            in
            let nested =
              KH.mk_map
                (KH.mk_lambda'
                  (p2_outs_el @ [KU.id_of_var p2_ret_ve, p2_ret_t])
                  prod_e)
                p2
            in
            union_el,
            KH.mk_flatten @@
              KH.mk_map
                (KH.mk_lambda'
                  (p1_outs_el @ [KU.id_of_var p1_ret_ve, p1_ret_t])
                  nested)
              p1

        in
        p_outs_el, (ret_ve, ret_vt), p
      in

      let prod_result = List.fold_left prod_fn prod_expr_hd prod_exprs_tl in
      prod_result, nm

    | C.Neg(neg_arg) ->
        (* build up a calculus multiplication by -1 and translate that *)
        rcr @@
          CalcRing.mk_prod [Calculus.mk_value (Arithmetic.mk_int(-1)); neg_arg]
  in (*
    print_endline ("Converting: " ^ (Calculus.string_of_expr calc));
    print_string ("Into: " ^ (KP.string_of_expr k3_expr));
    print_endline ("Type: " ^ (KP.string_of_type (snd k3_ret_v)));
    print_endline "\n\n";
  *)
  (k3_out_el, k3_ret_v, k3_expr), k3_meta



(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)



(**[m3_stmt_to_k3_stmt meta generate_init trig_args m3_stmt]

   Converts a M3 statement into a K3 statement.
   @param meta      Metadata associated with the translation process.
                    See [meta_t] definition above.
   @param generate_init Flag specifying whether to generate initial
                        value computation or not. By default is set to false.
   @param m3_stmt   The M3 statement being translated.
   @param trig_args The arguments of the trigger to which this statement
                    belongs to.
   @return          The [m3_stmt] translated into K3 and the updated meta. *)
let m3_stmt_to_k3_stmt (meta: meta_t) ?(generate_init = false)
                       trig_args (m3_stmt: Plan.stmt_t):
                       K.expr_t * meta_t =
  let (mapn, lhs_ins, lhs_outs, map_type, init_calc_opt) =
    Plan.expand_ds_name m3_stmt.Plan.target_map
  in
  let {Plan.update_type = update_type; Plan.update_expr = incr_calc} =
    m3_stmt
  in
  (* all lhs input variables must be free, they cannot be bound
    by trigger args *)
  if ListAsSet.inter trig_args lhs_ins <> [] then
    begin
      print_endline @@ "Trigger Arguments: "^
        T.string_of_vars ~verbose:true trig_args;
      print_endline @@ "Lhs Input Variables: " ^
        T.string_of_vars ~verbose:true lhs_ins;
      error ("M3ToK3: All lhs input variables must be free. "^
              "They cannot be bound by trigger args.")
    end;

  (* K3 collection used for storing the result of the statement.*)
  let lhs_collection = KH.mk_var mapn in
  let map_k3_type = m3_type_to_k3_base_type map_type in
  let lhs_outs_kt = List.map (m3_type_to_k3_base_type |- snd) lhs_outs in
  let lhs_ins_kt  = List.map (m3_type_to_k3_base_type |- snd) lhs_ins in
  (* NOTE: assumes we don't have any ins *)
  let out_tier_t = mk_k3_collection [] lhs_outs_kt map_k3_type in

  let lhs_ins_el  = KH.ids_to_vars @@ fst_many lhs_ins in
  let lhs_outs_el = KH.ids_to_vars @@ fst_many lhs_outs in

  let trig_args_el = KH.ids_to_vars @@ fst_many trig_args in

  let trig_w_ins_el = ListAsSet.union trig_args_el lhs_ins_el in

  let existing_out_tier = KH.mk_var "existing_out_tier" in

  (* Determine the current value of the entry in "existing_out_tier" *)
  (* corresponding to a particular output variables key. Used for *)
  (* updating the "existing_out_tier".*)
  (* If this is a replace statement then we'll consider zero as the *)
  (* existing value to be updated. *)
  (* If this is an update statement then we do one of the following: *)
  (* - if there aren't any output variables then "existing_out_tier" *)
  (* is a singleton and can be used directly in the updating expression. *)
  (* - if we only have output variables then we Lookup for the desired value*)
  (*   and we use zero or init_calc_opt for initialization, if required. *)
  (* - If we have a full map and initialization is not required then a *)
  (*   simple Lookup suffices. Otherwise we use the initialization *)
  (*   expression if the desired value is not found in "existing_out_tier". *)
  (*   The scope of the initialization expression will also include the *)
  (*   lhs  output variables and it will return a singleton. *)
   let existing_v =
      match update_type, lhs_outs_el with
      | Plan.ReplaceStmt, _ ->
         init_val_from_type @@ m3_type_to_k3_type map_type

      | Plan.UpdateStmt, [] -> KH.mk_peek_or_zero existing_out_tier

      | Plan.UpdateStmt, _ ->
         let init_expr_opt =
           if not generate_init then None else
           match init_calc_opt, lhs_ins_el with
           | None, []  ->
              Some(init_val_from_type @@ m3_type_to_k3_type map_type)
           | None, _   -> None
           | Some _, _ -> (* TODO: ivc code *)
              failwith "IVC code not yet implemented."
         in
         match init_expr_opt with
         | Some init_expr ->
            (* check for the presence of a value in the existing out tier
             * before the init expression *)
            KH.mk_if
              (mk_test_member existing_out_tier
                (KH.vars_to_ids lhs_outs_el)
                (List.map KH.canonical lhs_outs_kt) @@
                KH.canonical map_k3_type)
              (mk_lookup existing_out_tier
                map_k3_type (KH.vars_to_ids lhs_outs_el) lhs_outs_kt)
              init_expr
         | None ->
           mk_lookup existing_out_tier map_k3_type
            (KH.vars_to_ids lhs_outs_el) lhs_outs_kt
   in

   (* Translate the rhs calculus expression into a k3 expression and its *)
   (* corresponding schema. Beside the trigger variables we also have *)
   (* the input variables of the "lhs_collection" in scope as we will *)
   (* update "lhs_collection" while iterating over all lhs input variables. *)
   let (rhs_outs_el_and_ty, (rhs_ret_ve, rhs_ret_vt), incr_expr), nm =
      if not(Debug.active "DEBUG-DM-WITH-M3") then
         calc_to_k3_expr meta ~generate_init:(generate_init)
                         (KH.vars_to_ids trig_w_ins_el) incr_calc
      else
         let (rhs_outs_el, rhs_ret_ve, incr_expr), nm =
            calc_to_k3_expr meta ~generate_init:(true)
                            (KH.vars_to_ids trig_w_ins_el) incr_calc
        in
        (rhs_outs_el, rhs_ret_ve, incr_expr), nm
   in
   let rhs_outs_ids, rhs_outs_t = List.split rhs_outs_el_and_ty in
   let rhs_outs_el = List.map KH.mk_var rhs_outs_ids in

   (* Make sure that the lhs collection and the incr_expr have *)
   (* the same schema. *)
   let free_lhs_outs = ListAsSet.diff (List.map fst lhs_outs) (List.map fst trig_args) in
   if not (ListAsSet.seteq free_lhs_outs rhs_outs_ids) then
      (print_endline ("Stmt: "^
                      (Plan.string_of_statement m3_stmt));
       print_endline ("Trigger Variables: "^
                      (String.concat ", " (List.map fst trig_args)));
       print_endline ("Lhs Output Variables: "^
                      (String.concat ", " free_lhs_outs));
       print_endline ("Rhs Output Variables: "^
                      (String.concat ", " rhs_outs_ids));
       error ("M3ToK3: The lhs and rhs must have the same set " ^
              "of free out variables. "));
   let _ = escalate_type rhs_ret_vt @@ KH.canonical map_k3_type in
   let free_lhs_outs_el = List.map KH.mk_var free_lhs_outs in

   (* Iterate over all the tuples in "incr_expr" collection and update *)
   (* the lhs_collection accordingly. *)
   let coll_update_expr =
      let single_update_expr =
          mk_update mapn map_k3_type
            (KH.vars_to_ids lhs_ins_el) (lhs_ins_kt) (KH.vars_to_ids lhs_outs_el)
            lhs_outs_kt (KH.mk_add existing_v rhs_ret_ve)
      in
      let args = (List.combine rhs_outs_el rhs_outs_t) @ [rhs_ret_ve, rhs_ret_vt] in
      let args = List.map (first KU.id_of_var) args in
      let update_body = match rhs_outs_el with
        | [] -> KH.mk_let (fst_many args) incr_expr single_update_expr
        | _  -> KH.mk_iter (KH.mk_lambda' args single_update_expr) incr_expr
      in

      if update_type = Plan.UpdateStmt ||
         free_lhs_outs_el = [] ||
         Debug.active "UNSAFE-REPLACE" then
        update_body
      else
        let old_slice =
          if ListAsSet.seteq lhs_outs_el free_lhs_outs_el
          then existing_out_tier
          else
            let bound_out_names =
              ListAsSet.diff (KH.vars_to_ids lhs_outs_el)
                             (KH.vars_to_ids free_lhs_outs_el) in
            mk_slice existing_out_tier (KH.vars_to_ids lhs_outs_el) bound_out_names
        in
        KH.mk_block [
          KH.mk_iter
            (KH.mk_lambda' ((List.combine (List.map KU.id_of_var lhs_outs_el)
                                    (List.map KH.canonical lhs_outs_kt))@
                      [KU.id_of_var rhs_ret_ve, rhs_ret_vt])
                    (KH.mk_delete mapn
                                  (lhs_outs_el@ [rhs_ret_ve])))
            old_slice;
          update_body
         ]
    in

   (* In order to implement a statement we iterate over all the values *)
   (* of the input variables of the lhs collection, and for each of them *)
   (* we update the corresponding output tier. *)
   let statement_expr =
     let args =
       (list_zip (List.map KU.id_of_var lhs_ins_el) @@ List.map KH.canonical lhs_ins_kt)@
         [KU.id_of_var existing_out_tier, out_tier_t]
     in
     match lhs_ins_el with
     | [] -> KH.mk_let (fst_many args) lhs_collection coll_update_expr
     | _  -> KH.mk_iter (KH.mk_lambda' args coll_update_expr) lhs_collection
   in
   statement_expr, nm
;;
(**[m3_trig_to_k3_trig meta generate_init m3_trig]

   Transforms a M3 trigger into a K3 trigger.
   @param meta    Metadata associated with the translation process.
                  See [meta_t] definition above.
   @param generate_init Flag specifying whether to generate initial value
                        computation or not. By default is set to false.
   @param m3_trig The M3 trigger being translated.
   @return        The [m3_trig] translated into K3 and the updated meta.
*)
let m3_trig_to_k3_trig ?(generate_init = false)
                       (schema_env:(K.id_t * K.type_t) list)
                       (m3_trig: M3.trigger_t):
                       K.flow_program_t =
  if null !(m3_trig.M3.statements) then [] else
  let trig_args = Schema.event_vars m3_trig.M3.event in
  let k3_trig_stmts, new_meta = first List.rev @@
    List.fold_left
      (fun (old_stms, oldmeta) m3_stmt ->
        let k3_stmt, newmeta = m3_stmt_to_k3_stmt oldmeta ~generate_init:generate_init
          trig_args m3_stmt
        in
        k3_stmt::old_stms, newmeta)
      ([], [])
      !(m3_trig.M3.statements)
  in
  [KH.mk_code_sink'
    (match m3_trig.M3.event with
    (*| Schema.SystemInitializedEvent -> "at_init"*)
      | _ -> Schema.name_of_event m3_trig.M3.event)
    (match List.map (second m3_type_to_k3_type) trig_args with
      (* K3 triggers must have args *)
      | []   -> ["_", KH.t_int]
      | args -> args)
    [] @@
    KH.mk_block k3_trig_stmts]


let csv_adaptor_to_k3 (name_prefix: string)
                      (rel: Schema.rel_t)
                      (params: (string * string) list):
                        (K.flow_statement_t * K.type_t) =
  let del_var = "__m3_is_deletion" in
  let param p d =
    if List.mem_assoc p params then List.assoc p params else d
  in
  let (reln, relv, _) = rel in
  let with_deletions = (param "deletions" "false") = "true" in
  let args =
    List.map
      (fun (vn, vt) -> (vn,
        let input_type = match vt with T.TDate -> T.TString | _ -> vt in
          m3_type_to_k3_type input_type
      ))
      (if with_deletions then (del_var, T.TInt) :: relv else relv)
  in
  let child_params =
    List.map (fun (vn, vt) -> match vt with
      | T.TDate -> KH.mk_apply (KH.mk_var "parse_sql_date") [KH.mk_var vn]
      | _       -> KH.mk_var vn
    ) relv
  in
  let send_to_event evt =
    KH.mk_send (Schema.name_of_event evt) (KH.mk_var "me") child_params
  in
  let k3_code =
    if with_deletions then
      KH.mk_if
        (KH.mk_eq (KH.mk_var "__m3_is_deletion") (KH.mk_const (K.CInt(0))))
        (send_to_event (Schema.InsertEvent(rel)))
        (send_to_event (Schema.DeleteEvent(rel)))
    else
      (send_to_event (Schema.InsertEvent(rel)))
  in
    ( (K.Sink(K.Code(
        name_prefix ^ reln,
        KH.wrap_args args,
        [],
        k3_code
      ))),
      KH.wrap_ttuple @@ KH.type_of_arg @@ KH.wrap_args args
    )
;;

let known_adaptors = ref [
  "csv", csv_adaptor_to_k3
]

let add_adaptor adaptor = known_adaptors := adaptor :: !known_adaptors

let k3_demuxes stream_rels =
  let source_id = ref 0 in
  let next_source () =
    source_id := !source_id + 1;
    "s"^string_of_int !source_id
  in
  let k3_prog_demux_calls, k3_prog_demux_deep =
    List.split (List.map (fun (source, adaptors) ->
      let (demuxes, demux_types) =
        List.split (List.map (fun ((adaptor, params), rel) ->
          if not (List.mem_assoc adaptor !known_adaptors)
          then failwith ("Unknown adaptor "^adaptor)
          else ((List.assoc adaptor !known_adaptors) "demux_" rel params)
        ) adaptors)
      in
      let source_type =
        match (List.fold_left (function
          | None -> (fun x -> Some(x))
          | Some(s) -> (fun x ->
            if x <> s then failwith "Mismatched adaptor schemas" else Some(x)
          )
        ) None demux_types) with
          | None -> failwith "Each source must have an adaptor"
          | Some(s) -> s
      in
      let source_id = next_source() in
      let source_channel_type, source_channel_format =
        match source with
          | Schema.NoSource -> failwith "All streams must have a source"
          | Schema.PipeSource _ -> failwith "pipe sources unsupported"
          | Schema.SocketSource _ -> failwith "socket sources unsupported"
          | Schema.FileSource(fname, Schema.Delimited("\n")) ->
              (K.File(fname), K.CSV)
          | Schema.FileSource _ -> failwith "Unsupported file source framing"
      in
      let source = K.Source(K.Resource(source_id,
                      K.Handle(source_type, source_channel_type, source_channel_format)))
      in
      ( ( source,
          List.map (fun (_, (reln, _, _)) ->
            K.BindFlow(source_id, "demux_"^reln)
          ) adaptors,
          K.Instruction(K.Consume(source_id))
        ),
        demuxes
      )
    ) stream_rels)
  in
  k3_prog_demux_calls, k3_prog_demux_deep


(**[m3_to_k3 generate_init m3_program]

   Transforms a M3 program into a K3 program.
   @param generate_init Flag specifying whether to generate initial value
                        computation or not. By default is set to false.
   @param  m3_program The M3 program being translated.
   @return The [m3_program] translated into K3.
*)
let m3_to_k3 ?(generate_init = false) ?(role = "client")
             (m3_program: M3.prog_t) : (K.program_t * (K.program_t * string list)) =
  let {
        M3.maps = m3_prog_schema; M3.triggers = m3_prog_trigs;
        M3.queries = m3_prog_tlqs; M3.db = m3_database
      } = m3_program in
  (* declaration for parsing sql date *)
  let k3_prog_schema = List.map m3_map_to_k3_map !m3_prog_schema in
  let k3_prog_env = List.map (function
      | K.Global(id, ty, _) -> id, ty
      | _                   -> failwith "invalid map declaration"
    ) k3_prog_schema
  in
  let k3_prog_trigs =
    List.flatten @@ List.map
      (m3_trig_to_k3_trig ~generate_init k3_prog_env)
      !m3_prog_trigs
  in
  let _  = List.map (fun (qname, qexpr) -> match qexpr with
      | C.Val(Calculus.External(en, _, _, _, _)) -> qname, en
      | _ -> failwith "Complex TLQs presently unsupported"
    ) !m3_prog_tlqs
  in
  let table_rels, stream_rels =
    Schema.partition_sources_by_type m3_database
  in
  (* turn into hashtable *)
  let table_rel_h = Hashtbl.create 10 in
  List.iter (fun (src, rels) ->
      let (_, (nm, _, _)) = hd rels in
      Hashtbl.add table_rel_h nm (src, hd rels))
    table_rels;
  (* add initializations from csv *)
  let k3_prog_schema =
    List.map
      (function
      | K.Global(id, t, None) as x ->
          begin try
            begin match Hashtbl.find table_rel_h id with
            | (Schema.FileSource(f, Schema.Delimited "\n")), (_, (nm, vs, _)) ->
              let open K3Helpers in
              (* this will be missing the value type, just like the file *)
              let ts = List.map (fun (_, t) -> m3_type_to_k3_type t) vs in
              let id_ts = types_to_ids_types "m" ts in
              let vars = ids_to_vars @@ fst_many id_ts in
              K.Global(nm, t,
                some @@
                  mk_map
                    (mk_lambda' id_ts @@
                      mk_tuple @@ vars@[mk_cint 1])
                  (mk_apply (mk_var K3StdLib.csv_loader_name) [mk_cstring f]))

            | _ -> failwith "Table relations that aren't filesources are unsupported"
            end
          with Not_found -> x end
      | x -> x
      ) k3_prog_schema
  in

  let k3_prog_demux_calls, k3_prog_demux_deep = k3_demuxes stream_rels in

  let add_empty l = List.map (fun x -> x, []) l in

  let k3_prog_demux =
    add_empty @@ List.flatten k3_prog_demux_deep in

  let k3_prog_sources, k3_prog_bindings, k3_prog_consumes =
    List.fold_right (fun (s, b, c) (sources, bindings, consumes) ->
      s :: sources, b @ bindings, c :: consumes
    ) k3_prog_demux_calls ([], [], [])
  in
  let k3_prog_client_role =
    add_empty @@ k3_prog_sources @ k3_prog_bindings @ k3_prog_consumes
  in

  (* Warmup program construction *)
  let k3_warmup_rel (src, rels) =
    let (_, (nm, schema, _)) = hd rels in
    let ty = mk_k3_collection [] (mvar_btypes schema) K.TInt
    in K.Global(nm, ty, None)
  in

  let k3_warmup_of_m3_map (dacc, flacc) m3_map =
    begin match m3_map with
      | M3.DSView(ds) ->
        (* Global map declaration *)
        let map_nm, input_vars, output_vars, map_type, _ =
          Plan.expand_ds_name ds.Plan.ds_name
        in
        let element_type = m3_type_to_k3_base_type map_type in
        let ivar_types = mvar_btypes input_vars in
        let ovar_types = mvar_btypes output_vars in

        (* Map definition expression *)
        let argt = ["_", KH.t_unit] in
        let rt = [KH.t_unit] in
        let (_, _, defn_expr), _ =
          calc_to_k3_expr [] ~generate_init:false ~resultn:map_nm [] ds.Plan.ds_definition
        in
        let bootstrap_expr = KH.mk_assign ("bs_"^map_nm) defn_expr
        in
        (dacc @ [K.Global("bs_"^map_nm, (KH.mut @@ mk_k3_collection ivar_types ovar_types element_type), None)],
         flacc @ [KH.mk_code_sink' ("bootstrap_"^map_nm) argt [] booststrap_expr])

      | M3.DSTable(_, _, _) -> acc
    end
  in

  let k3_warmup_prog =
    let (warmup_ds, warmup_trigs) = List.fold_left k3_warmup_of_m3_map [] !m3_prog_schema in
    List.map k3_warmup_rel stream_rels
    @ List.map k3_warmup_rel table_rels
    @ warmup_ds @ [K.Flow(warmup_trigs)]
  in

  let k3_warmup_relname (src, rels) =
    let (_, (nm, _, _)) = hd rels in nm
  in

  let k3_wrelnames =
    List.map k3_warmup_relname stream_rels @ List.map k3_warmup_relname table_rels
  in

  ((add_empty @@
     k3_prog_schema @
     [ K.Flow(k3_prog_trigs @ k3_prog_demux) ] @
     [ K.Role(role, k3_prog_client_role);
       K.DefaultRole(role);
     ]),
   (add_empty k3_warmup_prog, k3_wrelnames))

