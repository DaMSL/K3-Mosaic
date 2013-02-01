module M3ToK3 where
-- Module for translating a M3 program into a K3 program. *)

import Arithmetic
import Calculus

import qualified Type as T
import qualified M3Constants as Const
import qualified Arithmetic.ValueRing as R
import qualified Calculus.CalcRing as C
import qualified Stages.K3 as K
import qualified Stages.K3.Printing as KP
import qualified Stages.K3.Util.K3Builders as KH
import qualified Stages.K3.Util.K3Utils as KH
import qualified Stages.K3.K3Typechecker as KT

{-exception M3ToK3Failure of -}
   {-Calculus.expr_t option * -}
   {-K.expr_t option *-}
   {-string-}

{-error ?(m3 = Nothing) ?(k3 = Nothing) msg =-}
   {-raise (M3ToK3Failure(m3, k3, msg))-}

pair_map pair_fn (a,b) = (pair_fn a, pair_fn b)

m3_type_to_k3_base_type t = case t of
   T.TBool            -> K.TBool
   T.TInt             -> K.TInt
   T.TFloat           -> K.TFloat
   T.TString          -> K.TString
   T.TDate            -> K.TInt
   T.TAny             -> error "Expecting M3 to be fully typed"
   T.TExternal(ext_t) -> error "External types unsupported"
{-      K.TTuple [
        KT.canonical K.TInt;
        KT.canonical K.TInt;
        KT.canonical K.TInt;
      ]
-}

var_ids = map KU.id_of_var

mvar_btypes = map (\(_,x) -> m3_type_to_k3_base_type x)

m3_type_to_k3_type t = KT.canonical (m3_type_to_k3_base_type t)

mk_k3_tuple elemslist = KT.canonical $ K.TTuple elems

mk_k3_collection base_ivars base_ovars base_v =
   let ivars = map KT.canonical base_ivars
       ovars = map KT.canonical base_ovars
       v = KT.canonical base_v
   in if null ivars
      then if null ovars
           then KH.wrap_tset v
           else KH.wrap_tset $ mk_k3_tuple $ ovars ++ [v]
      else if null ovars
           then KH.wrap_tset $ mk_k3_tuple $ ivars ++ [v]
           else KH.wrap_tset $ mk_k3_tuple $ ivars ++ 
               [KH.wrap_tset $ mk_k3_tuple $ ovars++[v]]

name_of_kvar K.Var(v_name) = v_name
name_of_kvar _ = error "M3ToK3:: K.Var expected."

m3_map_to_k3_map :: M3.Map -> K.Declaration
m3_map_to_k3_map m3_map = K.Global coll_name (K.TValue coll_type) Nothing
  where 
    (coll_name, coll_type) = case m3_map of
        M3.DSView(ds) -> 
           let (map_name, input_vars, output_vars, map_type, _) = 
                  Plan.expand_ds_name $ ds_name $ Plan $ ds
               element_type = m3_type_to_k3_base_type map_type
               ivar_types = mvar_btypes input_vars
               ovar_types = mvar_btypes output_vars
           in (map_name,
                mk_k3_collection ivar_types ovar_types element_type)
  
        | M3.DSTable(rel_name, rel_schema,_) -> 
                ( rel_name,
                  mk_k3_collection [] (mvar_btypes rel_schema) K.TInt
                )
   in
      
(**/**)

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**/**)

extract_value_type t =
  begin match t with
    | K.TFunction _ -> failwith "Expected a value type, not a function"
    | K.TValue(vt) -> vt
  end

tvar_to_vtvar (v,vt) =
  (v, extract_value_type vt)

init_val_from_type t =
  begin match KT.base_of t with
    | K.TInt   -> KH.mk_const (K.CInt(0))
    | K.TFloat -> KH.mk_const (K.CFloat(0.))
    | _ -> failwith "Map type with no initial value"
  end

init_val_from_full_type t = init_val_from_type (extract_value_type t)

base_of_any t = KT.base_of (extract_value_type t)

compatible_types t1 t2 = 
  begin match base_of_any t1, base_of_any t2 with
    | K.TInt,   K.TBool   -> True
    | K.TBool,  K.TInt    -> True
    | K.TFloat, K.TBool   -> True
    | K.TBool,  K.TFloat  -> True
    | K.TInt,   K.TFloat  -> True
    | K.TFloat, K.TInt    -> True
    | _,_                 -> (t1 = t2)
  end

numerical_type t = 
  begin match base_of_any t with
    | K.TBool
    | K.TInt   -> True
    | K.TFloat -> True
    | _ -> False
  end

arithmetic_return_types ?(expr=Nothing) t1 t2 = 
   begin match (base_of_any t1), (base_of_any t2) with
      | K.TBool,  K.TBool
      | K.TBool,  K.TInt
      | K.TInt,   K.TInt
      | K.TInt,   K.TBool -> 
                      K.TValue(K.TIsolated(K.TImmutable(K.TInt, [])))
      | K.TBool,  K.TFloat
      | K.TInt,   K.TFloat
      | K.TFloat, K.TFloat
      | K.TFloat, K.TBool
      | K.TFloat, K.TInt -> 
                      K.TValue(K.TIsolated(K.TImmutable(K.TFloat, [])))
      | _,_ -> error ~m3::expr "M3ToK3:: arguments must be of numerical type."
   end

escalate_type ?(expr=Nothing) from_t to_t = 
   begin match base_of_any from_t, base_of_any to_t with
      | K.TBool, K.TInt
      | K.TInt,  K.TInt ->
                      K.TValue(K.TIsolated(K.TImmutable(K.TInt, [])))
      | K.TBool, K.TFloat 
      | K.TInt,  K.TFloat 
      | K.TFloat,K.TFloat ->
                      K.TValue(K.TIsolated(K.TImmutable(K.TFloat, [])))
      | _,_ -> 
         if from_t = to_t then to_t
         else error ~m3::expr ("M3ToK3:: Unable to escalate type " ++
                              (KP.string_of_type from_t) ++
                              " to type " ++ (KP.string_of_type to_t))
   end

escalate_expr to_t expr = expr
(*
  print_endline ("Escalating "++(KP.string_of_expr expr)++" to "++(KP.string_of_type to_t));
  begin match ((K3Typechecker.type_of_expr expr), to_t) with
    | (K.TValue(a), K.TValue(b)) 
          when   ((a = KH.t_int)   || (a = KH.t_int_mut))
              && ((b = KH.t_float) || (b = KH.t_float_mut)) ->
        KH.mk_mult (KH.mk_const (K.CFloat(1.0))) expr
    | (K.TValue(a), K.TValue(b)) when a = b -> expr
    | _ -> failwith ("Can not escalate "++(KP.string_of_expr expr)++" to "++
                     (KP.string_of_type to_t))
  end
*)

m3_const_to_k3_const c =
  begin match c with 
    | Const.CInt(i) -> KH.mk_const (K.CInt(i))
    | Const.CFloat(f) -> KH.mk_const (K.CFloat(f))
    | Const.CString(s) -> KH.mk_const (K.CString(s))
    | Const.CBool(b) -> KH.mk_const (K.CBool(b))
    | Const.CDate(y,m,d) -> 
        KH.mk_const (K.CInt(y * 10000 + m * 100 + d))
(*        KH.mk_tuple [ KH.mk_const (K.CInt(y));
                      KH.mk_const (K.CInt(m));
                      KH.mk_const (K.CInt(d)) ]
*)

  end

zero_of_type t =
  begin match t with
    | K.TInt   -> (K.CInt(0))
    | K.TFloat -> (K.CFloat(0.))
    | _ -> failwith ("Can not generate zero for type:: "++
                        (KP.string_of_base_type t))
  end
    
  
  
extract_opt opt =  
   begin match opt with
      | Nothing -> error "M3ToK3:: Trying to extract some from Nothing"
      | Just(s) -> s
   end
(**/**)

(**********************************************************************)
(**/**)
unique l = 
   foldl' (\acc e -> if List.mem e acc then acc else acc++[e]) [] l
   
keys_from_kvars kvars  = List.map (\kv -> (name_of_kvar kv,kv)) kvars

exprs_to_tuple (el:: K.expr_t list) = 
   begin match el with
      |  [] -> error "M3ToK3:: invalid empty expressions list"  
      | [e] -> e
      | _   -> KH.mk_tuple el
   end

(**/**)

(**[lambda_args v_el]

   Converts a list of K3 variable expressions into K3 lambda arguments.
   ++param v_el The list of K3 variable expressions to be converted.
   ++return     The K3 lambda argument corresponding to [v_el] *)
lambda_args v_el =
   begin match v_el with
      | [] -> error "M3ToK3:: invalid empty variables list" 
      | [v_name,v_type] -> K.AVar(v_name,v_type)
      | _ ->
         let vars = List.map fst v_el in
         if (unique vars) <> vars then 
            error "M3ToK3:: invalid lambda with duplicate variables"
         else
            K.ATuple( List.map (\(vn, vt) -> K.AVar(vn, vt)) v_el )
   end

(**[lambda v_el body]

   Constructs a lambda expression from the expression [body] with the 
   variables in [v_el].
   ++param v_el The list of K3 variables of the [body].
   ++param body The body of the lambda expression.
   ++return     The K3 lambda expression expecting [v_el] as arguments and
               computing [body]. *)
lambda v_el body = KH.mk_lambda (lambda_args v_el) body

(**[assoc_lambda v1_el v2_el body]

   Constructs an associative lambda expression from the expression [body] 
   with the variables in [v1_el] and [v1_el].
   ++param v1_el The first list of K3 variables of the [body].
   ++param v2_el The second list of K3 variables of the [body].
   ++param body  The body of the associative lambda expression.
   ++return      The K3 associative lambda expression expecting [v1_el] and
                [v2_el] as arguments and computing [body]. *)
assoc_lambda v1_el v2_el body = 
   KH.mk_assoc_lambda (lambda_args v1_el) (lambda_args v2_el) body

(**[apply_lambda v_el el body]

   Evaluates the K3 expression [body] by binding the list of expressions 
   in [el] to the list of K3 variables in [v_el]. *)
apply_lambda v_el el body = 
   if (List.length v_el) != (List.length el) then
      error ("M3ToK3:: Applying lambda to expression with " ++ 
             "different size schema!");
   KH.mk_apply (lambda v_el body) (exprs_to_tuple el)

(**[apply_lambda_to_expr lambda_e expr]

   Applies the lambda expression [lambda_e] to the contents of [expr].
   [expr] must be either a singleton expression or a K3 collection 
   (ie. a mapping from keys to values, no collection with just values 
   are allowed). If [expr] is singleton and lambda_e outputs a tuple the 
   result is wrapped in a Singleton collection. *)
apply_lambda_to_expr lambda_e lambda_t expr =
   let lambda_body = (KU.decompose_lambda (lambda_e)) in
   let lambda_rett = 
      (KT.canonical (K.TCollection(K.TSet, KT.canonical lambda_t))) in
   begin match (KU.arg_of_lambda (lambda_e), 
                KU.tag_of_expr lambda_body) with
      | ((Just(K.AVar(_))), K.Tuple) -> 
            KH.mk_singleton lambda_rett
                            (KH.mk_apply lambda_e expr)
      | ((Just(K.AVar(_))), _)          -> KH.mk_apply lambda_e expr
      | ((Just(K.ATuple(_))), _)        -> KH.mk_map lambda_e expr
      | _ -> error "M3ToK3:: Invalid arguments to apply_lambda_to_expr."
   end

(**[project_fn from_v_el to_v_el]

   Projects the list of variables [from_v_el] to the list of variables
   [to_v_el]. When maped over a collection it will project away keys that
   are not present in [to_v_el]. *)
project_fn (from_v_el::(K.id_t * K.value_type_t) list)
               (to_v_el::(K.id_t * K.value_type_t) list) = 
   lambda from_v_el 
          (exprs_to_tuple (List.map (\(x,_) -> KH.mk_var x) 
                          to_v_el))

gen_accum_var = 
   FreshVariable.declare_class "functional/M3ToK3" "accv"
(**[aggregate_fn v_el multpl_e]

   Constructs an aggregation function.
   ++param v_el     The list of K3 variables of the collection being aggregated.
   ++param multpl_e Variable expression used for accessing the values associated
                   with each tuple in the aggregated collection. *)
aggregate_fn v_el multpl_e =
   let accv = gen_accum_var () in 
   let (multpl_n,multpl_t) = multpl_e in
      assoc_lambda [accv, multpl_t] (v_el++[multpl_n, multpl_t])
                   (KH.mk_add (KH.mk_var accv) (KH.mk_var multpl_n))

(**[external_lambda_args fn t_l]

   Converts a list of K3 types into K3 external lambda arguments.
   ++param fn   The name of the external function, used when naming 
               the arguments.
   ++param t_l  The list of K3 types to be converted.
   ++return     The K3 lambda argument corresponding to [t_l] *)
external_lambda_args fn t_l =
   begin match t_l with
      | [] -> error "M3ToK3:: invalid empty variables list" 
      | [t] -> K.AVar(fn++"_arg1",t)
      | _ -> 
         let arg_prefix  = fn++"_arg_" in
         let arg_counter = ref 0 in
         let next_arg_var() = 
            incr arg_counter;
            arg_prefix++(string_of_int !arg_counter) 
         in
            K.ATuple(List.map (\t -> K.AVar(next_arg_var(),t)) t_l)
   end

(**[external_lambda fn t_l ftype]

   Constructs an external lambda expression for the external function [fn] 
   with arguments of types corresponding to [t_l] and return type ftype.
   ++param fn    The name of the external function.
   ++param t_l   The list of K3 types used for typing the arguments.
   ++param ftype The return type of the function. *)
external_lambda fn t_l ftype =
   failwith "TODO:: Add support for external lambdas"

(**[apply_external_lambda fn te_l ftype]

   Evaluates the external function [fn] by binding its arguments to 
   the expressions in [te_l]. *)      
apply_external_lambda fn te_l ftype =
   failwith "TODO:: Add support for external lambdas"

mk_project ?(id="projected_field") width idx ret_t expr =
  let rec build_tuple w =
    if w >= width then []
    else (
      if w = idx then K.AVar(id, KT.canonical ret_t)
                 else K.AIgnored
    ) : (build_tuple (w+1))
  in
    KH.mk_apply (KH.mk_lambda (K.ATuple(build_tuple 0)) (KH.mk_var id)) expr

mk_slice collection all_keys bound_keys =
  KH.mk_slice collection (KH.mk_tuple ((
        List.map (\x -> 
           if List.mem x bound_keys
              then KH.mk_var x
              else KH.mk_const K.CUnknown) all_keys)++[KH.mk_const K.CUnknown]))

mk_lookup collection set_t keys key_types =
  let coll_type = 
    (KT.canonical (K.TCollection(K.TSet, 
      (KT.canonical (K.TTuple(
        List.map KT.canonical (key_types++[set_t])
      )))
    )))
  in
  let wrapped_value = KH.mk_var "wrapped_lookup_value" in
  KH.mk_let (KU.id_of_var wrapped_value)
            coll_type
            (mk_slice collection keys keys)
            (KH.mk_if (
                KH.mk_eq wrapped_value (KH.mk_empty coll_type)
              ) (
                KH.mk_const (zero_of_type set_t)
              ) (
                mk_project ((List.length keys)+1) (List.length keys)
                           set_t 
                           (KH.mk_peek wrapped_value)
              )
            )

mk_test_member collection keys key_types val_type =
  KH.mk_has_member collection (KH.mk_tuple ((List.map KH.mk_var keys)++
                                            [KH.mk_const K.CUnknown]))
                   (KH.wrap_ttuple_mut (key_types ++ [val_type]))

mk_arg x xt = K.AVar(x, K3Typechecker.canonical xt);;

mk_tuple_arg keys keys_tl v vt = K.ATuple( (List.map2 mk_arg keys keys_tl) ++ [mk_arg v vt] );;

mk_lambda keys keys_tl v vt body = KH.mk_lambda (mk_tuple_arg keys keys_tl v vt) body;;

mk_var_tuple keys v = KH.mk_tuple (List.map KH.mk_var (keys++[v]));;

mk_val_tuple keys v = KH.mk_tuple ((List.map KH.mk_var keys)++[v]);;

mk_iter = KH.mk_iter

mk_update collection set_t ivars ivar_t ovars ovar_t new_val =
  (* new_val might (and in fact, usually will) depend on the collection, so 
     we need to evaluate it and save it to a variable before clearing the
     existing elements out of the collection *)
  let new_val_var = KH.mk_var "update_value" in
  let update_block = 
    if ivars = [] then
      if ovars = [] then 
        KH.mk_block [
          mk_iter
            (KH.mk_lambda (mk_arg "value" set_t) 
              (KH.mk_delete collection (KH.mk_var "value"))
            )
            (KH.mk_slice collection (KH.mk_tuple [KH.mk_const K.CUnknown]));
          KH.mk_insert collection (mk_val_tuple [] new_val_var)
        ]
      else
        KH.mk_block [
          mk_iter
            (mk_lambda ovars ovar_t "value" set_t (
              KH.mk_delete collection (mk_var_tuple ovars "value")
            ))
            (mk_slice collection ovars ovars);
          KH.mk_insert collection (mk_val_tuple ovars new_val_var)
        ]
    else
      if ovars = [] then
        KH.mk_block [
          mk_iter
            (mk_lambda ivars ivar_t "value" set_t (
              KH.mk_delete collection (mk_var_tuple ivars "value")
            ))
            (mk_slice collection ivars ivars);
          KH.mk_insert collection (mk_val_tuple ivars new_val_var)
        ]
      else
        failwith "FullPC unsupported"
  in
    KH.mk_apply (KH.mk_lambda (mk_arg (KU.id_of_var new_val_var) set_t) update_block)
                (new_val)

(**********************************************************************)
(**/**)
(* Utility function for generating map access expression. *)
map_to_expr mapn ins outs map_type =
   KH.mk_var mapn

gen_map_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "map_ret_"

gen_slice_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "slice_"

gen_init_val_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "init_val_"

gen_val_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "val_ret_"

gen_cmp_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "cmp_ret_"

gen_lift_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "lift_ret_"

gen_exists_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "exists_ret_"

gen_sum_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "sum_ret_"

gen_prod_ret_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "prod_ret_"

   
         
(**/**)      
(**********************************************************************)

(**[map_access_to_expr mapn ins outs map_ret_t theta_vars_el init_expr_opt]

   Generates a K3 expression for accessing a map.
   ++param mapn          The name of the map being accessed.
   ++param ins           The input variables of the map access.
   ++param outs          The output variables of the map access.
   ++param map_ret_t     The type of the value associated with each tuple of 
                        the map.
   ++param theta_vars_el The variables that are in scope when accessing the map.
   ++param init_expr_opt The expression that should be used for initialization.
                        It is [Nothing] if no expression is provided.
   ++return              A tuple consisting of ::
                        - the schema of the collection resulting from 
                          the map access,
                        - a variable that can be used for binding against 
                          the value 
                          associated with each element of the map,
                        - the K3 expression for accessing the map. *)
map_access_to_expr mapn ins outs map_ret_t theta_vars_k init_expr_opt =
      
      let (ins_k::K.id_t list)  = List.map fst ins in
      let (outs_k::K.id_t list) = List.map fst outs in
      let free_vars_k    = ListAsSet.diff outs_k theta_vars_k in
      let bound_vars_k   = ListAsSet.diff outs_k free_vars_k in
      
      let ins_tl         = List.map (m3_type_to_k3_base_type)
                                    (List.map snd ins) in
      let outs_tl        = List.map (m3_type_to_k3_base_type)
                                    (List.map snd outs) in
      (* K3 type of the value associated with each tuple in the map *)
      let map_ret_kt = m3_type_to_k3_type map_ret_t in
      (* K3 type of the out tier of the map *)
      let (map_out_t::K.value_type_t) = 
         if outs_tl = [] then 
            m3_type_to_k3_type map_ret_t
         else 
            mk_k3_collection [] outs_tl (m3_type_to_k3_base_type map_ret_t)
      in
      
      let map_ret_ve = KH.mk_var (gen_map_ret_sym ()) in
      let map_out_ve = KH.mk_var (gen_slice_sym ()) in
      
      let type_map = (List.combine ins_k ins_tl) ++ 
                     (List.combine outs_k outs_tl) in
      let typed_var_pair v = 
              (v, KT.canonical (List.assoc v type_map))
      in
        
      
      (* Given a collection it slices it according to the bound variables *)
      (* and projects only the free variables *)
      let slice_and_project coll_ve :: K.expr_t =
         if free_vars_k = [] then
            error ("M3ToK3:: We shouldn't slice if all variables are bound."++
                   " We should Lookup instead.");
         if free_vars_k = outs_k then coll_ve
         else 
            KH.mk_map (project_fn ((List.map typed_var_pair outs_k)++
                                   [KU.id_of_var map_ret_ve, map_ret_kt])
                                  ((List.map typed_var_pair free_vars_k)++
                                   [KU.id_of_var map_ret_ve, map_ret_kt])) 
                      (mk_slice coll_ve outs_k bound_vars_k)
      in
      
      let expr = begin match ins, outs with
        | ([],[]) -> 
            (*No need to perform initial value computation. This should have *)
            (*already been initialized at system start-up.*) 
            KH.mk_peek (KH.mk_var mapn)
            
        | ([], y) ->       
            let map_expr = KH.mk_var mapn in
            if free_vars_k = [] then 
               if init_expr_opt = Nothing 
               then mk_lookup map_expr (KT.base_of map_ret_kt) outs_k outs_tl
               else 
                  let _,init_expr = extract_opt init_expr_opt in
                    KH.mk_if (mk_test_member map_expr outs_k 
                                             (List.map KT.canonical
                                                       outs_tl)
                                             map_ret_kt)
                             (mk_lookup map_expr (KT.base_of map_ret_kt) outs_k outs_tl)
                             init_expr
            else
               slice_and_project map_expr
                     
        | ( x,[]) -> 
            let map_expr = KH.mk_var mapn in
            if init_expr_opt = Nothing then mk_lookup map_expr (KT.base_of map_ret_kt) outs_k outs_tl
            else 
               let iv_e = KH.mk_var (gen_init_val_sym ()) in
               let _,init_expr = extract_opt init_expr_opt in   
               KH.mk_if (mk_test_member map_expr ins_k
                                        (List.map KT.canonical
                                                  ins_tl)
                                        map_ret_kt)
                        (mk_lookup map_expr (KT.base_of map_out_t) ins_k ins_tl)
                        (apply_lambda [KU.id_of_var iv_e, map_ret_kt] 
                                      [init_expr] iv_e)
                                 
        | ( x, y) ->
            let map_expr = KH.mk_var mapn in
                                    
            let out_access_expr coll_ve =   
               if free_vars_k = [] then  
                  (mk_lookup coll_ve (KT.base_of map_ret_kt) outs_k outs_tl)
               else
                  slice_and_project coll_ve
            in
            
            if init_expr_opt = Nothing then 
               apply_lambda [KU.id_of_var map_out_ve, map_out_t] 
                            [mk_lookup map_expr (KT.base_of map_out_t) ins_k ins_tl]
                            (out_access_expr map_out_ve)
            else 
               let init_outs_el,ie = extract_opt init_expr_opt in
               let init_outs_k = List.map fst init_outs_el in
               let iv_e = KH.mk_var (gen_init_val_sym ()) in   
               let init_expr, init_block = 
                  if ListAsSet.seteq init_outs_k outs_k then
                     ((if init_outs_k = outs_k then 
                          ie
                       else
                            (* Use projection to change the order of output 
                               variables from 'init_outs_el' to 'outs_el' *) 
                            KH.mk_map ( 
                               project_fn 
                                  ((List.map typed_var_pair init_outs_k) ++
                                   [KU.id_of_var map_ret_ve, map_ret_kt])
                                  ((List.map typed_var_pair outs_k) ++
                                   [KU.id_of_var map_ret_ve, map_ret_kt])
                            ) ie
                     ),(
                               KH.mk_block [ 
                                  mk_update map_expr (KT.base_of map_ret_kt) ins_k ins_tl [] [] iv_e;
                                  out_access_expr iv_e
                               ]
                            )
                     )
                  else if not (ListAsSet.seteq init_outs_k free_vars_k) then
                     (error ("Initialization expressions that span more " ++
                             "than the free vars of the expression are not "++
                             "supported for full pcs."))
                  else
                     (error ("Initialization expressions that span the " ++
                             "free vars of the expression are not supported " ++
                             "for full pcs."))
               in
                  KH.mk_if (mk_test_member map_expr ins_k
                                           (List.map KT.canonical
                                                     ins_tl)
                                           (map_out_t))
                           (apply_lambda [KU.id_of_var map_out_ve, map_out_t]
                                         [mk_lookup map_expr (KT.base_of map_out_t) ins_k ins_tl]
                                         (out_access_expr map_out_ve))
                           (apply_lambda [KU.id_of_var iv_e, map_out_t]
                                         [init_expr]
                                         init_block)
      end
      in (List.map (\(name,vtype) -> (name, K.TValue(vtype)))
                   (List.map typed_var_pair free_vars_k), map_ret_ve, expr)

      
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(**[value_to_k3_expr value_calc]

   Converts a value ring expression into a K3 expression.
   ++param value_calc The value ring expression being translated.
   ++return           The [value_calc] translated into K3 as a tuple
                     containing ::
                     - the K3 type of the expression,
                     - the K3 expression itself. *)
rec value_to_k3_expr (value_calc :: V.expr_t) :: K.type_t * K.expr_t =
   let bin_fn op_fn r1 c2 =
      let ret1_t, e1 = r1 in
      let ret2_t, e2 = value_to_k3_expr c2 in
      if not ((numerical_type ret1_t) && (numerical_type ret2_t)) then
         error ("M3ToK3:: ValueRing.Sum or Prod arguments should be of "++
                  "numerical type.");
      let ret_t = arithmetic_return_types ret1_t ret2_t
      in 
      ret_t, (op_fn e1 e2)
   in   
   begin match value_calc with
      | V.Val(AConst(i)) -> (K.TValue(m3_type_to_k3_type 
                                              (Const.type_of_const i))), 
                            m3_const_to_k3_const i
      | V.Val(AVar(v,t)) -> (K.TValue(m3_type_to_k3_type t)),
                            KH.mk_var v
      | V.Val( AFn(fn,fargs,ftype) ) -> 
            let ret_t = (K.TValue(m3_type_to_k3_type ftype)) in
            let te_l = List.map value_to_k3_expr fargs in 
            ret_t, apply_external_lambda fn te_l ret_t
      | V.Neg( neg_arg ) -> 
            let neg_t, neg_e = value_to_k3_expr neg_arg in
            let neg_cst = begin match base_of_any neg_t with
               | K.TInt
               | K.TFloat -> KH.mk_const (K.CInt(-1))
               | _ -> error "M3ToK3:: Negation type should be int or float." 
            end in
            (neg_t, KH.mk_mult neg_cst neg_e)
      | V.Sum( c1:sum_args_tl )   ->
          foldl' (bin_fn KH.mk_add) (value_to_k3_expr c1) sum_args_tl
      | V.Prod( c1:prod_args_tl )   ->     
          foldl' (bin_fn KH.mk_mult) (value_to_k3_expr c1) prod_args_tl
      | _  -> error "M3ToK3:: Empty V.Sum or V.Prod."
   end



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
type meta_t = ((string * (string list)) list)
empty_meta = ([],[])

(**/**)
meta_has_init_keys meta init_keys = 
      List.mem init_keys meta

meta_append_init_keys meta init_keys =
      meta++[init_keys]
(**/**)   
       
(**********************************************************************)


(**[calc_to_k3_expr meta generate_init theta_vars calc]

   Converts a calculus ring expression into a K3 expression.
   ++param meta       Metadata associated with the translation process. See 
                     [meta_t] definition above.   
   ++param generate_init Flag specifying whether to generate initial value
                        computation or not. By default is set to False.
   ++param theta_vars The variables that are in scope when evaluating the
                     expression.
   ++param calc       The calculus expression being translated.
   ++return           The [calc] translated into K3 and the updated meta. 
                     For the K3 result we return a tuple containing ::
                     - the schema of the K3 expression as a list of K3
                       variables. This can be used for binding against 
                       the keys of the collection represented by the
                       expression. This schema will contain only unbound
                       variables, all bound variables get projected away.
                     - a variable that can be used for binding against 
                       the value associated with each tuple in the collection
                       represented by the expression.
                     - the K3 expression itself *)
rec calc_to_k3_expr meta ?(generate_init = False) theta_vars_k calc :: 
            (((K.id_t * K.type_t) list * 
              (K.expr_t * K.type_t) * 
              K.expr_t) * meta_t) =
   let rcr        = calc_to_k3_expr meta  ~generate_init::generate_init
                                          theta_vars_k
   in
   let rcr2 meta2 = calc_to_k3_expr meta2 ~generate_init::generate_init
                                          theta_vars_k
   in
   
   let (ins,outs) = schema_of_expr calc in
   let ins_k = List.map fst ins in
   if (ListAsSet.diff ins_k theta_vars_k) != [] then
      (print_endline ("Expr:: \n"++(CalculusPrinter.string_of_expr calc));
       print_endline ("\ninput_vars:: "++(String.concat ", " ins_k));
       print_endline ("scope_vars:: "++(String.concat ", " theta_vars_k));
       error ("M3ToK3:: Error:: All inputs of calculus expression " ++ 
              "should be bound!"));
                   
   
   let cmp_fn op_fn c1 c2 =
      let ((ret1_t, e1),(ret2_t, e2)) = pair_map value_to_k3_expr (c1,c2) in
      let out_type = K.TValue(KT.canonical K.TInt) in
      let correct_type = escalate_expr out_type in
      if not (compatible_types ret1_t ret2_t) then
         error ("M3ToK3:: Error:: Incompatible argument types for " ++
                "comparison operation:: "++
                (KP.string_of_type ret1_t)++" <> "++(KP.string_of_type ret2_t));
      (([], (KH.mk_var (gen_cmp_ret_sym ()), out_type),
        KH.mk_if (op_fn (correct_type e1) (correct_type e2))
                 (KH.mk_const (K.CInt(1)))
                 (KH.mk_const (K.CInt(0)))
       ), meta)
   in
   let (k3_out_el, k3_ret_v, k3_expr), k3_meta = 
     begin match calc with
      | C.Val( calc_val ) -> begin match calc_val with
         | Value( calc_val_value ) ->
               let ret_t, expr = value_to_k3_expr calc_val_value in
               ([], (KH.mk_var (gen_val_ret_sym ()), ret_t), expr), meta
         | Cmp( T.Eq, c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_eq  e1 e2) c1 c2
         | Cmp( T.Lt, c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_lt  e1 e2) c1 c2
         | Cmp( T.Lte,c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_leq e1 e2) c1 c2
         | Cmp( T.Gt, c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_lt  e1 e2) c2 c1
         | Cmp( T.Gte,c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_leq e1 e2) c2 c1
         | Cmp( T.Neq,c1, c2 ) -> cmp_fn (\e1 e2 -> KH.mk_neq e1 e2) c1 c2
         
         | Rel(reln, rel_schema) -> 
            let rel_outs_el, rel_ret_ve, expr = 
                  map_access_to_expr reln [] rel_schema 
                                     T.TInt theta_vars_k Nothing 
            in
               ((rel_outs_el, (rel_ret_ve, K.TValue(KH.t_int)), expr), meta)
         
         | External(mapn, eins, eouts, ext_type, einit_calc_opt) ->
            (* init_expr_opt will contain if required the schema of the
               initialization expression and the expression itself. *)
            let eouts_k = List.map fst eouts in
            let free_eouts_el = ListAsSet.diff eouts_k theta_vars_k in
               
            let get_init_expr init_calc_opt = 
               let init_calc = extract_opt init_calc_opt in
               
               if eins = [] && (eouts = [] || free_eouts_el <> []) then
                  (print_endline ("External:: "++
                                  (CalculusPrinter.string_of_expr calc));
                  error ("M3ToK3:: No initialization should be required for "++
                         "singleton maps or slice accesses of out maps."));
               
               let init_theta_vars_k = 
                  if eins <> [] && eouts <> [] then 
                     ListAsSet.diff theta_vars_k eouts_k
                  else
                     theta_vars_k
               in
               let (init_outs_el, (init_ret_ve,init_ret_vt), init_expr), nm_1 = 
                  calc_to_k3_expr meta ~generate_init::generate_init 
                                       init_theta_vars_k init_calc 
               in
               let init_outs_k = List.map fst init_outs_el in
               if not (eins <> [] && eouts <> []) &&
                  not (ListAsSet.seteq free_eouts_el init_outs_k) then
                  (print_endline ("External:: "++
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3:: Schema of intialization expression should " ++
                       "coincide with the free out vars of the map access."));
               if eins <> [] && eouts <> [] &&
                  (ListAsSet.diff free_eouts_el init_outs_k) <> [] then
                  (print_endline ("External:: " ++
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3:: Schema of intialization expression should " ++
                       "include the free out vars of the map access."));
               if eins <> [] && eouts <> [] &&
                  (ListAsSet.diff init_outs_k eouts_k) <> [] then
                  (print_endline ("External:: " ++ 
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3:: Schema of intialization expression should " ++
                       "be included in the out vars of the map access."));
               let _ = escalate_type ~expr::(Just(calc)) 
                                     (init_ret_vt)
                                     (K.TValue(m3_type_to_k3_type ext_type))
               in
                  nm_1,Just(init_outs_el,init_expr)
            in
            let nm, init_expr_opt =
               if not generate_init then meta, Nothing
               else
                  if einit_calc_opt != Nothing then
                     get_init_expr einit_calc_opt
                  else
                     let default_init = 
                        if eins = [] && free_eouts_el = [] then 
                           Just([],init_val_from_type 
                                      (m3_type_to_k3_type ext_type))
                        else 
                           if (Debug.active "DEBUG-DM") || 
                              (Debug.active "M3TOK3-INIT-FOR-INS") then
                              Just([],init_val_from_type 
                                      (m3_type_to_k3_type ext_type))
                           else
                              Nothing
                     in                  
                     meta, default_init               
              in                 
              let (map_outs_el, map_ret_ve, expr) = 
                 map_access_to_expr mapn eins eouts ext_type 
                                    theta_vars_k init_expr_opt 
              in
                 ((map_outs_el, (map_ret_ve, 
                                 (K.TValue(m3_type_to_k3_type ext_type))),
                  expr), nm)
               
         | AggSum( agg_vars0, aggsum_calc ) ->
            (* Convert group by variables to K3 variables and eliminate 
               those that are bound. *)
            let agg_vars0_el = 
               ListAsSet.diff (List.map fst agg_vars0) theta_vars_k
            in
            
            let (aggsum_outs_el,ret_ve,aggsum_e),nm = rcr aggsum_calc in
            let aggsum_outs_k = List.map fst aggsum_outs_el in
            
            (* Make sure that all the group by variables are included in 
               the output schema of the "aggsum" expression. *)
            if not ( (ListAsSet.diff agg_vars0_el aggsum_outs_k) = []) then
               (print_endline(CalculusPrinter.string_of_expr calc);
               print_endline("GB vars:: "++(String.concat ", " agg_vars0_el));
               print_endline("AggSum outs:: "++
                             (String.concat ", " aggsum_outs_k));
               error 
                  ("M3ToK3:: The group by variables of aggsum should " ++ 
                   "be included in the schema of the aggsum expression."));
            let agg_vars_k = ListAsSet.inter agg_vars0_el aggsum_outs_k in
            let agg_vars_el = List.map (\elem -> 
                (elem, List.assoc elem aggsum_outs_el)
              ) agg_vars_k
            in
                           
            let mk_val_type_pair = 
                List.map (\(x,t) -> (x, extract_value_type t))
            in
            let agg_fn = aggregate_fn (mk_val_type_pair aggsum_outs_el)
                                      (KU.id_of_var (fst ret_ve), 
                                       extract_value_type (snd ret_ve))
            in
            let expr = 
              if aggsum_outs_el = [] then aggsum_e
              else if agg_vars_k = [] then
                KH.mk_agg agg_fn (init_val_from_full_type (snd ret_ve))
                                 aggsum_e
              else
                let gb_fn = 
                    project_fn ((mk_val_type_pair aggsum_outs_el)++
                                [KU.id_of_var (fst ret_ve),
                                 extract_value_type (snd ret_ve)]) 
                                (mk_val_type_pair agg_vars_el)
                in
                let gb_aggsum_e = 
                (* 
                   K.Slice(aggsum_e, k3_expr_to_k3_idType aggsum_outs_el,[]) 
                *)
                   aggsum_e
                in
                let agg_expr = 
                  KH.mk_gbagg gb_fn
                              agg_fn 
                              (init_val_from_full_type (snd ret_ve))
                              gb_aggsum_e
                in
                let flatten_fn = 
                  KH.mk_lambda (K.ATuple [
                    KH.wrap_args (mk_val_type_pair agg_vars_el);
                    K.AVar(KU.id_of_var (fst ret_ve), 
                           extract_value_type (snd ret_ve));
                  ])
                  (KH.mk_tuple (
                    (List.map (\(x,_) -> KH.mk_var x) agg_vars_el) ++
                    [fst ret_ve]
                  ))
                in
                  KH.mk_map flatten_fn agg_expr
            in
            ((agg_vars_el, ret_ve, expr), nm)
            
         | Lift(lift_v, lift_calc)          -> 
            let (lift_outs_el,lift_ret_ve,lift_e),nm = rcr lift_calc in
            let is_bound = List.mem (fst lift_v) theta_vars_k in
            (* If the variable is already bound, this lift should be treated as
               an equality predicate and we can leave its type in place.  If it
               isn't bound, we escalate the variable's type based on the lifted
               expression. *)
           let lift_vt =
              if is_bound then m3_type_to_k3_type (snd lift_v)
              else extract_value_type 
                      (escalate_type ~expr::(Just(calc))
                          (snd lift_ret_ve)
                          (K.TValue(m3_type_to_k3_type (snd lift_v))))
           in
            let lift_ve = KH.mk_var (fst lift_v) in
            let extra_ve, ret_ve, lift_body = 
               if is_bound then 
                  [], KH.mk_var (gen_lift_ret_sym ()),
                  (exprs_to_tuple ((List.map (\(x,_) -> KH.mk_var x)
                                             lift_outs_el) ++
                                    [ KH.mk_if
                                        (KH.mk_eq (fst lift_ret_ve) lift_ve)
                                        (KH.mk_const (K.CInt(1)))
                                        (KH.mk_const (K.CInt(0)))
                                    ]
                                  ))
               else
                  [KU.id_of_var lift_ve, K.TValue(lift_vt)], 
                  KH.mk_var (gen_lift_ret_sym ()),
                  (exprs_to_tuple ((List.map (\(x,_) -> KH.mk_var x)
                                             lift_outs_el) ++
                                    [fst lift_ret_ve;
                                     KH.mk_const (K.CInt(1))]))
            in
            let lift_lambda = 
              lambda (List.map (\(expr,ty) -> (expr, extract_value_type ty))
                               (lift_outs_el++
                                [KU.id_of_var (fst lift_ret_ve),
                                (snd lift_ret_ve)])) lift_body
            in
            let expr = apply_lambda_to_expr lift_lambda K.TInt lift_e   
            in   
               ((lift_outs_el++extra_ve, (ret_ve,K.TValue(KH.t_int)), expr), nm)
            
         (***** BEGIN EXISTS HACK *****)
         
         | Exists(exists_calc)          -> 
            let (exists_outs_el,exists_ret_ve,exists_e),nm = rcr exists_calc in
            let ret_ve = KH.mk_var (gen_exists_ret_sym ()) in
            let exists_body = 
               (exprs_to_tuple ((List.map (\(x,_) -> KH.mk_var x)
                                          exists_outs_el) ++ 
                                [ KH.mk_if 
                                    (KH.mk_neq (fst exists_ret_ve)
                                               (KH.mk_const (zero_of_type
                                                  (KT.base_of 
                                                    (extract_value_type
                                                      (snd exists_ret_ve))))))
                                    (KH.mk_const (K.CInt(1)))
                                    (KH.mk_const (K.CInt(0)))
                                ])) 
            in               
            let exists_lambda = 
                  lambda (List.map (\(x,xt) -> 
                                      (x, extract_value_type xt))
                                   (exists_outs_el ++
                                    [KU.id_of_var (fst exists_ret_ve),
                                     (snd exists_ret_ve)]))
                         exists_body
            in
            let expr = apply_lambda_to_expr exists_lambda K.TInt exists_e in   
               ((exists_outs_el, (ret_ve, K.TValue(KH.t_int)), expr), nm)
            
         (***** END EXISTS HACK *****)

      end
         
    | C.Sum( sum_args )   ->
         (* Compute the schema of the resulting sum expression *)
         let outs_k = ListAsSet.diff (List.map fst outs) 
                                      theta_vars_k in
         let outs_el = List.map (\x -> (x, K.TValue(m3_type_to_k3_type 
                                                (List.assoc x outs)))) outs_k in

         (* Translate all terms of the sum int K3 and make sure their schema *)
         (* corresponds to the sum expression's schema and that their return *)
         (* values have numerical types *)
         let prepare_fn old_meta old_ret_t c = 
            let (e_outs_el,(e_ret_ve,e_ret_vt),e),new_meta = rcr2 old_meta c in
            let e_outs_k = (List.map fst e_outs_el) in
            if not (ListAsSet.seteq e_outs_k outs_k) then
               (print_endline("Expression::\n" ++
                              CalculusPrinter.string_of_expr calc);
                print_endline("Scope:: "++String.concat ", " theta_vars_k);
                print_endline("Output vars:: "++String.concat ", " e_outs_k);
                print_endline("Sum output vars:: "++String.concat ", " outs_k);
               error 
                  ("M3ToK3:: The schema of a sum term should " ++ 
                   "be the same as the schema of the entire sum."));
            let new_outs_el, new_e =
               if e_outs_k <> outs_k then
                  (* If using combine, the sum terms must have the exact 
                    same schema (same vars in the same order) *)
                  outs_el, KH.mk_map (project_fn (List.map tvar_to_vtvar
                                                   (e_outs_el++
                                                      [KU.id_of_var e_ret_ve,
                                                         e_ret_vt]))
                                                 (List.map tvar_to_vtvar
                                                   (outs_el++
                                                     [KU.id_of_var e_ret_ve,
                                                        e_ret_vt]))) e
               else 
                  e_outs_el, e
            in
            let new_ret_t = arithmetic_return_types old_ret_t e_ret_vt in
            let e_txt = (CalculusPrinter.string_of_expr c) in
               ((new_outs_el,e_ret_ve,new_e,e_txt),new_meta,new_ret_t)
         in
                     
         let nm,ret_t,sum_exprs = 
            foldl' (\(old_meta,old_ret_t,old_el) c -> 
               let result,new_meta,new_ret_t = 
                  prepare_fn old_meta old_ret_t c 
               in
               new_meta,new_ret_t,old_el++[result] ) 
               (meta,(K.TValue(KH.t_int)),[]) 
               sum_args 
         in
                     
         let ret_ve = KH.mk_var (gen_sum_ret_sym ()) in
         let (hd_outs_el,hd_ret_ve,hd_s,hd_txt) = List.hd sum_exprs in
         let sum_exprs_tl = List.tl sum_exprs in
         let sum_expr_terms = List.map (\(_,_,x,_) -> x) sum_exprs in
         
         let sum_result = if outs_el <> [] then 
            begin match sum_expr_terms with 
              | [] -> failwith "empty combine"
              | hd:rest -> foldl' KH.mk_combine hd rest
            end
         else
            let sum_fn sum_e (s_outs_el,s_ret_ve,s,_) = 
               KH.mk_add sum_e s
            in
              (foldl' sum_fn hd_s sum_exprs_tl)
         in
         ((outs_el, (ret_ve, ret_t), sum_result), nm)            
    
    | C.Prod( prod_args )   ->   
       (* Translate all terms of the product int K3 and make sure *)
       (* their return values have numerical types *)
       let prepare_fn (old_meta, old_scope) c = 
          let (e_outs_el,e_ret_ve,e),new_meta = 
             calc_to_k3_expr old_meta ~generate_init::generate_init 
                             old_scope c 
          in
          let new_scope = ListAsSet.union old_scope (List.map fst e_outs_el) in
          ((e_outs_el,e_ret_ve,e),(new_meta,new_scope))
       in
       let (nm,_),prod_exprs = 
          foldl' (\(old_extra,old_el) c -> 
             let result,new_extra = prepare_fn old_extra c in
             new_extra,old_el++[result]
          ) ((meta,theta_vars_k),[]) prod_args 
       in
         
       let prod_expr_hd = List.hd prod_exprs in
       let prod_exprs_tl = List.tl prod_exprs in
         
       let prod_fn (p1_outs_el,((p1_ret_ve::K.expr_t),p1_ret_t),p1) 
                   (p2_outs_el,((p2_ret_ve::K.expr_t),p2_ret_t),p2) =
         let ret_ve = KH.mk_var (gen_prod_ret_sym ()) in
         let ret_vt = arithmetic_return_types p1_ret_t p2_ret_t in
         let p_outs_el,p = 
            begin match p1_outs_el,p2_outs_el with
               | [],[] -> [], KH.mk_mult p1 p2
               |  _,[] -> p1_outs_el,
                        KH.mk_map (lambda (List.map tvar_to_vtvar
                                          (p1_outs_el++[KU.id_of_var p1_ret_ve,
                                                       p1_ret_t]))
                                    (KH.mk_tuple ((List.map (\(x,_) ->
                                                              KH.mk_var x)
                                                            p1_outs_el) ++ 
                                                  [KH.mk_mult p1_ret_ve p2])))
                                  p1
               | [], _ -> p2_outs_el,
                        KH.mk_map (lambda (List.map tvar_to_vtvar
                                          (p2_outs_el++[KU.id_of_var p2_ret_ve,
                                                       p2_ret_t]))
                                    (KH.mk_tuple ((List.map (\(x,_) ->
                                                              KH.mk_var x)
                                                            p2_outs_el) ++
                                                  [KH.mk_mult p2_ret_ve p1])))
                                  p2
               |  _, _ -> 
                  let union_el = (ListAsSet.union p1_outs_el p2_outs_el) in
                  let prod_e = KH.mk_tuple ((List.map (\(x,_)->KH.mk_var x)
                                                      union_el) ++  
                                             [KH.mk_mult p1_ret_ve p2_ret_ve])
                  in
                  let nested = KH.mk_map (lambda (List.map tvar_to_vtvar
                                                    (p2_outs_el ++ 
                                                      [KU.id_of_var p2_ret_ve,
                                                       p2_ret_t]))
                                               prod_e) p2
                  in union_el, 
                     KH.mk_flatten (
                        KH.mk_map (lambda (List.map tvar_to_vtvar
                                            (p1_outs_el ++ 
                                              [KU.id_of_var p1_ret_ve,
                                               p1_ret_t])) nested)
                                  p1
                     )
            end
         in
            (p_outs_el, (ret_ve, ret_vt), p)
       in
         
          let prod_result = 
            foldl' prod_fn prod_expr_hd prod_exprs_tl 
         in (prod_result, nm)
            
      | C.Neg( neg_arg ) ->
         rcr (CalcRing.mk_prod( [ Calculus.mk_value (Arithmetic.mk_int(-1));
                                  neg_arg] ))
   end 
   in (*
     print_endline ("Converting:: " ++ (Calculus.string_of_expr calc));
     print_string ("Into:: " ++ (KP.string_of_expr k3_expr));
     print_endline ("Type:: " ++ (KP.string_of_type (snd k3_ret_v)));
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
   ++param meta      Metadata associated with the translation process. 
                    See [meta_t] definition above.   
   ++param generate_init Flag specifying whether to generate initial 
                        value computation or not. By default is set to False.
   ++param m3_stmt   The M3 statement being translated.
   ++param trig_args The arguments of the trigger to which this statement
                    belongs to.
   ++return          The [m3_stmt] translated into K3 and the updated meta. *)
m3_stmt_to_k3_stmt (meta:: meta_t) ?(generate_init = False) 
                       trig_args (m3_stmt:: Plan.stmt_t):: 
                       K.expr_t * meta_t =
  let (mapn, lhs_ins, lhs_outs, map_type, init_calc_opt) = 
    Plan.expand_ds_name m3_stmt.Plan.target_map 
  in
  let {Plan.update_type = update_type; Plan.update_expr = incr_calc} = 
    m3_stmt
  in 
  (* all lhs input variables must be free, they cannot be bound 
    by trigger args *)
  if not ((ListAsSet.inter trig_args  lhs_ins) = []) then
    (print_endline ("Trigger Arguments:: "++
                    (T.string_of_vars ~verbose::True trig_args));
     print_endline ("Lhs Input Variables:: " ++
                    (T.string_of_vars ~verbose::True lhs_ins));
     error ("M3ToK3:: All lhs input variables must be free. "++
            "They cannot be bound by trigger args."));

   (* K3 collection used for storing the result of the statement.*)
  let lhs_collection  = map_to_expr mapn lhs_ins lhs_outs map_type in
  let map_k3_type = m3_type_to_k3_base_type map_type in
  let lhs_outs_kt = List.map (\(_,x)->m3_type_to_k3_base_type x) lhs_outs in
  let lhs_ins_kt  = List.map (\(_,x)->m3_type_to_k3_base_type x) lhs_ins in
  let out_tier_t = mk_k3_collection [] lhs_outs_kt map_k3_type in
  
  let lhs_ins_el  = List.map (\(x,_)->KH.mk_var x) lhs_ins in
  let lhs_outs_el = List.map (\(x,_)->KH.mk_var x) lhs_outs in
  
  let trig_args_el=List.map (\(x,_)->KH.mk_var x) trig_args in
  
  let trig_w_ins_el = ListAsSet.union trig_args_el lhs_ins_el in
  
  let existing_out_tier = KH.mk_var "existing_out_tier" in
  
  (* Determine the current value of the entry in "existing_out_tier" *)
  (* corresponding to a particular output variables key. Used for *)
  (* updating the "existing_out_tier".*)
  (* If this is a replace statement then we'll consider zero as the *)
  (* existing value to be updated. *)
  (* If this is an update statement then we do one of the following:: *)
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
      if update_type = Plan.ReplaceStmt then 
         (init_val_from_type (m3_type_to_k3_type map_type))
      else if lhs_outs_el = [] then 
         KH.mk_peek (existing_out_tier)
      else
         let init_expr_opt = 
            if not generate_init then Nothing
            else if init_calc_opt != Nothing then (
               failwith "IVC code not yet implemented."
            )
            else if lhs_ins_el = [] then
               Just(init_val_from_type (m3_type_to_k3_type map_type))
            else 
               Nothing
         in 
         if (init_expr_opt != Nothing) then 
            KH.mk_if 
              (mk_test_member existing_out_tier 
                              (var_ids lhs_outs_el)
                              (List.map KT.canonical lhs_outs_kt)
                              (KT.canonical map_k3_type))
              (mk_lookup existing_out_tier map_k3_type (var_ids lhs_outs_el) lhs_outs_kt)
              (extract_opt init_expr_opt)
         else 
           (mk_lookup existing_out_tier map_k3_type (var_ids lhs_outs_el) lhs_outs_kt)
   in
   
   (* Translate the rhs calculus expression into a k3 expression and its *)
   (* corresponding schema. Beside the trigger variables we also have *)
   (* the input variables of the "lhs_collection" in scope as we will *)
   (* update "lhs_collection" while iterating over all lhs input variables. *)
   let (rhs_outs_el_and_ty, (rhs_ret_ve, rhs_ret_vt), incr_expr), nm =
      if not(Debug.active "DEBUG-DM-WITH-M3") then
         calc_to_k3_expr meta ~generate_init::(generate_init) 
                         (var_ids trig_w_ins_el) incr_calc
      else
         let (rhs_outs_el, rhs_ret_ve, incr_expr), nm = 
            calc_to_k3_expr meta ~generate_init::(True) 
                            (var_ids trig_w_ins_el) incr_calc
        in
        (rhs_outs_el, rhs_ret_ve, incr_expr), nm
   in
   let (rhs_outs_ids, rhs_outs_t) = List.split rhs_outs_el_and_ty in
   let (rhs_outs_el) = List.map KH.mk_var rhs_outs_ids in
   
   (* Make sure that the lhs collection and the incr_expr have *)
   (* the same schema. *)
   let free_lhs_outs = ListAsSet.diff (List.map fst lhs_outs) (List.map fst trig_args) in
   if not (ListAsSet.seteq free_lhs_outs rhs_outs_ids) then
      (print_endline ("Stmt:: "++
                      (Plan.string_of_statement m3_stmt));
       print_endline ("Trigger Variables:: "++
                      (String.concat ", " (List.map fst trig_args)));
       print_endline ("Lhs Output Variables:: "++
                      (String.concat ", " free_lhs_outs));
       print_endline ("Rhs Output Variables:: "++
                      (String.concat ", " rhs_outs_ids));
       error ("M3ToK3:: The lhs and rhs must have the same set " ++ 
              "of free out variables. "));
   let _ = escalate_type rhs_ret_vt (K.TValue(KT.canonical map_k3_type)) in
   let free_lhs_outs_el = List.map KH.mk_var free_lhs_outs in
      
   let vart_to_idvt = 
     List.map (\(x,xt) -> (KU.id_of_var x,extract_value_type xt)) 
   in
   (* Iterate over all the tuples in "incr_expr" collection and update *)
   (* the lhs_collection accordingly. *)
   let coll_update_expr =   
      let single_update_expr = 
        mk_update lhs_collection map_k3_type (var_ids lhs_ins_el) (lhs_ins_kt) (var_ids lhs_outs_el) lhs_outs_kt 
                  (KH.mk_add existing_v rhs_ret_ve)
      in
      let inner_loop_body = 
        lambda (vart_to_idvt ((List.combine rhs_outs_el rhs_outs_t) ++ 
                              [rhs_ret_ve, rhs_ret_vt]))
               single_update_expr
      in
      let update_body =
        if rhs_outs_el = [] then KH.mk_apply inner_loop_body incr_expr
        else                     mk_iter inner_loop_body incr_expr
      in
          
      if ( update_type = Plan.UpdateStmt || 
           free_lhs_outs_el = [] ||
           Debug.active "UNSAFE-REPLACE" ) then
        update_body        
      else
        let old_slice = (
          if ListAsSet.seteq lhs_outs_el free_lhs_outs_el
          then existing_out_tier
          else (
            let bound_out_names = 
              ListAsSet.diff (var_ids lhs_outs_el) 
                             (var_ids free_lhs_outs_el)
            in
              mk_slice existing_out_tier (var_ids lhs_outs_el) bound_out_names
          )
        ) in
          KH.mk_block [
            mk_iter
              (lambda ((List.combine (List.map KU.id_of_var lhs_outs_el)
                                     (List.map KT.canonical lhs_outs_kt))++
                       [KU.id_of_var rhs_ret_ve, 
                        extract_value_type rhs_ret_vt])
                      (KH.mk_delete lhs_collection 
                                    (KH.mk_tuple (lhs_outs_el++
                                                  [rhs_ret_ve]))))
              old_slice;
           update_body
         ]
    in
   
   (* In order to implement a statement we iterate over all the values *)
   (* of the input variables of the lhs collection, and for each of them *)
   (* we update the corresponding output tier. *)
   let statement_expr =
      let outer_loop_body = 
        lambda ((List.combine (List.map KU.id_of_var lhs_ins_el)
                              (List.map KT.canonical lhs_ins_kt))++
                [KU.id_of_var existing_out_tier, out_tier_t]) 
               coll_update_expr 
      in
      if lhs_ins_el = [] 
      then KH.mk_apply outer_loop_body lhs_collection
      else mk_iter outer_loop_body lhs_collection
   in
      (statement_expr, nm)
;;
(**[m3_trig_to_k3_trig meta generate_init m3_trig]

   Transforms a M3 trigger into a K3 trigger. 
   ++param meta    Metadata associated with the translation process. 
                  See [meta_t] definition above.   
   ++param generate_init Flag specifying whether to generate initial value 
                        computation or not. By default is set to False.
   ++param m3_trig The M3 trigger being translated.
   ++return        The [m3_trig] translated into K3 and the updated meta.
*)
m3_trig_to_k3_trig ?(generate_init = False) 
                       (schema_env::(K.id_t * K.type_t) list)
                       (m3_trig:: M3.trigger_t):: 
                       K.flow_program_t =
   if !(m3_trig.M3.statements) = [] then [] else
   let trig_args = Schema.event_vars m3_trig.M3.event in
(*   let trig_types = List.map (\(vn,vt) -> 
                                    (vn, K.TValue(m3_type_to_k3_type vt)))
                             trig_args in*)
   let k3_trig_stmts, new_meta = 
      foldl' 
         (\(old_stms,om) m3_stmt -> 
            let k3_stmt, nm = 
               m3_stmt_to_k3_stmt om ~generate_init::generate_init 
                                  trig_args m3_stmt 
            in 
            (*let typed_k3_stmt = *)
              (*KT.deduce_expr_type trig_types schema_env k3_stmt*)
            (*in*)
              (*(old_stms++[typed_k3_stmt], nm) )*)
              (old_stms++[k3_stmt], nm) )
         ([],[])
      !(m3_trig.M3.statements) 
   in
    [K.Sink(K.Code(
         begin match m3_trig.M3.event with
(*           | Schema.SystemInitializedEvent -> "at_init"*)
           | _ -> Schema.name_of_event m3_trig.M3.event
         end,
        (K.ATuple(
          List.map (\(vn,vt) -> K.AVar(vn, m3_type_to_k3_type vt)) trig_args
        )),
        [],
        KH.mk_block k3_trig_stmts
      )), []]


csv_adaptor_to_k3 (name_prefix:: string)
                      (rel:: Schema.rel_t) 
                      (params:: (string * string) list):: 
                        (K.flow_statement_t * K.type_t) =
  let del_var = "__m3_is_deletion" in
  let param p d = 
    if List.mem_assoc p params then List.assoc p params else d
  in
  let (reln, relv, _) = rel in
  let with_deletions = (param "deletions" "False") = "True" in
  let args = 
    List.map 
      (\(vn,vt) -> (vn, 
        let input_type = match vt with T.TDate -> T.TString | _ -> vt in
          m3_type_to_k3_type input_type
      ))
      (if with_deletions then (del_var, T.TInt) : relv else relv)
  in
  let child_params = 
    KH.mk_tuple 
      (List.map (\(vn, vt) -> 
        match vt with
          | T.TDate -> KH.mk_apply (KH.mk_var "parse_sql_date") (KH.mk_var vn)
          | _ -> (KH.mk_var vn)
      ) relv)
  in
  let send_to_event evt = 
    KH.mk_send (KH.mk_const (K.CTarget(Schema.name_of_event evt)))
               (KH.mk_var "me")
               (child_params)
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
        name_prefix ++ reln,
        lambda_args args,
        [],
        k3_code
      ))),
      K.TValue(KU.value_type_of_arg (lambda_args args))
    )
;;

known_adaptors = ref [
  "csv", csv_adaptor_to_k3
];;

add_adaptor adaptor = known_adaptors ::= adaptor : !known_adaptors;;

(**[m3_to_k3 generate_init m3_program]

   Transforms a M3 program into a K3 program. 
   ++param generate_init Flag specifying whether to generate initial value 
                        computation or not. By default is set to False.
   ++param  m3_program The M3 program being translated.
   ++return The [m3_program] translated into K3.
*)
m3_to_k3 ?(generate_init = False) ?(role = "client") 
             (m3_program:: M3.prog_t):: (K.program_t) =
  let {M3.maps = m3_prog_schema; M3.triggers = m3_prog_trigs;
       M3.queries = m3_prog_tlqs; M3.db = m3_database } = m3_program in
  let k3_prog_schema = List.map m3_map_to_k3_map !m3_prog_schema in
  let k3_prog_env = List.map (function 
      | K.Global(id, ty, _) -> (id, ty)
      | _ -> failwith "invalid map declaration"
    ) k3_prog_schema
  in
  let k3_prog_trigs = 
    List.flatten (
      List.map (m3_trig_to_k3_trig ~generate_init::generate_init k3_prog_env)
                  !m3_prog_trigs
     )
  in
  let _  = List.map (\(qname,qexpr) -> 
                        match qexpr with 
                          | C.Val(Calculus.External(en,_,_,_,_)) -> (qname, en)
                          | _ -> failwith "Complex TLQs presently unsupported"
                      ) !m3_prog_tlqs
  in
  let (table_rels, stream_rels) = 
    Schema.partition_sources_by_type m3_database
  in
  if table_rels <> []
    then failwith "Table relations presently unsupported"
  else
  let source_id = ref 0 in
  let next_source () = 
    source_id ::= !source_id + 1; ("s"++(string_of_int !source_id))
  in
  let (k3_prog_demux_calls, k3_prog_demux_deep) = 
    List.split (List.map (\(source, adaptors) ->
      let (demuxes, demux_types) = 
        List.split (List.map (\((adaptor, params), rel) ->
          if not (List.mem_assoc adaptor !known_adaptors)
          then failwith ("Unknown adaptor "++adaptor)
          else ((List.assoc adaptor !known_adaptors) "demux_" rel params)
        ) adaptors)
      in
      let source_type = 
        match (foldl' (function
          | Nothing -> (\x -> Just(x))
          | Just(s) -> (\x ->
            if x <> s then failwith "Mismatched adaptor schemas" else Just(x)
          )
        ) Nothing demux_types) with
          | Nothing -> failwith "Each source must have an adaptor"
          | Just(s) -> s
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
          List.map (\(_, (reln, _, _)) ->
            K.Bind(source_id, "demux_"++reln)
          ) adaptors,
          K.Instruction(K.Consume(source_id))
        ),
        demuxes
      )
    ) stream_rels)
  in
  let k3_prog_demux = List.map (\x -> x, []) (List.flatten k3_prog_demux_deep) in
  let (k3_prog_sources, k3_prog_bindings, k3_prog_consumes) = 
    foldr (\(s,b,c) (sources,bindings,consumes) ->
      (s : sources, b ++ bindings, c : consumes)
    ) k3_prog_demux_calls ([],[],[])
  in
  let k3_prog_client_role = 
    List.map (\x -> x, []) (k3_prog_sources ++ k3_prog_bindings ++ k3_prog_consumes)
  in
    List.map (\x -> x, []) (
      k3_prog_schema ++
      [ K.Flow(k3_prog_trigs ++  k3_prog_demux) ] ++
      [ K.Role(role, k3_prog_client_role);
        K.DefaultRole(role);
      ]
    )
