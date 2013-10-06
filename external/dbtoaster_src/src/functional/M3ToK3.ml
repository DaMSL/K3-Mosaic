(** Module for translating a M3 program into a K3 program. *)

(**/**)
open Arithmetic
open Calculus

module T = Type
module Const = Constants
module V = Arithmetic.ValueRing
module C = Calculus.CalcRing
module K = K3

exception M3ToK3Failure of 
   Calculus.expr_t option * 
   K3.expr_t option *
   string

let error ?(m3 = None) ?(k3 = None) msg =
   raise (M3ToK3Failure(m3, k3, msg))

let pair_map pair_fn (a,b) = (pair_fn a, pair_fn b)

let varIdType_to_k3_type   (vars : T.var_t list) = 
   List.map (fun (v,t) -> K.TBase(t)) vars
   
let varIdType_to_k3_idType (vars : T.var_t list) = 
   List.map (fun (v,t) -> v,K.TBase(t)) vars

let varIdType_to_k3_expr   (vars : T.var_t list) = 
   List.map (fun (v,t) -> K.Var(v,K.TBase(t))) vars

let k3_idType_to_k3_expr   (vars : (K.id_t * K.type_t) list) = 
   List.map (fun (v,t) -> K.Var(v,t)) vars

let k3_idType_to_k3_type   (vars : (K.id_t * K.type_t) list) = 
   List.map snd vars

let k3_expr_to_varIdType (var_el : K.expr_t list ) = 
   List.map (function 
               | (K.Var(v,K.TBase(t))) -> (v,t) 
               | _ ->  error "M3ToK3: expected variable of base type") var_el

let k3_expr_to_k3_idType   (var_el : K.expr_t list ) =
   List.map (fun v_e -> 
                begin match v_e with
                   | K.Var(v_name,vtype) -> (v_name,vtype)
                   | _ -> error "M3ToK3: K.Var expected."
                end ) var_el
               
let k3_expr_to_k3_type   (var_el : K.expr_t list ) =
   List.map (fun v_e -> 
                begin match v_e with
                   | K.Var(v_name,vtype) -> vtype
                   | _ -> error "M3ToK3: K.Var expected."
                 end ) var_el
               
let name_of_kvar kvar = 
   begin match kvar with
      | K.Var(v_name,vtype) -> v_name
      | _ -> error "M3ToK3: K.Var expected."
   end

let type_of_kvar kvar = 
   begin match kvar with
      | K.Var(v_name,vtype) -> vtype
      | _ -> error "M3ToK3: K.Var expected."
   end

let m3_map_to_k3_map (m3_map: M3.map_t) : K.map_t = 
   match m3_map with
      | M3.DSView(ds)                    -> 
         let (map_name, input_vars, output_vars, map_type, _) = 
            Plan.expand_ds_name ds.Plan.ds_name 
         in 
            (map_name, input_vars, output_vars, map_type )
      | M3.DSTable(rel_name, rel_schema,_) -> 
         (rel_name, [], rel_schema, T.TInt )

(**/**)

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(**/**)
let      zero_int_val = K.Const(Const.CInt(0))
let       one_int_val = K.Const(Const.CInt(1))
let minus_one_int_val = K.Const(Const.CInt(-1))

let      zero_flt_val = K.Const(Const.CFloat(0.0))
let       one_flt_val = K.Const(Const.CFloat(1.0))
let minus_one_flt_val = K.Const(Const.CFloat(-1.0))


let compatible_types t1 t2 = begin match t1, t2 with
   | K.TBase(T.TInt), K.TBase(T.TBool) -> true
   | K.TBase(T.TBool), K.TBase(T.TInt) -> true
   | K.TBase(T.TFloat), K.TBase(T.TBool) -> true
   | K.TBase(T.TBool), K.TBase(T.TFloat) -> true
   | K.TBase(T.TInt), K.TBase(T.TFloat) -> true
   | K.TBase(T.TFloat), K.TBase(T.TInt) -> true
   | _,_ -> (t1 = t2)
   end

let numerical_type t = begin match t with
   | K.TBase(T.TBool)  -> true
   | K.TBase(T.TInt)   -> true
   | K.TBase(T.TFloat) -> true
   | _ -> false
   end

let arithmetic_return_types ?(expr=None) t1 t2 = 
   begin match t1, t2 with
      | K.TBase(T.TBool), K.TBase(T.TBool) 
      | K.TBase(T.TBool), K.TBase(T.TInt)
      | K.TBase(T.TInt), K.TBase(T.TInt) 
      | K.TBase(T.TInt), K.TBase(T.TBool) -> K.TBase(T.TInt)
      | K.TBase(T.TBool), K.TBase(T.TFloat) 
      | K.TBase(T.TInt), K.TBase(T.TFloat)
      | K.TBase(T.TFloat), K.TBase(T.TFloat) 
      | K.TBase(T.TFloat), K.TBase(T.TBool) 
      | K.TBase(T.TFloat), K.TBase(T.TInt) -> K.TBase(T.TFloat)
      | _,_ -> error ~m3:expr "M3ToK3: arguments must be of numerical type."
   end

let escalate_type ?(expr=None) from_t to_t = 
   begin match from_t, to_t with
      | K.TBase(T.TBool), K.TBase(T.TInt) -> K.TBase(T.TInt)
      | K.TBase(T.TBool), K.TBase(T.TFloat) 
      | K.TBase(T.TInt), K.TBase(T.TFloat) -> K.TBase(T.TFloat)
      | _,_ -> 
         if from_t = to_t then to_t
         else error ~m3:expr ("M3ToK3: Unable to escalate type " ^
                              (K.string_of_type from_t) ^
                              " to type " ^ (K.string_of_type to_t))
   end

let init_val_from_type ret_type = 
   begin match ret_type with
      | T.TInt   -> zero_int_val
      | T.TFloat -> zero_flt_val
      | _ -> error ("M3ToK3: invalid return type (" ^
                    (T.string_of_type ret_type) ^ ".")
   end
      
let init_val ret_e = 
   begin match ret_e with
      | K.Var(_,K.TBase(ret_type)) -> init_val_from_type ret_type
      | _ -> error "M3ToK3: invalid return expression."
   end

let extract_opt opt =  
   begin match opt with
      | None -> error "M3ToK3: Trying to extract some from None"
      | Some(s) -> s
   end
(**/**)

(**********************************************************************)
(**/**)
let unique l = 
   List.fold_left (fun acc e -> if List.mem e acc then acc else acc@[e]) [] l
   
let keys_from_kvars kvars  = List.map (fun kv -> (name_of_kvar kv,kv)) kvars

let max_tuple_elems = 30

let list_split (l: 'a list) (n: int) : 'a list list =
  let rec list_split_0 l0 n res =
   let list_splitter l n = 
      let rec _list_split l1 l2 n =
         if(n > 0) then
            match l2 with
            | x :: xs -> _list_split (l1 @ [x]) xs (n - 1)
            | [] -> (l1, [])
         else
            (l1, l2)
      in
      _list_split [] l n
   in
     if (List.length l0) > n then
	    let (left_list, right) = list_splitter l0 n in
	    list_split_0 right n (res@[left_list])
     else 
	    res @ [l0]
  in
  list_split_0 l n []  

let exprs_to_tuple_wide (el: K.expr_t list) = 
   let splitted_list = list_split el max_tuple_elems in
   let elems = List.map (fun x -> if List.length x > 1 then K.Tuple(x) else List.hd x) splitted_list in
      K.Tuple(elems)

let exprs_to_tuple (el: K.expr_t list) = 
   if Debug.active "WIDE-TUPLE" && (List.length el) > max_tuple_elems then
      exprs_to_tuple_wide el
   else 
      begin match el with
         |  [] -> error "M3ToK3: invalid empty expressions list"  
         | [e] -> e
         | _   -> K.Tuple(el)
      end

(**/**)

(**[lambda_args v_el]

   Converts a list of K3 variable expressions into K3 lambda arguments.
   @param v_el The list of K3 variable expressions to be converted.
   @return     The K3 lambda argument corresponding to [v_el] *)
let lambda_args v_el =
   begin match v_el with
      | [] -> error "M3ToK3: invalid empty variables list" 
      | [K.Var(v_name,vtype)] -> K.AVar(v_name,vtype)
      | _ ->
         let v_l = k3_expr_to_k3_idType v_el in 
         let vars = List.map fst v_l in
         if (unique vars) <> vars then 
            error "M3ToK3: invalid lambda with duplicate variables"
         else
            K.ATuple(v_l)
   end

let tuple_var_create (elems: K.expr_t list) num : K.expr_t = 
    let tuple_type = 
	   let elems_type = k3_expr_to_k3_type elems in
	   if List.length elems > 1 then K.TTuple(elems_type) else List.hd elems_type
	in
	     K.Var("__tup_"^(string_of_int num),
		    tuple_type
		 )
		 
let rec lambda_gen l body n = 
      begin match l with
      | [] -> error "M3ToK3: invalid empty variables list" 
      | [x] -> K.Apply(K.Lambda(lambda_args x, body), tuple_var_create x n)
      | x :: xs ->
	     let l_args = lambda_args x in
	     let tup = tuple_var_create x n in (** TODO *)
         let wrap_body = lambda_gen xs body (n+1) in
         K.Apply(K.Lambda(l_args, wrap_body), tup)
	  end
   
(**[lambda v_el body]

   Constructs a lambda expression from the expression [body] with the 
   variables in [v_el].
   @param v_el The list of K3 variables of the [body].
   @param body The body of the lambda expression.
   @return     The K3 lambda expression expecting [v_el] as arguments and
               computing [body]. *)
let rec lambda v_el body =
   if Debug.active "WIDE-TUPLE" && (List.length v_el) > max_tuple_elems then
      lambda_wide v_el body
   else
      K.Lambda( lambda_args v_el, body)
	  
and lambda_wide v_el body =
   let args = list_split v_el max_tuple_elems in
   let rec args_create l n = 
      begin match l with
      | [] -> error "M3ToK3: invalid empty variables list" 
	  | [arg] -> [tuple_var_create arg n]
	  | x :: xs ->
	     (tuple_var_create x n) :: args_create xs (n+1)
      end in
   let v_args = args_create args 1 in (** TODO *)
   K.Lambda(lambda_args v_args, lambda_gen args body 1)

(**[assoc_lambda v1_el v2_el body]

   Constructs an associative lambda expression from the expression [body] 
   with the variables in [v1_el] and [v1_el].
   @param v1_el The first list of K3 variables of the [body].
   @param v2_el The second list of K3 variables of the [body].
   @param body  The body of the associative lambda expression.
   @return      The K3 associative lambda expression expecting [v1_el] and
                [v2_el] as arguments and computing [body]. *)
let rec assoc_lambda v1_el v2_el body = 
   if Debug.active "WIDE-TUPLE" && ((List.length v1_el) > max_tuple_elems || (List.length v2_el) > max_tuple_elems) then
      assoc_lambda_wide v1_el v2_el body
   else
      K.AssocLambda( lambda_args v1_el, lambda_args v2_el, body )
	  
and assoc_lambda_wide v1_el v2_el body =
   let args = list_split v1_el max_tuple_elems in
   let rec args_create l n = 
      begin match l with
      | [] -> error "M3ToK3: invalid empty variables list" 
	  | [arg] -> [tuple_var_create arg n]
	  | x :: xs ->
	     (tuple_var_create x n) :: args_create xs (n+1)
      end in
   let v_args = args_create args 1 in (** TODO *)
   K.AssocLambda(lambda_args v_args, lambda_args v2_el, lambda_gen args body 1)

(**[apply_lambda v_el el body]

   Evaluates the K3 expression [body] by binding the list of expressions 
   in [el] to the list of K3 variables in [v_el]. *)
let apply_lambda v_el el body = 
   if (List.length v_el) != (List.length el) then
      error ("M3ToK3: Applying lambda to expression with " ^ 
             "different size schema!");
   K.Apply(lambda v_el body, (exprs_to_tuple el))

(**[apply_lambda_to_expr lambda_e expr]

   Applies the lambda expression [lambda_e] to the contents of [expr].
   [expr] must be either a singleton expression or a K3 collection 
   (ie. a mapping from keys to values, no collection with just values 
   are allowed). If [expr] is singleton and lambda_e outputs a tuple the 
   result is wrapped in a Singleton collection. *)
let apply_lambda_to_expr lambda_e expr =
   begin match lambda_e with
      | K.Lambda( K.AVar(_,_), K.Tuple(_)) -> 
         K.Singleton( K.Apply(lambda_e, expr) )
      | K.Lambda( K.AVar(_,_), _) -> K.Apply(lambda_e, expr)
      | K.Lambda( K.ATuple(_), _) -> K.Map(  lambda_e, expr)
      | _ -> error "M3ToK3: Invalid arguments to apply_lambda_to_expr."
   end

(**[project_fn from_v_el to_v_el]

   Projects the list of variables [from_v_el] to the list of variables
   [to_v_el]. When maped over a collection it will project away keys that
   are not present in [to_v_el]. *)
let project_fn from_v_el to_v_el = 
   lambda from_v_el (exprs_to_tuple to_v_el)   

let gen_accum_var = 
   FreshVariable.declare_class "functional/M3ToK3" "accv"
(**[aggregate_fn v_el multpl_e]

   Constructs an aggregation function.
   @param v_el     The list of K3 variables of the collection being aggregated.
   @param multpl_e Variable expression used for accessing the values associated
                   with each tuple in the aggregated collection. *)
let aggregate_fn v_el multpl_e =
   let accv = gen_accum_var () in 
   let multpl_t = snd (List.hd (k3_expr_to_k3_idType [multpl_e])) in
   let acce = K.Var( accv, multpl_t ) in
   assoc_lambda (v_el@[multpl_e]) [acce] (K.Add(acce, multpl_e))


(**[external_lambda_args fn t_l]

   Converts a list of K3 types into K3 external lambda arguments.
   @param fn   The name of the external function, used when naming 
               the arguments.
   @param t_l  The list of K3 types to be converted.
   @return     The K3 lambda argument corresponding to [t_l] *)
let external_lambda_args fn t_l =
   begin match t_l with
      | [] -> error "M3ToK3: invalid empty variables list" 
      | [t] -> K.AVar(fn^"_arg1",t)
      | _ -> 
         let arg_prefix  = fn^"_arg_" in
         let arg_counter = ref 0 in
         let next_arg_var() = 
            incr arg_counter;
            arg_prefix^(string_of_int !arg_counter) 
         in
            K.ATuple(List.map (fun t -> next_arg_var(),t) t_l)
   end

(**[external_lambda fn t_l ftype]

   Constructs an external lambda expression for the external function [fn] 
   with arguments of types corresponding to [t_l] and return type ftype.
   @param fn    The name of the external function.
   @param t_l   The list of K3 types used for typing the arguments.
   @param ftype The return type of the function. *)
let external_lambda fn t_l ftype =
   K.ExternalLambda( fn, external_lambda_args fn t_l, ftype)

(**[apply_external_lambda fn te_l ftype]

   Evaluates the external function [fn] by binding its arguments to 
   the expressions in [te_l]. *)      
let apply_external_lambda fn te_l ftype =
   let (t_l, e_l) = List.split te_l in
   K.Apply( external_lambda fn t_l ftype, (exprs_to_tuple e_l))
                  


(**********************************************************************)
(**/**)
(* Utility function for generating map access expression. *)
let map_to_expr mapn ins outs map_type =
   let (ins_k,outs_k) = pair_map varIdType_to_k3_idType (ins,outs) in
   let map_type_k = K.TBase(map_type) in
   begin match ins, outs with
       | ([],[]) -> K.SingletonPC(mapn,             map_type_k)
       | ([], x) ->       K.OutPC(mapn,      outs_k,map_type_k)
       | ( x,[]) ->        K.InPC(mapn,ins_k,       map_type_k)
       | ( x, y) ->          K.PC(mapn,ins_k,outs_k,map_type_k)
   end


(* Utility function for generating temporary maps used in computing sums. *)
let gen_sum_var_sym = 
   FreshVariable.declare_class "functional/M3ToK3" "sum_tmp"
let next_sum_tmp_coll outs_el sum_type_k = 
   let colln = gen_sum_var_sym () in
   let outs_k  = k3_expr_to_k3_idType outs_el in
   begin match outs_el with
      | [] -> error ("M3ToK3: sum_tmp collection required only " ^ 
                     "if 'outs_el' is not empty!")
      |  x -> K.OutPC(colln, outs_k, sum_type_k),
             (colln, [], List.combine (List.map fst outs_k) 
              (List.map K.base_type_of (List.map snd outs_k)), 
              K.base_type_of sum_type_k)
   end


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

(**[map_access_to_expr mapn ins outs map_ret_t theta_vars_el init_expr_opt]

   Generates a K3 expression for accessing a map.
   @param mapn          The name of the map being accessed.
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
let map_access_to_expr mapn ins outs map_ret_t theta_vars_el init_expr_opt =
      
      let (ins_k,outs_k)   = pair_map varIdType_to_k3_idType (ins,outs) in
      let (ins_el,outs_el) = pair_map varIdType_to_k3_expr   (ins,outs) in
      let free_vars_el  = ListAsSet.diff outs_el theta_vars_el in
      let bound_vars_el = ListAsSet.diff outs_el free_vars_el in
      
      let (ins_tl,outs_tl) = pair_map varIdType_to_k3_type (ins,outs) in
      (* K3 type of the value associated with each tuple in the map *)
      let map_ret_k = K.TBase(map_ret_t) in
      (* K3 type of the out tier of the map *)
      let map_out_t = 
         if outs_tl = [] then 
            map_ret_k
         else 
            K.Collection(K.Unknown,K.TTuple(outs_tl@[map_ret_k])) 
      in
      
      let map_ret_ve = K.Var(gen_map_ret_sym (),map_ret_k) in
      let map_out_ve = K.Var(gen_slice_sym (),map_out_t) in
      
      (* Given a collection it slices it according to the bound variables *)
      (* and projects only the free variables *)
      let slice_and_project coll_ve : K.expr_t =
         if free_vars_el = [] then
            error ("M3ToK3: We shouldn't slice if all variables are bound."^
                   " We should Lookup instead.");
         if free_vars_el = outs_el then coll_ve
         else 
            K.Map( project_fn (outs_el@[map_ret_ve])
                              (free_vars_el@[map_ret_ve]), 
                   K.Slice(coll_ve, outs_k, keys_from_kvars bound_vars_el))
      in
      
      let expr = begin match ins, outs with
        | ([],[]) ->
            (*No need to perform initial value computation. This should have *)
            (*already been initialized at system start-up.*) 
            K.SingletonPC(mapn, map_ret_k)
        | ([], y) ->       
            let map_expr = K.OutPC(mapn, outs_k, map_ret_k) in
            if free_vars_el = [] then 
               if init_expr_opt = None then
                  if Debug.active "M3TOK3-SKIP-LOOKUP-VALIDATION"
                  then K.Lookup(map_expr,outs_el) 
                  else K.IfThenElse(K.Member(map_expr,outs_el), 
                                    K.Lookup(map_expr,outs_el),
                                    init_val_from_type map_ret_t)
               else 
                  let _,init_expr = extract_opt init_expr_opt in
                  K.IfThenElse(K.Member(map_expr,outs_el), 
                               K.Lookup(map_expr,outs_el), init_expr)
            else
               slice_and_project map_expr
                     
        | (x, []) -> 
            let map_expr = K.InPC(mapn, ins_k, map_ret_k) in
            if init_expr_opt = None then K.Lookup(map_expr,ins_el) 
            else 
               let iv_e = K.Var(gen_init_val_sym (), map_ret_k) in
               let _,init_expr = extract_opt init_expr_opt in   
               let init_lambda = 
                  if Debug.active "M3TOK3-RETURN-INIT" then iv_e
                  else
                     K.Block(
                        [K.PCValueUpdate(map_expr, ins_el, outs_el, iv_e);
                         iv_e])
               in
               K.IfThenElse(K.Member(map_expr,ins_el), 
                            K.Lookup(map_expr,ins_el), 
                            apply_lambda [iv_e] [init_expr] init_lambda )
                                 
        | (x, y) ->
            let map_expr = K.PC(mapn, ins_k, outs_k, map_ret_k) in
                                    
            let out_access_expr coll_ve =   
               if free_vars_el = [] then 
                  K.IfThenElse(K.Member(coll_ve,outs_el),
                               K.Lookup(coll_ve,outs_el),
                               init_val_from_type map_ret_t)
               else
                  slice_and_project coll_ve
            in
            
            if init_expr_opt = None then 
               apply_lambda [map_out_ve] 
                            [K.Lookup(map_expr,ins_el)] 
                            (out_access_expr map_out_ve)
            else 
               let init_outs_el,ie = extract_opt init_expr_opt in
               let iv_e = K.Var(gen_init_val_sym (), map_out_t) in   
               let init_expr, init_block = 
                  if ListAsSet.seteq init_outs_el outs_el then
                     ((if init_outs_el = outs_el then 
                          ie
                       else
                          (* Use projection to change the order of output 
                             variables from 'init_outs_el' to 'outs_el' *) 
                          K.Map( project_fn (init_outs_el@[map_ret_ve])
                                            (outs_el@[map_ret_ve]), ie)),
                      K.Block([ K.PCUpdate(map_expr, ins_el, iv_e);
                                out_access_expr iv_e ]))
                  else if not (ListAsSet.seteq init_outs_el free_vars_el) then
                     (error ("Initialization expressions that span more " ^
                             "than the free vars of the expression are not "^
                             "supported for full pcs."))
                  else
                     (error ("Initialization expressions that span the " ^
                             "free vars of the expression are not supported " ^
                             "for full pcs."))
               in
                  K.IfThenElse( K.Member(map_expr,ins_el), 
                                apply_lambda [map_out_ve]
                                             [K.Lookup(map_expr,ins_el)]
                                             (out_access_expr map_out_ve), 
                                apply_lambda [iv_e] [init_expr] init_block )
            
      end
      in (free_vars_el, map_ret_ve, expr)

      
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
      if not ((numerical_type ret1_t) && (numerical_type ret2_t)) then
         error ("M3ToK3: ValueRing.Sum or Prod arguments should be of "^
                  "numerical type.");
      let ret_t = arithmetic_return_types ret1_t ret2_t
      in 
      ret_t, (op_fn e1 e2)
   in   
   begin match value_calc with
      | V.Val( AConst(i) )  -> K.TBase(Const.type_of_const i), K.Const(i)
      | V.Val( AVar(v,t) )  -> K.TBase(t), K.Var(v,K.TBase(t))
      | V.Val( AFn(fn,fargs,ftype) ) -> 
            let ret_t = K.TBase(ftype) in
            let te_l = List.map value_to_k3_expr fargs in 
            ret_t, apply_external_lambda fn te_l ret_t
      | V.Neg( neg_arg ) -> 
            let neg_t, neg_e = value_to_k3_expr neg_arg in
            let neg_cst = begin match neg_t with
               | K.TBase( T.TInt )   -> minus_one_int_val
               | K.TBase( T.TFloat ) -> minus_one_flt_val
               | _ -> error "M3ToK3: Negation type should be int or float." 
            end in
            neg_t, K.Mult (neg_cst, neg_e)
      | V.Sum( c1::sum_args_tl )   ->
            let add_fun e1 e2 = K.Add(e1,e2) in
            List.fold_left (bin_fn add_fun) (value_to_k3_expr c1) sum_args_tl
      | V.Prod( c1::prod_args_tl )   ->     
            let mult_fun e1 e2 = K.Mult(e1,e2) in
            List.fold_left (bin_fn mult_fun) (value_to_k3_expr c1) prod_args_tl
      | _  -> error "M3ToK3: Empty V.Sum or V.Prod."
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
type meta_t = ((string * (T.var_t list)) list) * (K.map_t list)
let empty_meta = ([],[])

(**/**)
let meta_append_sum_map meta sum_map =
      (fst meta), (snd meta)@[sum_map]
      
let meta_has_init_keys meta init_keys = 
      List.mem init_keys (fst meta)

let meta_append_init_keys meta init_keys =
      (fst meta)@[init_keys],(snd meta)
      
let dummy_k3_map =
    K.SingletonPC("dummy_map", K.TBase(T.TInt))

let get_unit_statement () = 
    if not (Debug.active "DEBUG-DM-UNIT") then
        K.Unit
    else
        K.PCValueUpdate(dummy_k3_map, [], [], zero_int_val)
(**/**)   
       
(**********************************************************************)


(**[calc_to_k3_expr meta generate_init theta_vars calc]

   Converts a calculus ring expression into a K3 expression.
   @param meta       Metadata associated with the translation process. See 
                     [meta_t] definition above.   
   @param generate_init Flag specifying whether to generate initial value
                        computation or not. By default is set to false.
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
let rec calc_to_k3_expr meta ?(generate_init = false) theta_vars_el calc : 
            ((K.expr_t list * K.expr_t * K.expr_t) * meta_t) =
   let rcr        = calc_to_k3_expr meta  ~generate_init:generate_init
                                          theta_vars_el 
   in
   let rcr2 meta2 = calc_to_k3_expr meta2 ~generate_init:generate_init
                                          theta_vars_el 
   in
   
   let (ins,outs) = schema_of_expr calc in
   let ins_el = varIdType_to_k3_expr ins in
   if (ListAsSet.diff ins_el theta_vars_el) != [] then
      (print_endline ("Expr: \n"^(CalculusPrinter.string_of_expr calc));
       print_endline ("\ninput_vars: "^(K.string_of_exprs ins_el));
       print_endline ("scope_vars: "^(K.string_of_exprs theta_vars_el));
       error ("M3ToK3: Error: All inputs of calculus expression " ^ 
              "should be bound!"));
                   
   
   let cmp_fn op_fn c1 c2 =
      let ((ret1_t, e1),(ret2_t, e2)) = pair_map value_to_k3_expr (c1,c2) in
      if not (compatible_types ret1_t ret2_t) then
         error ("M3ToK3: Error: Incompatible argument types for " ^
                "comparison operation: "^
                (K.string_of_type ret1_t)^" <> "^(K.string_of_type ret2_t));
      ([], K.Var(gen_cmp_ret_sym (),K.TBase(T.TInt)), (op_fn e1 e2)), meta
   in
   let (k3_out_el, k3_ret_v, k3_expr), k3_meta = 
     begin match calc with
      | C.Val( calc_val ) -> begin match calc_val with
         | Value( calc_val_value ) ->
               let ret_t, expr = value_to_k3_expr calc_val_value in
               ([], K.Var(gen_val_ret_sym (),ret_t), expr), meta
         | Cmp( T.Eq, c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Eq  (e1,e2) ) c1 c2
         | Cmp( T.Lt, c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Lt  (e1,e2) ) c1 c2
         | Cmp( T.Lte,c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Leq (e1,e2) ) c1 c2
         | Cmp( T.Gt, c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Lt  (e1,e2) ) c2 c1
         | Cmp( T.Gte,c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Leq (e1,e2) ) c2 c1
         | Cmp( T.Neq,c1, c2 ) -> cmp_fn ( fun e1 e2 -> K.Neq (e1,e2) ) c1 c2
         
         | Rel(reln, rel_schema) -> 
            let rel_outs_el, rel_ret_ve, expr = 
                  map_access_to_expr reln [] rel_schema 
                                     T.TInt theta_vars_el None 
            in
               ((rel_outs_el, rel_ret_ve, expr), meta)
         
         | External(mapn, eins, eouts, ext_type, einit_calc_opt) ->
            (* init_expr_opt will contain if required the schema of the
               initialization expression and the expression itself. *)
            let eouts_el = varIdType_to_k3_expr eouts in
            let free_eouts_el = ListAsSet.diff eouts_el theta_vars_el in
               
            let get_init_expr init_calc_opt = 
               let init_calc = extract_opt init_calc_opt in
               
               if eins = [] && (eouts = [] || free_eouts_el <> []) then
                  (print_endline ("External: "^
                                  (CalculusPrinter.string_of_expr calc));
                  error ("M3ToK3: No initialization should be required for "^
                         "singleton maps or slice accesses of out maps."));
               
               let init_theta_vars_el = 
                  if eins <> [] && eouts <> [] then 
                     ListAsSet.diff theta_vars_el eouts_el
                  else
                     theta_vars_el
               in
               let (init_outs_el, init_ret_ve, init_expr), nm_1 = 
                  calc_to_k3_expr meta ~generate_init:generate_init 
                                       init_theta_vars_el init_calc 
               in
               if not (eins <> [] && eouts <> []) &&
                  not (ListAsSet.seteq free_eouts_el init_outs_el) then
                  (print_endline ("External: "^
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3: Schema of intialization expression should " ^
                       "coincide with the free out vars of the map access."));
               if eins <> [] && eouts <> [] &&
                  (ListAsSet.diff free_eouts_el init_outs_el) <> [] then
                  (print_endline ("External: " ^
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3: Schema of intialization expression should " ^
                       "include the free out vars of the map access."));
               if eins <> [] && eouts <> [] &&
                  (ListAsSet.diff init_outs_el eouts_el) <> [] then
                  (print_endline ("External: " ^ 
                                  (CalculusPrinter.string_of_expr calc));
                   error 
                      ("M3ToK3: Schema of intialization expression should " ^
                       "be included in the out vars of the map access."));
               let _ = escalate_type ~expr:(Some(calc)) 
                                     (type_of_kvar init_ret_ve)
                                     (K.TBase(ext_type)) 
               in
                  nm_1,Some(init_outs_el,init_expr)
            in
            let nm, init_expr_opt =
               if not generate_init then meta, None
               else
                  if einit_calc_opt != None then
                     get_init_expr einit_calc_opt
                  else
                     let default_init = 
                        if eins = [] && free_eouts_el = [] then 
                           Some([],init_val_from_type ext_type)
                        else 
                           if (Debug.active "DEBUG-DM") || 
                              (Debug.active "M3TOK3-INIT-FOR-INS") then
                              Some([],init_val_from_type ext_type)
                           else
                              None
                     in                  
                     meta, default_init               
              in                 
              let (map_outs_el, map_ret_ve, expr) = 
                 map_access_to_expr mapn eins eouts ext_type 
                                    theta_vars_el init_expr_opt 
              in
                 ((map_outs_el, map_ret_ve, expr), nm)
               
         | AggSum( agg_vars0, aggsum_calc ) ->
            (* Convert group by variables to K3 variables and eliminate 
               those that are bound. *)
            let agg_vars0_el = 
               ListAsSet.diff (varIdType_to_k3_expr agg_vars0) theta_vars_el 
            in
            
            let (aggsum_outs_el,ret_ve,aggsum_e),nm = rcr aggsum_calc in
            
            (* Make sure that all the group by variables are included in 
               the output schema of the "aggsum" expression. *)
            if not ( (ListAsSet.diff agg_vars0_el aggsum_outs_el) = []) then
               (print_endline(CalculusPrinter.string_of_expr calc);
               print_endline("GB vars: "^K3.string_of_exprs agg_vars0_el);
               print_endline("AggSum outs: " ^
                             K3.string_of_exprs aggsum_outs_el);
               error 
                  ("M3ToK3: The group by variables of aggsum should " ^ 
                   "be included in the schema of the aggsum expression."));
            let agg_vars_el = ListAsSet.inter agg_vars0_el aggsum_outs_el in
                           
            let agg_fn = aggregate_fn aggsum_outs_el ret_ve in
            let expr = 
              if aggsum_outs_el = [] then aggsum_e
              else if agg_vars_el = [] then
                K.Aggregate(agg_fn, init_val ret_ve, aggsum_e)
              else
                let gb_fn = project_fn (aggsum_outs_el@[ret_ve]) agg_vars_el in 
                let gb_aggsum_e = 
                (* 
                   K.Slice(aggsum_e, k3_expr_to_k3_idType aggsum_outs_el,[]) 
                *)
                   aggsum_e
                in
                K.GroupByAggregate(agg_fn, init_val ret_ve, gb_fn, gb_aggsum_e)
            in
            ((agg_vars_el, ret_ve, expr), nm)
            
         | Lift(lift_v, lift_calc)          -> 
            let (lift_outs_el,lift_ret_ve,lift_e),nm = rcr lift_calc in
            let is_bound = List.mem (K.Var(fst lift_v, K.TBase(snd lift_v))) 
                                    theta_vars_el in
            (* If the variable is already bound, this lift should be treated as
               an equality predicate and we can leave its type in place.  If it
               isn't bound, we escalate the variable's type based on the lifted
               expression. *)
           let lift_vt =
              if is_bound then (K.TBase(snd lift_v))
              else escalate_type ~expr:(Some(calc))
                  (type_of_kvar lift_ret_ve) (K.TBase(snd lift_v))
           in
            let lift_ve = K.Var((fst lift_v),lift_vt) in
            let extra_ve, ret_ve, lift_body = 
               if is_bound then 
                  [], K.Var(gen_lift_ret_sym (),K.TBase(T.TInt)),
                  (exprs_to_tuple (lift_outs_el@[K.Eq(lift_ret_ve,lift_ve)]))
               else
                  [lift_ve], K.Var(gen_lift_ret_sym (),K.TBase(T.TInt)),
                  (exprs_to_tuple (lift_outs_el@[lift_ret_ve;one_int_val]))
            in
            let lift_lambda = 
               lambda (lift_outs_el@[lift_ret_ve]) lift_body
            in
            let expr = apply_lambda_to_expr lift_lambda lift_e   
            in   
               ((lift_outs_el@extra_ve, ret_ve, expr), nm)
            
         (***** BEGIN EXISTS HACK *****)
         
         | Exists(exists_calc)          -> 
            let (exists_outs_el,exists_ret_ve,exists_e),nm = rcr exists_calc in
            let ret_ve = K.Var(gen_exists_ret_sym (), K.TBase(T.TInt)) in
            let exists_body = 
               (exprs_to_tuple (exists_outs_el @ 
                                [ K.Neq(exists_ret_ve, zero_int_val) ])) 
            in               
            let exists_lambda = lambda (exists_outs_el@[exists_ret_ve])
                                       exists_body
            in
            let expr = apply_lambda_to_expr exists_lambda exists_e in   
               ((exists_outs_el, ret_ve, expr), nm)
            
         (***** END EXISTS HACK *****)

      end
         
    | C.Sum( sum_args )   ->
         (* Compute the schema of the resulting sum expression *)
         let outs_el = ListAsSet.diff (varIdType_to_k3_expr outs) 
                                      theta_vars_el 
         in
         (* Translate all terms of the sum int K3 and make sure their schema *)
         (* corresponds to the sum expression's schema and that their return *)
         (* values have numerical types *)
         let prepare_fn old_meta old_ret_t c = 
            let (e_outs_el,e_ret_ve,e),new_meta = rcr2 old_meta c in
            if not (ListAsSet.seteq e_outs_el outs_el) then
               (print_endline("Expression:\n" ^
                              CalculusPrinter.string_of_expr calc);
                print_endline("Scope: "^K3.string_of_exprs theta_vars_el);
                print_endline("Output vars: "^K3.string_of_exprs e_outs_el);
                print_endline("Sum output vars: "^K3.string_of_exprs outs_el);
               error 
                  ("M3ToK3: The schema of a sum term should " ^ 
                   "be the same as the schema of the entire sum."));
            let new_outs_el, new_e =
               if not (Debug.active "M3TOK3-SUM-WITHOUT-COMBINE") && 
                  e_outs_el <> outs_el then
                  (* If using combine, the sum terms must have the exact 
                    same schema (same vars in the same order) *)
                  outs_el, K.Map( project_fn (e_outs_el@[e_ret_ve]) 
                                         (outs_el@[e_ret_ve]), e)
               else 
                  e_outs_el, e
            in
            let e_ret_t = type_of_kvar e_ret_ve in
            let new_ret_t = arithmetic_return_types old_ret_t e_ret_t in
            let e_txt = (CalculusPrinter.string_of_expr c) in
               ((new_outs_el,e_ret_ve,new_e,e_txt),new_meta,new_ret_t)
         in
                     
         let nm,ret_t,sum_exprs = 
            List.fold_left (fun (old_meta,old_ret_t,old_el) c -> 
               let result,new_meta,new_ret_t = 
                  prepare_fn old_meta old_ret_t c 
               in
               new_meta,new_ret_t,old_el@[result] ) 
               (meta,(K.TBase(T.TInt)),[]) 
               sum_args 
         in
                     
         let ret_ve = K.Var(gen_sum_ret_sym (),ret_t) in
         let (hd_outs_el,hd_ret_ve,hd_s,hd_txt) = List.hd sum_exprs in
         let sum_exprs_tl = List.tl sum_exprs in
         
         let sum_result, nm2 = if outs_el <> [] then 
         (if (Debug.active "M3TOK3-SUM-WITHOUT-COMBINE") then
               (* For computing sums between collections we use temporary*)
               (* maps. The operation has 3 steps:*)
               (* 1. Reset the temporary collection. *)
               (* 2. Insert into the temporary collection the contents of *)
               (*    the collection corresponding to the first term.*)
               (* 3. Update the temporary collection with the contents of *)
               (*    the collections corresponding to the rest of the sum *)
               (*    terms.*)
               let sum_coll,sum_map = next_sum_tmp_coll outs_el ret_t in
               
               let reset_update = 
                  K.PCElementRemove(sum_coll, [], outs_el) in
               let reset_stmt = 
                  K.Iterate( lambda (outs_el@[ret_ve]) reset_update, 
                                    sum_coll) 
               in
               
               let head_update = 
                  K.PCValueUpdate(sum_coll, [], outs_el,
                                  K.Comment(hd_txt,hd_ret_ve)) 
               in
               let head_stmt =
                  K.Iterate(lambda (hd_outs_el@[hd_ret_ve]) head_update, 
                            hd_s) 
               in
               
               let sum_fn (s_outs_el,s_ret_ve,s,txt) =                     
                  let sum_update =
                     K.PCValueUpdate(
                        sum_coll, [], outs_el, 
                        K.Comment(
                           txt, 
                           K.IfThenElse(K.Member(sum_coll,outs_el),
                                        K.Add(s_ret_ve,
                                              K.Lookup(sum_coll,outs_el)), 
                                        s_ret_ve) )  ) 
                  in
                     K.Iterate(lambda (s_outs_el@[s_ret_ve]) 
                                      sum_update, s) 
               in
               let tail_stmts = List.map sum_fn sum_exprs_tl in
               
               (K.Block( reset_stmt::head_stmt::tail_stmts@[sum_coll] )),
               (meta_append_sum_map nm sum_map)
          else
             let sum_result_combine = 
                K.Combine(List.map (fun (s_outs_el,s_ret_ve,s,_) -> s) 
                                   sum_exprs)
             in
             (sum_result_combine, nm)
         )
         else
            let sum_fn sum_e (s_outs_el,s_ret_ve,s,_) = 
               K.Add(sum_e,s) 
            in
            (List.fold_left sum_fn hd_s sum_exprs_tl), nm
         in
         ((outs_el, ret_ve, sum_result), nm2)            
    
    | C.Prod( prod_args )   ->   
       (* Translate all terms of the product int K3 and make sure *)
       (* their return values have numerical types *)
       let prepare_fn (old_meta, old_scope) c = 
          let (e_outs_el,e_ret_ve,e),new_meta = 
             calc_to_k3_expr old_meta ~generate_init:generate_init 
                             old_scope c 
          in
          let new_scope = ListAsSet.union old_scope e_outs_el in
          ((e_outs_el,e_ret_ve,e),(new_meta,new_scope))
       in
       let (nm,_),prod_exprs = 
          List.fold_left (fun (old_extra,old_el) c -> 
             let result,new_extra = prepare_fn old_extra c in
             new_extra,old_el@[result]
          ) ((meta,theta_vars_el),[]) prod_args 
       in
         
       let prod_expr_hd = List.hd prod_exprs in
       let prod_exprs_tl = List.tl prod_exprs in
         
       let prod_fn (p1_outs_el,p1_ret_ve,p1) 
                   (p2_outs_el,p2_ret_ve,p2) =
         let p1_ret_t,p2_ret_t = 
            pair_map type_of_kvar (p1_ret_ve,p2_ret_ve) 
         in
         let ret_ve = K.Var(gen_prod_ret_sym (),
                            arithmetic_return_types p1_ret_t p2_ret_t) 
         in
         let p_filter = 
            begin match p1, p2 with
               | K.Lt (e1, e2), _ 
               | K.Leq(e1, e2), _ 
               | K.Eq (e1, e2), _ 
               | K.Neq(e1, e2), _ ->
                  begin match K3Typechecker.typecheck_expr p2 with
                  | K.Collection(_, _) -> 
                    Some(K.Filter(lambda (p2_outs_el@[p2_ret_ve]) p1, p2))
                  | _ -> None
                  end
               | _, K.Lt (e1, e2) 
               | _, K.Leq(e1, e2) 
               | _, K.Eq (e1, e2) 
               | _, K.Neq(e1, e2) ->
                  begin match K3Typechecker.typecheck_expr p1 with
                  | K.Collection(_, _) -> 
                    Some(K.Filter(lambda (p1_outs_el@[p1_ret_ve]) p2, p1))
                  | _ -> None
                  end
               | _, _ -> None
            end
         in
         let p_outs_el,p = 
            begin match p1_outs_el,p2_outs_el with
               | [],[] -> [], K.Mult(p1,p2)
               |  _,[] -> p1_outs_el,
                        K.Map(lambda (p1_outs_el@[p1_ret_ve])
(*                           (K.Tuple(p1_outs_el @ 
                                    [K.Mult(p1_ret_ve,p2)])),
*)
                           (exprs_to_tuple(p1_outs_el @ 
                                    [K.Mult(p1_ret_ve,p2)])),
                                 p1) 
               | [], _ -> p2_outs_el,
                        K.Map( lambda (p2_outs_el@[p2_ret_ve])   
(*                            (K.Tuple(p2_outs_el @ 
                                     [K.Mult(p2_ret_ve,p1)])),
*)
                            (exprs_to_tuple(p2_outs_el @ 
                                     [K.Mult(p2_ret_ve,p1)])),
                                 p2)
               |  _, _ -> 
                  let union_el = (ListAsSet.union p1_outs_el p2_outs_el) in
(*                  let prod_e = K.Tuple(union_el @  
                                       [K.Mult(p1_ret_ve,p2_ret_ve)]) 
*)
                  let prod_e = exprs_to_tuple(union_el @  
                                       [K.Mult(p1_ret_ve,p2_ret_ve)]) 
                  in
                  let nested = K.Map(lambda (p2_outs_el @ 
                                             [p2_ret_ve]) prod_e, p2) 
                  in 
                     union_el, 
                     K.Flatten( K.Map(lambda (p1_outs_el @ [p1_ret_ve]) nested,
                                      p1) )
            end
         in
         begin match p_filter with
            | Some(x) when Debug.active "USE-FILTER" -> (p_outs_el, ret_ve, x)
            | _ -> (p_outs_el, ret_ve, p)
          end
       in
         
          let prod_result = 
            List.fold_left prod_fn prod_expr_hd prod_exprs_tl 
         in (prod_result, nm)
            
      | C.Neg( neg_arg ) ->
         rcr (CalcRing.mk_prod( [ Calculus.mk_value (Arithmetic.mk_int(-1));
                                  neg_arg] ))
   end 
   in
   
   let _ = 
      try K3Typechecker.typecheck_expr k3_expr
      with 
         | K3Typechecker.K3TypecheckError(stack,msg) ->
            raise (K3Typechecker.K3TypecheckError(stack,msg ^
                     ("\nin calc expr: "^
                     (CalculusPrinter.string_of_expr calc)) ^
					 ("\nfor k3 expr: ")^
					 (K3.nice_string_of_expr k3_expr [])))
          
  in
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
                       K.statement_t * meta_t =
   let (mapn, lhs_ins, lhs_outs, map_type, init_calc_opt) = 
      Plan.expand_ds_name m3_stmt.Plan.target_map 
   in
   let {Plan.update_type = update_type; Plan.update_expr = incr_calc} = 
      m3_stmt
   in 
   
   (* all lhs input variables must be free, they cannot be bound 
      by trigger args *)
   if not ((ListAsSet.inter trig_args  lhs_ins) = []) then
      (print_endline ("Trigger Arguments: "^
                      (T.string_of_vars ~verbose:true trig_args));
       print_endline ("Lhs Input Variables: " ^
                      (T.string_of_vars ~verbose:true lhs_ins));
       error ("M3ToK3: All lhs input variables must be free. "^
              "They cannot be bound by trigger args."));
   (* K3 collection used for storing the result of the statement.*)
   let lhs_collection  = map_to_expr mapn lhs_ins lhs_outs map_type in
    
   let map_k3_type = K.TBase(map_type) in
   let out_tier_t  = 
      if lhs_outs = [] then map_k3_type 
      else K.Collection(K.Unknown, 
                        K.TTuple( (varIdType_to_k3_type lhs_outs) @ 
                                  [map_k3_type] )) in
           
   let (lhs_ins_el,lhs_outs_el) = 
      pair_map varIdType_to_k3_expr (lhs_ins, lhs_outs) 
   in
   let trig_args_el = varIdType_to_k3_expr trig_args in
   let trig_w_ins_el = ListAsSet.union trig_args_el  lhs_ins_el  in
   let trig_w_lhs_el = ListAsSet.union trig_w_ins_el lhs_outs_el in
      
   (* Variable that will get bound to the output tier corresponding *)
   (* to a particular input variables key.*)
   let existing_out_tier = K.Var("existing_out_tier",out_tier_t) in
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
   let existing_v, nm0 =
      if update_type = Plan.ReplaceStmt then 
         (init_val_from_type map_type), meta
      else if lhs_outs_el = [] then 
         existing_out_tier, meta
      else
         let init_expr_opt = 
            if not generate_init then None
            else if init_calc_opt != None then (
               let init_calc = extract_opt init_calc_opt in
               let init_result, init_meta = 
                  calc_to_k3_expr meta ~generate_init:generate_init 
                                  trig_w_lhs_el init_calc 
               in
               let (init_outs_el, init_ret_ve, init_expr) = init_result in
               if not (init_outs_el = []) then
                  (print_endline ("Initialization Calculus:\n"^
                                 (CalculusPrinter.string_of_expr init_calc));
                  error 
                     ("M3ToK3: Initialization expression of lhs map should " ^
                      "produce a singleton."));
               let _ = escalate_type (type_of_kvar init_ret_ve) map_k3_type in
               Some(init_expr))    
            (*Return default init value irrespective of the type of the map*)
            else Some(init_val_from_type map_type)

         in 
         if (init_expr_opt != None) then 
            if Debug.active "GENERATE-DEF-VAL-LOOKUPS" then
               K.LookupOrElse(existing_out_tier, lhs_outs_el, 
                              (extract_opt init_expr_opt)), meta
            else 
               K.IfThenElse( K.Member(existing_out_tier, lhs_outs_el),
                             K.Lookup(existing_out_tier, lhs_outs_el),
                             (extract_opt init_expr_opt) ), meta
         else 
            K.Lookup(existing_out_tier, lhs_outs_el), meta
   in
   
   (* Translate the rhs calculus expression into a k3 expression and its *)
   (* corresponding schema. Beside the trigger variables we also have *)
   (* the input variables of the "lhs_collection" in scope as we will *)
   (* update "lhs_collection" while iterating over all lhs input variables. *)
   let (rhs_outs_el, rhs_ret_ve, incr_expr), nm =
      if not(Debug.active "DEBUG-DM-WITH-M3") then
         calc_to_k3_expr nm0 ~generate_init:(generate_init) 
                         trig_w_ins_el incr_calc
      else
         let (rhs_outs_el, rhs_ret_ve, incr_expr), nm = 
            calc_to_k3_expr nm0 ~generate_init:(true) 
                            trig_w_ins_el incr_calc
        in
        (rhs_outs_el, rhs_ret_ve, incr_expr), nm
   in
   
   (* Make sure that the lhs collection and the incr_expr have *)
   (* the same schema. *)
   let free_lhs_outs_el = ListAsSet.diff lhs_outs_el trig_args_el in
   if not (ListAsSet.seteq free_lhs_outs_el rhs_outs_el) then
      (print_endline ("Stmt: "^
                      (Plan.string_of_statement m3_stmt));
       print_endline ("Trigger Variables: "^
                      (K.string_of_exprs trig_args_el));
       print_endline ("Lhs Output Variables: "^
                      (K.string_of_exprs free_lhs_outs_el));
       print_endline ("Rhs Output Variables: "^
                      (K.string_of_exprs rhs_outs_el));
       error ("M3ToK3: The lhs and rhs must have the same set " ^ 
              "of free out variables. "));
   let _ = escalate_type (type_of_kvar rhs_ret_ve) map_k3_type in
      
   (* Iterate over all the tuples in "incr_expr" collection and update *)
   (* the lhs_collection accordingly. *)
   let coll_update_expr =   
      let single_update_expr = K.PCValueUpdate( lhs_collection, lhs_ins_el, 
                                                lhs_outs_el, 
                                                K.Add(existing_v,rhs_ret_ve) )
      in
      let inner_loop_body=lambda (rhs_outs_el@[rhs_ret_ve]) single_update_expr 
      in
      let update_body =
        if rhs_outs_el = [] then   K.Apply(  inner_loop_body,incr_expr)  
        else                       K.Iterate(inner_loop_body,incr_expr)
      in
          
      if ( update_type = Plan.UpdateStmt || 
           free_lhs_outs_el = [] ||
           Debug.active "UNSAFE-REPLACE" ) then
        update_body        
      else
        let old_slice = 
          if ListAsSet.seteq lhs_outs_el free_lhs_outs_el
          then existing_out_tier
          else let slice_mapping = 
                 List.map (function (vn,vt) -> (vn, K.Var(vn, vt))) 
                          (k3_expr_to_k3_idType free_lhs_outs_el)
               in
                 K.Slice(existing_out_tier, 
                         varIdType_to_k3_idType lhs_outs, 
                         slice_mapping)
        in
           K.Block [
             K.Iterate(
               lambda (lhs_outs_el@[rhs_ret_ve]) (
                 K.PCElementRemove(lhs_collection, lhs_ins_el, lhs_outs_el);
               ),
               old_slice
             );
             update_body
           ]
            
    in
   
   (* In order to implement a statement we iterate over all the values *)
   (* of the input variables of the lhs collection, and for each of them *)
   (* we update the corresponding output tier. *)
   let statement_expr =
      let outer_loop_body = 
        lambda (lhs_ins_el@[existing_out_tier]) coll_update_expr 
      in
      if lhs_ins_el = [] then
         K.Apply(  outer_loop_body,lhs_collection)
      else
         K.Iterate(outer_loop_body,lhs_collection)
   in
   let _ = K3Typechecker.typecheck_expr statement_expr in
      (statement_expr, nm)


(**[target_of_statement stmt]

   Returns an K3 expression representing the collection being updated
   by the statement. 
   @param stmt    A K3 statement.   
   @return        The collection updated by [stmt].
*)
let target_of_statement (stmt : K.statement_t) : K.expr_t =
   begin match stmt with
      | K.Apply( _,lhs_collection)   -> lhs_collection
      | K.Iterate( _,lhs_collection) -> lhs_collection
      | _ -> error "M3ToK3: Invalid statement expression"
   end

(**[m3_trig_to_k3_trig meta generate_init m3_trig]

   Transforms a M3 trigger into a K3 trigger. 
   @param meta    Metadata associated with the translation process. 
                  See [meta_t] definition above.   
   @param generate_init Flag specifying whether to generate initial value 
                        computation or not. By default is set to false.
   @param m3_trig The M3 trigger being translated.
   @return        The [m3_trig] translated into K3 and the updated meta.
*)
let m3_trig_to_k3_trig (meta: meta_t) ?(generate_init = false) 
                       (m3_trig: M3.trigger_t): 
                       K.trigger_t * meta_t =
   let trig_args = Schema.event_vars m3_trig.M3.event in
   let k3_trig_stmts, new_meta = 
      List.fold_left 
         (fun (old_stms,om) m3_stmt -> 
            let k3_stmt, nm = 
               m3_stmt_to_k3_stmt om ~generate_init:generate_init 
                                  trig_args m3_stmt 
            in 
               (old_stms@[k3_stmt], nm) )
         ([],([],snd meta))
      !(m3_trig.M3.statements) 
   in
     ((m3_trig.M3.event, k3_trig_stmts), new_meta)

(**[m3_to_k3 generate_init m3_program]

   Transforms a M3 program into a K3 program. 
   @param generate_init Flag specifying whether to generate initial value 
                        computation or not. By default is set to false.
   @param  m3_program The M3 program being translated.
   @return The [m3_program] translated into K3.
*)
let m3_to_k3 ?(generate_init = false) (m3_program : M3.prog_t) : (K.prog_t) =
   let {M3.maps = m3_prog_schema; M3.triggers = m3_prog_trigs;
        M3.queries = m3_prog_tlqs; M3.db = k3_database } = m3_program in
   let k3_prog_schema = List.map m3_map_to_k3_map !m3_prog_schema in
   let patterns_map = 
      List.fold_left (fun patterns_map (_,tlq) ->
         Patterns.merge_pattern_maps 
            patterns_map
            (Patterns.extract_from_calc [] tlq)
      ) (Patterns.extract_patterns !m3_prog_trigs) (!m3_prog_tlqs)
   in
   let k3_prog_trigs, (_,sum_maps) = 
      List.fold_left
         (fun (old_trigs,om) m3_trig -> 
            let k3_trig,nm = 
               m3_trig_to_k3_trig om ~generate_init:generate_init m3_trig in
            (old_trigs@[k3_trig], nm) )
         ([],empty_meta) 
         !m3_prog_trigs 
   (* The list of temporary maps required for performing sum operations is *)
   (* appended to the K3 program schema. *)
   in
   let (k3_prog_tlqs,tlq_sum_maps) = 
     List.split (
        List.map (fun (name, query) ->
           let ((_,_,k3_query_compiled),(_,tlq_sum_maps)) =
              calc_to_k3_expr empty_meta ~generate_init:generate_init [] query
           in
              ((name, k3_query_compiled), tlq_sum_maps)
        ) !m3_prog_tlqs
      )
   in
   ( k3_database, 
     (k3_prog_schema@sum_maps@(List.flatten tlq_sum_maps), patterns_map), 
     k3_prog_trigs, 
     k3_prog_tlqs 
   )
