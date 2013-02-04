module Stages.M3.Arithmetic where
{-
   A ring of values (i.e., operations for doing arithmetic over constants),
   variables, and functions thereof.
   
   This approach nearly matches the syntax given in the SIGMOD submission, 
   except that we don't include singleton sums.  These can be effected by 
   computing a product with a definition term.  Doing so saves us from having
   a mutual recursion between ValueRing and CalcRing (which in turn saves 
   Oliver's sanity), and ensures that the delta of a value is always 0.
   
   Note that the functions that appear here and operate over values are assumed 
   to be deterministic and to have no side effects.
-}

import qualified ListAsSet as LAS
import qualified ListExtras as LE
import Stages.M3.M3Type
import Stages.M3.M3Constants
import Stages.M3.Ring as Ring

{-
   Template for the base type for the Arithmetic ring
-}
data ArithmeticLeaf t =
   | AConst Const                       -- A constant value
   | AVar Var                           -- A variable
   | AFn String [t] Type             -- A function application

instance Ringable (ArithmeticLeaf t) where
  zero = AConst $ CInt 0
  one  = AConst $ CInt 1

{-
   The value ring
-}
{-ValueRing :: Ring.Ring with type leaf_t = ValueBase.t-}
         {-= Ring.Make(ValueBase)-}

{-{--}
   {-The base type for the Arithmetic ring (see [arithmetic_leaf_t] above)-}
{--}-}
{-type value_leaf_t = ValueRing.leaf_t-}
type ValueLeaf t = ArithmeticLeaf t

{-(**-}
   {-Values, or elements of the Arithmetic ring-}
{-*)-}
{-type value_t      = ValueRing.expr_t-}
type Value t = Expr (ArithmeticLeaf t)

---- Constructors ----
-- Produce the value equivalent of a boolean
mk_bool :: Bool -> Value t
mk_bool b = mk_val $ AConst $ CBool b
-- Produce the value equivalent of an integer
mk_int :: Int -> Value t
mk_int i = mk_val $ AConst $ CInt i
-- Produce the value equivalent of a floating point number
mk_float f = mk_val $ AConst $ CFloat f   
-- Produce the value equivalent of a string
mk_string s = mk_val $ AConst $ CString s
-- Produce the value equivalent of an arbitrary constant
mk_const c = mk_val $ AConst c
-- Produce the value equivalent of a variable
mk_var v = mk_val $ AVar v
-- Produce the value equivalent of a function
mk_fn :: String -> [Value t] -> Type -> Value t
mk_fn n a t = mk_val $ AFn n a t
             

---- Stringifiers ----
{-
   Generate the (Calculusparser-compatible) string representation of a base 
   type in the Arithmetic ring
   ++param leaf   A base (leaf) element of the Arithmetic ring
   ++return       The string representation of [leaf]
-}
string_of_value_leaf :: ValueLeaf t : String =
string_of_value_leaf leaf =
   case leaf of
      AConst c -> sql_of_const c
      AVar v   -> string_of_var v
      AFn fname fargs ftype ->
         "[" ++ fname ++ " : " ++ (string_of_type ftype) ++ 
         "](" ++ (LE.string_of_list string_of_value fargs) ++ ")"

{-
   Generate the (Calculusparser-compatible) string representation of a value.
   ++param v       A value (Arithmetic ring expression)
   ++return        The string representation of [v]
-}
string_of_value :: Value -> String
string_of_value a_value =
   Ring.fold
      (\sum_list -> "("++(intersperse " + " sum_list )++")")
      (\prod_list -> "("++(intersperse " * " prod_list)++")")
      (\neg_term  -> "(-1*"++neg_term++")")
      string_of_value_leaf
      a_value

---- Variable Operations ----
{-
   Obtain the set of variables that appear in the specified value.
   ++param v    A value
   ++return     The set of variables that appear in [v] as a list
-}
vars_of_value :: Value -> [Var]
vars_of_value v =
   Ring.fold
      LAS.multiunion
      LAS.multiunion
      id
      (\x -> case x of
         AConst _ -> []
         AVar v -> [v]
         AFn _ tl _ -> LAS.multiunion (map vars_of_value tl))
      v

{-
   Apply the provided mapping to the variables appearing in the specified value.
   ++param mapping The mapping to apply to [v]
   ++param v       A value
   ++return        [v] with [mapping] applied to all of its variables
-}
rename_vars (mapping:(var_t,var_t)ListAsFunction.table_fn_t) 
                    (v: value_t): value_t =
   ValueRing.fold
      ValueRing.mk_sum
      ValueRing.mk_prod
      ValueRing.mk_neg
      (\lf -> ValueRing.mk_val (begin match lf with
         | AConst(c)     -> lf
         | AVar(v)       -> AVar(ListAsFunction.apply_if_present mapping v)
         | AFn(fn,fa,ft) -> AFn(fn, List.map (rename_vars mapping) fa, ft)
      end))
      v

(**** Typechecker ****)
(**
   Compute the type of the specified value.
   ++param v   A value
   ++return    The type of [v]
*)
rec type_of_value (a_value: value_t): type_t =
   ValueRing.fold
      (escalate_type_list ~opname:"+")
      (escalate_type_list ~opname:"*")
      (\t -> match t with | TInt | TFloat -> t 
        | _ -> failwith ("Can not compute type of -1 * "++(string_of_type t)))
      (\leaf -> match leaf with 
         | AConst(c)  -> type_of_const c
         | AVar(_,vt) -> vt
         | AFn(_,fn_args,fn_type) ->
            List.iter (\x -> let _ = type_of_value x in ())
                      fn_args;
            fn_type
      )
      a_value


(**
   Compare two values for equivalence under an undefined mapping from variable
   names in one value to variable names in the other.
   ++param val1   A value
   ++param val2   A value
   ++return       [Nothing] if no variable mapping exists to transform [val1] into 
                 [val2].  If such a mapping exists, it is returned wrapped in
                 a [Just]
*)
rec cmp_values ?(cmp_opts:ValueRing.cmp_opt_t list =
                     ValueRing.default_cmp_opts) 
                   (val1:value_t) (val2:value_t):
                   ((var_t * var_t) list option) =
   let rcr = cmp_values ~cmp_opts:cmp_opts in
   
   ValueRing.cmp_exprs ~cmp_opts:cmp_opts 
      ListAsFunction.multimerge 
      ListAsFunction.multimerge 
      (\lf1 lf2 ->
         match (lf1,lf2) with 
            | ((AConst(c1)),(AConst(c2))) ->
               if c1 <> c2 then Nothing else Just([])
         
            | ((AVar(v1)),(AVar(v2))) ->
               Just([v1,v2])
         
            | ((AFn(fn1,subt1,ft1)),(AFn(fn2,subt2,ft2))) ->
               if (fn1 <> fn2) || (ft1 <> ft2) then Nothing
               else begin try 
                  ListAsFunction.multimerge (List.map2 (\a b -> 
                  begin match rcr a b with 
                     | Nothing -> raise Not_found
                     | Just(s) -> s
                  end) subt1 subt2)
               with Not_found -> Nothing
               end
      
            | (_,_) -> Nothing
      ) val1 val2
   


(**
   Returns sign of a value.
   ++param a_value       A value (Arithmetic ring expression)
   ++return              Sign of [a_value]
*)
rec sign_of_value (a_value:value_t): value_t =
  let one_expr = ValueRing.Val(ValueBase.one) in
  let zero_expr = ValueRing.Val(ValueBase.zero) in
  let sign_of_leaf (a_leaf: value_leaf_t): value_t = 
    match a_leaf with
      | AConst(c) ->
        begin match c with
          | CInt(number) -> 
             if number > 0 
             then one_expr 
             else if number < 0 
                  then ValueRing.Neg(one_expr) 
                  else zero_expr
          | CFloat(number) -> 
             if number > 0. 
             then one_expr 
             else if number < 0. 
                  then ValueRing.Neg(one_expr) 
                  else zero_expr
          | _ -> one_expr
        end
      | AVar(v) -> a_value
      | AFn(_, _, _) -> one_expr
  in
  match a_value with
    | ValueRing.Val(v) -> sign_of_leaf(v)
    | ValueRing.Sum(l) -> ValueRing.Sum(List.map (sign_of_value) l)
    | ValueRing.Prod(l)-> ValueRing.Prod(List.map (sign_of_value) l)
    | ValueRing.Neg(a) ->
      let sign_of_a = sign_of_value a in
      match sign_of_a with
        | ValueRing.Neg(x) -> sign_of_value x
        | _                -> ValueRing.Neg(sign_of_a)

(**** Evaluation ****)
(**
   Evaluate the specified value to a constant
   ++param scope (optional) A set of variable->value mappings
   ++param v     The value to evaluate
   ++return      The constant value that v evaluates to
   ++raise Failure If [v] contains a variable that is not defined in [scope] or
                  if [v] contains a function that is not defined in the globally
                  maintained set of [standard_functions].
*)
rec eval ?(scope=StringMap.empty) (v:value_t): const_t = 
   ValueRing.fold M3Constants.Math.suml 
                  M3Constants.Math.prodl 
                  M3Constants.Math.neg 
                  (\lf ->
      match lf with 
         | AConst(c) -> c
         | AVar(v,_) -> 
            if StringMap.mem v scope then StringMap.find v scope
            else failwith 
                    ("Variable "++v++" not found while evaluating arithmetic")
         | AFn(fn,fargs,ftype) -> M3Functions.invoke fn
                                     (List.map (eval ~scope:scope) fargs)
                                     ftype
   ) v

(**
   Evaluate/reduce the specified value as far as possible
   ++param scope (optional) A set of variable->value mappings
   ++param v     The value to evaluate
   ++return      A value representing the most aggressively reduced/evaluated
                form of [v] possible.
*)
rec eval_partial ?(scope=[]) (v:value_t): value_t = 
   let merge v_op c_op (term_list:value_t list): value_t = 
      let (v, c) = foldr (\(term) (v,c) ->
         match (term, c) with
            | (ValueRing.Val(AConst(c2)), Nothing) -> (v, Just(c2))
            | (ValueRing.Val(AConst(c2)), Just(c1)) -> (v, Just(c_op c1 c2))
            | (_,_) -> (term :: v, c)
      ) term_list ([], Nothing) 
      in v_op ((match c with 
         | Nothing -> [] 
         | Just(c) -> [mk_const c]
      ) ++ v)
   in
   ValueRing.fold 
      (merge ValueRing.mk_sum M3Constants.Math.sum)
      (merge ValueRing.mk_prod M3Constants.Math.prod)
      (\x -> merge ValueRing.mk_prod M3Constants.Math.prod [mk_int (-1); x])
      (\lf -> match lf with
         | AFn(fname, fargs_unevaled, ftype) -> 
            let fargs = List.map (eval_partial ~scope:scope) fargs_unevaled in
            begin try 
               let farg_vals = 
                  (List.map (\x -> match x with 
                                      | ValueRing.Val(AConst(c)) -> c
                                      | _ -> raise Not_found) fargs)
               in
                  mk_const (M3Functions.invoke fname farg_vals ftype)
            with 
               | Not_found
               | M3Functions.InvalidInvocation(_) ->
                  ValueRing.mk_val (AFn(fname, fargs, ftype))
            end
         | AVar(vn, vt) ->
            if List.mem_assoc (vn,vt) scope 
               then (List.assoc (vn,vt) scope)
               else ValueRing.mk_val lf
         | AConst(c) -> ValueRing.mk_val lf
      )
      v
