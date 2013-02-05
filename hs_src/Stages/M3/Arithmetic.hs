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

import qualified Util.ListAsSet as LAS
import qualified Stages.M3.ListExtras as LE
import qualified Stages.M3.ListAsFunction as LAF
import qualified Stages.M3.M3Functions as M3Func
import qualified Stages.M3.M3ConstMath as M3Math
import Stages.M3.M3Type
import Stages.M3.M3Constants
import Stages.M3.Ring as Ring

import Data.List (intercalate)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

{-
   Template for the base type for the Arithmetic ring
-}
data ArithmeticLeaf t =
     AConst Const                       -- A constant value
   | AVar Var                           -- A variable
   | AFn String [t] Type             -- A function application

instance Ringable (ArithmeticLeaf t) where
  ring_zero = AConst $ CInt 0
  ring_one  = AConst $ CInt 1

{-
   The value ring
-}
{-ValueRing :: Ring.Ring with type leaf_t = ValueBase.t-}
         {-= Ring.Make(ValueBase)-}

{-{--}
   {-The base type for the Arithmetic ring (see [arithmetic_leaf_t] above)-}
{--}-}
{-type value_leaf_t = ValueRing.leaf_t-}
newtype ValueLeaf = ValueLeaf { getLeaf :: ArithmeticLeaf (Expr ValueLeaf) }
newtype Value = Value { getVal :: Expr (

{-(**-}
   {-Values, or elements of the Arithmetic ring-}
{-*)-}
{-type value_t      = ValueRing.expr_t-}
type Value = Expr (ArithmeticLeaf Expr)

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
string_of_value_leaf :: ValueLeaf t -> String
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
string_of_value :: Value t -> String
string_of_value a_value =
   Ring.fold
      (\sum_list -> "("++(intercalate " + " sum_list )++")")
      (\prod_list -> "("++(intercalate " * " prod_list)++")")
      (\neg_term  -> "(-1*"++neg_term++")")
      string_of_value_leaf
      a_value

---- Variable Operations ----
{-
   Obtain the set of variables that appear in the specified value.
   ++param v    A value
   ++return     The set of variables that appear in [v] as a list
-}
vars_of_value :: Value t -> [Var]
vars_of_value v =
   Ring.fold
      LAS.multiunion
      LAS.multiunion
      id
      (\x -> case x of
         AConst _ -> []
         AVar v -> [v]
         AFn _ tl _ -> LAS.multiunion $ map vars_of_value tl)
      v

{-
   Apply the provided mapping to the variables appearing in the specified value.
   ++param mapping The mapping to apply to [v]
   ++param v       A value
   ++return        [v] with [mapping] applied to all of its variables
-}
rename_vars :: LAF.TableFn Var Var -> Value t -> Value t
rename_vars mapping v =
   Ring.fold
      mk_sum
      mk_prod
      mk_neg
      (\lf -> mk_val $ 
        case lf of
          AConst c     -> lf
          AVar v       -> AVar LAF.apply_if_present mapping v
          AFn fn fa ft -> AFn fn (map (rename_vars mapping) fa) ft)
      v

---- Typechecker ----
{-
   Compute the type of the specified value.
   ++param v   A value
   ++return    The type of [v]
-}
type_of_value :: Value t -> Type
type_of_value a_value =
   Ring.fold
      (escalate_type_list_opname "+")
      (escalate_type_list_opname "*")
      (\t -> case t of 
               TInt   -> t
               TFloat -> t 
               _ -> error $ "Can not compute type of -1 * "++string_of_type t)
      (\leaf -> case leaf of
                  AConst c  -> type_of_const c
                  AVar _ vt -> vt
                  AFn _ fn_args fn_type ->
                    {-List.iter (\x -> let _ = type_of_value x in ()) fn_args;-}
                    fn_type
      )
      a_value


{-
   Compare two values for equivalence under an undefined mapping from variable
   names in one value to variable names in the other.
   ++param val1   A value
   ++param val2   A value
   ++return       [Nothing] if no variable mapping exists to transform [val1] into 
                 [val2].  If such a mapping exists, it is returned wrapped in
                 a [Just]
-}
cmp_values = cmp_values_opts Ring.default_cmp_opts

cmp_values_opts :: [CmpOpt] -> Value t -> Value t -> Maybe [(Var, Var)]
cmp_values_opts cmp_opts val1 val2 =
  let rcr = cmp_values_opts cmp_opts in

  Ring.cmp_exprs_opts cmp_opts 
    LAF.multimerge 
    LAF.multimerge 
    (\lf1 lf2 ->
      case (lf1, lf2) of 
             (AConst c1, AConst c2) | c1 /= c2  -> Nothing
                                    | otherwise -> Just []
             (AVar v1, AVar v2) -> Just [v1, v2]
             (AFn fn1 subt1 ft1, AFn fn2 subt2 ft2) | fn1 /= fn2 -> Nothing
                                                    | ft1 /= ft2 -> Nothing
                                                    | otherwise  -> do
               res <- mapM (\(a, b) -> rcr a b) $ zip subt1 subt2
               return $ LAF.multimerge res
             (_,_) -> Nothing
    ) val1 val2

{-
   Returns sign of a value.
   ++param a_value       A value (Arithmetic ring expression)
   ++return              Sign of [a_value]
-}
sign_of_value :: Value t -> Value t
sign_of_value a_value =
  let one_expr = one
      zero_expr = zero
      sign_of_leaf a_leaf =
        case a_leaf of
          AConst c ->
            case c of
              CInt n | n > 0        -> one_expr
                     | n < 0        -> Ring.Neg one_expr
                     | otherwise    -> zero_expr
              CFloat n | n > 0      -> one_expr
                       | n < 0      -> Ring.Neg one_expr
                       | otherwise  -> zero_expr
              _ -> one_expr
          AVar v    -> a_value
          AFn _ _ _ -> one_expr
  in
  case a_value of
    Ring.Val v  -> sign_of_leaf v
    Ring.Sum l  -> Ring.Sum map sign_of_value l
    Ring.Prod l -> Ring.Prod map sign_of_value l
    Ring.Neg a  ->
      let sign_of_a = sign_of_value a in
            case sign_of_a of
              Ring.Neg x -> sign_of_value x
              _          -> Ring.Neg sign_of_a

---- Evaluation ----
{-
   Evaluate the specified value to a constant
   ++param scope (optional) A set of variable->value mappings
   ++param v     The value to evaluate
   ++return      The constant value that v evaluates to
   ++raise Failure If [v] contains a variable that is not defined in [scope] or
                  if [v] contains a function that is not defined in the globally
                  maintained set of [standard_functions].
-}
eval = eval_scope Map.empty

eval_scope :: Map String a -> Value t -> Const
eval_scope scope v =
  Ring.fold 
    M3Math.suml 
    M3Math.prodl 
    M3Math.neg 
    (\lf -> 
      case lf of
        AConst c -> c
        AVar v _ -> maybe 
          (error $ "Variable "++v++" not found while evaluating arithmetic")
          id $ Map.lookup v scope
        AFn fn fargs ftype -> M3Func.invoke fn
          (map (eval_scope scope) fargs)
          ftype
   ) v

{-
   Evaluate/reduce the specified value as far as possible
   ++param scope (optional) A set of variable->value mappings
   ++param v     The value to evaluate
   ++return      A value representing the most aggressively reduced/evaluated
                form of [v] possible.
-}
eval_partial = eval_partial []

eval_partial_scope scope v =
   let merge v_op c_op term_list =
        let (v, c) = foldr f term_list ([], Nothing)
             where f term (v,c) = case (term, c) of
                    (Ring.Val(AConst c2), Nothing) -> (v, Just c2)
                    (Ring.Val(AConst c2), Just c1) -> (v, Just $ c_op c1 c2)
                    (_,_) -> (term : v, c)
        in v_op $ case c of
                     Nothing -> [] 
                     Just c  -> [mk_const c]
                  ++ v
   in
   Ring.fold 
      (merge Ring.mk_sum M3Math.sum)
      (merge Ring.mk_prod M3Math.prod)
      (\x -> merge Ring.mk_prod M3Math.prod [mk_int (-1), x])
      (\lf -> 
        case lf of
          AFn fname fargs_unevaled ftype -> 
            let fargs = map (eval_partial_scope scope) fargs_unevaled
                f = do
                  farg_vals <- mapM (\x -> case x of
                                   Ring.Val(AConst c) -> return c
                                   _ -> return Nothing) -- Not found
                                 fargs
                  return $ mk_const $ M3Func.invoke fname farg_vals ftype
            in maybe (Ring.mk_val $ AFn fname fargs ftype) id f

          AVar vn vt -> maybe (Ring.mk_val lf) id $ List.lookup (vn,vt) scope 
          AConst c   -> Ring.mk_val lf
      )
      v

