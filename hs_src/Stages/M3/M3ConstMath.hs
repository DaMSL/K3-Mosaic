module Stages.M3.M3ConstMath where
-- Typechecks
-- Math operations over constants ----

import Prelude hiding (sum)
import Stages.M3.M3Constants
import Stages.M3.M3Type
import Data.List (foldl')

{-
    Perform a type-escalating binary arithmetic operation over two constants
    ++param b_op   The operation to apply to boolean constants
    ++param i_op   The operation to apply to integer constants
    ++param f_op   The operation to apply to floating point constants
    ++param a      A constant
    ++param b      A constant
    ++return       The properly wrapped result of applying [b_op], [i_op], or 
                [f_op] to [a] and [b], as appropriate.
    ++raise Failure If [a] or [b] is a string.
-}

binary_op :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int) ->
             (Float -> Float -> Float) -> 
             Type -> Const -> Const -> Const

binary_op b_op i_op f_op op_type a b = 
  let i () = CInt $ i_op (int_of_const a) $ int_of_const b
      f () = CFloat $ f_op (float_of_const a) $ float_of_const b
      err_s () = error "Binary math op over a string"
      err_d () = error "Binary math op over a date"
  in case (a, b, op_type) of
    (CBool av, CBool bv, TBool)  -> CBool $ b_op av bv
    (CBool av, CBool bv, TAny)   -> CBool $ b_op av bv
    (CBool _,  CBool _,  TInt)   -> i ()
    (CBool _,  CInt _,   TInt)   -> i ()
    (CBool _,  CInt _,   TAny)   -> i ()
    (CInt _,   CBool _,  TInt)   -> i ()
    (CInt _,   CBool _,  TAny)   -> i ()
    (CInt _,   CInt _,   TInt)   -> i ()
    (CInt _,   CInt _,   TAny)   -> i ()
    (CBool _,  CBool _,  TFloat) -> f ()
    (CInt _,   CInt _,   TFloat) -> f ()
    (CFloat _, CBool _,  TAny)   -> f ()
    (CFloat _, CInt _,   TAny)   -> f ()
    (CFloat _, CFloat _, TAny)   -> f ()
    (CFloat _, CBool _,  TFloat) -> f ()
    (CFloat _, CInt _,   TFloat) -> f ()
    (CFloat _, CFloat _, TFloat) -> f ()
    (CString _, _, _)            -> err_s ()
    (_, CString _, _)            -> err_s ()
    (CDate _ _ _, _, _)          -> err_d ()
    (_, CDate _ _ _, _)          -> err_d ()
    (_, _, _) -> error $ "Binary math op with incompatible return type:: "++
        string_of_const a ++" "++ string_of_const b ++
        " -> "++ string_of_type op_type
   
-- Perform type-escalating addition over two constants
sum = binary_op (\x -> error "sum of booleans" ) (+) (+) TAny
-- Perform type-escalating addition over an arbitrary number of constants
suml = foldl' sum $ CInt 0
-- Perform type-escalating multiplication over two constants
prod = binary_op (&&) (*) (*) TAny
-- Perform type-escalating multiplication over an arbitrary number of 
--  constants
prodl= foldl' prod $ CInt 1
-- Negate a constant
neg  = binary_op (\_-> error "Negation of a boolean") (*) (*) TAny $ CInt (-1)
-- Compute the multiplicative inverse of a constant *)
div1 dtype a = 
  binary_op (\_-> error "Dividing a boolean 1") (div) (/) dtype (CInt 1) a
-- Perform type-escalating division of two constants *)
div2 dtype a b = 
  binary_op (\_-> error "Dividing a boolean 2") (div) (/) dtype a b

----
comparison_op :: String -> (Int -> Int -> Bool) -> (Float -> Float -> Bool) ->
                 Const -> Const -> Const
comparison_op opname iop fop a b =
  let op_type = escalate_type_opname opname (type_of_const a) $ type_of_const b 
  in case op_type of
    TInt   -> CBool $ iop (int_of_const a) $ int_of_const b
    TFloat -> CBool $ fop (float_of_const a) $ float_of_const b
    TDate  -> 
      case (a, b) of
        (CDate y1 m1 d1, CDate y2 m2 d2) ->
            CBool $ iop (y1*10000+m1*100+d1) $ y2*10000+m2*100+d2
        (_,_) -> error $ opname++" over invalid types"
    _ -> error $ opname++" over invalid types"
----

-- Perform a type-escalating less-than comparison
cmp_lt  = comparison_op "<"  (<)  (<)
-- Perform a type-escalating less-than or equals comparison
cmp_leq = comparison_op "<=" (<=) (<=)
-- Perform a type-escalating greater-than comparison
cmp_gt  = comparison_op ">"  (>)  (>)
-- Perform a type-escalating greater-than or equals comparison
cmp_geq = comparison_op ">=" (>=) (>=)
-- Perform a type-escalating equals comparison
cmp_eq a b = CBool $
  case (a, b) of
    (CBool av, CBool bv)             -> av == bv
    (CBool _, _)                     -> error "= of boolean and other"
    (_, CBool _)                     -> error "= of boolean and other"
    (CString av, CString bv)         -> av == bv
    (CString _, _)                   -> error "= of string and other"
    (_, CString _)                   -> error "= of string and other"
    (CDate y1 m1 d1, CDate y2 m2 d2) -> y1==y2 && m1==m2 && d1==d2
    (CDate _ _ _, _)                 -> error "= of date and other"
    (_, CDate _ _ _)                 -> error "= of date and other"
    (CFloat _, _)                    -> float_of_const a == float_of_const b
    (_, CFloat _)                    -> float_of_const a == float_of_const b
    (CInt av, CInt bv)               -> av == bv
-- Perform a type-escalating not-equals comparison
cmp_neq a b = CBool $ cmp_eq a b == CBool False
-- Find the type-escalating comparison operation for a Type.cmp_t
cmp op = case op of
  Lt  -> cmp_lt
  Leq -> cmp_leq
  Gt  -> cmp_gt
  Geq -> cmp_geq
  Eq  -> cmp_eq
  Neq -> cmp_neq

