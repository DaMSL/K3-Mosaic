-- Math operations over constants ----
module Stages.M3.M3Constants.Math where

import Stages.M3.M3Type

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

binary_op b_op i_op f_op op_type a b = case (a, b, op_type) of
  (CBool av, CBool bv, TBool) 
  (CBool av, CBool bv, TAny)   -> CBool $ b_op av bv
  (CBool _,  CBool _,  TInt)
  (CBool _,  CInt _,   TInt)
  (CBool _,  CInt _,   TAny)
  (CInt _,   CBool _,  TInt) 
  (CInt _,   CBool _,  TAny) 
  (CInt _,   CInt _,   TInt)
  (CInt _,   CInt _,   TAny)   -> CInt $ i_op (int_of_const a) $ int_of_const b
  (CBool _,  CBool _,  TFloat)
  (CInt _,   CInt _,   TFloat)      
  (CFloat _, CBool _,  TAny)
  (CFloat _, CInt _,   TAny)
  (CFloat _, CFloat _, TAny)
  (CFloat _, CBool _,  TFloat)
  (CFloat _, CInt _,   TFloat)
  (CFloat _, CFloat _, TFloat) -> CFloat $ f_op (float_of_const a) $ 
    float_of_const b
  (CString _, _, _) 
  (_, CString _, _)
  (CString _, _, _) 
  (_, CString _, _) -> error "Binary math op over a string"
  (CDate _, _, _) 
  (_, CDate _, _) -> error "Binary math op over a date"
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
    (CBool _, _)
    (_, CBool _)                     -> error "= of boolean and other"
    (CString av, CString bv)         -> av == bv
    (CString _, _)
    (_, CString _)                   -> error "= of string and other"
    (CDate y1 m1 d1, CDate y2 m2 d2) -> y1==y2 && m1==m2 && d1==d2
    (CDate _, _)
    (_, CDate _)                     -> error "= of date and other"
    (CFloat _, _)
    (_, CFloat _)                    -> float_of_const a == float_of_const b
    (CInt av, CInt bv)               -> av = bv
-- Perform a type-escalating not-equals comparison
cmp_neq a b = CBool $ cmp_eq a b == CBool False
-- Find the type-escalating comparison operation for a Type.cmp_t
cmp op = case op of
  Lt  -> cmp_lt
  Lte -> cmp_leq
  Gt  -> cmp_gt
  Gte -> cmp_geq
  Eq  -> cmp_eq
  Neq -> cmp_neq
