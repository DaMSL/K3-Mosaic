module Stages.M3.Type where
--
--   Global typesystem for use by all of DBToaster's compilation stages.

import Data.List as L

import ListExtras as LE
--
{-module StringMap = Map.Make(String)-}
--

---- Global type definitions ----
-- Comparison operations --
data Cmp = Eq  | Lt  | Leq | Gt  | Geq  | Neq 
  deriving (Show, Eq)

-- Basic Types
data Type = 
   | TBool | TInt | TFloat | TDate
   | TString               -- A string of bounded length n (0 is infinite)
   | TAny                  -- An unspecified type
   | TExternal String      -- An externally defined type
   deriving (Show, Eq)

-- Basic (typed) variables
type Var = String * Type

-- Equality over variables
var_eq ((a,_)::Var) ((b,_)::Var) = (a = b)

-- Inequality over variables
var_neq ((a,_)::Var) ((b,_)::Var) = (a /= b)

-- Membership testing
mem_var ((a,_)::Var) = L.find $ \(b,_) -> a = b

--
--   Identify the inverse of a comparison operation (Eq -> Neq, etc...)
--
inverse_of_cmp Eq = Neq
inverse_of_cmp Neq = Eq
inverse_of_cmp Lt = Geq
inverse_of_cmp Leq = Gt
inverse_of_cmp Gt = Leq
inverse_of_cmp Geq = Lt
 
---- Conversion to Strings ----
{-
   Get the string representation (according to SQL syntax) of a comparison 
   operation.
   @param op   A comparison operation
   @return     The string representation of [op]
-}
string_of_cmp Eq  = "="
string_of_cmp Neq = "!="
string_of_cmp Lt  = "<"
string_of_cmp Gt  = ">"
string_of_cmp Lte = "<="
string_of_cmp Gte = ">="

{-
   Get the string representation (corresponding to the OCaml defined above) of
   a type
   @param ty   A type
   @return     The string representation of the OCaml type declaration of [ty]
-}
ocaml_of_type t = show t

{-
   Get the human-readable string representation of a type.  (Corresponds to
   values accepted by Calculusparser)
   @param ty   A type
   @return     The human-readable representation of [ty]
-}
string_of_type TAny             = "?"
string_of_type TAny             = "?"
string_of_type TBool            = "bool"
string_of_type TInt             = "int"
string_of_type TFloat           = "float"
string_of_type TString          = "string"
string_of_type TDate            = "date"
string_of_type TExternal(etype) = etype

{-
   Get the human-readable string representation of a type.  (Corresponds to
   values accepted by Calculusparser)
   @param ty   A type
   @return     The human-readable representation of [ty]
-}
cpp_of_type TAny             = "?"
cpp_of_type TBool            = "bool"
cpp_of_type TInt             = "long"
cpp_of_type TFloat           = "double"
cpp_of_type TString          = "string"
cpp_of_type TDate            = "date"
cpp_of_type TExternal(etype) = etype

{-
   Get the string representation of a variable.  If the PRINT-VERBOSE debug mode
   is active, the variable will be printed with its full type using syntax
   accepted by Calculusparser.
   @param var   A variable
   @return      The string representation of [var]
-}
{-?(verbose = Debug.active "PRINT-VERBOSE")-}
string_of_var d v =
   if Debug.verbose () then string_of_var_verbose True v
   else string_of_var_verbose False v

string_of_var_verbose :: Var -> String
string_of_var_verbose (name, vt) =
   if Debug.verbose d then
   then name++"::"++(string_of_type vt)
   else name

{-
   Get the string representation of a list of variables.  If the PRINT-VERBOSE
   debug mode is active, the variable will be printed with its full type using
   syntax accepted by Calculusparser.
   @param vars  A list of variables
   @return      The string representation of [vars]
-}
string_of_vars d vars =
   LE.string_of_list (string_of_var d) vars



---- Escalation ----
{-
   Given two types, return the "greater" of the two.  
   {ul
      { [TAny] can be escalated to any other type}
      { [TBool] can be escalated to [TInt]}
      { [TInt] can be escalated to [TFloat]}
      { Two types that can not be escalated will trigger an error.}
   }
   @param opname  (optional) The operation name to include in error messages
   @param a       The first type
   @param b       The second type
   @return        A type that both [a] and [b] escalate to.
-}
escalate_type = escalate_type_opname "<op>"
escalate_type_opname opname a b = 
  case escalate_type_maybe opname a b of
    Just x  -> x
    Nothing -> error ("Can not compute type of "++string_of_type a++" "++
                 opname++" "++string_of_type b)

escalate_type_maybe opname a b = case (a,b) of
  (at, bt) | at = bt -> Just at
  (TAny,t)
  (t, TAny -> Just t
  (TInt, TBool)
  (TBool, TInt) -> Just TInt
  (TFloat, TBool)
  (TBool, TFloat) -> Just TFloat
  (TInt, TFloat)
  (TFloat, TInt) -> Just TFloat
  _ -> Nothing
  
{-
   Given two types, determine if the first may be escalated into the second
   @param from_type  The source type
   @param to_type    The destination type
   @return True if [from_type] escalates safely to [to_type]
-}
can_escalate_type from_type to_type =
   case escalate_type_maybe "<op>" from_type to_type of
     Just _ -> True
     Nothing -> False

{-
   Given a list of types, return the "greatest" (as [escalate_type])
   @param opname  (optional) The operation name to include in error messages
   @param tlist   A list of types
   @return        A type that every element of [tlist] escalates to
-}
escalate_type_list = escalate_type_list_opname "<op>"
escalate_type_list_opname opname tlist = 
   if null tlist then TInt
   else 
      foldl' (escalate_type_opname) (head tlist) (tail tlist)
