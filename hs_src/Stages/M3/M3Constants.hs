module Stages.M3.M3Constants where
-- Typechecks
--
--   Global system for wrapping typed primitive constants throughout DBToaster
--
import Text.Regex as Regex

import Stages.M3.M3Type

-- Basic Constants --
data Const = 
     CBool   Bool
   | CInt    Int
   | CFloat  Float
   | CString String
   | CDate   Int Int Int -- Date
     deriving (Show, Eq)

---- Basic Operations ----
{-
   Compute the type of a given constant
   ++param a   A constant
   ++return    The type of [a]
-}
type_of_const a = case a of
  CBool _     -> TBool
  CInt _      -> TInt
  CFloat _    -> TFloat
  CString s   -> TString
  CDate _ _ _ -> TDate

{-
   Cast a constant to an integer.  Floats are truncated, and booleans are 
   converted to 1/0.  Strings produce an error.
   ++param a   A constant
   ++return    The integer value of [a]
   ++raise Failure If the constant can not be cast to an integer
-}
int_of_const a = case a of
  CBool True  -> 1
  CBool False -> 0
  CInt av     -> av
  CFloat av   -> truncate av
  CString av  -> read av
  CDate _ _ _ -> error "Cannot produce integer of date"

{-
   Cast a constant to a float. Integers are promoted, booleans are converted to
   1./0..  Strings produce an error.
   ++param a   A constant
   ++return    The floating point value of [a]
   ++raise Failure If the constant can not be cast to a float
-}
float_of_const a = case a of
  CBool True  -> 1.0
  CBool False -> 0.0
  CInt av     -> realToFrac av
  CFloat av   -> av
  CString av  -> read av
  CDate _ _ _ -> error "Cannot produce float of date"


{-
   Parses a string and converts it into corresponding Date constant.
   ++param str  A string
   ++return     The Date value of [str]
   ++raise Failure If the string does not correspond to any date
-}
parse_date str = 
  let err () = error $ "Improperly formatted date:: "++str
      regtest = Regex.matchRegex
        (Regex.mkRegex "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)") str
  in case regtest of
    Just(s1:s2:s3:[]) ->
      let y = read s1
          m = read s2
          d = read s3
      in if m > 12 
      then error $ "Invalid month ("++show m++") in date:: "++str
      else if (d > 31) then error $
          "Invalid day ("++show d++") in date:: "++str
      else CDate y m d
    Just _  -> err ()
    Nothing -> err ()
      
      
---- Conversion to Strings ----

{-
   Get the human-readable string representation of a constant.
   ++param a   A constant
   ++return    The human-readable string-representation of [a]
-}
string_of_const a = case a of
  CBool True  -> "true"
  CBool False -> "false"
  CInt av     -> show av
  CFloat av   -> show av
  CString av  -> av
  CDate y m d -> show y ++ "-" ++ show m ++ "-" ++ show d

{-
   Get the string representation (corresponding to the OCaml defined above) of
   a constant
   ++param a   A constant
   ++return    The string representation of the OCaml constant declaration of [a]
-}
ocaml_of_const :: Const -> String
ocaml_of_const = show

{-
   Get the string representation (corresponding to what the Sql and Calculus 
   parsers expect) of a constant
   ++param a   A constant
   ++return    The string representation of the SQL constant form of [a]
-}
sql_of_const a = case a of
  CBool True  -> "True"
  CBool False -> "False"
  CInt i      -> show i
  CFloat f    -> show f
  CString s   -> "'"++s++"'"
  CDate y m d -> "DATE '"++ string_of_const a++"'"

---- Zero Constants ----
{-
   Returns a constant reprezenting zero of type [zt].
   ++param zt    A type. Can be TBool, TInt or TFloat.
   ++return      The constant zero of type [zt]
   ++raise Failure If there is no zero constant corrsponding to [zt]
-}
zero_of_type zt = case zt of
  TBool -> CBool False
  TInt  -> CInt 0
  TFloat -> CFloat 0
  _ -> error $ "Cannot produce zero of type '"++string_of_type zt++"'"

---- Type Casting ----
{-
   Type-cast a constant to a specified type.  Raise an error if the type 
   conversion is not permitted.
   ++param t  A type
   ++param a  A constant
   ++return   [a] cast to [t]
-}
type_cast t a = 
  let int () = CInt $ int_of_const a
      float () = CFloat $ float_of_const a
  in case (t,a) of
    (TInt, CInt _)     -> int ()
    (TInt, CBool _)    -> int ()
    (TFloat, CInt _)   -> float ()
    (TFloat, CBool _)  -> float ()
    (TFloat, CFloat _) -> float ()
    (_, _) | t == type_of_const a -> a
    (_, _)             -> error $
      "Cannot cast "++ string_of_const a ++" to "++ string_of_type t

