module Stages.M3.M3Functions where
-- Typechecks

-- Conversion NOTE: I made this use unsafeIO since the original used refs. 
-- Ideally you'd use something nicer, like just making the whole thing pure

{-
   A module for managing global definitions of library and user-defined 
   functions.  
   
   The functions defined using this module are referenced in
      - Sql via ExternalFn
      - Calculus/M3 via Arithmetic's AFn type
      - K3 via ExternalLambda()
   
   Each function has two names:: 
      - A sql-friendly string identifier that can be used in Sql queries.
      - A more complex string identifier that corresponds to name of the
        function in the target environments.  
   
   Sql's ExternalFn refers to the first of these.  The SqlToCalculus module 
   translates the sql-friendly string name of the function into the more 
   complex target-language string.
   
   okennedy:: This might be considered to be a hack, and we may want to put this
   translation into the K3 stage, or even the code generators.  That would allow
   users to declare separate identifiers for each target-language.  That said,
   Calculus and K3 both require function invocations to explicitly provide 
   typing information (while SQL infers it).  This saves us from having to use
   separate function declarations in Calculus/M3/K3.  
   
   A set of standard library functions may be found in the StandardFunctions
   module.
-}

import Stages.M3.M3Type
import Stages.M3.M3Constants
import qualified Stages.M3.ListExtras as LE

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toUpper)
import Data.IORef
import System.IO.Unsafe


data Decl = Decl {
  ret_type :: Type,
  impl :: String
}

type FTypeChecker = [Type] -> Decl

-----
{- We keep separate maps containing the reference implementation of the 
   standard library functions, and a separate map for typing rules -- the latter
   is needed, since we will also use it for custom user-provided functions. -}
standard_functions :: IORef (Map String ([Const] -> Type -> Const))
standard_functions = unsafePerformIO $ newIORef Map.empty

function_definitions:: IORef (Map String FTypeChecker)
function_definitions = unsafePerformIO $ newIORef Map.empty

-----

{-
   Determine whether an arithmetic function is defined internally.
   
   ++param name   The name of a cuntion
   ++return       True if [name] is defined
-}
function_is_defined name = unsafePerformIO $
  readIORef standard_functions >>= return . Map.member name

{-
   This exception is thrown if a function is invoked with invalid or otherwise
   mis-typed arguments.
-}
{-exception InvalidFunctionArguments of string-}

{-
   Declare a new standard library function.
-}
declare_std_function :: String -> ([Const] -> Type -> Const) ->
                         ([Type] -> Type) -> ()
declare_std_function name fn typing_rule = unsafePerformIO $ do
  stdfs <- readIORef standard_functions
  fundefs <- readIORef function_definitions
  let stdfs' = Map.insert (map toUpper name) fn stdfs
      fundefs' = Map.insert (map toUpper name)
          (\tl -> Decl {ret_type = typing_rule tl, impl = name})
          fundefs
  writeIORef standard_functions stdfs'
  writeIORef function_definitions fundefs'

{-
   Declare a new user-defined function
-}
declare_usr_function :: String -> [Type] -> Type -> String -> ()
declare_usr_function name arg_types ret_type implementation = 
  unsafePerformIO $ do
  let upper_name = map toUpper name
  stdfuns <- readIORef standard_functions
  fundefs <- readIORef function_definitions
  let stdfuns' = Map.delete upper_name stdfuns
      fundefs' = Map.insert upper_name (
        let old_decl = 
              maybe (error "Invalid function args") 
                id $ Map.lookup upper_name fundefs
        in
        \cmp_types ->
          if cmp_types == arg_types 
          then Decl {ret_type = ret_type,
                impl = implementation }
          else old_decl cmp_types
        ) fundefs
  writeIORef standard_functions stdfuns'
  writeIORef function_definitions fundefs'


{- An invalid function invocation -- This is typically non-fatal, as long as it
    happens before the interpreter stage, as it means that insufficient 
    information exists to pre-evaluate the specified function -}
{-exception InvalidInvocation of string;;-}

----
invalid_args :: String -> [Const] -> Type -> Const
invalid_args fn arglist ftype =
   error $ "Invalid arguments of "++fn++
     case ftype of
        TAny -> "" 
        _    -> "::"++ string_of_type ftype
     ++"("++ LE.string_of_list_sep "," sql_of_const arglist ++")"
----

{-
   Invoke an arithmetic function
-}
invoke fn arglist ftype =
   let upfn = map toUpper fn in
   unsafePerformIO $ do
     stdfuns <- readIORef standard_functions
     let f = maybe (error $ "Undefined function "++fn) id $ 
           Map.lookup upfn stdfuns
     return $ f arglist ftype

{-
   Find a function declaration
-}
declaration fn argtypes = unsafePerformIO $ do
  let upfn = map toUpper fn
  fundefs <- readIORef function_definitions
  let f = maybe (error $ "declaration "++ fn ++ " not found") id $ 
              Map.lookup upfn (fundefs :: Map String FTypeChecker)
  return $ f argtypes

{-
   Compute the type of a function
-}
infer_type :: String -> [Type] -> Type
infer_type fn argtypes = ret_type $ declaration fn argtypes

{-
   Compute the implementation name of a function
-}
implementation :: String -> [Type] -> String
implementation fn argtypes = impl $ declaration fn argtypes

----
escalate :: [Type] -> Type
escalate argtypes = escalate_type_list argtypes
   {-with Failure(msg) -> raise (InvalidFunctionArguments(msg))-}

inference_error () = error "InvalidFunctionArguments"

