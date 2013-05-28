(* Utility to stringify K3 type errors. 
 * This file is needed to break circular dependencies between the typechecker
 * and K3PrintSyntax *)
open Util
open K3Typechecker
module P = K3PrintSyntax

let string_of_error = function
  | TMismatch(t1,t2,s)  -> s^" This expression has type "^ P.string_of_type t1^
      "\nBut an expression was expected of type "^P.string_of_type t2
  
  | VTMismatch(t1,t2,s) -> s^" This expression has type "^
      P.string_of_value_type t1^"\nBut an expression was expected of type "^
      P.string_of_value_type t2
  
  | BTMismatch(t1,t2,s) -> s^" This expression has type "^
      P.string_of_base_type t1^"\nBut an expression was expected of type "^
      P.string_of_base_type t2
  
  | TBad(t)           -> "Bad type "^P.string_of_type t
  | VTBad(t)          -> "Bad type "^P.string_of_value_type t
  | BTBad(t)          -> "Bad type "^P.string_of_base_type t
  | MTBad(t)          -> "Bad type "^P.string_of_mutable_type t
  | TMsg(s)           -> s
  

