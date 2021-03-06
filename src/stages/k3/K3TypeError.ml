(* Utility to stringify K3 type errors.
 * This file is needed to break circular dependencies between the typechecker
 * and K3PrintSyntax *)
open Util
open K3Typechecker
module P = K3PrintSyntax

let string_of_error = function
  | TMismatch(t1,t2,s)  -> s^" This expression has type "^ P.string_of_type t1^
      "\nBut an expression was expected of type "^P.string_of_type t2

  | BTMismatch(t1,t2,s) -> s^" This expression has type "^
      P.string_of_base_type t1^"\nBut an expression was expected of type "^
      P.string_of_base_type t2

  | FunMismatch(fn, ts, s) -> s^"The applied function has type "^
      P.string_of_type fn^"\nBut the arguments have types "^
      strcatmap P.string_of_type ts

  | TBad(t, s)        -> "Bad type "^P.string_of_type t^": "^s
  | BTBad(t, s)       -> "Bad type "^P.string_of_base_type t^": "^s
  | TMsg(s)           -> s
  | InvalidTypeAnnotation   -> "Invalid type annotation"
  | MultiplePossibleTypes s -> "Multiple types are possible: "^s
  | UntypedExpression       -> "Untyped expression found"


