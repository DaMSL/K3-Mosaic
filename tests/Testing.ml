open Symbols
open Tree
open K3
open K3Util
open K3Typechecker
open K3Values
open K3Interpreter

type assertion_t =
    AssertTypeEquals of type_t * type_t
  | AssertValueEquals of value_t * value_t

type test_t =
    TestCase of string * assertion_t
  | TestGroup of string * test_t list

let group name tests = TestGroup(name, tests)
let case name assertion = TestCase(name, assertion)

let (@:) f x = f x

let (@=:?) actual expected = AssertTypeEquals(expected, actual)
let (@=?)  actual expected = AssertValueEquals(expected, actual)

(* Parsing *)
let parse_expr s = K3Parser.expr K3Lexer.tokenize (Lexing.from_string s)
let parse_expression_test s = K3Parser.expression_test K3Lexer.tokenize (Lexing.from_string s)

(* Evaluation *)
let eval_test_expr (decl_prog, e) = 
  let tdecl_prog = deduce_program_type decl_prog in
  let _, val_env = env_of_program tdecl_prog in  
  value_of_eval (snd (eval_expr val_env (deduce_expr_type [] [] e)))

(* Tests *)
let equals_assertion expected actual string_fn =
  if expected = actual then "PASSED."
  else "FAILED: Expected " ^ string_fn expected ^ ", but got " ^ string_fn actual ^ "."

let ensure assertion = match assertion with
  | AssertTypeEquals(expected, actual) -> equals_assertion expected actual string_of_type
  | AssertValueEquals(expected, actual) -> equals_assertion expected actual string_of_value

let rec run_tests test =
    match test with
    | TestCase(name, assertion) -> (
        let result_string = ensure assertion
        in print_endline (name ^ ": " ^ result_string);
    )
    | TestGroup(name, tests) -> (
        print_endline(name ^ ":"); ignore @: List.map run_tests tests; ()
    )
