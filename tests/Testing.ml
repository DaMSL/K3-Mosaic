open Symbols
open Tree
open K3
open K3Util
open K3Typechecker

type assertion_t =
    AssertTypeEquals of type_t * type_t

type test_t =
    TestCase of string * assertion_t
  | TestGroup of string * test_t list

let group name tests = TestGroup(name, tests)
let case name assertion = TestCase(name, assertion)

let (@:) f x = f x

let (@=?) actual expected = AssertTypeEquals(expected, actual)

let give_type expr = type_of_texpr (deduce_expr_type [] [] expr)
let iparse s = K3Parser.expr K3Lexer.tokenize (Lexing.from_string s)

let equals_assertion expected actual string_fn =
  if expected = actual then "PASSED."
  else "FAILED: Expected " ^ string_fn actual ^ ", but got " ^ string_fn expected ^ "."

let ensure assertion = match assertion with
  | AssertTypeEquals(expected, actual) -> equals_assertion expected actual string_of_type

let rec run_tests test =
    match test with
    | TestCase(name, assertion) -> (
        let result_string = ensure assertion
        in print_endline (name ^ ": " ^ result_string);
    )
    | TestGroup(name, tests) -> (
        print_endline(name ^ ":"); ignore @: List.map run_tests tests; ()
    )
