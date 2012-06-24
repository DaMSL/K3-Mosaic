open Symbols
open Tree
open K3
open K3Util
open K3Typechecker

type type_assertion_t = AssertTypeEquals of type_t * type_t

type type_test_t
    = TestCase of string * type_assertion_t
    | TestGroup of string * type_test_t list

let group name tests = TestGroup(name, tests)
let case name assertion = TestCase(name, assertion)

let (@:) f x = f x

let (@=?) actual expected = AssertTypeEquals(expected, actual)

let give_type expr = type_of (deduce_expr_type [] expr)
let iparse s = K3Parser.expr K3Lexer.tokenize (Lexing.from_string s)

let rec run_tests test =
    match test with
    | TestCase(name, assertion) -> (
        let result_string = match assertion with
        | AssertTypeEquals(expected, actual) ->
            if expected = actual then "PASSED."
            else "FAILED: Expected " ^ string_of_type actual ^ ", but got " ^ string_of_type expected ^ "."
        in print_endline (name ^ ": " ^ result_string);
    )
    | TestGroup(name, tests) -> (
        print_endline(name ^ ":"); ignore @: List.map run_tests tests; ()
    )
