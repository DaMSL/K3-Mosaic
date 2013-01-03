open Symbols
open Tree
open K3.AST
open K3Util
open K3Printing
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
let parse_program_test s = K3Parser.program_test K3Lexer.tokenize (Lexing.from_string s)

(* Evaluation *)
let eval_test_expr (decl_prog, e) = 
  let tdecl_prog, env, trig_env, _ = type_bindings_of_program decl_prog in
  let _, val_env = env_of_program tdecl_prog in  
  value_of_eval (snd (eval_expr val_env (deduce_expr_type trig_env env e)))

(* Tests *)
let equals_assertion expected actual string_fn =
  if expected = actual then "PASSED."
  else "FAILED: Expected " ^ string_fn expected ^ ", but got " ^ string_fn actual ^ "."

let ensure assertion = match assertion with
  | AssertTypeEquals(expected, actual) -> equals_assertion expected actual string_of_type
  | AssertValueEquals(expected, actual) -> equals_assertion expected actual string_of_value

let rec run_tests ?(indent="") test =
    match test with
    | TestCase(name, assertion) -> (
        let result_string = ensure assertion
        in print_endline (indent ^ name ^ ": " ^ result_string);
    )
    | TestGroup(name, tests) -> (
        print_endline(indent ^ name ^ ":"); 
        List.iter (run_tests ~indent:("  "^indent)) tests; ()
    )

(* Driver methods *)
let test_expressions test_file_name expr_tests = 
  let test_cases = snd (List.fold_left (fun (i, test_acc) (decls, e, x) ->
      let name = test_file_name^" "^(string_of_int i) in
      let test_case = case name @: eval_test_expr (decls, e) @=? eval_test_expr (decls, x) 
      in i+1, test_acc@[test_case]
    ) (0, []) expr_tests)
  in List.iter run_tests test_cases

let test_program test_file_name address role_opt (program,expected) =
  let _,(env,_) = eval_program address role_opt program in
  let test_cases = List.fold_left (fun test_acc (id, x) -> 
      let name = test_file_name^" "^id in
      let evaluated = try !(List.assoc id env) with Not_found -> VUnknown in
      let test_case = case name @: evaluated @=? eval_test_expr ([], x)
      in test_acc@[test_case]
    ) [] expected
  in List.iter run_tests test_cases
