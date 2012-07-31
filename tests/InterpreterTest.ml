open Util
open Testing
open K3
open K3Typechecker
open K3Values
open K3Interpreter

let parse = parse_expr

let case_list l = List.map (fun (id, expr_str, expected_val) ->
    case id @: eval_test_expr ([], parse_expr expr_str) @=? expected_val
  ) l

let case_files l = case_list (List.map (fun (id, test_f, expected_val) ->
    id, (read_file test_f), expected_val
  ) l)
  
let tests = group "all" [
    group "Constants" (case_list [
        "Booleans" , "true",   VBool(true);
        "Integers" , "1",      VInt(1);
        "Floats"   , "1.0",    VFloat(1.0);
        "Tuples"   , "(1, 2)", VTuple([VInt 1; VInt 2]);
    ]);
    group "Arithmetic" [
        group "Boolean" (case_list [
            "Addition"       , "false + false", VBool(false);
            "Multiplication" , "true * false" , VBool(false);
            "Negation"       , "-true"        , VBool(false);
        ]);
        group "Integers" (case_list [
            "Addition"       , "1 + 1", VInt(2);
            "Multiplication" , "1 * 1", VInt(1);
            "Negation"       , "-1"   , VInt(-1);
        ]);
        group "Floats" (case_list [
            "Addition"       , "1.0 + 1.0", VFloat(2.0);
            "Multiplication" , "1.0 * 1.0", VFloat(1.0);
            "Negation"       , "-1.0"     , VFloat(-. 1.0);
        ]);
        group "Mixed" (case_list [
            "Addition"       , "1 + 1.0", VFloat(2.0);
            "Multiplication" , "1.0 * 1", VFloat(1.0);
        ])
    ];
    group "Comparisons" (case_list [
        "Eq"  , "0 == 1", VBool(false);
        "Lt"  , "0 < 1" , VBool(true);
        "Neq" , "0 != 1", VBool(true);
        "Leq" , "0 <= 1", VBool(true);
    ]);
]

let _ = run_tests tests
