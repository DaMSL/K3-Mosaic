module T = M3Type
module C = M3Constants
module K = K3
module MK = M3ToK3
module VK = K3Values
;;
open Util
open Testing
open K3.AST
open K3Interpreter
open M3ToK3
;;


let parse_k3 s = 
  try parse_expr s
  with Parsing.Parse_error ->
    let _ = Parsing.set_trace true in
      parse_expr s
      
let parse_k3prog s = 
  K3Parser.program K3Lexer.tokenize (Lexing.from_string s);;
let parse_calc s = 
  Calculusparser.calculusExpr Calculuslexer.tokenize (Lexing.from_string s);;
let parse_stmt s = 
  Calculusparser.mapTriggerStmt Calculuslexer.tokenize (Lexing.from_string s);;

let sol f x = String.concat "," (List.map f x);;
let noop x = x;;

let map_decls = 
  String.concat "\n" (List.map (fun (map,key,v,indices,values) -> 
      if key = [] then (
        "declare "^map^" : {|"^v^"|} = {|"^values^"|}"
      ) else (
        "declare "^map^" : {|("^(sol noop (key@[v]))^")|} @ { "^(
        let (idx,key_ids) = List.fold_left (fun (idx,key_ids) _ ->
          (idx + 1, key_ids @ [idx])
        ) (0,[]) key in
          (sol string_of_int key_ids)^"->"^(string_of_int idx)
        )^(
          String.concat "" (List.map (fun cols ->
            "; index("^(sol string_of_int cols)^")"
          ) indices)
        )^" } = {|"^values^"|}"
      )
    ) [
      "A",     [],                   "int",   [],         "2";
      "B",     [],                   "int",   [],         "1";
      "A_F",   [],                   "float", [],         "2.0";
      "B_F",   [],                   "float", [],         "1.0";
      "S",     [],                   "float", [],         "5.0";
      "S_I",   [],                   "int",   [],         "5";
      "R",     ["float"; "float"],   "float", [[0]; [1]], 
        "(1.0,1.0,1.0);(1.0,2.0,2.0);(2.0,1.0,2.0);(2.0,2.0,4.0);(1.0,3.0,3.0)";
      "R_STR", ["string"; "string"], "float", [], 
        "(\"aha\",\"ahaa\",1.0)";
      "QS",    [],                   "float", [], "5.0";
      "QR",    ["float"; "float"],   "float", [], 
        "(1.0,1.0,1.0);(3.0,2.0,3.0);(2.0,1.0,9.0);(2.0,3.0,1.0);(1.0,3.0,2.0)";
    ])
;;

let prog_decls = (parse_k3prog map_decls);;

let env_decls = 
  List.map (fun (id, s_expr) ->
    let k3_expr = parse_k3 s_expr in
    let k3_type = 
      K3Typechecker.type_of_expr (K3Typechecker.deduce_expr_type [] [] k3_expr)
    in
      (Global(id, k3_type, Some(k3_expr)), [])
  )

let calc_to_k3 args calc = 
  let ((_,_,k3),_) = calc_to_k3_expr [] (List.map fst args) calc in 
      (k3,Calculus.string_of_expr) 

let stmt_to_k3 args stmt =
  let typed_args = List.map (fun (vn, vdefn) ->
    (vn, Calculus.type_of_expr (parse_calc vdefn))
  ) args in
  let (k3,_) = m3_stmt_to_k3_stmt [] typed_args stmt in 
      (k3, Plan.string_of_statement)

let translate op ?(tc = false) ?(dump=false) args calc = 
  let (k3,calc_str) = op args calc in
  let typed_k3 = 
      if tc 
      then 
        let prog_decls, env, trig_env, _ = 
          K3Typechecker.type_bindings_of_program (prog_decls@(env_decls args))
        in
          K3Typechecker.deduce_expr_type trig_env env k3
      else k3 in
  (
    if dump then (
      print_endline "-------------------------------\n";
      print_endline (calc_str calc);
      print_endline (K3Printing.string_of_expr typed_k3);
      print_endline "-------------------------------";
  )); typed_k3;;

let eval args e = 
  let _, val_env = env_of_program (prog_decls@(env_decls args)) in
    value_of_eval (snd (eval_expr val_env e))

let test_base name args actual expected =
  case name ((eval args actual) @=? (eval [] expected))
             
;;
let test_str name actual expected = 
  test_base name [] (parse_k3 actual) (parse_k3 expected)
;;
let test_calc ?(dump=false) name args calc k3val =
  test_base name args 
            (translate calc_to_k3 ~dump:dump args (parse_calc calc)) 
            (parse_k3 k3val)
;;
let test_stmt ?(dump=false) name args calc testexpr k3val =
  test_base name args 
            (K3Helpers.mk_block [
              (translate stmt_to_k3 ~dump:dump args (parse_stmt calc));
              (parse_k3 testexpr);
            ])
            (parse_k3 k3val)
;;

Debug.activate "PRINT-VERBOSE";;

List.iter run_tests [
  group "Setup Config" [
    test_str "S"   "S"   "{|5.0|}";
    test_str "A_F" "A_F" "{|2.0|}";
    test_str "B_F" "B_F" "{|1.0|}";
  ];
  group "Simple Translations" [
    group "Constants" [
      test_calc "int"    []  "3"     "3";
      test_calc "float"  []  "3."    "3.0";
      test_calc "string" []  "'foo'" "\"foo\"";
    ];
    test_calc   "Var" ["X","3.0"] "X" "3.0";
    (* unsupported atm
    group "Functions" [
      test_calc ["X";"B"] "Fn-Div-II" "{[/:float](A,B)}" "2.0"
      test_calc ["A_F";"B"] "Fn-Div-FI" "{[/:float](A_F,B)}" "2.0"
      test_calc ["A";"B_F"] "Fn-Div-IF" "{[/:float](A,B_F)}" "2.0"
      test_calc ["A_F";"B_F"] "Fn-Div-FF" "{[/:float](A_F,B_F)}" "2.0"
    ]; *)
    group "Singleton Ops" [
      test_calc "Negation-Int"         ["X","2"]           "{-X}"      "-2";
      test_calc "Negation-Float"       ["X","2.0"]         "{-X}"      "-2.0";
      test_calc "Addition-Int"         ["X","1";"Y","2"]   "{X+Y+X+Y}" "6";
      test_calc "Addition-Float"       ["X","1.0";"Y","2"] "{X+Y+X+Y}" "6.0";
      test_calc "Multiplication-Int"   ["X","2";"Y","3"]   "{X*Y*X*Y}" "36";
      test_calc "Multiplication-Float" ["X","2.0";"Y","3"] "{X*Y*X*Y}" "36.0";
    ];
    group "Comparisons" [
      test_calc "Eq-II-False"   ["X","2";"Y","1"]   "{X =  Y}"     "0";
      test_calc "Eq-II-True"    ["X","2";"Y","1"]   "{X =  Y*2}"   "1";
   (* test_calc "Eq-FI-True"    ["X","1.0";"Y","1"] "{X =  Y*2}"   "1"; *)
      
      test_calc "Lt-II-True"    ["X","2";"Y","1"]   "{X <  3*Y}"  "1";
      test_calc "Lt-II-False"   ["X","2";"Y","1"]   "{X <  2*Y}"  "0";
      test_calc "Lte-II-True"   ["X","2";"Y","1"]   "{X <= 2*Y}"  "1";
      test_calc "Lte-II-False"  ["X","2";"Y","1"]   "{X <= Y}"    "0";
      test_calc "Gt-II-True"    ["X","2";"Y","1"]   "{X >  Y}"    "1";
      test_calc "Gt-II-False"   ["X","2";"Y","1"]   "{X >  2*Y}"  "0";
      test_calc "Gte-II-True"   ["X","2";"Y","1"]   "{X >= 2*Y}"  "1";
      test_calc "Gte-II-False"  ["X","2";"Y","1"]   "{X >= 3*Y}"  "0";
      test_calc "Neq-II-True"   ["X","2";"Y","1"]   "{X <> 3*Y}"  "1";
      test_calc "Neq-II-False"  ["X","2";"Y","1"]   "{X <> 2*Y}"  "0";
    ];
    group "Externals" [
      group "Singletons" [
        test_calc "SingletonPC" []                    "A[][]"    "2";
        test_calc "OutPC"       ["X","2.0";"Y","1.0"] "R[][X,Y]" "2.0";
      ];
      group "Collections" [
        test_calc "OutPC" [] "R[][X,Y]" 
          ("{|(1.0,1.0,1.0);(1.0,2.0,2.0);(2.0,1.0,2.0);(2.0,2.0,4.0);"^
             "(1.0,3.0,3.0)|}");
        test_calc "OutPC-Slice" ["X","2.0"] "R[][X,Y]" 
          ("{|(1.0,2.0);(2.0,4.0)|}");
        test_calc "OutPC-Filtered" [] "R[][X,Y]*{X = Y}" 
          ("{|(1.0,1.0,1.0);(1.0,2.0,0.0);(2.0,1.0,0.0);(2.0,2.0,4.0);"^
             "(1.0,3.0,0.0)|}");
      ];
    ];
    group "AggSum" [
      test_calc "Singleton-Singleton"   [] "AggSum([], {5.})" "5.0";
      test_calc "Singleton-Collection"  [] "AggSum([], R[][X,Y] * {X=Y})" "5.0";
      test_calc "Collection-Collection" [] "AggSum([Y], R[][X,Y])" 
          ("{|(1.0,3.0);(2.0,6.0);(3.0,3.0)|}");
    ];
    group "Lift" [
      test_calc "Singleton1" ["X","5.0"] "(L ^= X)" "{|(5.0,1)|}";
      test_calc "Singleton2" ["X","2.0";"Y","1.0"] "(L ^= R[][X,Y])"
            "{|(2.0,1)|}";
      test_calc "Collection" [] "(L ^= R[][X,Y])"
          ("{|(1.0,1.0,1.0,1);(1.0,2.0,2.0,1);(2.0,1.0,2.0,1);(2.0,2.0,4.0,1);"^
             "(1.0,3.0,3.0,1)|}");
    ];
    group "Collection Arithmetic" [
      test_calc "Sum-Singleton" ["X","2.0";"Y","1.0"] 
            "R[][X,Y] + QR[][X,Y]"
            "11.0";
      test_calc "Sum-Collection" []
            "R[][X,Y] - QR[][X,Y]"
            ("{|(1.0,1.0,1.0);(1.0,2.0,2.0);(2.0,1.0,2.0);(2.0,2.0,4.0);"^
               "(1.0,3.0,3.0);(1.0,1.0,-1.0);(3.0,2.0,-3.0);(2.0,1.0,-9.0);"^
               "(2.0,3.0,-1.0);(1.0,3.0,-2.0)|}");
      test_calc "Prod-Singleton" ["X","2.0";"Y","1.0"] 
            "R[][Y,X] * QR[][X,Y]"
            "18.0";
      test_calc "Prod-Collection" []
            "R[][X,Y] * QR[][Y,Z]"
            ("{|(1.0,1.0,1.0,1.0);(1.0,1.0,3.0,2.0);"^
               "(1.0,2.0,1.0,18.0);(1.0,2.0,3.0,2.0);"^
               "(2.0,1.0,1.0,2.0);(2.0,1.0,3.0,4.0);"^
               "(2.0,2.0,1.0,36.0);(2.0,2.0,3.0,4.0);"^
               "(1.0,3.0,2.0,9.0)|}");
    ];
    group "Replace" [
      test_stmt "SingletonPC-Singleton" [] "QS[][] := {12.}" "QS" "{|12.0|}";
      test_stmt "OutPC-Singleton" ["A","1.0";"B","1.0"] 
          "R[][A,B] := {12.}" 
          "R" 
          ("{|(1.0,1.0,12.0);(1.0,2.0,2.0);(2.0,1.0,2.0);(2.0,2.0,4.0);"^
             "(1.0,3.0,3.0)|}");
      test_stmt ~dump:true "OutPC-Slice" ["A","1.0"] 
          "R[][A,B] := (B ^= 1.) * {12.}" 
          "R" 
          ("{|(1.0,1.0,12.0);(2.0,1.0,2.0);(2.0,2.0,4.0)|}");
(*      test_stmt "OutPC-Full" ["A","1.0"] 
          "R[][A:float,B:float] := 1* QR[][A:float,B:float]" 
          "R" 
          ("{|(1.0,1.0,1.0);(3.0,2.0,3.0);(2.0,1.0,9.0);(2.0,3.0,1.0);"^
             "(1.0,3.0,2.0)|}");*)
    ];
  ];
];;