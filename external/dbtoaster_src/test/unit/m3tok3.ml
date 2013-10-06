module T = Type
module C = Constants
module K = K3
module MK = M3ToK3
module U = UnitTest
module Vs = Values
module VK = Vs.K3Value
module Interpreter = K3Compiler.Make(K3Interpreter.K3CG)
;;

let maps = [
   "S", [], [], T.TFloat;
   "S_I", [], [], T.TInt;
   "R", [], ["X", T.TFloat; "Y", T.TFloat], T.TFloat;
   "R_STR", [], ["X", T.TString; "Y", T.TString], T.TFloat;
   "T", ["X", T.TFloat; "Y", T.TFloat], [], T.TFloat;
   "W", ["X", T.TFloat; "Y", T.TFloat], ["Z", T.TFloat; "ZZ", T.TFloat],
        T.TFloat;
 
   "sum_tmp_1", [],["X", T.TFloat], T.TFloat;
   "QS", [], [], T.TFloat;
   "QR", [], ["X", T.TFloat; "Y", T.TFloat], T.TFloat;
   "QT", ["X", T.TFloat; "Y", T.TFloat], [], T.TFloat;
   "QW", ["X", T.TFloat; "Y", T.TFloat], ["Z", T.TFloat; "ZZ", T.TFloat],
         T.TFloat;
]
;;

let patterns = [
   "R", [Patterns.Out(["X"], [0]); Patterns.Out(["Y"], [1])];
   "W", [Patterns.Out(["Z"], [0]); Patterns.Out(["ZZ"], [1])];
]
;;

let init_code = 
   let pc_s = K.SingletonPC("S", K.TBase(T.TFloat)) in
   let pc_si = K.SingletonPC("S_I", K.TBase(T.TInt)) in
   let pc_r = K.OutPC("R", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)],
                      K.TBase(T.TFloat)) 
   in
   let pc_r_str = K.OutPC("R_STR", ["X", K.TBase(T.TString); "Y",
                          K.TBase(T.TString)], K.TBase(T.TFloat))
   in
   let pc_t = K.InPC("T", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)],
                     K.TBase(T.TFloat)) 
   in
   let pc_w = K.PC("W", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)], 
                   ["Z", K.TBase(T.TFloat); "ZZ", K.TBase(T.TFloat)],
                   K.TBase(T.TFloat)) 
   in

   let pc_qs = K.SingletonPC("QS", K.TBase(T.TFloat)) in
   let pc_qr = K.OutPC("QR", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)],
                       K.TBase(T.TFloat)) 
   in
   let pc_qt = K.InPC("QT", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)],
                      K.TBase(T.TFloat)) 
   in
   let pc_qw = K.PC("QW", ["X", K.TBase(T.TFloat); "Y", K.TBase(T.TFloat)],
                    ["Z", K.TBase(T.TFloat); "ZZ", K.TBase(T.TFloat)],
                    K.TBase(T.TFloat)) 
   in

   let to_kconst cst = K.Const(C.CFloat(cst)) in
   let to_kconsts csts = List.map to_kconst csts in
   Interpreter.compile_k3_expr (K.Block([
      K.PCValueUpdate(pc_s, [], [], to_kconst 5.);
      K.PCValueUpdate(pc_si, [], [], K.Const(C.CInt(5)));

      K.PCValueUpdate(pc_r, [], to_kconsts [1.; 1.;], to_kconst 1.);
      K.PCValueUpdate(pc_r, [], to_kconsts [1.; 2.;], to_kconst 2.);
      K.PCValueUpdate(pc_r, [], to_kconsts [2.; 1.;], to_kconst 2.);
      K.PCValueUpdate(pc_r, [], to_kconsts [2.; 2.;], to_kconst 4.);
      K.PCValueUpdate(pc_r, [], to_kconsts [1.; 3.;], to_kconst 3.);

      K.PCValueUpdate(pc_r_str, [], [K.Const(C.CString("aha"));
                      K.Const(C.CString("ahaa"));], to_kconst 1.);

      K.PCValueUpdate(pc_t, to_kconsts [1.; 1.;], [], to_kconst 1.);
      K.PCValueUpdate(pc_t, to_kconsts [1.; 2.;], [], to_kconst 2.);
      K.PCValueUpdate(pc_t, to_kconsts [2.; 1.;], [], to_kconst 2.);
      K.PCValueUpdate(pc_t, to_kconsts [2.; 2.;], [], to_kconst 4.);
      K.PCValueUpdate(pc_t, to_kconsts [1.; 3.;], [], to_kconst 3.);

      K.PCValueUpdate(pc_w, to_kconsts [1.; 1.;], to_kconsts [1.; 1.;],
                      to_kconst 1.);
      K.PCValueUpdate(pc_w, to_kconsts [1.; 1.;], to_kconsts [1.; 2.;],
                      to_kconst 2.);
      K.PCValueUpdate(pc_w, to_kconsts [2.; 2.;], to_kconsts [2.; 1.;],
                      to_kconst 2.);
      K.PCValueUpdate(pc_w, to_kconsts [2.; 2.;], to_kconsts [2.; 2.;],
                      to_kconst 4.);
      K.PCValueUpdate(pc_w, to_kconsts [3.; 3.;], to_kconsts [1.; 3.;],
                     to_kconst 3.);

      K.PCValueUpdate(pc_qs, [], [], to_kconst 5.);

      K.PCValueUpdate(pc_qr, [], to_kconsts [1.; 1.;], to_kconst 1.);
      K.PCValueUpdate(pc_qr, [], to_kconsts [1.; 2.;], to_kconst 2.);
      K.PCValueUpdate(pc_qr, [], to_kconsts [2.; 1.;], to_kconst 2.);
      K.PCValueUpdate(pc_qr, [], to_kconsts [2.; 2.;], to_kconst 4.);
      K.PCValueUpdate(pc_qr, [], to_kconsts [1.; 3.;], to_kconst 3.);

      K.PCValueUpdate(pc_qt, to_kconsts [1.; 1.;], [], to_kconst 1.);
      K.PCValueUpdate(pc_qt, to_kconsts [1.; 2.;], [], to_kconst 2.);
      K.PCValueUpdate(pc_qt, to_kconsts [2.; 1.;], [], to_kconst 2.);
      K.PCValueUpdate(pc_qt, to_kconsts [2.; 2.;], [], to_kconst 4.);
      K.PCValueUpdate(pc_qt, to_kconsts [1.; 3.;], [], to_kconst 3.);

      K.PCValueUpdate(pc_qw, to_kconsts [1.; 1.;], to_kconsts [1.; 1.;],
                      to_kconst 1.);
      K.PCValueUpdate(pc_qw, to_kconsts [1.; 1.;], to_kconsts [1.; 2.;],
                      to_kconst 2.);
      K.PCValueUpdate(pc_qw, to_kconsts [2.; 2.;], to_kconsts [2.; 1.;],
                      to_kconst 2.);
      K.PCValueUpdate(pc_qw, to_kconsts [2.; 2.;], to_kconsts [2.; 2.;],
                      to_kconst 4.);
      K.PCValueUpdate(pc_qw, to_kconsts [2.; 2.;], to_kconsts [1.; 1.;],
                      to_kconst 1.);
      K.PCValueUpdate(pc_qw, to_kconsts [2.; 2.;], to_kconsts [1.; 2.;],
                      to_kconst 2.);
      K.PCValueUpdate(pc_qw, to_kconsts [3.; 3.;], to_kconsts [1.; 3.;],
                      to_kconst 3.);
      K.PCValueUpdate(pc_qw, to_kconsts [3.; 3.;], to_kconsts [1.; 1.;],
                      to_kconst 1.);
      K.PCValueUpdate(pc_qw, to_kconsts [3.; 3.;], to_kconsts [1.; 2.;],
                      to_kconst 2.);
   ]))
in

let calc_string_to_code env generate_init calc_s =
   let calc = U.parse_calc calc_s in
   let env_vars = List.map (fun (vn,vl) -> (vn,C.type_of_const vl)) env in
   let env_el = MK.varIdType_to_k3_expr env_vars in
   let (_,_,code),_ = MK.calc_to_k3_expr MK.empty_meta
                                        ~generate_init:generate_init 
                                        env_el calc 
   in
   (*
   print_endline "\n--------------\n--------------";
   print_endline ("Calculus: "^(Calculus.string_of_expr calc));
   print_endline "\n--------------";
   print_endline ("K3Expr: "^(K.nice_code_of_expr code));
   *)
   Interpreter.compile_k3_expr code
in

let stmt_string_to_code ?(debug=false) env generate_init stmt_s =
   let stmt = U.parse_stmt stmt_s in
   let env_vars = List.map (fun (vn,vl) -> (vn,C.type_of_const vl)) env in
   let code,_ = MK.m3_stmt_to_k3_stmt MK.empty_meta 
                                      ~generate_init:generate_init 
                                      env_vars stmt 
   in
   if debug then (
      print_endline (K3.nice_string_of_expr code [])
   );
   let target_coll = MK.target_of_statement code in
   (*
   print_endline "\n--------------\n--------------";
   print_endline ("Calculus: "^(Plan.string_of_statement stmt));
   print_endline "\n--------------";
   print_endline ("K3Expr: "^(K.nice_code_of_expr code));
   *)
   Interpreter.compile_k3_expr (K.Block([code;target_coll]))
in

let test_code env msg code rval =
   let (vars, vals) = List.split env in
   let db = Database.NamedK3Database.make_empty_db maps patterns in
   let _ = (K3Interpreter.K3CG.eval None init_code [] [] db) in
      U.log_test ("M3ToK3("^msg^")")
                 Vs.K3Value.string_of_value
                 (K3Interpreter.K3CG.eval None code vars vals db)
                 rval
in

let test_code_coll env msg code rval =
   try 
      let (vars, vals) = List.split env in
      let db = Database.NamedK3Database.make_empty_db maps patterns in
      let _ = (K3Interpreter.K3CG.eval None init_code [] [] db) in
      U.log_collection_test ("M3ToK3("^msg^")")
                            (K3Interpreter.K3CG.eval None code vars vals db)
                            rval
   with 
      | K3Interpreter.InterpreterException(expr, msg) ->
         print_endline "Bug in program: ";
         print_endline (K3Interpreter.K3CG.debug_string code);
         print_endline ("Error: "^msg^"; in: ");
         begin match expr with 
            | Some(s) -> 
               print_endline (K3.nice_string_of_expr s [])
            | None -> print_endline "[???]"
         end;
         exit(-1)
in

let test_expr ?(env = []) ?(generate_init = false) msg calc_s rval =
   let code = calc_string_to_code env generate_init calc_s in
   test_code env msg code rval
in
let test_expr_coll ?(env = []) ?(generate_init = false) msg calc_s rval =
   let code = calc_string_to_code env generate_init calc_s in
   test_code_coll env msg code (U.mk_float_collection rval)
in
let test_expr_coll2 ?(env = []) ?(generate_init = false) msg calc_s rval =
   let code = calc_string_to_code env generate_init calc_s in
   test_code_coll env msg code rval
in

let test_stmt ?(env = []) ?(generate_init = false) ?(debug=false) 
              msg stmt_s rval =
   let code = stmt_string_to_code ~debug:debug env generate_init stmt_s in
   test_code env msg code rval
in
let test_stmt_coll ?(env = []) ?(generate_init = false) ?(debug=false) 
                   msg stmt_s rval =
   let code = stmt_string_to_code ~debug:debug env generate_init stmt_s in
   test_code_coll env msg code (U.mk_float_collection rval)
in
   (*
      test_expr " Lift and Divide "
                "[ [ / : INT ] (0, 0) ]"
                (VK.BaseValue(C.CFloat(0.125)));*)
      (**)
      test_expr " AConst " 
                "3" (VK.BaseValue(C.CInt(3)));
      test_expr " AVar "
                ~env:["A", (C.CFloat(3.))]
                "A" (VK.BaseValue(C.CFloat(3.)));
      test_expr " AFn " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(2.))]
                "{ [ / : FLOAT ] ( A, B ) }" (VK.BaseValue(C.CFloat(1.5)));
      test_expr " AFn "
                ~env:["A", (C.CFloat(3.));"B", (C.CInt(2))]
                "{ [/ : FLOAT ] ( A, B : INT) }" (VK.BaseValue(C.CFloat(1.5)));
      test_expr " AFn "
                ~env:["A", (C.CInt(3));"B", (C.CFloat(2.))]
                "{ [ / : FLOAT ] ( A : INT, B ) }"
                (VK.BaseValue(C.CFloat(1.5)));
      test_expr " AFn "
                ~env:["A", (C.CInt(2));"B", (C.CInt(2))]
                "{ [ / : INT ] ( A : INT, B : INT ) }" 
                (VK.BaseValue(C.CInt(1)));

      test_expr " Singleton Negation " 
                ~env:["A", (C.CFloat(3.))]
                "{-A}" (VK.BaseValue(C.CFloat(-3.)));
      test_expr " Singleton Sum " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A+B+A+B}" (VK.BaseValue(C.CFloat(12.)));
      test_expr " Singleton Prod " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A*B*A*B}" (VK.BaseValue(C.CFloat(81.)));

      test_expr " Cmp Eq Float = Float" 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A=B}" (VK.BaseValue(C.CInt(1)));
      test_expr " Cmp Eq Float = Int" 
                ~env:["A", (C.CFloat(3.));"B", (C.CInt(3))]
                "{ A = B:INT }" (VK.BaseValue(C.CInt(1)));

      test_expr " Cmp Lt " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A<B}" (VK.BaseValue(C.CInt(0)));
      test_expr " Cmp Lte " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A<=B}" (VK.BaseValue(C.CInt(1)));
      test_expr " Cmp Gt " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A>B - 1}" (VK.BaseValue(C.CInt(1)));
      test_expr " Cmp Gte " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A>=B+1}" (VK.BaseValue(C.CInt(0)));
      test_expr " Cmp Neq " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.))]
                "{A!=B}" (VK.BaseValue(C.CInt(0)));

      test_expr " External - SingletonPC - Singleton " 
                "S[][]" (VK.BaseValue(C.CFloat(5.)));
      test_expr " External - OutPC - Singleton " 
                ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.))]
                "R[][A,B]" (VK.BaseValue(C.CFloat(4.)));
      test_expr " External - InPC - Singleton " 
                ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.))]
                "T[A,B][]" (VK.BaseValue(C.CFloat(4.)));
      test_expr " External - PC - Singleton " 
                ~env:["A", (C.CFloat(3.));"B", (C.CFloat(3.));
                      "C", (C.CFloat(1.));"D", (C.CFloat(3.))]
                "W[A,B][C,D]" (VK.BaseValue(C.CFloat(3.)));

      test_expr_coll " External - OutPC - Collection " 
                     "R[][A,B]" 
                     [  [1.; 3.], 3.;
                        [1.; 2.], 2.;
                        [2.; 2.], 4.;
                        [1.; 1.], 1.;
                        [2.; 1.], 2.;
                     ];
      test_expr_coll " External - PC - Collection " 
                     ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.))]
                     "W[A,B][C,D]" 
                     [  [2.; 2.], 4.;
                        [2.; 1.], 2.;
                     ];
      
      test_expr_coll " External - OutPC - Collection Slice " 
                     ~env:["B", (C.CFloat(2.))]
                     "R[][A,B]" 
                     [  [2.], 4.;
                        [1.], 2.;
                     ];
      test_expr_coll " External - PC - Collection Slice " 
                     ~env:["A", (C.CFloat(1.));
                           "B", (C.CFloat(1.));
                           "C", (C.CFloat(1.))]
                     "W[A,B][C,D]" 
                     [  [2.], 2.;
                        [1.], 1.;
                     ];
      
      
      test_expr " External - OutPC - Singleton Init " 
                ~env:["A", (C.CFloat(2.));"B", (C.CFloat(3.))]
                ~generate_init:true
                "R[][A,B]" (VK.BaseValue(C.CFloat(0.)));
      test_expr " External - InPC - Singleton Init " 
                ~env:["A", (C.CFloat(2.));
                      "B", (C.CFloat(3.));
                      "D", (C.CFloat(1.))]
                ~generate_init:true
                "T[A,B][]( R[][A,D] * 3 )" 
                (VK.BaseValue(C.CFloat(6.)));
      test_expr " External - PC - Singleton Init " 
                ~env:["A", (C.CFloat(2.));
                      "B", (C.CFloat(3.));
                      "C", (C.CFloat(1.));
                      "D", (C.CFloat(3.))]
                ~generate_init:true
                "W[A,B][C,D]( R[][C,D] * 3 )" 
                (VK.BaseValue(C.CFloat(9.)));
      test_expr_coll " External - PC - Collection Init " 
                     ~env:["A", (C.CFloat(2.));"B", (C.CFloat(3.))]
                     ~generate_init:true
                     "W[A,B][C,D]( R[][C,D] * 3 )" 
                     [  [1.; 3.], 9.;
                        [1.; 2.], 6.;
                        [2.; 2.], 12.;
                        [1.; 1.], 3.;
                        [2.; 1.], 6.;
                     ];
      test_expr_coll " External - PC - Slice Init " 
                     ~env:["A", (C.CFloat(2.));
                           "B", (C.CFloat(3.));
                           "C", (C.CFloat(2.))]
                     ~generate_init:true
                     "W[A,B][C,D]( R[][C,D] * 3 )" 
                     [  [2.], 12.;
                        [1.], 6.;
                     ];
      
      
      test_expr " AggSum Singleton from Singleton "
                "AggSum([], {5.})" 
                (VK.BaseValue(C.CFloat(5.)));
      test_expr " AggSum Singleton from Collection "
                "AggSum([], R[][A,B] * {A = B})" 
                (VK.BaseValue(C.CFloat(5.)));
      test_expr_coll " AggSum Collection "
                     "AggSum([B], R[][A,B])" 
                     [  [2.], 6.;
                        [1.], 3.;
                        [3.], 3.;
                     ];
      
      test_expr_coll2 " Lift Singleton "
                     ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.));]
                     "( L ^= ( R[][A,B] ) )"
                     [ [(VK.BaseValue(C.CFloat(4.)))],
                       (VK.BaseValue(C.CInt(1)))];
      test_expr_coll2 " Lift Collection "
                     ~env:[]
                     "QR[][A,B] * ( L ^= ( R[][A,B] ) )"
                     [ [(VK.BaseValue(C.CFloat(1.)));
                        (VK.BaseValue(C.CFloat(1.)));
                        (VK.BaseValue(C.CFloat(1.)))],
                       (VK.BaseValue(C.CFloat(1.)));
                       [(VK.BaseValue(C.CFloat(1.)));
                        (VK.BaseValue(C.CFloat(2.)));
                        (VK.BaseValue(C.CFloat(2.)))],
                       (VK.BaseValue(C.CFloat(2.)));
                       [(VK.BaseValue(C.CFloat(2.)));
                        (VK.BaseValue(C.CFloat(1.)));
                        (VK.BaseValue(C.CFloat(2.)))],
                       (VK.BaseValue(C.CFloat(2.)));
                       [(VK.BaseValue(C.CFloat(2.)));
                        (VK.BaseValue(C.CFloat(2.)));
                        (VK.BaseValue(C.CFloat(4.)))],
                       (VK.BaseValue(C.CFloat(4.)));
                       [(VK.BaseValue(C.CFloat(1.)));
                        (VK.BaseValue(C.CFloat(3.)));
                        (VK.BaseValue(C.CFloat(3.)))],
                       (VK.BaseValue(C.CFloat(3.)));                       
                     ];
                     
      test_expr " Sum Singleton "
                ~env:["A", (C.CFloat(1.));
                      "B", (C.CFloat(3.));
                      "C", (C.CFloat(2.));
                      "D", (C.CFloat(2.));
                      "E", (C.CFloat(2.));
                      "F", (C.CFloat(2.));]
                " R[][A,B] + W[C,D][E,F] " 
                (VK.BaseValue(C.CFloat(7.)));
      test_expr_coll " Sum Collection "
                     ~env:["A", (C.CFloat(1.));
                           "C", (C.CFloat(3.));
                           "D", (C.CFloat(2.));
                           "E", (C.CFloat(2.));]
                     " R[][A,B] + W[C,C][A,B] - W[D,D][E,B] " 
                     [  [2.], -2.;
                        [1.], -1.;
                        [3.], 6.;
                     ];
      
      test_expr " Prod Singleton "
                ~env:["A", (C.CFloat(1.));
                      "B", (C.CFloat(3.));
                      "C", (C.CFloat(2.));
                      "D", (C.CFloat(2.));
                      "E", (C.CFloat(2.));
                      "F", (C.CFloat(2.));]
                " R[][A,B] * W[C,D][E,F] " 
                (VK.BaseValue(C.CFloat(12.)));
      test_expr_coll " Prod Collection "
                     ~env:["A", (C.CFloat(1.));
                           "C", (C.CFloat(3.));
                           "D", (C.CFloat(2.));
                           "E", (C.CFloat(2.));]
                     " R[][A,Y] * W[C,C][A,ZZ] * W[C,C][A,ZZ] * W[D,D][E,ZZZ] "
                     [  [2.; 3.; 1.], 36.;
                        [2.; 3.; 2.], 72.;
                        [1.; 3.; 1.], 18.;
                        [1.; 3.; 2.], 36.;
                        [3.; 3.; 1.], 54.;
                        [3.; 3.; 2.], 108.;
                     ];
      
      test_stmt " Replace - SingletonPC - Singleton"
                " QS[][] := {12.} " 
                (VK.BaseValue(C.CFloat(12.)));

      test_stmt_coll " Replace - OutPC - Singleton"
                     ~env:["A", (C.CFloat(1.));
                           "B", (C.CFloat(1.));
                           "C", (C.CFloat(2.));]
                     " QR[][B,C] := W[A,A][B,C] * 3. " 
                     [  [1.; 1.], 1.;
                        [1.; 2.], 6.;
                        [1.; 3.], 3.;
                        [2.; 1.], 2.;
                        [2.; 2.], 4.;
                     ];
      test_stmt_coll " Replace - InPC - Singleton"
                     " QT[X,Y][] := {X  + Y} " 
                     [  [1.; 1.], 2.;
                        [1.; 2.], 3.;
                        [1.; 3.], 4.;
                        [2.; 1.], 3.;
                        [2.; 2.], 4.;
                     ];
      test_stmt_coll " Replace - PC - Singleton"
                     ~env:["A", (C.CFloat(1.));
                           "B", (C.CFloat(1.));
                           "C", (C.CFloat(2.));]
                     " QW[X,Y][B,C] := W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 1.; 
                        [1.; 1.; 1.; 2.], 6.;
                        [2.; 2.; 2.; 1.], 2.; 
                        [2.; 2.; 2.; 2.], 4.; 
                        [2.; 2.; 1.; 1.], 1.; 
                        [2.; 2.; 1.; 2.], 6.;
                        [3.; 3.; 1.; 3.], 3.; 
                        [3.; 3.; 1.; 1.], 1.; 
                        [3.; 3.; 1.; 2.], 6.;  
                  ];    
      test_stmt_coll " Replace - OutPC - Collection"
                     ~env:["A", (C.CFloat(2.));]
                     " QR[][B,C] := W[A,A][B,C] * 3. " 
                     [  
                        [2.; 1.], 6.;
                        [2.; 2.], 12.;
                     ];
      test_stmt_coll " Replace - PC - Collection"
                     ~env:["A", (C.CFloat(1.));]
                     " QW[X,Y][B,C] := W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 3.; [1.; 1.; 1.; 2.], 6.;
                        [2.; 2.; 1.; 1.], 3.; [2.; 2.; 1.; 2.], 6.;
                        [3.; 3.; 1.; 1.], 3.; [3.; 3.; 1.; 2.], 6.;  
                     ];    
      
      test_stmt " Update - SingletonPC - Singleton"
                " QS[][] += 12. " 
                (VK.BaseValue(C.CFloat(17.)));
      test_stmt_coll " Update - OutPC - Singleton"
                     ~env:["A", (C.CFloat(1.));
                           "B", (C.CFloat(1.));
                           "C", (C.CFloat(2.));]
                     " QR[][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.], 1.;
                        [1.; 2.], 8.;
                        [1.; 3.], 3.;
                        [2.; 1.], 2.;
                        [2.; 2.], 4.;
                     ];
      test_stmt_coll " Update - InPC - Singleton"
                     " QT[X,Y][] += {X  + Y} " 
                     [  [1.; 1.], 3.;
                        [1.; 2.], 5.;
                        [1.; 3.], 7.;
                        [2.; 1.], 5.;
                        [2.; 2.], 8.;
                     ];
      test_stmt_coll " Update - PC - Singleton"
                     ~env:["A", (C.CFloat(1.));
                           "B", (C.CFloat(1.));
                           "C", (C.CFloat(2.));]
                     " QW[X,Y][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 1.; 
                        [1.; 1.; 1.; 2.], 8.;
                        [2.; 2.; 2.; 1.], 2.; 
                        [2.; 2.; 2.; 2.], 4.; 
                        [2.; 2.; 1.; 1.], 1.; 
                        [2.; 2.; 1.; 2.], 8.;
                        [3.; 3.; 1.; 3.], 3.; 
                        [3.; 3.; 1.; 1.], 1.; 
                        [3.; 3.; 1.; 2.], 8.;
                  ];    
      test_stmt_coll " Update - OutPC - Collection"
                     ~env:["A", (C.CFloat(2.));]
                     " QR[][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.], 1.;
                        [1.; 2.], 2.;
                        [1.; 3.], 3.;
                        [2.; 1.], 8.;
                        [2.; 2.], 16.;
                     ];
      test_stmt_coll " Update - PC - Collection"
                     ~env:["A", (C.CFloat(1.));]
                     " QW[X,Y][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 4.; [1.; 1.; 1.; 2.], 8.;
                        [2.; 2.; 2.; 1.], 2.; [2.; 2.; 2.; 2.], 4.; 
                        [2.; 2.; 1.; 1.], 4.; [2.; 2.; 1.; 2.], 8.;
                        [3.; 3.; 1.; 3.], 3.; [3.; 3.; 1.; 1.], 4.; 
                        [3.; 3.; 1.; 2.], 8.;  
                     ];    
      test_stmt_coll " Update - OutPC - Slice"
                     ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.));]
                     " QR[][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.], 1.;
                        [1.; 2.], 2.;
                        [1.; 3.], 3.;
                        [2.; 1.], 8.;
                        [2.; 2.], 16.;
                     ];
      test_stmt_coll " Update - PC - Slice"
                     ~env:["A", (C.CFloat(1.));"B", (C.CFloat(1.));]
                     " QW[X,Y][B,C] += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 4.; [1.; 1.; 1.; 2.], 8.;
                        [2.; 2.; 2.; 1.], 2.; [2.; 2.; 2.; 2.], 4.; 
                        [2.; 2.; 1.; 1.], 4.; [2.; 2.; 1.; 2.], 8.;
                        [3.; 3.; 1.; 3.], 3.; [3.; 3.; 1.; 1.], 4.; 
                        [3.; 3.; 1.; 2.], 8.;  
                  ];
      
      test_stmt_coll " Update - PC - Singleton Init "
                     ~env:["A", (C.CFloat(2.));
                           "B", (C.CFloat(2.));
                           "C", (C.CFloat(1.));]
                     ~generate_init:true
                     " QW[X,Y][B,C] ( R[][B,C] * 3 ) += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 1.; 
                        [1.; 1.; 1.; 2.], 2.; 
                        [1.; 1.; 2.; 1.], 12.;
                        [2.; 2.; 2.; 1.], 8.; 
                        [2.; 2.; 2.; 2.], 4.; 
                        [2.; 2.; 1.; 1.], 1.; 
                        [2.; 2.; 1.; 2.], 2.;
                        [3.; 3.; 1.; 3.], 3.; 
                        [3.; 3.; 1.; 1.], 1.; 
                        [3.; 3.; 1.; 2.], 2.; 
                        [3.; 3.; 2.; 1.], 12.;
                  ];    
      test_stmt_coll " Update - PC - Collection Init "
                     ~env:["A", (C.CFloat(2.));"B", (C.CFloat(2.));]
                     ~generate_init:true
                     " QW[X,Y][B,C] ( R[][B,C] * 3 ) += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 1.; [1.; 1.; 1.; 2.], 2.; 
                        [1.; 1.; 2.; 1.], 12.; [1.; 1.; 2.; 2.], 24.;
                        [2.; 2.; 2.; 1.], 8.; [2.; 2.; 2.; 2.], 16.; 
                        [2.; 2.; 1.; 1.], 1.; [2.; 2.; 1.; 2.], 2.;
                        [3.; 3.; 1.; 3.], 3.; [3.; 3.; 1.; 1.], 1.; 
                        [3.; 3.; 1.; 2.], 2.; [3.; 3.; 2.; 1.], 12.; 
                        [3.; 3.; 2.; 2.], 24.;
                  ];    
      test_stmt_coll " Update - PC - Slice Init "
                     ~env:["A", (C.CFloat(2.));]
                     ~generate_init:true
                     " QW[X,Y][B,C] ( R[][B,C] * 3 ) += W[A,A][B,C] * 3. " 
                     [  [1.; 1.; 1.; 1.], 1.; [1.; 1.; 1.; 2.], 2.; 
                        [1.; 1.; 2.; 1.], 12.; [1.; 1.; 2.; 2.], 24.;
                        [2.; 2.; 2.; 1.], 8.; [2.; 2.; 2.; 2.], 16.; 
                        [2.; 2.; 1.; 1.], 1.; [2.; 2.; 1.; 2.], 2.;
                        [3.; 3.; 1.; 3.], 3.; [3.; 3.; 1.; 1.], 1.; 
                        [3.; 3.; 1.; 2.], 2.; [3.; 3.; 2.; 1.], 12.; 
                        [3.; 3.; 2.; 2.], 24.;
                  ];    
      (**)
      ()