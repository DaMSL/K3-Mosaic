open Symbols
open Tree
open Util
open K3.AST
open K3Util
open K3Printing
open K3Typechecker
open K3Values
open K3Values.Value
open K3Interpreter
module LAS = ListAsSet
module K3V = K3Values

type assertion_t =
    AssertTypeEquals of type_t * type_t
  | AssertValueEquals of value_t * value_t

type test_t =
    TestCase of string * assertion_t
  | TestGroup of string * test_t list

let group name tests = TestGroup(name, tests)
let case name assertion = TestCase(name, assertion)

let (@@) f x = f x

let (@=:?) actual expected = AssertTypeEquals(expected, actual)
let (@=?)  actual expected = AssertValueEquals(expected, actual)

(* Parsing *)
let parse_expr s = K3Parser.expr K3Lexer.tokenize (Lexing.from_string s)

(* --- Evaluation --- *)
(* we pass in an optional interpreter environment, a program for type bindings,
 * and an expression *)
let eval_test_expr_env tdecl_prog t_env ta_env trig_env val_env expr =
  let typed_expr = deduce_expr_type trig_env t_env ta_env expr in
  value_of_eval @@ snd @@
    eval_expr ("localhost", 1) None val_env typed_expr

(* we pass in an optional interpreter environment, a program for type bindings,
 * and an expression *)
let eval_test_expr peers decl_prog expr =
  (* get the type bindings from the typechecker so we have an environment for
   * typechecking *)
  let tdecl_prog, t_env, ta_env, trig_env, _ = type_bindings_of_program decl_prog in
  (* get the value environment from the interpreter, excluding the trigger
   * functions. if we pass an environment, use that *)
  let val_env =
    env_of_program ~peers ~role:"" ~type_aliases:(hashtbl_of_list ta_env)
      (K3Runtime.init_scheduler_state peers) tdecl_prog in
  eval_test_expr_env tdecl_prog t_env ta_env trig_env val_env expr

(* Tests *)
let equals_assertion comp_fn expected actual string_fn =
  if comp_fn expected actual then true, "PASSED."
  else false, "FAILED: Expected " ^ string_fn expected ^ ",\n\n but got " ^
    string_fn actual ^ "."

let ensure assertion = match assertion with
  | AssertTypeEquals(expected, actual) ->
      equals_assertion (=) expected actual string_of_type
  | AssertValueEquals(expected, actual) ->
      match find_inequality expected actual with
      | None   -> true, "PASSED"
      | Some x -> false, sp "FAILED: Expected %s, but got %s."
                (string_of_value ~mark_points:[x] expected)
                (string_of_value ~mark_points:[x] actual)

let run_tests ?(indent="") test =
  let rec loop indent test =
    match test with
    | TestCase(name, assertion) -> (
        let passed, result_string = ensure assertion in
        passed, indent ^ name ^ ": " ^ result_string
    )
    | TestGroup(name, tests) ->
        let passed, ss = list_unzip @@ List.map (loop @@ "  "^indent) tests in
        List.exists id_fn passed, indent^name^":\n"^String.concat "\n" ss
  in
  loop "" test

let print_tests tests =
  let passed, ss = list_unzip @@ List.map run_tests tests in
  let passed = List.for_all id_fn passed in
  let s = String.concat "\n" ss in
  if passed then print_endline s
  else prerr_endline @@ "ERROR: "^s

(* Driver methods *)
let check_as_expr ce = match ce with
  | InlineExpr (nm, e) -> nm, e
  | FileExpr (fp)      -> fp, parse_expr @@ read_file fp

let test_expressions peers file_name = function
  | ExprTest expr_tests ->
    let test_cases = snd @@
      List.fold_left (fun (i, test_acc) (decls, e, x) ->
        let name, expr = check_as_expr x in
        let test_case  = case name @@
          eval_test_expr peers decls e @=? eval_test_expr peers decls expr
        in i+1, test_acc@[test_case]
      ) (0, []) expr_tests
    in print_tests test_cases
  | _ -> failwith "not expression tests"

let env_deref_refs env = List.rev_map (fun (id, v) -> (id, !v)) env

let extract_first_env = function
  | (addr, env)::_ -> env
  | [] -> invalid_arg "no environment"

let unify_tuple_lists id l1 l2 =
  (* hack to make sure the disjoint test doesn't pick up on things that always
   * overlap *)
  match id with
  | "pmap_data" | "node_ring" | "peers" -> LAS.union l1 l2
  | _ ->
    LAS.union l1 l2

(* unify the values of the same ids in different environments *)
(* only does one level of unification *)
let unify_values id r_newval = function
  | None -> Some r_newval
  | Some r_oldval -> (* we found an old value *)
    let newval, oldval = !r_newval, !r_oldval in
    let unwrap_ind = function
      | VIndirect r -> !r
      | x           -> x
    in
    let wrap_ind x =
      match oldval with VIndirect _ -> ref (VIndirect(ref x)) | _ -> ref x
    in
    let err _ s = failwith @@ sp "(unify_values):%s" s in
    let v, v' = unwrap_ind oldval, unwrap_ind newval in
    if K3V.matching_collections v v' then some @@ wrap_ind @@ v_combine err v v'
    else Some r_newval

(* unify the environments of different nodes *)
let unify_envs (envs : (address * env_t) list) =
  (* fold over all the nodes. we discard triggers and frames *)
  let unified_env =
    (* ignore triggers and frames *)
    List.fold_left (fun acc (addr, env) ->
      IdMap.fold (fun id newval acc' ->
          map_modify (unify_values id newval) id acc')
        acc env.globals)
      IdMap.empty envs
  in
  {default_env with globals=unified_env} (* no frames *)

(* test a program and comare it to the expected output. Takes an interpretation
 * function that expects an untyped AST (this takes care of handling any extra
 * information needed by the interpreter) also takes a program_test data
 * structure indicating the kind of test desired *)
let test_program peers globals_k3 interpret_fn file_name test =
  let op_fn, program, tests = match test with
    (* for a single-site test, we just extract the env. For multi-site, we unify
     * the environments, and rename all node variables to be node_var (TODO) *)
    | ProgTest(prog, checkl)    -> extract_first_env, prog, checkl
    | NetworkTest(prog, checkl) -> unify_envs, prog, checkl
    | _ -> failwith "unsupported test"
  in
  let node_envs = interpret_fn program in

  (* print out the environments *)
  (* debug
  List.iter (fun (addr, env) ->
    Printf.printf "Environment for %s\n" (K3Printing.string_of_address addr);
    print_endline @@ K3Values.string_of_env ~skip_functions:true ~skip_empty:false ~accessed_only:false env
  ) node_envs;
  *)

  (* unify the node value environments if needed *)
  let v_env = op_fn node_envs in

  (* debug - print the unified env *)
  (*Printf.printf "Unified environment:\n";*)
  (*print_endline @@ K3Values.string_of_env ~skip_functions:false v_env;*)

  let numbered_tests = insert_index_fst ~first:1 tests in
  let prog_globals = globals_k3 @ program in
  (* get the type bindings from the typechecker so we have an environment for
   * typechecking *)
  let tdecl_prog, t_env, ta_env, trig_env, _ = type_bindings_of_program prog_globals in
  let test_cases = list_map (fun (i, (lexp, rexp)) ->
      let name, expr = check_as_expr rexp in
      let v1 = eval_test_expr_env tdecl_prog t_env ta_env trig_env v_env lexp in
      let v2 = eval_test_expr peers [] expr in
      case name @@ v1 @=? v2
    ) numbered_tests
  in print_tests test_cases

