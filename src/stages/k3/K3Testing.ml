open Symbols
open Tree
open Util
open K3.AST
open K3Util
open K3Printing
open K3Typechecker
open K3Values
open K3Interpreter
module LAS = ListAsSet

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

(* --- Evaluation --- *)
(* we pass in an optional interpreter environment, a program for type bindings,
 * and an expression *)
let eval_test_expr_env tdecl_prog t_env trig_env val_env expr = 
  let typed_expr = deduce_expr_type trig_env t_env expr in
  value_of_eval @: snd @: 
    eval_expr ("localhost", 1) None val_env typed_expr

(* we pass in an optional interpreter environment, a program for type bindings,
 * and an expression *)
let eval_test_expr decl_prog expr = 
  (* get the type bindings from the typechecker so we have an environment for
   * typechecking *)
  let tdecl_prog, t_env, trig_env, _ = type_bindings_of_program decl_prog in
  (* get the value environment from the interpreter, excluding the trigger
   * functions. if we pass an environment, use that *)
  let _, val_env = env_of_program (K3Runtime.init_scheduler_state ()) tdecl_prog in
  eval_test_expr_env tdecl_prog t_env trig_env val_env expr

(* Tests *)
let equals_assertion comp_fn expected actual string_fn =
  if comp_fn expected actual then "PASSED."
  else "FAILED: Expected " ^ string_fn expected ^ ", but got " ^ 
    string_fn actual ^ "."

let ensure assertion = match assertion with
  | AssertTypeEquals(expected, actual) -> 
      equals_assertion (=) expected actual string_of_type
  | AssertValueEquals(expected, actual) -> 
      match find_inequality expected actual with
      | [] -> "PASSED"
      | xs -> "FAILED: Expected " ^ string_of_value ~mark_points:xs expected ^ ", but got " ^
      string_of_value ~mark_points:xs actual ^ "."

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
let check_as_expr ce = match ce with
  | InlineExpr e -> e
  | FileExpr (fp) -> parse_expr @: read_file fp

let test_expressions file_name = function
  | ExprTest expr_tests ->
    let test_cases = snd @:
      List.fold_left (fun (i, test_acc) (decls, e, x) ->
        let name = file_name^" "^(string_of_int i) in
        let test_case = case name @: 
          eval_test_expr decls e @=? eval_test_expr decls @: check_as_expr x
        in i+1, test_acc@[test_case]
      ) (0, []) expr_tests
    in List.iter run_tests test_cases
  | _ -> failwith "not expression tests"

let env_deref_refs env = List.rev_map (fun (id, v) -> (id, !v)) env

let extract_first_env = function
  | (addr, (_, (env, _)))::_ -> (env, [])
  | [] -> invalid_arg "no environment"

let unify_tuple_lists id l1 l2 =
  (* hack to make sure the disjoint test doesn't pick up on things that always
   * overlap *)
  match id with
  | "pmap_data" | "node_ring" | "peers" -> LAS.union l1 l2
  | _ -> 
    (* can't use disjointness since nodes copy over maps to work with them *)
    (* check that our lists are disjoint *)
    (*let tuple_remove_value = function*)
      (*| VTuple tuplist -> VTuple(list_drop_end 1 tuplist)*)
      (*| _ -> failwith @: Printf.sprintf "%s is not a tuple!" id*)
    (*in*)
    (*let l1' = List.rev_map tuple_remove_value l1 in*)
    (*let l2' = List.rev_map tuple_remove_value l2 in*)
    (*if LAS.inter l1' l2' <> []*)
    (*then failwith @: Printf.sprintf "In %s, lists not disjoint!" id*)
    (*else LAS.union l1 l2*)
    LAS.union l1 l2

(* unify the values of the same ids in different environments *)
let unify_values id r_newval = function
  | None -> Some r_newval
  | Some r_oldval -> (* we found an old value *)
    let newval, oldval = !r_newval, !r_oldval in
    let is_tup_list = function
      | [] -> true
      | (VTuple(_))::_ -> true
      | _ -> false
    in
    let both_tup_list l1 l2 = is_tup_list l1 && is_tup_list l2 in
    let unwrap_ind = function
      | VIndirect r -> !r
      | x           -> x
    in
    let wrap_ind x =
      match oldval with VIndirect _ -> ref (VIndirect(ref x)) | _ -> ref x
    in
    match unwrap_ind oldval, unwrap_ind newval with
    | VSet l1, VSet l2 when both_tup_list l1 l2 -> 
        Some(wrap_ind @: VSet(unify_tuple_lists id l1 l2))
    | VSet l1, VSet l2 -> Some(ref @: VSet(LAS.union l1 l2))
    | VBag l1, VBag l2 when both_tup_list l1 l2 -> 
        Some(wrap_ind @: VBag(unify_tuple_lists id l1 l2))
    | VBag l1, VBag l2 -> Some(ref @: VBag(LAS.union l1 l2))
    | VList l1, VList l2 when both_tup_list l1 l2 -> 
        Some(wrap_ind @: VList(unify_tuple_lists id l1 l2))
    | VList l1, VList l2 -> Some(ref @: VList(LAS.union l1 l2))
    | _,_ -> Some r_newval

(* unify the environments of different nodes *)
let unify_envs (envs : (address * program_env_t) list) = 
  (* fold over all the nodes. we discard triggers and frames *)
  let unified_env = 
    (* ignore triggers and frames *)
    List.fold_left (fun acc (addr, (_, (m_env, _))) ->
      IdMap.fold (fun id newval acc' ->
        map_modify (unify_values id newval) id acc'
      ) acc m_env
    ) IdMap.empty envs
  in
  (unified_env, []) (* no frames *)

(* test a program and comare it to the expected output. Takes an interpretation
 * function that expects an untyped AST (this takes care of handling any extra
 * information needed by the interpreter) also takes a program_test data
 * structure indicating the kind of test desired *)
let test_program globals_k3 interpret_fn file_name test = 
  let op_fn, program, tests = match test with
    (* for a single-site test, we just extract the env. For multi-site, we unify
     * the environments, and rename all node variables to be node_var (TODO) *)
    | ProgTest (prog, checkl)    -> extract_first_env, prog, checkl
    | NetworkTest (prog, checkl) -> unify_envs, prog, checkl
    | _ -> failwith "unsupported test"
  in
  let node_envs = interpret_fn program in

  (* print out the environments *)
  (*List.iter (fun (addr, env) -> *)
    (*Printf.printf "Environment for %s\n" (K3Printing.string_of_address addr); *)
    (*print_endline @: K3Values.string_of_program_env env*)
  (* ) node_envs;*)

  (* unify the node value environments if needed *)
  let (v_env:env_t) = op_fn node_envs in

  (* debug - print the unified env *)
  (*Printf.printf "Unified environment:\n";*)
  (*print_endline @: K3Values.string_of_env ~skip_functions:false v_env;*)

  let numbered_tests = insert_index_fst 1 tests in
  let prog_globals = K3Global.add_globals_k3 globals_k3 program in
  (* get the type bindings from the typechecker so we have an environment for
   * typechecking *)
  let tdecl_prog, t_env, trig_env, _ = type_bindings_of_program prog_globals in
  let test_cases = list_map (fun (i, (lexp, rexp)) -> 
      let name = file_name^"_"^soi i in
      let v1 = sort_values @: eval_test_expr_env tdecl_prog t_env trig_env v_env lexp in
      let v2 = sort_values @: eval_test_expr [] @: check_as_expr rexp in
      case name @: v1 @=? v2
    ) numbered_tests
  in List.iter run_tests test_cases

