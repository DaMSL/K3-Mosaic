(* Module to print AST into new K3 syntax *)

open Util
open Printing
open Lazy
open K3.AST
open K3.Annotation

module U = K3Util
module T = K3Typechecker
module KH = K3Helpers

exception MissingType of int * string

let force_list = List.iter force

(* lazy functions *)
let lhv i = [lazy (obc i)]     (* open box *)
let lhov i = [lazy (obv i)]
let lv i = [lazy (obx i)]   (* open vertical box *)
let lcb () = [lazy (cb ())]     (* close box *)
(*let lhv 2 = [lazy (obc 2)]*)
let lcut () = [lazy (pbsi 0 0)] (* print nothing or split line *)
let lind () = [lazy (pbsi 1 2)] (* print break with indent or space *)
let lsp () = [lazy (pbsi 1 0)] (* print space or split line *)
let lps s = [lazy (ps s)]      (* print string *)
let lps_list ?(sep=", ") cut_t f l =
  [lazy (ps_list ~sep:sep cut_t (force_list |- f) l)]

(* low precedence joining of lists *)
let (<|) a b = a @ b

let lazy_paren f = lps "(" <| f <| lps ")"
let lazy_bracket f = lps "[" <| f <| lps "]"
let lazy_brace f = lps "{" <| f <| lps "}"
let lazy_box_brace f =
  lps "{" <| lind () <| lv 0 <| f <|
  lcb () <| lcut () <| lps "}"
let lbox b f = b <| f <| lcb ()
let wrap_hv i f = lbox (lhv i) f
let wrap_hov i f = lbox (lhov i) f
let wrap_indent f = wrap_hov 2 f

let lazy_concat ?(sep=lsp) f l =
  let len = List.length l - 1 in
  let l2 = list_populate (fun _ -> sep ()) 0 len in
  wrap_hov 0 @: List.flatten @: list_intersperse (List.map f l) l2

(* separator for lazy_concat *)
let lcomma () = lps "," <| lsp ()

(* type we pass all the way down for configuring behaviors *)
type config = {
                env:T.type_bindings_t;  (* type bindings for environment *)
                trig_env:T.type_bindings_t;
                map_to_fold:bool;       (* convert maps and ext to fold, due to k3new limitations *)
              }

let default_config = {
                       env = [];
                       trig_env = [];
                       map_to_fold = false;
                     }

let verbose_types_config = default_config

(* light type checking for expressions we create *)
let light_type c e =
  try
     T.deduce_expr_type ~override:false c.trig_env c.env e
  with T.TypeError (uuid, name, err) ->
    prerr_string @: Printf.sprintf "Typechecker Error @%d: %s\n%s\n\n%s" uuid name
      (K3TypeError.string_of_error err) (K3PrintSyntax.string_of_expr ~uuid_highlight:uuid e);
    exit 1

(* Get a binding id from a number *)
let id_of_num i = Printf.sprintf "_b%d_" i
(* Get a record id from a number *)
let record_id_of_num ?(prefix="r") i = Printf.sprintf "_%s%d_" prefix i

(* Add record ids to a list *)
let add_record_ids ?prefix l =
  match l with
  | []    -> failwith "No list to add record ids to"
  | [x]   -> ["i", x]
  | [x;y] -> ["key", x; "value", y] (* to make gbaggs easy *)
  | _     ->
    let i_l = insert_index_fst 1 l in
    List.map (fun (i, x) -> record_id_of_num ?prefix i, x) i_l

(* Add record ids to a string *)
let add_record_ids_str ?prefix ?(sep=":") l =
  List.map (fun (s,x) -> Printf.sprintf "%s%s%s" s sep x) @: add_record_ids ?prefix l

let error () = lps "???"

let s_of_col_type = function
  | TSet  -> "Set"
  | TBag  -> "Collection"
  | TList -> "Seq"
  | TMap  -> "Map"

let lazy_control_anno c = function
  | Effect ids -> lps "effect " <| lazy_paren @:
      lps_list NoCut lps ids
  | Parallel i -> lps "parallel " <| lazy_paren (lps @: string_of_int i)

let lazy_data_anno c a =
  let positions ps = lps_list NoCut (lps |- string_of_int) ps in
  match a with
  | FunDep (ps, Element) -> lps "key " <| lazy_paren (positions ps)
  | FunDep (ps, Positions ps2) -> positions ps <| lps "->" <| positions ps2
  | MVFunDep (ps, Element) -> lps "index " <| lazy_paren (positions ps)
  | MVFunDep (ps, Positions ps2) -> positions ps <| lps "=>" <| positions ps2
  | Unique ps -> lps "unique " <| lazy_paren (positions ps)
  | Ordered ps -> lps "ordered " <| lazy_paren (positions ps)
  | Sequential -> lps "sequential"
  | RandomAccess -> lps "randomaccess"

let lazy_anno c = function
  | Data(r,a)    -> lazy_data_anno c a
  | Control(r,a) -> lazy_control_anno c a
  | _ -> []

let lazy_annos c = function
  | [] -> []
  | annos -> lps "@ " <| lazy_brace @: lps_list ~sep:"; " NoCut (lazy_anno c) annos

let rec lazy_base_type ?(brace=true) ?(mut=false) ?(empty=false) c ~in_col t =
  let wrap_mut f = if mut && not empty then lps "mut " <| f else f in
  let wrap_single f =
    let wrap = if brace then lazy_brace else id_fn in
    if in_col then wrap(lps "i:" <| f) else f
  in
  let wrap = wrap_single |- wrap_mut in
  match t with
  | TUnit        -> wrap @: lps "()"
  | TBool        -> wrap @: lps "bool"
  | TByte        -> wrap @: lps "byte"
  | TInt
  | TDate        -> wrap @: lps "int"
  | TFloat       -> wrap @: lps "real"
  | TString      -> wrap @: lps "string"
  | TMaybe(vt)   -> wrap(lps "option " <| lazy_value_type c ~in_col vt)
  | TAddress     -> wrap @: lps "address" (* ? *)
  | TTarget bt   -> wrap (lps "target" <| lazy_base_type c ~in_col bt)
  | TUnknown     -> wrap @: lps "unknown"
  | TIndirect vt -> wrap (lps "ind " <| lazy_value_type c ~in_col vt)
  | TTuple(vts)  -> (* tuples become records *)
      let rec_vts = add_record_ids vts in
      let inner = lazy_concat ~sep:lcomma (fun (id, vt) ->
        lps (id^":") <| lazy_value_type c ~in_col:false vt) rec_vts in
      let wrap = if brace then lazy_brace else id_fn in
      wrap (lsp () <| inner <| lsp ())
  | TCollection(ct, vt) -> wrap (
    (if not empty then lps "collection " else [])
    <| lazy_value_type c ~in_col:true vt <| lps " @ " <| lps
        begin match ct with
          | TSet  -> "{ Set }"
          | TList -> "{ Seq }"
          | TBag  -> "{ Collection }"
          | TMap  -> "{ Map }"
        end
      )

and lazy_mutable_type ?empty c ~in_col = function
  | TMutable (bt, a)   -> lazy_base_type ?empty c ~in_col ~mut:true bt
  | TImmutable (bt, a) -> lazy_base_type ?empty c ~in_col bt

and lazy_value_type ?empty c ~in_col = function
  | TIsolated mt  -> lazy_mutable_type ?empty c ~in_col mt
  | TContained mt -> lazy_mutable_type ?empty c ~in_col mt

let lazy_type c = function
  | TFunction(f,t) ->
      lazy_value_type c ~in_col:false f <| lps " -> " <|
      lazy_value_type c ~in_col:false t
  | TValue vt -> lazy_value_type c false vt

let rec lazy_arg c drop_tuple_paren = function
  | AIgnored      -> lps "_"
  | AVar (id, vt) -> lps id
  | _             -> failwith "shouldn't be here"

let lazy_id_type c (id,t) = lps (id^" : ") <| lazy_value_type c false t

let lazy_const c v =
  let remove_float_dot s =
    let l = String.length s in
    if s.[l - 1] = '.' then
      str_take (l-1) s
    else s
  in
  match v with
  | CUnknown       -> lps "_"
  | CUnit          -> lps "()"
  | CBool true     -> lps "true"
  | CBool false    -> lps "false"
  | CInt i         -> lps @: string_of_int i
  | CFloat f       -> lps @: remove_float_dot @: string_of_float f
  | CString s      -> lps @: Printf.sprintf "\"%s\"" (String.escaped s)
  | CAddress(s, i) -> lps @: Printf.sprintf "%s:%d" s i
  | CTarget id     -> lps id

(* wrap a const collection expression with collection notation *)
let lazy_collection_vt c vt eval = match KH.unwrap_vtype vt with
  | _, TCollection(ct, et) ->
      (* preceding list of element types *)
      let mut, t = KH.unwrap_vtype et in
      let lazy_elem_list =
        lazy_base_type ~brace:false ~in_col:true ~mut c t <| lps "|" <| lsp ()
      in
      lps "{|" <| lazy_elem_list <| eval <| lps "|}" <|
      lps @:
        begin match ct with
        | TSet  -> " @ { Set }"
        | TBag  -> " @ { Collection }"
        | TList -> " @ { Seq }"
        | TMap  -> " @ { Map }"
        end
  | _ -> error () (* type error *)

(* arg type with numbers included in tuples and maybes *)
type arg_num
    = NIgnored
    | NVar      of int * id_t * value_type_t
    | NMaybe    of int * arg_num
    | NTuple    of int * arg_num list

(* get an id for the argument at the shallow level for trigger or lambda *)
let shallow_bind_id ~in_record = function
  | NIgnored        -> "_"
  (* A case of a single variable in an in_record (map etc) *)
  | NVar (i, id, vt) when in_record ->
      begin match snd @: KH.unwrap_vtype vt with
      | TTuple _ -> id (* we have a single variable representing a tuple -- don't bind *)
      (*| TCollection _ -> id [> single variable representing collection <]*)
      | _        -> id_of_num i
      end
  | NVar (_, id, _) -> id
  | NMaybe (i,_)
  | NTuple (i,_)    -> id_of_num i

(* convert args to an arg type containing numbers for binding *)
let arg_num_of_arg a =
  let i = ref 1 in
  let rec loop = function
    | AIgnored     -> NIgnored
    (* We assign to variables as well for record unpacking *)
    | AVar(id, v)  -> let n = !i in incr i; NVar(n, id, v)
    | AMaybe x     -> let n = !i in incr i; NMaybe(n, loop x)
    | ATuple xs    -> let n = !i in incr i; NTuple(n, List.map loop xs)
  in loop a

(* retrieve the ids from one level of arg_num *)
let get_id_of_arg = function
  | NIgnored     -> "_"
  | NVar(_,id,_) -> id
  | NMaybe(i,_)
  | NTuple(i,_)  -> id_of_num i

(* convert an arg_num to a value type *)
let rec value_type_of_arg_num = function
  | NIgnored        -> T.canonical TUnknown
  | NVar(_, _, t)      -> t
  | NMaybe(_, a')   -> T.canonical (TMaybe(value_type_of_arg_num a'))
  | NTuple(_, args) -> T.canonical (TTuple(List.map value_type_of_arg_num args))

(* Break args down for lambdas with multiple values *)
let break_args = function
  | NTuple(_,args) -> args
  | _              -> failwith "Can't break args"

(* Break args down for lambdas with multiple values *)
let peel_arg = function
  | NTuple(_,[x])     -> x, None
  | NTuple(a,x::xs)   -> x, Some(NTuple(a, xs))
  | _                 -> failwith "Can't break args"

(* code to unwrap an option type *)
(* project: add a projection out of a record *)
let unwrap_option ?(project=false) f =
  let p = if project then ".i" else "" in
  lps "case " <| f <| lps " of" <| lsp () <|
  lps (Printf.sprintf "{ Some x -> x%s }" p) <| lsp () <|
  lps "{ None -> error () }"

(* A slice can have other statements inside it. We need to get the inner tuple
 * out, and to make a function that will construct everything inside the lambda
 * once given an inner expression
 *)
let rec extract_slice e =
  match U.tag_of_expr e with
  | Tuple -> U.decompose_tuple e, id_fn
    (* let statement *)
  | Apply -> let lambda, arg = U.decompose_apply e in
    let t, f = extract_slice lambda in
    t, (fun fn -> KH.mk_apply fn arg) |- f
  | Lambda _ -> let argt, body = U.decompose_lambda e in
    let t, f = extract_slice body in
    t, KH.mk_lambda argt |- f
  | _ -> failwith "extract_slice unhandled expression"

(* identify if a lambda is an id lambda *)
let is_id_lambda e =
  (* get a flat list of only the first level of ids *)
  let id_of_arg = function
    | AVar(v,_) -> Some v
    | _         -> None
  in
  let flat_ids_of_arg = function
    | AVar(v,_)  -> [v]
    | ATuple(vs) ->
        let vs' = List.map id_of_arg vs in
        if List.mem None vs' then []
        else flatten_some vs'
    | _          -> []
  in
  let args, body = U.decompose_lambda e in
  let ids = flat_ids_of_arg args in
  if null ids then false
  else match U.tag_of_expr body, ids with
    | Var id, [id'] when id = id' -> true
    | Tuple, _ -> ids = KH.vars_to_ids @: U.decompose_tuple body
    | _, _     -> false

(* Variable names to translate *)
module StringMap = Map.Make(struct type t = string let compare = String.compare end)
let var_translate = List.fold_left (fun acc (x,y) -> StringMap.add x y acc) StringMap.empty @:
  ["int_of_float", "truncate"; "float_of_int", "real_of_int"; "peers", "my_peers"]

type in_record = InRec | In
type out_record = OutRec | Out
type arg_info  = ALambda of in_record list | ANonLambda
type arg_info_l = (arg_info * out_record) list

(* create a deep bind for lambdas, triggers, and let statements
 * -depth allows to skip one depth level of binding
 * -top_expr allows us to use an expression at the top bind
 * -top_rec indicates that the first level of binding should be to a record *)
let rec deep_bind ?(depth=0) ?top_expr ~in_record c arg_n =
  let rec loop d a =
    let record = in_record && d=depth in (* do we want a record now *)
    (* we allow binding an expression at the top level *)
    let bind_text i = match top_expr, d with
      | Some e, d
          when d=depth -> lazy_expr c e
      | _              -> lps @: id_of_num i
    in
    match a with
      (* pretend to unwrap a record *)
    | NVar(i, id, vt) when record ->
        begin match snd @: KH.unwrap_vtype vt with
        | TTuple _  -> []    (* don't bind if we have an id representing a record *)
        (*| TCollection _ -> [] [> or a collection <]*)
        | _        ->
          (* force bind a variable that comes in as a pretend record *)
          lps "bind " <| lps (id_of_num i) <| lps " as {i:" <| lps id
          <| lps "} in " <| lcut ()
        end
    | NIgnored
    | NVar _      -> [] (* no binding needed *)
    | NTuple(i, args) ->
        (* only produce binds if we're deeper than specified depth *)
        (if d < depth then [] else
          let args_id = List.map get_id_of_arg args in
          let args_rec = add_record_ids_str args_id in
          let sub_ids = lazy_concat ~sep:lcomma lps args_rec in
          lps "bind " <| bind_text i <| lps " as {" <| sub_ids <| lps "} in "
          <| lcut ()) <|
      List.flatten @: List.map (loop @: d+1) args
  | NMaybe(i, arg)  ->
      if d < depth then [] else
        lps "let " <| lps (get_id_of_arg arg) <| lps " = " <| unwrap_option (bind_text i) <|
        lps " in" <| lsp () <| loop (d+1) arg
  in loop 0 arg_n

(* Apply a method -- the lambda part *)
and apply_method_nocol ?prefix_fn c ~name ~args ~arg_info =
  let wrap_if_big e = match U.tag_of_expr e with
      | Var _ | Const _ | Tuple | Empty _ -> id_fn
      | _ -> lazy_paren
  in
  let args' = list_zip args arg_info in
  lps ("."^name) <| lsp () <|
    lazy_concat (fun (e, info) ->
      wrap_if_big e @: lazy_expr ~expr_info:info ?prefix_fn c e) args'

(* Apply a method to a collection *)
and apply_method ?prefix_fn c ~name ~col ~args ~arg_info =
  (* we only need parens if we're not applying to a variable *)
  let f = match U.tag_of_expr col with
  | Var _ -> id_fn
  | _     -> lazy_paren
    in f @: lazy_expr c col <| apply_method_nocol c ~name ~args ~arg_info ?prefix_fn

(* apply a function to arguments *)
and function_application c fun_e l_e =
  (* Check if we need to modify the function *)
  (* for example, hash gets only one function in the new k3 *)
  let id, tag, anns, children = U.details_of_expr fun_e in
  let fun_e = match tag with
  | Var x when str_take 4 x = "hash" ->
         U.expr_of_details id (Var "hash") anns children
  | _ -> fun_e
  in
  let print_fn e = match U.tag_of_expr e with
    | Var _ | Const _ | Tuple   -> lazy_expr c e
    | _                         -> lazy_paren @: lazy_expr c e
  in
  wrap_indent (lazy_expr c fun_e <| lsp () <| lazy_concat print_fn l_e)

(* handle the printing of a lambda *)
and handle_lambda c ~expr_info ~prefix_fn arg e =
  let arg_n = arg_num_of_arg arg in (* convert to arg_num *)
  let arg_l, out_rec = match expr_info with
    | ALambda [], _  -> failwith @: Printf.sprintf "Missing lambda arg list at %d" (U.id_of_expr e)
    | ANonLambda, o  -> [In], o  (* deal with cases where we don't prepare it *)
    | ALambda l,  o  -> l,    o
  in
  let many_args = List.length arg_l > 1 in
  let write arg in_record =
    lps "\\" <| lps @: shallow_bind_id ~in_record arg <| lps " ->" <| lsp ()
  in
  let exec arg in_record =
    (* for curried arguments, we deep bind at a deeper level *)
    write arg in_record <| lind () <|
      wrap_hov 0 (deep_bind ~depth:0 ~in_record c arg <|
      (* for the final expr, we may need to wrap the output in a record *)
      lazy_expr c (prefix_fn e) ~expr_info:(ANonLambda, out_rec))
  in
  (* loop over the lambda arguments *)
  let rec loop a = function
  | []    -> lps @: Printf.sprintf "Incorrect number of args at %d" (U.id_of_expr e)
  | x::xs ->
    let in_record = match x with InRec -> true | _ -> false in
    (* handle argument by argument if lambda_many_args *)
    if many_args then
      match peel_arg a with
      | arg, Some tup_arg ->
          lazy_paren (write arg in_record <| wrap_hov 0 (deep_bind ~depth:0 ~in_record c arg) <|
            loop tup_arg xs)
      | arg, None         -> lazy_paren @: exec arg in_record
    else lazy_paren @: exec a in_record
  in loop arg_n arg_l


(* create a fold instead of a map or ext (for typechecking reasons) *)
(* expects a lambda expression, and collection expression inside a map/flattenMap *)
and fold_of_map_ext c expr =
  let self_t_out = U.unwrap_t_val @: T.type_of_expr expr in
  let map_t_out  = match KH.unwrap_vtype @: self_t_out with
                   | mut, TCollection(_, t_e)  -> if mut then KH.wrap_tbag_mut t_e
                                                  else KH.wrap_tbag t_e
                   | _ -> failwith "unexpected"
  in
  (* customize for the different operations *)
  let (lambda, col), wrap_fn, suffix, t_out = match U.tag_of_expr expr with
    | Map     -> U.decompose_map expr, KH.mk_singleton map_t_out, "map", map_t_out
    | MapSelf -> U.decompose_map_self expr, KH.mk_singleton self_t_out, "selfMap", self_t_out
    | Flatten -> U.decompose_map @: U.decompose_flatten expr, id_fn, "ext", self_t_out
    | _       -> failwith "Can only convert flatten-map, map, or selfMap to fold"
  in
  let empty = KH.mk_empty t_out in
  let args, body = U.decompose_lambda lambda in
  let acc_id = "__acc_"^suffix in
  let acc_arg = AVar (acc_id, t_out) in
  let args' = ATuple [acc_arg; args] in
  let body' = KH.mk_combine (KH.mk_var acc_id) (wrap_fn body) in
  lazy_expr c @: light_type c @: KH.mk_agg (KH.mk_lambda args' body') empty col

(* printing expressions *)
(* argnums: lambda only   -- number of expected arguments *)
(* prefix_fn: lambda only -- modify the lambda with a prefix *)
(* expr_info: additional info about the expression in the form of an arg_info structure *)
and lazy_expr ?(prefix_fn=id_fn) ?(expr_info=(ANonLambda,Out)) c expr =
  let expr_pair ?(sep=lps "," <| lsp ()) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(lazy_expr c e1) <| sep <| wr(lazy_expr c e2) in
  let is_apply_let e = let e1, e2 = U.decompose_apply e in
    match U.tag_of_expr e1 with
      | Var _ -> false | Lambda _ -> true | _ -> invalid_arg "bad apply input"
  in
  (* handle parentheses:
     - If a sub-element is mult or add, we wrap it.
     - If a left sub-element is 'ifthenelse' or a 'let', we wrap it.
     - For a == or !=, we also wrap sub- ==/!= *)
  let arith_paren e = match U.tag_of_expr e with
    | Mult
    | Add   -> lazy_paren
    | Apply -> let e1, _ = U.decompose_apply e in
               begin match U.tag_of_expr e1 with
               | Var "divf" -> lazy_paren
               | Var "mod"  -> lazy_paren
               | _          -> id_fn
               end
    | _     -> id_fn in
  (* we're more sensitive for left side *)
 (*let paren_l e = match U.tag_of_expr e with*)
    (*| IfThenElse -> lazy_paren*)
    (*| Apply when is_apply_let e -> lazy_paren*)
    (*| _ -> id_fn in*)
 let arith_paren_l e = match U.tag_of_expr e with
    | IfThenElse -> lazy_paren
    | Apply when is_apply_let e -> lazy_paren
    | _ -> arith_paren e
  (* for == and != *)
  in let logic_paren e = match U.tag_of_expr e with
    | Eq | Neq -> lazy_paren
    | _        -> arith_paren e (* arith ast is used for logic *)
  in let logic_paren_l e = match U.tag_of_expr e with
    | Eq | Neq -> lazy_paren
    | _        -> arith_paren_l e
  in let arith_paren_pair sym (el, er) =
    let wrapl = arith_paren_l el in
    let wrapr = arith_paren er in
    expr_pair ~sep:(lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let logic_paren_pair sym (el, er) =
    let wrapl = logic_paren_l el in
    let wrapr = logic_paren er in
    expr_pair ~sep:(lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let expr_type_is_bool e =
    try (match T.type_of_expr e with
    | TValue x | TFunction(_,x) -> match T.base_of x () with
        | TBool -> true
        | _     -> false)
    with T.TypeError(_,_,_) -> false (* assume additive *)
  (* many instructions need to wrap the same way *)
  in let wrap e = match U.tag_of_expr expr with
    Insert | Iterate | Map | Filter | Flatten | Send | Delete | Update |
    Aggregate | GroupByAggregate -> wrap_hv 2 e
    | IfThenElse -> wrap_hv 0 e
    | _ -> id_fn e
  in
  (* begin analysis of tags *)
  let analyze () = match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id    -> begin try lps @: StringMap.find id var_translate with Not_found -> lps id end
  | Tuple     -> let es = U.decompose_tuple expr in
    let id_es = add_record_ids es in
    let inner = lazy_concat ~sep:lcomma (fun (id, e) ->
        lps (id^":") <| lazy_expr c e) id_es
    in lazy_brace inner
  | Just -> let e = U.decompose_just expr in
    lps "Some " <| lazy_expr c e
  | Nothing vt -> lps "None " <| if fst @: KH.unwrap_vtype vt
      then lps "mut" else lps "immut"
  | Empty vt -> lps "empty " <| lazy_value_type ~empty:true c ~in_col:false vt
  | Singleton _ ->
    (* Singletons are sometimes typed with unknowns (if read from a file) *)
    let e = U.decompose_singleton expr in
    let t = U.unwrap_t_val @: T.type_of_expr expr in
    lazy_collection_vt c t @: lazy_expr c e
  | Combine ->
    let rec assemble_list c e =  (* print out in list format *)
      let l, r = U.decompose_combine e in
      begin match U.tag_of_expr l, U.tag_of_expr r with
        | Singleton _, Combine -> let l2 = U.decompose_singleton l in
            lazy_expr c l2 <| lps "," <| lsp () <|
            assemble_list c r
        | Singleton _, Singleton _ -> let l2 = U.decompose_singleton l in
          let r2 = U.decompose_singleton r in
          lazy_expr c l2 <| lps "," <| lsp () <|
            lazy_expr c r2
        | Singleton _, Empty _ -> let l2 = U.decompose_singleton l in
          lazy_expr c l2
        | _ -> error () (* type error *)
      end in
    let (e1, e2) = U.decompose_combine expr in
    (* wrap the left side of the combine if it's needed *)
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
      | Singleton vt, Combine | Singleton vt, Singleton _
      | Singleton vt, Empty _ ->
        let t = U.unwrap_t_val @: T.type_of_expr expr in
        lazy_collection_vt c t @: assemble_list c expr
      | _ -> apply_method c ~name:"combine" ~col:e1 ~args:[e2] ~arg_info:[ANonLambda, Out]
    end
  | Range ct -> let st, str, num = U.decompose_range expr in
    (* newk3 range only has the last number *)
    let range_type = TFunction(KH.t_int, KH.wrap_tlist KH.t_int) in
    function_application c (U.attach_type range_type @: KH.mk_var "range") [num]
  | Add -> let (e1, e2) = U.decompose_add expr in
    begin match U.tag_of_expr e2, expr_type_is_bool e1 with
      | Neg, false -> let e3 = U.decompose_neg e2 in
        arith_paren_pair "-" (e1, e3)
      | _, false -> arith_paren_pair "+" (e1, e2)
      | _, true -> arith_paren_pair "or" (e1, e2)
    end
  | Mult -> let (e1, e2) = U.decompose_mult expr in
    let is_neg = begin match U.tag_of_expr e1 with
      | Neg -> let e = U.decompose_neg e1 in
        begin match U.tag_of_expr e with
          | Const(CInt(1)) -> true
          | _ -> false
        end
      | Const(CInt(-1)) -> true
      | _ -> false
    end in
    begin match expr_type_is_bool e1, is_neg with
      | true, _ -> arith_paren_pair "and" (e1, e2)
      | false, true -> lps "-" <| lazy_expr c e2 (* just a minus *)
      | _ -> arith_paren_pair "*" (e1, e2)
    end
  | Neg -> let e = U.decompose_neg expr in
    let sym = if expr_type_is_bool e then "not " else "-" in
    begin match U.tag_of_expr e with
      | Var _
      | Const _ -> lps sym <| lazy_expr c e
      | Lt -> let p = U.decompose_lt e in
        arith_paren_pair ">=" p
      | Leq -> let p = U.decompose_leq e in
        arith_paren_pair ">" p
      | _ -> lps sym <| lazy_paren @: lazy_expr c e
    end
  | Eq -> let p = U.decompose_eq expr in
    logic_paren_pair "==" p
  | Lt -> let p = U.decompose_lt expr in
    arith_paren_pair "<" p
  | Neq -> let p = U.decompose_neq expr in
    logic_paren_pair "!=" p
  | Leq -> let p = U.decompose_leq expr in
    arith_paren_pair "<=" p
  | Lambda arg ->
      let _, e = U.decompose_lambda expr in
      handle_lambda c ~prefix_fn ~expr_info arg e
  | Apply -> let e1, e2 = U.decompose_apply expr in
    let do_pair_paren sym =
      begin match U.decompose_tuple e2 with
        | [x; y] -> arith_paren_pair sym (x, y)
        | _      -> failwith "malformed tuple"
      end
    in
    begin match U.tag_of_expr e1 with (* can be let *)
      (* divide becomes an infix operator *)
      | Var "divf" -> do_pair_paren "/"
      | Var "mod"  -> do_pair_paren "%"
      (* function application *)
      | Var _ -> function_application c e1 [e2]
      (* let expression *)
      | Lambda arg ->
        let t_e2 = begin try U.unwrap_t_val @: T.type_of_expr e2
          with _ -> KH.t_unit end in
        let _, body = U.decompose_lambda e1 in
        let print_let assign_exp =
          wrap_hov 2 (lps "let " <| lazy_arg c false arg <|
            lps " =" <| lsp () <| lazy_expr c assign_exp <| lsp () ) <| lps "in" <| lsp ()
            <| lazy_expr c body
        in
        begin match arg with
        (* If we have an arg tuple, it's a bind. A maybe is similar *)
        | ATuple _
        | AMaybe _     -> let arg_n = arg_num_of_arg arg in
                          deep_bind c arg_n ~top_expr:e2 ~in_record:false <|
                            lazy_expr c body
        (* Otherwise it's a let *)
        | AVar(id, vt) ->
            (* check if we're casting the type of the collection *)
            begin match snd @: KH.unwrap_vtype t_e2, snd @: KH.unwrap_vtype vt with
            | TCollection(ct,_), TCollection(ct',_) when ct <> ct' ->
                let e2' = light_type c @: KH.mk_combine (KH.mk_empty vt) e2 in
                print_let e2'
            | _ -> print_let e2
            end
        | AIgnored     -> print_let e2
        end
      | _ -> error () (* type error *)
    end
  | Block -> let es = U.decompose_block expr in
    lps "(" <| lind () <|
    wrap_hv 0 (lps_list ~sep:";" CutHint (lazy_expr c) es <| lsp ())
      <| lps ")"

  | IfThenElse -> let (e1, e2, e3) = U.decompose_ifthenelse expr in
    wrap_indent (lps "if " <| lazy_expr c e1) <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3)

  | Iterate -> let lambda, col = U.decompose_iterate expr in
    apply_method c ~name:"iterate" ~col ~args:[lambda] ~arg_info:[ALambda [InRec], Out]

  | Map ->
      if c.map_to_fold then
        fold_of_map_ext c expr

      else (* normal map *)
        let lambda, col = U.decompose_map expr in
        apply_method c ~name:"map" ~col ~args:[lambda] ~arg_info:[ALambda [InRec], OutRec]

  | MapSelf ->
      if c.map_to_fold then
        fold_of_map_ext c expr

      else (* normal map *)
        let lambda, col = U.decompose_map_self expr in
        apply_method c ~name:"mapself" ~col ~args:[lambda] ~arg_info:[ALambda [InRec], OutRec]

  | Filter -> let lf, col = U.decompose_filter expr in
    apply_method c ~name:"filter" ~col ~args:[lf] ~arg_info:[ALambda [InRec], Out]

  (* flatten(map(...)) becomes ext(...) *)
  | Flatten ->
      if c.map_to_fold then
        (* both map and ext become folds *)
        fold_of_map_ext c expr
      else (* normal ext *)
        let e = U.decompose_flatten expr in
        (* ext needs an empty type right now to know what to do if the result is empty
        * flatten should always return a bag type since we can't guarantee uniqueness*)
        let t = U.unwrap_t_val @: T.type_of_expr expr in
        let t = match KH.unwrap_vtype t with
          | _, TCollection(TList, x) -> KH.canonical @: TCollection(TList, x)
          | _, TCollection(_, x)     -> KH.canonical @: TCollection(TBag, x)
          | _, _                     -> failwith "not a collection"
        in
        let empty_c = light_type c @: KH.mk_empty t in
        begin match U.tag_of_expr e with
        | Map ->
            let lambda, col = U.decompose_map e in
            apply_method c ~name:"ext" ~col ~args:[lambda; empty_c]
              ~arg_info:[ALambda [InRec], Out; ANonLambda, Out]
        | _   -> failwith "Unhandled Flatten without map"
        end

  | Aggregate -> let lambda, acc, col = U.decompose_aggregate expr in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"fold" ~col ~args:[lambda; acc]
      ~arg_info:[ALambda [In; InRec], Out; ANonLambda, Out]
  | GroupByAggregate -> let lam1, lam2, acc, col = U.decompose_gbagg expr in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"groupBy" ~col ~args:[lam1; lam2; acc]
      ~arg_info:[ALambda [InRec], Out; ALambda [In; InRec], Out; ANonLambda, Out]
  | Sort -> let col, lambda = U.decompose_sort expr in
    apply_method c ~name:"sort" ~col ~args:[lambda] ~arg_info:[ALambda [InRec; InRec], Out]
      ~prefix_fn:(fun e -> light_type c @: KH.mk_if e (KH.mk_cint (-1)) @: KH.mk_cint 1)
  | Peek -> let col = U.decompose_peek expr in
    (* get the type of the collection. If it's a singleton type, we need to add
     * projection *)
    let col_t = U.unwrap_t_val @: T.type_of_expr col in
    let project = (* not a tuple, so need projection out of the record *)
      begin match snd @: KH.unwrap_vtype col_t with
      | TCollection(_, vt) -> begin match snd @: KH.unwrap_vtype vt, snd expr_info with
        | TTuple _, _ -> false
        | _, OutRec   -> false (* we need a record output *)
        | _           -> true
        end
      | _ -> failwith "expected a collection type"
      end
    in
    (* peeks return options and need to be pattern matched *)
    unwrap_option ~project @:
      lazy_paren @: apply_method c ~name:"peek" ~col ~args:[light_type c @: KH.mk_cunit] ~arg_info:[ANonLambda, Out]
  | Slice -> let col, pat = U.decompose_slice expr in
    let es, lam_fn = begin match U.tag_of_expr pat with
      | Tuple -> U.decompose_tuple pat, id_fn
      (* if we have a let, we need a proper tuple extraction *)
      | Apply -> extract_slice pat
      | _     -> [pat], id_fn
    end in
    let ts = List.map (U.unwrap_t_val |- T.type_of_expr) es in
    let id_e = add_record_ids es in
    let id_t = add_record_ids ts in
    (* find the non-unknown slices *)
    let filter_e = List.filter (fun (_,c) ->
      begin match U.tag_of_expr c with
      | Const CUnknown -> false
      | _              -> true
      end) id_e
    in
    if null filter_e then lazy_expr c col (* no slice needed *)
    else
      let do_eq (id, v) = KH.mk_eq (KH.mk_var id) v in
      let lambda = light_type c @:
        KH.mk_lambda (KH.wrap_args id_t) @:
          lam_fn @: (* apply an inner lambda constructor *)
          List.fold_right (fun x acc ->
            KH.mk_and acc (do_eq x)
          )
          (tl filter_e)
          (do_eq @: hd filter_e)
      in
      apply_method c ~name:"filter" ~col ~args:[lambda] ~arg_info:[ALambda[InRec], Out]
  | Insert -> let col, x = U.decompose_insert expr in
    apply_method c ~name:"insert" ~col ~args:[x] ~arg_info:[ANonLambda,OutRec]
  | Delete -> let col, x = U.decompose_delete expr in
    apply_method c ~name:"erase" ~col ~args:[x] ~arg_info:[ANonLambda,OutRec]
  | Update -> let col, oldx, newx = U.decompose_update expr in
    apply_method c ~name:"update" ~col ~args:[oldx;newx] ~arg_info:[ANonLambda,OutRec;ANonLambda,OutRec]
  | Assign -> let l, r = U.decompose_assign expr in
    (* assignment must be within a bind *)
    lps "bind" <| lsp () <| lazy_expr c l <| lsp () <| lps "as ind __y in"
      <| lsp () <| lps "__y =" <| lsp () <| lazy_expr c r
  | Indirect -> let x = U.decompose_indirect expr in
    lps "ind" <| lsp () <| lazy_expr c x
  | Deref -> let e = U.decompose_deref expr in
    (* dereference must be within a bind *)
    lps "bind" <| lsp () <| lazy_expr c e <| lsp () <| lps "as " <| lps "ind __x"
      <| lsp () <| lps "in __x"
  | Send -> let target, addr, args = U.decompose_send expr in
    wrap_indent @: lazy_paren (expr_pair (target, addr)) <| lps "<- " <|
      lps_list CutHint (lazy_expr c) args
  in
  (* check if we need to wrap our output in a tuple (record) *)
  match snd expr_info with
  | OutRec ->
    begin match U.tag_of_expr expr with
    | Lambda _ -> wrap @: analyze ()
      (* don't wrap a lambda itself *)
    | _ ->
        (* check the type of the expression *)
        let t =
          begin try T.type_of_expr expr
            with T.TypeError(id, _, T.UntypedExpression) ->
              raise @: MissingType(id, K3Printing.string_of_expr expr)
          end
        in
        begin match snd @: KH.unwrap_vtype @: U.unwrap_t_val t with
        | TTuple _ -> wrap @: analyze ()
        | _        -> lazy_expr ~expr_info:((fst expr_info, Out))
                        ~prefix_fn c @: light_type c @: KH.mk_tuple ~force:true [expr]
        end
    end
  | _ -> wrap @: analyze ()

let lazy_trigger c id arg vars expr =
  let is_block expr = match U.tag_of_expr expr with Block -> true | _ -> false in
  let indent f = if is_block expr
               then lps " " <| f
               else lind () <| lbox (lhov 0) f in
  lps ("trigger "^id) <| lps " : " <|
  lazy_value_type ~in_col:false c @: KH.value_type_of_arg arg <| lsp () <|
  lps "=" <| indent (lazy_expr c @: KH.mk_lambda arg expr) <| lcut ()

let channel_format c = function
  | CSV  -> "csv"
  | JSON -> "json"

let lazy_channel c chan_t chan_f = match chan_t with
  | File s -> lps @: "file(\""^s^"\", "^channel_format c chan_f^")"
  | Network(str, port) -> lps @: "socket(\""^str^"\":"^string_of_int port^")"

let rec lazy_resource_pattern c = function
  | Terminal id -> lps id
  | Choice rps  -> lps_list ~sep:"| " CutHint (lazy_resource_pattern c) rps
  | Sequence rps -> lps_list ~sep:" " CutHint (lazy_resource_pattern c) rps
  | Optional rp -> lazy_resource_pattern c rp <| lps "?"
  | Repeat(rp, _) -> lazy_resource_pattern c rp <| lps "*"

let lazy_stream c = function
  | RandomStream i -> lps "random" <| lazy_paren (lps @: string_of_int i)
  | ConstStream e  -> lps "stream" <| lazy_paren (lazy_expr c e)

let lazy_resource c r =
  let common t = lazy_type c t <| lps " = " in
  match r with
  | Handle(t, chan_t, chan_f) -> common t <| lazy_channel c chan_t chan_f
  | Stream(t, st) -> common t <| lazy_stream c st
  | Pattern(pat) -> lps "pattern " <| lazy_resource_pattern c pat

let lazy_flow c = function
  | Source(Code(id, arg, vars, expr))
  | Sink(Code(id, arg, vars, expr)) -> lazy_trigger c id arg vars expr
  | Source(Resource(id, r)) ->
      lps ("source "^id^" : ") <| lazy_resource c r
  | Sink(Resource(id, r)) ->
      lps ("sink "^id^" : ") <| lazy_resource c r
  | Bind(id1, id2) -> lps @: "bind "^id1^" -> "^id2
  | Instruction(Consume id) -> lps @: "consume "^id

let lazy_flow_program c fas = lps_list ~sep:"" CutHint (lazy_flow c |- fst) fas

let lazy_declaration c d =
  let out = match d with
  | Global(id, t, expr) -> let end_part = begin match expr with
      | None -> []
      | Some e -> lps " =" <| lsp () <| lazy_expr c e
    end in
    wrap_hov 2 (lps @: "declare "^id^" :" <| lsp () <| lazy_type c t <| end_part)
  | Role(id, fprog) ->
      lps ("role "^id^" ") <| lazy_box_brace (lazy_flow_program c fprog)
  | Flow fprog -> lazy_flow_program c fprog
  | DefaultRole id -> lps ("default role "^id)
  | Foreign(id, t) -> lps ("declare "^id^" :") <| lsp () <| lazy_type c t
  in
  wrap_hv 0 out <| lcut ()

let wrap_f = wrap_formatter ~margin:80

(* print a K3 type in syntax *)
let string_of_base_type t = wrap_f @: fun () ->
  force_list @: lazy_base_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_value_type t = wrap_f @: fun () ->
  force_list @: lazy_value_type verbose_types_config false t

module StringSet = Set.Make(struct type t=string let compare=String.compare end)

(* remove/convert functions that are renamed in k3new *)
let filter_incompatible prog =
  let r_demux = Str.regexp "^demux_.*" in
  let filter_trigs fl =
    filter_map (fun ((s,_) as trig) -> match s with
      | Sink(Code(id, _, _, _)) when r_match r_demux id -> None
      | _ -> Some trig
    ) fl
  in
  let r_hash = Str.regexp "^hash.*" in
  let drop_globals = List.fold_left (flip StringSet.add) StringSet.empty
    ["divf"; "mod"; "float_of_int"; "int_of_float"; "get_max_int"; "parse_sql_date"; "peers"; "pmap_input" ]
  in
  filter_map (fun ((d,a) as dec) ->
    (* we don't want the monomorphic hash functions *)
    match d with
    | Foreign(id, _)   when StringSet.mem id drop_globals -> None
    | Foreign(id, _)   when r_match r_hash id             -> None
    | Global(id, _, _) when StringSet.mem id drop_globals -> None
    | Flow(fl)      -> Some((Flow(filter_trigs fl), a))
    | Role _        -> None
    | DefaultRole _ -> None
    | _             -> Some dec
  ) prog

(* print a K3 program in syntax *)
(* We get the typechecking environments so we can do incremental typechecking where needed *)
let string_of_program ?(map_to_fold=false) prog (env, trig_env) =
  let config = {env; trig_env; map_to_fold} in
  wrap_f @: fun () ->
    let l = lps_list ~sep:"" CutHint (lazy_declaration config |- fst) prog in
    obx 0;  (* vertical box *)
    force_list l;
    cb

let r_insert = Str.regexp "^insert_\\(.+\\)$"
let r_insert_bad = Str.regexp "^insert_.*\\(do\\|send\\|rcv\\)"

(* add sources and feeds to the program *)
(* expects a distributed program *)
let add_sources p envs filename =
  let open K3Helpers in
  (* find all the insert triggers and concatenate their arguments into one big argument
   * list with maybes *)
  let is_insert s     = r_match r_insert s && not @: r_match r_insert_bad s in
  let insert_trigs    = List.filter (is_insert |- U.id_of_code) @:
    U.triggers_of_program p in
  match insert_trigs with [] -> "" | _ -> (* check for no insert trigs *)
  (* Get lexicographical order so we know where the arguments belong *)
  let insert_trigs    = List.sort (fun x y ->
    String.compare (U.id_of_code x) (U.id_of_code y)) insert_trigs in
  let insert_ids      = List.map U.id_of_code insert_trigs in
  (* arg for each trigger *)
  let insert_args     = List.map U.args_of_code insert_trigs in
  let arg_types       = List.map KH.value_type_of_arg insert_args in
  (* the source has dates as strings, before we convert them *)
  let arg_types_source = List.map (fun args ->
    wrap_ttuple @: List.map (fun t ->
      if t = t_date then t_string else t)
      (unwrap_ttuple args)
  ) arg_types
  in
  let maybe_arg_types = wrap_tmaybes arg_types_source in
  let arg_ids         = List.map (fun trig -> trig^"_args") insert_ids in
  let new_args        = list_zip arg_ids maybe_arg_types in
  let trig_info       = list_zip insert_ids arg_types in
  (* add a demultiplexing argument *)
  let new_args'       = wrap_args @: ("trigger_id", t_string)::new_args in
  (* write the demultiplexing trigger *)
  let code =
    mk_code_sink "switch_main" new_args' [] @:
      List.fold_left (fun acc_code (trig_id, trig_t) ->
        let trig_args_unwrap_nm = trig_id^"_args_unwrap" in
        let trig_t_l = unwrap_ttuple trig_t in
        let handle_args =
          (* check if there's a date *)
          if List.exists ((=) t_date) trig_t_l then
            mk_unwrap_maybe [trig_id^"_args", wrap_tmaybe trig_t] @:
              let ids_types = types_to_ids_types ~first:1 "_a" trig_t_l in
              (* deconstruct the tuple and rebuild it, changing dates to ints *)
              mk_let_many ids_types (mk_var trig_args_unwrap_nm) @:
                mk_tuple @: List.map (fun (id, t) ->
                  if t = t_date then mk_apply (mk_var "parse_sql_date") (mk_var id)
                  else mk_var id
                ) ids_types
          (* no date, so simple code path *)
          else
            mk_unwrap_maybe [trig_id^"_args", wrap_tmaybe trig_t] @:
              mk_var @: trig_args_unwrap_nm
        in
        mk_if (mk_eq (mk_var "trigger_id") @: mk_cstring trig_id)
          (mk_send (mk_ctarget @: trig_id) K3Global.me_var handle_args)
          acc_code
        )
        mk_cunit
        (List.rev trig_info)
  in
  let flow = mk_flow [code] in
  let full_arg_types = t_string::maybe_arg_types in
  let source_s = "source s1 : "^string_of_value_type (wrap_ttuple full_arg_types)^
    " = file \""^filename^"\" k3\n" in
  let feed_s   = "feed s1 |> switch_main\n" in
  string_of_program [flow] envs ^ source_s ^ feed_s

(* print a new k3 program with added sources and feeds *)
(* envs are the typechecking environments to allow us to do incremental typechecking *)
let string_of_dist_program ?(file="default.txt") ?map_to_fold (p, envs) =
  let p' = filter_incompatible p in
  string_of_program ?map_to_fold p' envs ^ add_sources p' envs file

