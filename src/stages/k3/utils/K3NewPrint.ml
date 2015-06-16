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

let indent = ref 0

let force_list = List.iter force

(* lazy functions *)
let lhv i = [lazy (obc i)]     (* open box *)
let lhov i = [lazy (obv i)]
let lv i = [lazy (obx i)]   (* open vertical box *)
let lcb () = [lazy (cb ())]     (* close box *)
(*let lhv 2 = [lazy (obc 2)]*)
let lcut () = [lazy (pbsi 0 0)] (* print nothing or split line *)
let lind () = [lazy (indent := !indent + 2; pbsi 1 !indent)] (* print break with indent or space *)
let undo_indent () = [lazy (indent := !indent - 2)]
let lsp () = [lazy (pbsi 1 !indent)] (* print space or split line *)
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
(* let wrap_indent f = f (* lind () <| f <| undo_indent () *) *)

let lazy_concat ?(sep=lsp) f l =
  let len = List.length l - 1 in
  let l2 = list_populate (fun _ -> sep ()) 0 len in
  List.flatten @@ list_intersperse (List.map f l) l2

(* separator for lazy_concat *)
let lcomma () = lps "," <| lsp ()

(* type we pass all the way down for configuring behaviors *)
type config = {
                env:T.type_bindings_t;  (* type bindings for environment *)
                trig_env:T.type_bindings_t;
                map_to_fold:bool;       (* convert maps and ext to fold, due to k3new limitations *)
                project:StrSet.t;       (* need to project further down *)
              }

let default_config = {
                       env = [];
                       trig_env = [];
                       map_to_fold = false;
                       project = StrSet.empty;
                     }

let verbose_types_config = default_config

let typecheck e =
  try
    T.type_of_expr e
  with T.TypeError _ ->
    (* try to deduce expression type if missing *)
    T.type_of_expr @@ T.deduce_expr_type ~override:false [] [] e

(* light type checking for expressions we create *)
let light_type c e =
  try
     T.deduce_expr_type ~override:false c.trig_env c.env e
  with T.TypeError (uuid, name, err) as exc ->
    prerr_string @@ Printf.sprintf "Typechecker Error @%d: %s\n%s\n\n%s" uuid name
      (K3TypeError.string_of_error err) (K3PrintSyntax.string_of_expr ~uuid_highlight:uuid e);
    raise exc

(* Get a binding id from a number *)
let id_of_num i = Printf.sprintf "b%d" i

let abc_str = "abcdefghijklmnopqrstuvwxyz"

(* Get a record id from a number
 * we use a scheme of a..z, za, zb..zz, zza..
 *)
let record_id_of_num ?(prefix="r") i =
  let rec s_of_i acc i =
    if i <= String.length abc_str then
      Printf.sprintf "%s%c" acc (abc_str.[i-1])
    else
      s_of_i (acc ^ "z") (i-String.length abc_str)
  in
  prefix ^ (s_of_i "" i)

(* Add record ids to a list *)
let add_record_ids ?prefix l =
  match l with
  | []    -> failwith "No list to add record ids to"
  | [x]   -> ["i", x]
  | [x;y] -> ["key", x; "value", y] (* to make gbaggs easy *)
  | _     ->
    let i_l = insert_index_fst ~first:1 l in
    List.map (fun (i, x) -> record_id_of_num ?prefix i, x) i_l

(* Add record ids to a string *)
let concat_record_str ?(sep=":") l =
  List.map (fun (s,x) -> Printf.sprintf "%s%s%s" s sep x) l

let error () = lps "???"

let lazy_bracket_list f l = lazy_bracket (lps_list NoCut f l)

let lazy_col = function
  | TSet        -> lps "{ Set }"
  | TBag        -> lps "{ Collection }"
  | TList       -> lps "{ Seq }"
  | TMap        -> lps "{ Map }"
  | TVMap       -> lps "{ VMap }"

let lazy_control_anno c = function
  | Effect ids -> lps "effect " <| lazy_paren @@
      lps_list NoCut lps ids
  | Parallel i -> lps "parallel " <| lazy_paren (lps @@ string_of_int i)

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
  | annos -> lps "@ " <| lazy_brace @@ lps_list ~sep:"; " NoCut (lazy_anno c) annos

let rec lazy_base_type ?(brace=true) ?(mut=false) ?(empty=false) c ~in_col t =
  let wrap_mut f = if mut && not empty then lps "mut " <| f else f in
  let wrap_single f =
    let wrap = if brace then lazy_brace else id_fn in
    if in_col then wrap(lps "i:" <| f) else f
  in
  let wrap = wrap_single |- wrap_mut in
  match t with
  | TUnit        -> wrap @@ lps "()"
  | TBool        -> wrap @@ lps "bool"
  | TByte        -> wrap @@ lps "byte"
  | TInt
  | TDate        -> wrap @@ lps "int"
  | TFloat       -> wrap @@ lps "real"
  | TString      -> wrap @@ lps "string"
  | TMaybe(vt)   -> wrap(lps "option " <| lazy_type c ~in_col vt)
  | TAddress     -> wrap @@ lps "address" (* ? *)
  | TTarget bt   -> wrap (lps "target" <| lazy_type c ~in_col bt)
  | TUnknown     -> wrap @@ lps "()"
  | TTop         -> wrap @@ lps "top"
  | TIndirect vt -> wrap (lps "ind " <| lazy_type c ~in_col vt)
  | TTuple(vts)  -> (* tuples become records *)
      let rec_vts = add_record_ids vts in
      let inner = lazy_concat ~sep:lcomma (fun (id, vt) ->
        lps (id^":") <| lazy_type c ~in_col:false vt) rec_vts in
      let wrap = if brace then lazy_brace else id_fn in
      wrap_mut (wrap (lsp () <| inner <| lsp ()))
  | TCollection(ct, vt) -> wrap (
    (if not empty then lps "collection " else [])
    <| lazy_type c ~in_col:true vt <| lps " @ " <| lazy_col ct)
  | TFunction(it, ot) -> lazy_type c it <| lps " -> " <| lazy_type c ot

and lazy_type ?empty ?(in_col=false) c {typ; mut; anno} =
  lazy_base_type ?empty ~in_col ~mut c typ

let rec lazy_arg c drop_tuple_paren = function
  | AIgnored      -> lps "_"
  | AVar (id, vt) -> lps id
  | _             -> failwith "shouldn't be here"

let lazy_id_type c (id,t) = lps (id^" : ") <| lazy_type c t

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
  | CInt i         -> lps @@ string_of_int i
  | CFloat f       -> lps @@ remove_float_dot @@ string_of_float f
  | CString s      -> lps @@ Printf.sprintf "\"%s\"" (String.escaped s)
  | CAddress(s, i) -> lps @@ Printf.sprintf "%s:%d" s i
  | CTarget id     -> lps id

(* wrap a const collection expression with collection notation *)
let lazy_collection_vt c vt eval = match vt.typ with
  | TCollection(ct, et) ->
      (* preceding list of element types *)
      let mut = et.mut in
      let lazy_elem_list =
        lazy_base_type ~brace:false ~in_col:true ~mut c et.typ <| lps "|" <| lsp ()
      in
      lps "{|" <| lazy_elem_list <| eval <| lps "|}" <| lps " @ " <| lazy_col ct
  | _ -> error () (* type error *)

(* arg type with numbers included in tuples and maybes *)
type arg_num
    = NIgnored
    | NVar      of int * id_t * type_t
    | NMaybe    of int * arg_num
    | NTuple    of int * arg_num list

(* get an id for the argument at the shallow level for trigger or lambda *)
let shallow_bind_id ~in_record = function
  | NIgnored        -> "_"
  (* A case of a single variable in an in_record (map etc) *)
  | NVar (i, id, vt) when in_record ->
      begin match vt.typ with
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
  | NIgnored        -> KH.canonical TUnknown
  | NVar(_, _, t)      -> t
  | NMaybe(_, a')   -> KH.canonical (TMaybe(value_type_of_arg_num a'))
  | NTuple(_, args) -> KH.canonical (TTuple(List.map value_type_of_arg_num args))

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
 * once given a bottomed-out inner expression
 *)
let rec extract_slice e =
  match U.tag_of_expr e with
  | Tuple -> U.decompose_tuple e, id_fn
  | Let _ ->
      let ids, bound, expr = U.decompose_let e in
      let tup, sub_expr    = extract_slice expr in
      tup, (KH.mk_let ids bound) |- sub_expr
  | _ -> failwith "extract_slice unhandled expression"

(* for vmaps, we encode many of our functions with just a vid inside a tuple. Extract
  * this for the API *)
let maybe_vmap c col pat fun_no fun_yes =
  let col, _ = KH.unwrap_tcol @@ T.type_of_expr col in
  if col = TVMap then match U.decompose_tuple pat with
    | vid::rest -> fun_yes vid (light_type c @@ KH.mk_tuple rest)
    | _         -> failwith "missing vid in pattern for vmap"
  else fun_no pat

(* check if a collection is a vmap *)
let is_vmap col = fst @@ KH.unwrap_tcol @@ T.type_of_expr col = TVMap

(* We return the pattern breakdown: a list, and a lambda forming the internal structure *)
let breakdown_pat pat = match U.tag_of_expr pat with
  | Tuple -> U.decompose_tuple pat, id_fn
  (* if we have a let, we need a proper tuple extraction *)
  | Let _ -> extract_slice pat
  | _     -> [pat], id_fn

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
    | Tuple, _ -> ids = KH.vars_to_ids @@ U.decompose_tuple body
    | _, _     -> false

(* Variable names to translate *)
module StringMap = Map.Make(struct type t = string let compare = String.compare end)
let var_translate = List.fold_left (fun acc (x,y) -> StringMap.add x y acc) StringMap.empty @@
  ["int_of_float", "truncate";
   "float_of_int", "real_of_int";
   "peers", "my_peers";
   "parse_sql_date", "tpch_date"]

type in_record = InRec | In
type out_record = OutRec | Out
type arg_info  = ALambda of in_record list | ANonLambda
type arg_info_l = (arg_info * out_record) list

let is_unknown c = match U.tag_of_expr c with
  | Const CUnknown -> true
  | _              -> false

(* create a deep bind for lambdas, triggers, and let statements
 * -depth allows to skip one depth level of binding
 * -top_expr allows us to use an expression at the top bind
 * -top_rec indicates that the first level of binding should be to a record *)
let rec deep_bind ?top_expr ~in_record c arg_n =
  let rec loop d a =
    let record = in_record && d=0 in (* do we want a record now *)
    (* we allow binding an expression at the top level *)
    let bind_text i = match top_expr, d with
      | Some e, d
          when d=0 -> lazy_expr c e
      | _              -> lps @@ id_of_num i
    in
    match a with
      (* pretend to unwrap a record *)
    | NVar(i, id, vt) when record ->
        begin match vt.typ with
        | TTuple _  -> []    (* don't bind if we have an id representing a record *)
        | _         ->
          (* force bind a variable that comes in as a pretend record *)
          lps "bind " <| lps (id_of_num i) <| lps " as {i:" <| lps id
          <| lps "} in " <| lcut ()
        end
    | NIgnored
    | NVar _      -> [] (* no binding needed *)
    | NTuple(i, args) ->
        (* only produce binds if we're deeper than specified depth *)
        (if d < 0 then [] else
          let args_id = List.map get_id_of_arg args in
          let rec_ids = List.filter ((<>) "_" |- snd) @@ add_record_ids args_id in
          (* filter out all the ignored ids *)
          begin match rec_ids with
          | []      -> [] (* if there's nothing to bind, skip it *)
          | rec_ids ->
            let args_rec = concat_record_str rec_ids in
            let sub_ids = lazy_concat ~sep:lcomma lps args_rec in
            lps "bind " <| bind_text i <| lps " as {" <| sub_ids <| lps "} in "
            <| lcut () end)  <|
        (* rest of the binds *)
        List.flatten @@ List.map (loop @@ d+1) args
  | NMaybe(i, arg)  ->
      if d < 0 then [] else
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
      wrap_if_big e @@ lazy_expr ~expr_info:info ?prefix_fn c e) args'

(* Apply a method to a collection *)
and apply_method ?prefix_fn c ~name ~col ~args ~arg_info =
  (* we only need parens if we're not applying to a variable *)
  let f = match U.tag_of_expr col with
  | Var _ -> id_fn
  | _     -> lazy_paren
    in f @@ lazy_expr c col <| apply_method_nocol c ~name ~args ~arg_info ?prefix_fn

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
    | _                         -> lazy_paren @@ lazy_expr c e
  in
  wrap_indent (lazy_expr c fun_e <| lsp () <| lazy_concat print_fn l_e)

(* handle the printing of a lambda *)
and handle_lambda c ~expr_info ~prefix_fn arg e =
  let arg_n = arg_num_of_arg arg in (* convert to arg_num *)
  let arg_l, out_rec = match expr_info with
    | ALambda [], _  -> failwith @@ Printf.sprintf "Missing lambda arg list at %d" (U.id_of_expr e)
    | ANonLambda, o  -> [In], o  (* deal with cases where we don't prepare it *)
    | ALambda l,  o  -> l,    o
  in
  let many_args = List.length arg_l > 1 in
  let write_lambda arg in_record =
    lps "\\" <| lps @@ shallow_bind_id ~in_record arg <| lps " ->" <| lsp ()
  in
  let final_exec bindings arg in_record =
    let binds = bindings <| deep_bind ~in_record c arg in
    (* for curried arguments, we deep bind at a deeper level *)
    write_lambda arg in_record <|
      (* for the final expr, we may need to wrap the output in a record *)
        binds <| lazy_expr c (prefix_fn e) ~expr_info:(ANonLambda, out_rec)
  in
  (* loop over the lambda arguments *)
  (* bindings are lazy binds which we only write at the end *)
  let rec loop bindings a = function
  | []    -> lps @@ Printf.sprintf "Incorrect number of args at %d" (U.id_of_expr e)
  | x::xs ->
    let in_record = match x with InRec -> true | _ -> false in
    (* handle argument by argument if lambda_many_args *)
    if many_args then
      match peel_arg a with
      | arg, Some tup_arg ->
          let binds = bindings <| deep_bind ~in_record c arg in
          lazy_paren (write_lambda arg in_record <| loop binds tup_arg xs)
      | arg, None         -> lazy_paren @@ final_exec bindings arg in_record
    else lazy_paren @@ final_exec bindings a in_record
  in loop [] arg_n arg_l


(* create a fold instead of a map or ext (for typechecking reasons) *)
(* expects a lambda expression, and collection expression inside a map/flattenMap *)
and fold_of_map_ext c expr =
  let open KH in
  let t_out = T.type_of_expr expr in
  (* customize for the different operations *)
  let (lambda, col), suffix = match U.tag_of_expr expr with
    | Map     -> U.decompose_map expr, "map"
    | Flatten -> U.decompose_map @@ U.decompose_flatten expr, "ext"
    | _       -> failwith "Can only convert flatten-map or map to fold"
  in
  let args, body = U.decompose_lambda lambda in
  let acc_id = "_acc"^suffix in
  let acc_arg = AVar (acc_id, t_out) in
  let args' = ATuple [acc_arg; args] in
  let body' = match U.tag_of_expr expr with
    | Map     -> mk_block [ mk_insert acc_id [body]; mk_var acc_id ]
    | Flatten -> KH.mk_combine (KH.mk_var acc_id) body
    | _       -> failwith "Can only convert flatten-map or map to fold"
  in
  lazy_expr c @@ light_type c @@
    KH.mk_agg (KH.mk_lambda args' body') (mk_empty t_out) col

(* convert a slice to a filter function.
 * @frontier: for a vmap, can change to a frontier lookup
 * NOTE: a direct lookup should be picked up by Peek(Slice(x,_)) matching
 *)
and filter_of_slice ~frontier c col pat =
  let es, lam_fn = breakdown_pat pat in
  let ts = List.map T.type_of_expr es in
  let id_e = add_record_ids es in
  let id_t = add_record_ids ts in
  (* find the non-unknown slices *)
  let filter_e = List.filter (not |- is_unknown |- snd) id_e in
  (* obvious optimization - no slice needed *)
  if null filter_e && not frontier then lazy_expr c col
  else
    (* adjust filter_e for frontier (remove vid) *)
    let filter_e' = if frontier then tl filter_e else filter_e in
    let do_eq (id, v) = KH.mk_eq (KH.mk_var id) v in
    let lambda = light_type c @@
      KH.mk_lambda' id_t @@
        lam_fn @@ (* apply an inner lambda constructor *)
        List.fold_right (fun x acc ->
          KH.mk_and acc @@ do_eq x
        )
        (try tl filter_e'
         with Invalid_argument _ -> [])
        (try do_eq @@ hd filter_e'
         with Invalid_argument _ -> light_type c KH.mk_ctrue)
    in
    let args, arg_info =
      if frontier then [snd @@ hd filter_e; lambda], [ANonLambda, OutRec; ALambda[InRec], Out]
      else [lambda], [ALambda[InRec], Out] in
    apply_method c ~name:"filter" ~col ~args ~arg_info

(* printing expressions *)
(* argnums: lambda only   -- number of expected arguments *)
(* prefix_fn: lambda only -- modify the lambda with a prefix *)
(* expr_info: additional info about the expression in the form of an arg_info structure *)
and lazy_expr ?(prefix_fn=id_fn) ?(expr_info=(ANonLambda,Out)) c expr =

  let expr_pair ?(sep=lps "," <| lsp ()) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(lazy_expr c e1) <| sep <| wr(lazy_expr c e2) in
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
(* for a stmt of a block *)
 let paren_stmt e = match U.tag_of_expr e with
   | IfThenElse | CaseOf _ | Apply -> lazy_paren
   | _ -> id_fn in
(* for things like .map *)
 let paren_l e = match U.tag_of_expr e with
    | Var _ | Const _ -> id_fn
    | _ -> lazy_paren in
  let paren_r e = match U.tag_of_expr e with
    | Nothing _ | Just | Const _ | Tuple | Var _
    | Empty _ | Singleton _ -> id_fn
    | _                     -> lazy_paren in
 let arith_paren_l e = match U.tag_of_expr e with
    | IfThenElse -> lazy_paren
    | Let _      -> lazy_paren
    | _          -> arith_paren e
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
    try begin match (T.type_of_expr e).typ with
        | TBool -> true
        | _     -> false
        end
    with T.TypeError(_,_,_) -> false (* assume additive *)
  in
  (* many instructions need to wrap the same way *)
  (* in let wrap e = match U.tag_of_expr expr with
    | Insert _ | Iterate | Map | Filter | Flatten | Send | Delete _ | Update _
    | Aggregate | GroupByAggregate -> wrap_hov 2 e
    | IfThenElse -> wrap_hov 0 e
    | _ -> id_fn e
  in *)
  (* begin analysis of tags *)
  let analyze () = match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id    ->
      begin try lps @@ StringMap.find id var_translate
      with Not_found ->
        (* check if we need to do projection *)
        if StrSet.mem id c.project then lps id <| lps ".i"
        else lps id
      end
  | Tuple     -> let es = U.decompose_tuple expr in
    let id_es = add_record_ids es in
    let inner = lazy_concat ~sep:lcomma (fun (id, e) ->
        lps (id^":") <| lazy_expr c e) id_es
    in lazy_brace inner
  | Just -> let e = U.decompose_just expr in
    lps "Some " <| paren_r e (lazy_expr c e)
  | Nothing vt -> lps "None " <| if vt.mut then lps "mut" else lps "immut"
  | Empty vt   -> lps "empty " <| lazy_type ~empty:true c ~in_col:false vt
  | Singleton _ ->
    (* Singletons are sometimes typed with unknowns (if read from a file) *)
    let e = U.decompose_singleton expr in
    let t = typecheck expr in
    (* for vmaps, we need to convert to a sequence with empty *)
    if is_vmap expr then
      let e' =
        let open KH in
        mk_let ["x"] (mk_empty t) @@
          mk_block [
            mk_insert "x" [e];
            mk_var "x" ] in
      lazy_expr c @@ light_type c e'
    else
      lazy_collection_vt c t @@ lazy_expr c e
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
    let e1, e2 = U.decompose_combine expr in
    (* wrap the left side of the combine if it's needed *)
    (* for vmaps, we can't use sugared syntax. Must have ugly expressions *)
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
      | Singleton vt, Combine
      | Singleton vt, Singleton _
      | Singleton vt, Empty _ when not @@ is_vmap e1 && not @@ is_vmap e2 ->
          let t = T.type_of_expr expr in
          lazy_collection_vt c t @@ assemble_list c expr
      | _ -> apply_method c ~name:"combine" ~col:e1 ~args:[e2] ~arg_info:[ANonLambda, Out]
    end

  | Range ct -> let st, str, num = U.decompose_range expr in
    (* newk3 range only has the last number *)
    let range_type = KH.canonical @@ TFunction(KH.t_int, KH.wrap_tlist KH.t_int) in
    function_application c (U.attach_type range_type @@ KH.mk_var "range") [num]
  | Add -> let (e1, e2) = U.decompose_add expr in
    begin match U.tag_of_expr e2, expr_type_is_bool e1 with
      | Neg, false -> let e3 = U.decompose_neg e2 in
        arith_paren_pair "-" (e1, e3)
      | _, false -> arith_paren_pair "+" (e1, e2)
      | _, true -> arith_paren_pair "or" (e1, e2)
    end
  | Mult -> let e1, e2 = U.decompose_mult expr in
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
      | _ -> lps sym <| lazy_paren @@ lazy_expr c e
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
      | _ -> function_application c e1 [e2]
    end
  | Block -> let es = U.decompose_block expr in
    lazy_paren @@ wrap_indent (wrap_hv 0 @@
      lps_list ~sep:";" CutHint (fun e ->
        wrap_hov 0 @@ paren_stmt e @@ lazy_expr c e) es)

  | IfThenElse -> let e1, e2, e3 = U.decompose_ifthenelse expr in
    wrap_indent (lps "if " <| lazy_expr c e1) <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3)

  | CaseOf id ->
    let e1, e2, e3 = U.decompose_caseof expr in
    (* HACK to make peek work: records of one element still need projection *)
    (* NOTE: caseOf will only work on peek if peek is the first expr inside,
     * and projection will only work if the expression is very simple *)
    let project =
      begin match U.tag_of_expr e1 with
      | Peek ->
          let col = U.decompose_peek e1 in
          (* get the type of the collection. If it's a singleton type, we need to add
          * projection *)
          let col_t = T.type_of_expr col in
          begin match col_t.typ with
          | TCollection(_, vt) -> begin match vt.typ, snd expr_info with
            | TTuple _, _ -> false
            | _, OutRec   -> false (* we need a record output *)
            | _           -> true
            end
          | _ -> failwith "expected a collection type"
          end
      | _ -> false
      end
    in
    let c' = {c with project=StrSet.remove id c.project} in
    let c' =
      (* if we're projecting, let future expressions know *)
      if project && id <> "_" then {c' with project=StrSet.add id c'.project}
      else c' in
    lps "case" <| lsp () <| lazy_expr c e1 <| lsp () <| lps "of" <| lsp () <|
    wrap_indent (lazy_brace (lps ("Some "^id^" ->") <| lsp () <|
      lazy_expr c' e2)) <|
    wrap_indent (lazy_brace (lps "None ->" <| lsp () <| lazy_expr c e3))

  | BindAs _ -> let bind, id, r = U.decompose_bind expr in
    let c = {c with project=StrSet.remove id c.project} in
    lps "bind" <| lsp () <| wrap_indent(lazy_expr c bind) <| lsp () <| lps "as" <| lsp () <|
    lps "ind" <| lsp () <| lps id <| lsp () <| lps "in" <| lsp () <|
    wrap_indent (lazy_expr c r)

  | Let _ ->
      (* let can be either bind (destruct tuples) or let in new k3 *)
      let ids, bound, bexpr = U.decompose_let expr in
      (* remove ids that need projection *)
      let c =
        {c with project=List.fold_left (fun acc x -> StrSet.remove x acc) c.project ids}
      in
      begin match ids with
        | [id] -> (* let *)
          lps "let" <| lsp () <| lps id <| lsp () <| lps "=" <| lsp () <|
          wrap_indent (lazy_expr c bound) <| lsp () <| lps "in" <| lsp () <|
          wrap_indent (lazy_expr c bexpr)
        | _   ->  (* bind deconstruct *)
            let ids' = concat_record_str @@ add_record_ids ids in
            lps "bind" <| lsp () <| wrap_indent(lazy_expr c bound) <| lsp () <|
            lps "as" <| lsp () <| lps "{" <| lps_list NoCut lps ids' <|
            lps "}" <| lsp () <| lps "in" <| lsp () <|
            wrap_indent (lazy_expr c bexpr)
      end

  | Iterate -> let lambda, col = U.decompose_iterate expr in
    apply_method c ~name:"iterate" ~col ~args:[lambda] ~arg_info:[ALambda [InRec], Out]

  | Map ->
      if c.map_to_fold then fold_of_map_ext c expr
      else (* normal map *)
        let lambda, col = U.decompose_map expr in
        apply_method c ~name:"map" ~col ~args:[lambda] ~arg_info:[ALambda [InRec], OutRec]

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
        let t = T.type_of_expr expr in
        let t = match t.typ with
          | TCollection(TList, x) -> KH.canonical @@ TCollection(TList, x)
          | TCollection(_, x)     -> KH.canonical @@ TCollection(TBag, x)
          | _                     -> failwith "not a collection"
        in
        let empty_c = light_type c @@ KH.mk_empty t in
        begin match U.tag_of_expr e with
        | Map ->
            let lambda, col = U.decompose_map e in
            apply_method c ~name:"ext" ~col ~args:[lambda; empty_c]
              ~arg_info:[ALambda [InRec], Out; ANonLambda, Out]
        | _   -> failwith "Unhandled Flatten without map"
        end

  | Aggregate -> let lambda, acc, col = U.decompose_aggregate expr in
    let name = if is_vmap col then "fold_all" else "fold" in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name ~col ~args:[lambda; acc]
      ~arg_info:[ALambda [In; InRec], Out; ANonLambda, Out]

  | AggregateV -> let lambda, acc, col = U.decompose_aggregatev expr in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"fold" ~col ~args:[lambda; acc]
      ~arg_info:[ALambda [In; In; InRec], Out; ANonLambda, Out]

  | GroupByAggregate -> let lam1, lam2, acc, col = U.decompose_gbagg expr in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"groupBy" ~col ~args:[lam1; lam2; acc]
      ~arg_info:[ALambda [InRec], Out; ALambda [In; InRec], Out; ANonLambda, Out]

  | Sort -> let lambda, col = U.decompose_sort expr in
    apply_method c ~name:"sort" ~col ~args:[lambda] ~arg_info:[ALambda [InRec; InRec], Out]
      ~prefix_fn:(fun e -> light_type c @@ KH.mk_if e (KH.mk_cint (-1)) @@ KH.mk_cint 1)

  | Size -> let col = U.decompose_size expr in
    let name = if is_vmap col then "total_size" else "size" in
    apply_method c ~name ~col ~args:[light_type c KH.mk_cunit]
      ~arg_info:[ANonLambda, Out]

  | Peek -> let col = U.decompose_peek expr in
    (* normal peek applications *)
    let name = if is_vmap col then "peek_now" else "peek" in
    let normal () = lazy_paren @@ apply_method c ~name ~col
      ~args:[light_type c @@ KH.mk_cunit] ~arg_info:[ANonLambda, Out] in

    (* to handle the case where we have a full slice over a vmap, we need to look ahead *)
    let tag = U.tag_of_expr col in
    let col_t = fst @@ KH.unwrap_tcol @@ T.type_of_expr col in

    (* common pattern for vmap lookups *)
    let handle_slice_vmap decomp_fn name =
      let col', pat = decomp_fn col in
      (* turn pat into a list. drop the value *)
      let pat  = fst @@ breakdown_pat pat in
      let pat' = list_drop_end 1 pat in
      (* check if we have a specific value rather than an open slice pattern *)
      if List.for_all (not |- is_unknown) pat' then
        apply_method c ~name ~col:col' ~args:[hd pat; KH.mk_tuple (tl pat)]
          ~arg_info:[ANonLambda, Out; ANonLambda, Out]
      else normal ()
    in
    begin match col_t, tag with
    | TVMap, Slice         -> handle_slice_vmap U.decompose_slice "lookup"
    | TVMap, SliceFrontier -> handle_slice_vmap U.decompose_slice_frontier "lookup_before"
    | _ -> normal ()
    end

  | Subscript _ -> let i, tup = U.decompose_subscript expr in
      let t = KH.unwrap_ttuple @@ T.type_of_expr tup in
      let id_t = add_record_ids t in
      let id = fst @@ at id_t (i-1) in
      (paren_l tup @@ lazy_expr c tup) <| lps "." <| lps id

  | SliceFrontier ->
      (* this only works on a vmap, so we can assume we have a vmap *)
      let col, pat = U.decompose_slice_frontier expr in
      filter_of_slice ~frontier:true c col pat

  | Slice ->
      let col, pat = U.decompose_slice expr in
      filter_of_slice ~frontier:false c col pat

  | Insert -> let col, x = U.decompose_insert expr in
    maybe_vmap c col x
      (fun x -> lazy_expr c col <| apply_method_nocol c ~name:"insert" ~args:[x]
          ~arg_info:[ANonLambda,OutRec])
      (fun vid x -> lazy_expr c col <| apply_method_nocol c ~name:"insert" ~args:[vid;x]
          ~arg_info:[ANonLambda, OutRec; ANonLambda,OutRec])
  | Delete -> let col, x = U.decompose_delete expr in
    maybe_vmap c col x
      (fun x -> lazy_expr c col <| apply_method_nocol c ~name:"erase" ~args:[x]
        ~arg_info:[ANonLambda,OutRec])
      (fun vid x -> lazy_expr c col <| apply_method_nocol c ~name:"erase" ~args:[vid;x]
        ~arg_info:[ANonLambda,OutRec; ANonLambda,OutRec])

  | DeletePrefix -> let col, x = U.decompose_delete_prefix expr in
    maybe_vmap c col x
      (fun x -> lazy_expr c col <| apply_method_nocol c ~name:"erase_prefix" ~args:[x]
        ~arg_info:[ANonLambda,OutRec])
      (fun vid x -> lazy_expr c col <| apply_method_nocol c ~name:"erase_prefix" ~args:[vid;x]
        ~arg_info:[ANonLambda, OutRec; ANonLambda,OutRec])

  | Update -> let col, oldx, newx = U.decompose_update expr in
    maybe_vmap c col newx
      (fun newx ->
        lazy_expr c col <| apply_method_nocol c ~name:"update" ~args:[oldx;newx]
        ~arg_info:[ANonLambda,OutRec; ANonLambda,OutRec])
      (fun vid newx ->
        lazy_expr c col <| apply_method_nocol c ~name:"update" ~args:[vid;oldx;newx]
        ~arg_info:[ANonLambda,OutRec; ANonLambda,OutRec; ANonLambda,OutRec])

  | UpdateSuffix -> let col, key, lambda = U.decompose_update_suffix expr in
    begin match U.decompose_tuple key with
    | vid::key ->
        lazy_expr c col <| apply_method_nocol c ~name:"update_suffix"
        ~args:[vid; light_type c @@ KH.mk_tuple key; lambda]
        ~arg_info:[ANonLambda,OutRec; ANonLambda,OutRec; ALambda[In;InRec],OutRec]
    | _ -> failwith "UpdateSuffix: bad key"
    end

  | UpsertWith -> let col, key, lam_no, lam_yes = U.decompose_upsert_with expr in
    maybe_vmap c col key
      (fun key -> lazy_expr c col <| apply_method_nocol  c ~name:"upsert_with" ~args:[key; lam_no; lam_yes]
        ~arg_info:[ANonLambda,OutRec; ALambda[In],OutRec; ALambda[InRec],OutRec])
      (fun vid key -> lazy_expr c col <| apply_method_nocol  c ~name:"upsert_with" ~args:[vid; key; lam_no; lam_yes]
        ~arg_info:[ANonLambda, OutRec; ANonLambda,OutRec; ALambda[In],OutRec; ALambda[InRec],OutRec])

  | Assign -> let l, r = U.decompose_assign expr in
    lazy_expr c l <| lsp () <| lps "=" <| lsp () <| lazy_expr c r

  | Indirect -> let x = U.decompose_indirect expr in
    lps "ind" <| lsp () <| lazy_expr c x

  | Send -> let target, addr, args = U.decompose_send expr in
    wrap_indent @@ lazy_paren (expr_pair (target, addr)) <| lps "<- " <|
      lps_list CutHint (lazy_expr c) args
  in
  (* check if we need to wrap our output in a tuple (record) *)
  match snd expr_info with
  | OutRec ->
    begin match U.tag_of_expr expr with
    | Lambda _ -> analyze ()
      (* don't wrap a lambda itself *)
    | _ ->
        (* check the type of the expression *)
        let t =
          begin try T.type_of_expr expr
            with T.TypeError(id, _, T.UntypedExpression) ->
              raise @@ MissingType(id, K3Printing.string_of_expr expr)
          end
        in
        begin match t.typ with
        | TTuple _ -> analyze ()
        | _        -> lazy_expr ~expr_info:((fst expr_info, Out))
                        ~prefix_fn c @@ light_type c @@ KH.mk_tuple ~force:true [expr]
        end
    end
  | _ -> analyze ()

let lazy_trigger c id arg vars expr =
  (*let is_block expr = match U.tag_of_expr expr with Block -> true | _ -> false in
  let indent f = if is_block expr
               then lps " " <| f
               else wrap_indent f in*)
  wrap_indent (lps ("trigger "^id) <| lps " : " <|
  lazy_type ~in_col:false c @@ KH.type_of_arg arg <| lsp () <|
  lps "=" <| lsp () <| lazy_expr c @@ KH.mk_lambda arg expr <| lcut ())

let channel_format c = function
  | CSV  -> "psv"
  | JSON -> "json"

let lazy_channel c chan_t chan_f = match chan_t with
  | File s -> lps @@ Printf.sprintf "file \"%s\" %s" s (channel_format c chan_f)
  | Network(str, port) -> lps @@ "socket(\""^str^"\":"^string_of_int port^")"

let rec lazy_resource_pattern c = function
  | Terminal id -> lps id
  | Choice rps  -> lps_list ~sep:"| " CutHint (lazy_resource_pattern c) rps
  | Sequence rps -> lps_list ~sep:" " CutHint (lazy_resource_pattern c) rps
  | Optional rp -> lazy_resource_pattern c rp <| lps "?"
  | Repeat(rp, _) -> lazy_resource_pattern c rp <| lps "*"

let lazy_stream c = function
  | RandomStream i -> lps "random" <| lazy_paren (lps @@ string_of_int i)
  (* k3o must put a collection here, and k3 expects a value, so extract it *)
  | ConstStream e  ->
      begin match KH.list_of_k3_container e with
      | [e] -> lps "value" <| lazy_paren (lazy_expr c e)
      | _   -> failwith "cannot translate stream to k3new"
      end

let lazy_resource c r =
  let common t = lazy_type c t <| lps " = " in
  match r with
  | Handle(t, chan_t, chan_f) -> common t <| lazy_channel c chan_t chan_f
  | Stream(t, st) -> common t <| lazy_stream c st
  | Pattern(pat) -> lps "pattern " <| lazy_resource_pattern c pat

let lazy_flow c e =
  let out =
    match e with
    | Source(Code(id, arg, vars, expr))
    | Sink(Code(id, arg, vars, expr)) -> lazy_trigger c id arg vars expr
    | Source(Resource(id, r)) ->
        lps ("source "^id^" : ") <| lazy_resource c r
    | BindFlow(id1, id2) -> lps @@ "feed "^id1^" |> "^id2
    | _ -> []
  in out <| lcut () <| lcut ()

let lazy_flow_program c fas = lps_list ~sep:"" CutHint (lazy_flow c |- fst) fas

let lazy_declaration c d =
  let out = match d with
  | Global(id, t, expr) -> let end_part = begin match expr with
      | None -> []
      | Some e ->
          (* handle the special case of the csv loader *)
          try
            let lam, s = U.decompose_apply e in
            begin match U.tag_of_expr lam, U.tag_of_expr s with
            | Var "load_csv_col", Const(CString f) -> lps (Printf.sprintf "@@LoadFile(%s)" f) <| lcut ()
            | _ -> failwith "Not found"
            end
          with Failure _ -> (* normal global *)
            lps " =" <| lsp () <| lazy_expr c e
    end in
    wrap_indent (lps @@ "declare "^id^" :" <| lsp () <| lazy_type c t <| end_part)
  | Role(id, fprog) -> lazy_flow_program c fprog
  | Flow fprog -> lazy_flow_program c fprog
  | DefaultRole id -> []
  | Foreign(id, t) -> lps ("declare "^id^" :") <| lsp () <| lazy_type c t
  in
  wrap_hov 0 out <| lcut () <| lcut ()

let wrap_f = wrap_formatter ~margin:80

(* print a K3 type in syntax *)
let string_of_base_type t = wrap_f @@ fun () ->
  force_list @@ lazy_base_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_value_type t = wrap_f @@ fun () ->
  force_list @@ lazy_type verbose_types_config t

module StringSet = Set.Make(struct type t=string let compare=String.compare end)

(* remove/convert functions that are renamed in k3new *)
let filter_incompatible prog =
  let drop_globals = List.fold_left (flip StringSet.add) StringSet.empty
    ["error"; "divf"; "mod"; "float_of_int"; "int_of_float"; "get_max_int"; "parse_sql_date"; "peers"; ]
  in
  filter_map (fun ((d,a) as dec) ->
    (* we don't want the monomorphic hash functions *)
    match d with
    | Foreign(id, _)   -> None
    | Global(id, _, _) when StringSet.mem id drop_globals -> None
    | DefaultRole _ -> None
    | _             -> Some dec
  ) prog

(* print a K3 program in syntax *)
(* We get the typechecking environments so we can do incremental typechecking where needed *)
let string_of_program ?(map_to_fold=false) prog (env, trig_env) =
  let config = {default_config with env; trig_env; map_to_fold} in
  wrap_f @@ fun () ->
    let l = wrap_hv 0 (lps_list ~sep:"" CutHint (lazy_declaration config |- fst) prog) in
    force_list l

(* print a new k3 program with added sources and feeds *)
(* envs are the typechecking environments to allow us to do incremental typechecking *)
let string_of_dist_program ?(file="default.txt") ?map_to_fold (p, envs) =
  let p' = filter_incompatible p in
  "include \"Core/Builtins.k3\"\n"^
  "include \"Annotation/Map.k3\"\n"^
  "include \"Annotation/Maps/VMap.k3\"\n"^
  "include \"Annotation/Set.k3\"\n"^
  "include \"Annotation/Seq.k3\"\n"^
  "declare my_peers : collection { i:address } @ {Collection} =\n"^
  "  peers.fold (\\acc -> (\\x -> (acc.insert {i:x.addr}; acc))) empty { i:address} @ Collection\n"^
  string_of_program ?map_to_fold p' envs

