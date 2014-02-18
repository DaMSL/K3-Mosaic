(* Module to print AST into new K3 syntax *)

open Util
open Printing
open Lazy
open K3.AST
open K3.Annotation

module U = K3Util
module T = K3Typechecker
module KH = K3Helpers

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

(* type we pass all the way down for configuring behaviors *)
type config = {verbose_types:bool; (* more verbose type printing *)
               uuid:int option;    (* highlight a particular uuid *)
               lambda_ret:bool} (* highlight a lambda with a non-tuple return type *)

let default_config = {verbose_types=false;
                      uuid=None;
                      lambda_ret=false}

let verbose_types_config = {default_config with verbose_types=true}

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

let error () = lps "???"

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

let rec lazy_base_type c ~in_col ?(no_paren=false) t = 
  let proc () = match t with
  | TUnit       -> lps "unit"
  | TBool       -> lps "bool"
  | TByte       -> lps "byte"
  | TInt        -> lps "int"
  | TFloat      -> lps "float"
  | TString     -> lps "string"
  | TMaybe(vt)  -> lps "maybe " <| lazy_value_type c ~in_col vt
  | TTuple(vts) ->
      let inner () = lps_list NoCut (lazy_value_type c ~in_col) vts in
      if not c.verbose_types && no_paren then inner ()
      else lps "(" <| inner () <| lps ")"
  | TCollection(TSet, vt) -> 
      lps "{" <| lazy_value_type c ~in_col:true ~no_paren:true vt <| lps "}"
  | TCollection(TBag, vt) -> 
      lps "{|" <| lazy_value_type c ~in_col:true ~no_paren:true vt <| lps "|}"
  | TCollection(TList, vt) -> 
      lps "[" <| lazy_value_type c ~in_col:true ~no_paren:true vt <| lps "]"
  | TAddress -> lps "address" (* ? *)
  | TTarget bt -> lps "target" <| lazy_base_type c ~in_col bt
  | TUnknown -> lps "unknown"
  in
  if c.verbose_types && in_col then lps "c:" <| proc () 
  else proc ()

(* TODO: annotations *)
and lazy_mutable_type c ~in_col ?(no_paren=false) = function
  | TMutable (bt, a)   -> 
      lps "ref " <| lazy_base_type c ~in_col:true ~no_paren:false bt
  | TImmutable (bt, a) -> lazy_base_type c ~in_col ~no_paren bt

and lazy_value_type c ~in_col ?(no_paren=false) = function
  | TIsolated mt  -> lazy_mutable_type c ~in_col ~no_paren mt
  | TContained mt -> lazy_mutable_type c ~in_col ~no_paren mt

let lazy_type c = function
  | TFunction(f,t) ->
      lazy_value_type c ~in_col:false f <| lps " -> " <| 
      lazy_value_type c ~in_col:false t
  | TValue vt -> lazy_value_type c false vt

let rec lazy_arg c drop_tuple_paren a = 
  let paren = if drop_tuple_paren then id_fn else lazy_paren in
  match a with
  | AIgnored      -> lps "_"
  | AVar (id, vt) -> lps id
  | AMaybe(arg)   -> lps "just " <| lazy_arg c false arg
  | ATuple(args)  ->
      lhov 0 <| paren (lps_list CutHint (lazy_arg c false) args) <| lcb ()

let lazy_id_type c (id,t) = lps (id^" : ") <| lazy_value_type c false t

let lazy_const c = function
  | CUnknown       -> lps "_"
  | CUnit          -> lps "()"
  | CBool(true)    -> lps "true"
  | CBool(false)   -> lps "false"
  | CInt(i)        -> lps @: string_of_int i
  | CFloat(f)      -> lps @: string_of_float f
  | CString(s)     -> lps @: "\""^String.escaped s^"\""
  | CAddress(s, i) -> lps @: s^":"^string_of_int i
  | CTarget(id)    -> lps id

let lazy_collection c ct eval = match ct with
    | TSet  -> lps "{" <| eval <| lps "}"
    | TBag  -> lps "{|" <| eval <| lps "|}"
    | TList -> lps "[" <| eval <| lps "]"

let lazy_collection_vt c vt eval = match vt with
  | TIsolated(TMutable(TCollection(ct, _),_))
  | TIsolated(TImmutable(TCollection(ct, _),_))
  | TContained(TMutable(TCollection(ct, _),_))
  | TContained(TImmutable(TCollection(ct, _),_)) -> lazy_collection c ct eval
  | _ -> error () (* type error *)

let lazy_concat ?(sep=lsp) f l = 
  let len = List.length l - 1 in
  let l2 = list_populate (fun _ -> sep ()) 0 len in
  List.flatten @: list_intersperse (List.map f l) l2

(* Get an id from a number *)
let id_of_num i = Printf.sprintf "_b%d_" i

(* arg type with numbers included in tuples and maybes *)
type arg_num
    = NIgnored
    | NVar      of int * id_t * value_type_t
    | NMaybe    of int * arg_num 
    | NTuple    of int * arg_num list

(* get an id for the argument at the shallow level for trigger or lambda *)
let shallow_bind_id ?(in_record=false) = function
  | NIgnored        -> "_"
  (* if we want a record, we'll need *)
  | NVar (i, id, _) when in_record
                    -> id_of_num i
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

(* create a deep bind for lambdas, triggers, and let statements
 * -depth allows to skip one depth level of binding
 * -top_expr allows us to use an expression at the top bind
 * -top_rec indicates that the first level of binding should be to a record *)
let rec deep_bind ?(depth=0) ?top_expr ?(in_record=false) c arg_n =
  let rec loop d a = 
    let record = in_record && d=depth in (* do we want a record now *)
    match a with
      (* unwrap a record *)
    | NVar(i, id, _) when record -> 
        lps "bind " <| lps (id_of_num i) <| lps " as {elem:" <| lps id
        <| lps "} in " <| lcut ()
    | NIgnored 
    | NVar _      -> [] (* do nothing *)
    | NTuple(i, args) -> 
        (* we allow binding an expression at the top level *)
        let bind_text = begin match top_expr, d with 
          | Some e, d when d=depth -> lazy_expr c e
          | _                      -> lps @: id_of_num i
        end in
        (* only produce binds if we're deeper than specified depth *)
        (if d >= depth then
          if record then 
            let to_rec_id i  = Printf.sprintf "_r%d_" i in
            let add_rec_id (i,s) = Printf.sprintf "%s:%s" (to_rec_id i) s in
            let args_id = List.map get_id_of_arg args in
            let args_i = insert_index_fst 1 args_id in
            let args_rec : string list = List.map add_rec_id args_i in
            let sub_ids = lazy_concat ~sep:(fun () -> lps ", " <| lsp ()) lps args_rec in
            lps "bind " <| bind_text <| lps " as {" <| sub_ids <| lps "} in " 
              <| lcut ()
          else
            let sub_ids = lazy_concat ~sep:(fun () -> lps "," <| lsp ())
              (lps |- get_id_of_arg) args in
            lps "bind " <| bind_text <| lps " as (" <| sub_ids <| lps ") in " 
              <| lcut ()
        else []) 
        <| List.flatten @: List.map (loop @: d+1) args
    | NMaybe(i, arg)  -> 
        (* we don't have such sophisticated pattern matching methods. On the nothing side, 
         * we put a canonical value *)
        let typ = value_type_of_arg_num arg in
        let default_v = T.canonical_value_of_type typ in
        lps "case " <| lps (id_of_num i) <| lps " of " <| lcut () <|
        lps "Nothing -> " <| lazy_expr c default_v <| lcut () <|
        lps "Just " <| lps (get_id_of_arg arg) <| lps " -> " <| lcut ()
  in loop 0 arg_n

(* Apply a method -- the lambda part
 * -in_record: the lambda should take in a record *)
and apply_method_nocol ?many_args ?in_record c ~name ~args =
  let wrap_if_big e = match U.tag_of_expr e with
      | Var _ | Const _ | Tuple | Empty _ -> id_fn
      | _ -> lazy_paren
  in
  let args = match many_args with
    | None    -> List.map (fun x -> x, false) args
    | Some rs -> list_zip args rs
  in
  lps ("."^name) <| lsp () <| 
    lazy_concat (fun (e, m_args) -> 
      wrap_if_big e @: lazy_expr ~many_args:m_args ?in_record c e) args

(* Apply a method to a collection *)
and apply_method ?many_args ?in_record c ~name ~col ~args =
  (* we only need parens if we're not applying to a variable *)
  let f = match U.tag_of_expr col with
  | Var _ -> id_fn
  | _     -> lazy_paren
  in f @: lazy_expr c col <| apply_method_nocol c ~name ~args ?many_args ?in_record

(* printing expressions *)
(* argnums is for lambda only: number of expected arguments *)
and lazy_expr ?(many_args=false) ?in_record c expr =
  let expr_pair ?(sep=lps "," <| lsp ()) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(lazy_expr c e1) <| sep <| wr(lazy_expr c e2) in
  let expr_sub ?(sep=lps "," <| lsp ()) p =
    expr_pair ~sep:sep ~wl:wrap_indent ~wr:(wrap_hov 0) p in
  let expr_triple ?(sep=fun () -> lps "," <| lsp ()) (e1,e2,e3) =
    let w = wrap_indent in
    w(lazy_expr c e1) <| sep () <| w(lazy_expr c e2) <| sep () <| w(lazy_expr c
    e3) in
  let expr_quad ?(sep=fun () -> lps "," <| lsp ()) (e1,e2,e3,e4) =
    let w = wrap_indent in
    w(lazy_expr c e1) <| sep () <| w(lazy_expr c e2) <| sep () <| w(lazy_expr c
    e3) <| sep () <| w(lazy_expr c e4) in
  (* TODO: do comparisons also *)
  let is_apply_let e = let e1, e2 = U.decompose_apply e in
    match U.tag_of_expr e1 with
      | Var _ -> false | Lambda _ -> true | _ -> invalid_arg "bad apply input"
  in
  (* handle parentheses:
     - If a sub-element is mult or add, we wrap it.
     - If a left sub-element is 'ifthenelse' or a 'let', we wrap it.
     - For a == or !=, we also wrap sub- ==/!= *)
  let arith_paren e = match U.tag_of_expr e with
    | Mult | Add -> lazy_paren
    | _ -> id_fn in
  (* we're more sensitive for left side *)
 let paren_l e = match U.tag_of_expr e with
    | IfThenElse -> lazy_paren
    | Apply when is_apply_let e -> lazy_paren
    | _ -> id_fn in
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
    | TValue x | TFunction(_,x) -> match T.base_of x with
        | TBool -> true
        | _     -> false)
    with T.TypeError(_,_,_) -> false (* assume additive *)
  (* many instructions need to wrap the same way *)
  in let wrap e = match U.tag_of_expr expr with
    Insert | Iterate | Map | FilterMap | Flatten | Send | Delete | Update |
    Aggregate | GroupByAggregate -> wrap_hv 2 e
    | IfThenElse -> wrap_hv 0 e
    | _ -> id_fn e
  in let out = match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id -> lps id
  | Tuple -> let es = U.decompose_tuple expr in
    lazy_paren @: lps_list CutHint (lazy_expr c) es
  | Just -> let e = U.decompose_just expr in
    lps "just " <| lazy_expr c e
  | Nothing vt -> lps "nothing:" <| lsp () <| lazy_value_type c false vt
  | Empty vt ->
      lazy_collection_vt c vt [] <| lsp () <| lps ":" <| lsp () <|
        lazy_value_type c false vt
  | Singleton vt -> let e = U.decompose_singleton expr in
    lazy_collection_vt c vt @: lazy_expr c e
  | Combine ->
    let rec assemble_list c e =  (* print out in list format *)
      let (l, r) = U.decompose_combine e in
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
    let wrapl = paren_l e1 in
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
      | Singleton vt, Combine | Singleton vt, Singleton _
      | Singleton vt, Empty _ ->
        lazy_collection_vt c vt @: assemble_list c expr
      | _ -> expr_pair ~sep:(lcut() <| lps "++" <| lcut()) ~wl:wrapl (e1, e2)
    end
  | Range ct -> let st, str, num = U.decompose_range expr in
    apply_method c ~name:"range" ~col:(KH.mk_empty @: KH.wrap_tlist KH.t_int) ~args:[st;str;num]
  | Add -> let (e1, e2) = U.decompose_add expr in
    begin match U.tag_of_expr e2, expr_type_is_bool e1 with
      | Neg, false -> let e3 = U.decompose_neg e2 in
        arith_paren_pair "-" (e1, e3)
      | _, false -> arith_paren_pair "+" (e1, e2)
      | _, true -> arith_paren_pair "|" (e1, e2)
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
      | true, _ -> arith_paren_pair "&" (e1, e2)
      | false, true -> lps "-" <| lazy_expr c e2 (* just a minus *)
      | _ -> arith_paren_pair "*" (e1, e2)
    end 
  | Neg -> let e = U.decompose_neg expr in
    let sym = if expr_type_is_bool e then "!" else "-" in
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
      let arg_n = arg_num_of_arg arg in (* convert to arg_num *)
      let write_args = 
        (* check whether we need to handle curried arguments *)
        if many_args then
          let args_n = break_args arg_n in
          lazy_concat (lps |- shallow_bind_id ?in_record) args_n
        else lps @: shallow_bind_id arg_n
      in
      (* for curried arguments, we deep bind at a deeper level *)
      let depth = if many_args then 1 else 0 in
      wrap_indent (lps "\\" <| write_args <| lps " ->") <| lind () <| 
      wrap_hov 0 (deep_bind ~depth ?in_record c arg_n <| lazy_expr c e)
  | Apply -> let e1, e2 = U.decompose_apply expr in
    begin match U.tag_of_expr e1 with (* can be let *)
      (* function application *)
      | Var _ -> 
          let wrap_fn = begin match U.tag_of_expr e2 with
            | Var _
            | Const _ -> id_fn
            | _       -> lazy_paren
          end in
          wrap_indent (lazy_expr c e1 <| lsp () <| wrap_fn @: lazy_expr c e2)
      (* let expression *)
      | Lambda arg -> let _, body = U.decompose_lambda e1 in
        begin match arg with
        (* If we have an arg tuple, it's a bind *)
        | ATuple _ -> let arg_n = arg_num_of_arg arg in
                      deep_bind c arg_n ~top_expr:e2 <| lazy_expr c body 
        (* Otherwise it's a let *)
        | _        -> 
            wrap_hov 2 (lps "let " <| lazy_arg c false arg <|
              lps " =" <| lsp () <| lazy_expr c e2 <| lsp () ) <| lps "in"
              <| lsp () <| lazy_expr c body
        end
      | _ -> error () (* type error *)
    end
  | Block -> let es = U.decompose_block expr in
    lps "(" <| lind () <| 
    wrap_hv 0 (lps_list ~sep:";" CutHint (lazy_expr c) es <| lsp ()) 
      <| lps ")"
  | Iterate -> let lambda, col = U.decompose_iterate expr in
    apply_method c ~name:"iterate" ~col ~args:[lambda]
  | IfThenElse -> let (e1, e2, e3) = U.decompose_ifthenelse expr in
    wrap_indent (lps "if " <| lazy_expr c e1) <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3)
  | Map -> let lambda, col = U.decompose_map expr in
    apply_method c ~name:"map" ~col ~args:[lambda] ~in_record:true
  | FilterMap -> let lf, lm, col = U.decompose_filter_map expr in
    apply_method c ~name:"filter" ~col ~args:[lf] ~in_record:true <|
      apply_method_nocol c ~name:"map" ~args:[lm] ~in_record:true
  (* flatten(map(...)) becomes ext(...) *)
  | Flatten -> let e = U.decompose_flatten expr in
    begin match U.tag_of_expr e with
    | Map -> 
        let lambda, col = U.decompose_map e in
        apply_method c ~name:"ext" ~col ~args:[lambda]
    | _   -> failwith "Unhandled Flatten without map"
    end
  | Aggregate -> let lambda, acc, col = U.decompose_aggregate expr in
    apply_method c ~name:"fold" ~col ~args:[lambda; acc] ~many_args:[true;false]
      ~in_record:true
  | GroupByAggregate -> let lam1, lam2, zero, col = U.decompose_gbagg expr in
    apply_method c ~name:"groupby" ~col ~args:[lam1; lam2; zero] ~many_args:[false;true;false]
      ~in_record:true
  | Sort -> let lambda, col = U.decompose_sort expr in
    apply_method c ~name:"sort" ~col ~args:[lambda] ~in_record:true
  | Peek -> let col = U.decompose_peek expr in
    apply_method c ~name:"peek" ~col ~args:[]
  | Slice -> let col, pat = U.decompose_slice expr in
    lazy_expr c col <| lazy_bracket @: lazy_expr c pat
  | Insert -> let col, x = U.decompose_insert expr in
    apply_method c ~name:"insert" ~col ~args:[x]
  | Delete -> let col, x = U.decompose_delete expr in
    apply_method c ~name:"delete" ~col ~args:[x]
  | Update -> let col, oldx, newx = U.decompose_update expr in
    apply_method c ~name:"update" ~col ~args:[oldx;newx]
  | Assign -> let (l, r) = U.decompose_assign expr in
    lazy_expr c l <| lps " <- " <| lazy_expr c r
  | Deref -> let e = U.decompose_deref expr in
    lps "!" <| lazy_expr c e
  | Send -> let target, addr, args = U.decompose_send expr in
    wrap_indent @: lazy_paren (expr_pair (target, addr)) <| lps "<- " <| 
      lps_list CutHint (lazy_expr c) args 
  in
  (* if we asked to highlight a uuid, do so now *)
  match c.uuid with 
  | Some i when i = U.id_of_expr expr -> lps "%" <| wrap out
  | _ -> wrap out

let lazy_trigger c id arg vars expr = 
  let is_block expr = match U.tag_of_expr expr with Block -> true | _ -> false in
  let indent f = if is_block expr 
               then lps " " <| f
               else lind () <| lbox (lhov 0) f in
  lps ("trigger "^id) <| lps " : " <| 
  lazy_value_type ~in_col:false c @: U.value_type_of_arg arg <| lsp () <|
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
  | Foreign(id, t) -> lps ("foreign "^id^" :") <| lsp () <| lazy_type c t
  in
  wrap_hv 0 out <| lcut ()

let wrap_f = wrap_formatter ~margin:80

(* print a K3 type in syntax *)
let string_of_base_type t = wrap_f @: fun () -> 
  force_list @: lazy_base_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_value_type t = wrap_f @: fun () ->
  force_list @: lazy_value_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_mutable_type t = wrap_f @: fun () ->
  force_list @: lazy_mutable_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_type t = wrap_f @: fun () ->
  force_list @: lazy_type verbose_types_config t

(* print a K3 expression in syntax *)
let string_of_expr ?uuid_highlight ?(lambda_ret=false) e = 
  let config = {default_config with lambda_ret} in
  let config = match uuid_highlight with 
    | None   -> config
    | _      -> {config with uuid=uuid_highlight}
  in
  wrap_f @: fun () -> force_list @: lazy_expr config e

(* print a K3 program in syntax *)
let string_of_program ?uuid_highlight ?(lambda_ret=false) prog = 
  let config = {default_config with lambda_ret} in
  let config = match uuid_highlight with 
    | None -> config
    | _    -> {config with uuid=uuid_highlight}
  in
  wrap_f @: fun () -> 
    let l = lps_list ~sep:"" CutHint (lazy_declaration config |- fst) prog in
    obx 0;  (* vertical box *)
    force_list l;
    cb

(* print a k3 program with test expressions *)
let string_of_program_test ?uuid_highlight ?lambda_ret ptest = 
  (* print a check_expr *)
  let string_of_check_expr = function
      | FileExpr s -> "file "^s
      | InlineExpr e -> string_of_expr ?uuid_highlight ?lambda_ret e
  in
  (* print a test expression *)
  let string_of_test_expr (e, check_e) =
    Printf.sprintf "(%s) = %s"
      (string_of_expr ?uuid_highlight ?lambda_ret e) @:
      string_of_check_expr check_e
  in
  match ptest with
  | NetworkTest(p, checklist) -> 
      Printf.sprintf "%s\n\nnetwork expected\n\n%s"
        (string_of_program ?uuid_highlight ?lambda_ret p)
        (String.concat ",\n\n" @: list_map string_of_test_expr checklist)
  | ProgTest(p, checklist) -> 
      Printf.sprintf "%s\n\nexpected\n\n%s"
        (string_of_program ?uuid_highlight ?lambda_ret p)
        (String.concat ",\n\n" @: list_map string_of_test_expr checklist)
  | ExprTest _ -> failwith "can't print an expression test"

