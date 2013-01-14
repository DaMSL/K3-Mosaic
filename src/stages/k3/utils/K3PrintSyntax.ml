(* Module to print AST into syntax *)

open Util
open Printing
open Lazy
open K3.AST

module U = K3Util
module T = K3Typechecker

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
let lps_list ?(sep=", ") cut_t f l = [lazy (ps_list ~sep:sep cut_t (force_list |- f) l)]

(* type we pass all the way down for configuring behaviors *)
type config = {dummy:int}

(* low precedence joining of lists *)
let (<|) a b = a @ b

let lazy_paren f = lps "(" <| f <| lps ")"
let lazy_bracket f = lps "[" <| f <| lps "]"
let lazy_box_brace f = 
  lps "{" <| lind () <| lv 0 <| f <| 
  lcb () <| lcut () <| lps "}"
let lbox b f = b <| f <| lcb ()
let wrap_hv i f = lbox (lhv i) f
let wrap_hov i f = lbox (lhov i) f
let wrap_indent f = wrap_hov 2 f

let error () = lps "???"

let rec lazy_base_type c = function
  | TUnit -> lps "unit"
  | TBool -> lps "bool"
  | TByte -> lps "byte"
  | TInt -> lps "int"
  | TFloat -> lps "float"
  | TString -> lps "string"
  | TMaybe(vt) -> lps "maybe " <| lazy_value_type c vt
  | TTuple(vts) -> lps "(" <| lps_list NoCut (lazy_value_type c) vts <| lps ")"
  | TCollection(TSet, vt) -> lps "{" <| lazy_value_type c vt <| lps "}"
  | TCollection(TBag, vt) -> lps "{|" <| lazy_value_type c vt <| lps "|}"
  | TCollection(TList, vt) -> lps "[" <| lazy_value_type c vt <| lps "]"
  | TAddress -> lps "socket" (* ? *)
  | TTarget bt -> lps "target" <| lazy_base_type c bt
  | TUnknown -> lps "unknown"

(* TODO: annotations *)
and lazy_mutable_type c = function
  | TMutable (bt, a) -> lazy_base_type c bt
  | TImmutable (bt, a) -> lazy_base_type c bt

and lazy_value_type c = function
  | TIsolated mt -> lazy_mutable_type c mt
  | TContained mt -> lazy_mutable_type c mt

let lazy_type c = function
  | TFunction(f,t) -> lazy_value_type c f <| lps " -> " <| lazy_value_type c t
  | TValue vt -> lazy_value_type c vt

let rec lazy_arg c drop_tuple_paren a = 
  let paren = if drop_tuple_paren then id_fn else lazy_paren in
  match a with
  | AIgnored -> lps "_"
  | AVar (id, vt) -> lps (id^":") <| lazy_value_type c vt
  | AMaybe(arg) -> lps "maybe " <| lazy_arg c false arg
  | ATuple(args) -> 
      lhov 0 <| paren (lps_list CutHint (lazy_arg c false) args) <| lcb ()

let lazy_id_type c (id,t) = lps @: id^" : " <| lazy_value_type c t

let lazy_trig_vars c = function
  | [] -> lps "{}"
  | vars -> lazy_box_brace 
      (lps_list ~sep:", " CutHint (fun (id,t,_) -> lazy_id_type c (id,t)) vars)

let lazy_const c = function
  | CUnknown -> lps "_"
  | CUnit -> lps "()"
  | CBool(true) -> lps "true"
  | CBool(false) -> lps "false"
  | CInt(i) -> lps @: string_of_int i
  | CFloat(f) -> lps @: string_of_float f
  | CString(s) -> lps s
  | CAddress(s, i) -> lps @: s^":"^string_of_int i
  | CTarget(id) -> lps id
  | CNothing -> lps "nothing"

let lazy_collection c ct eval = match ct with
    | TSet -> lps "{" <| eval <| lps "}"
    | TBag -> lps "{|" <| eval <| lps "|}"
    | TList -> lps "[" <| eval <| lps "]"

let lazy_collection_vt c vt eval = match vt with
  | TIsolated(TMutable(TCollection(ct, _),_))
  | TIsolated(TImmutable(TCollection(ct, _),_))
  | TContained(TMutable(TCollection(ct, _),_))
  | TContained(TImmutable(TCollection(ct, _),_)) -> lazy_collection c ct eval
  | _ -> error () (* type error *)


let rec lazy_expr c expr = 
  let expr_pair ?(sep=lps "," <| lsp ()) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(lazy_expr c e1) <| sep <| wr(lazy_expr c e2) in
  let expr_triple ?(sep=fun () -> lps "," <| lsp ()) (e1,e2,e3) = 
    lazy_expr c e1 <| sep () <| lazy_expr c e2 <| sep () <| lazy_expr c e3 in
  let expr_quad ?(sep=fun () -> lps "," <| lsp ()) (e1,e2,e3,e4) = 
    lazy_expr c e1 <| sep () <| lazy_expr c e2 <| sep () <| lazy_expr c e3 <|
    sep () <| lazy_expr c e4 in
  let arith_paren e = match U.tag_of_expr e with
    | Mult | Add -> lazy_paren
    | _ -> id_fn
  in let tuple_no_paren c e = match U.tag_of_expr e with
    | Tuple -> let es = U.decompose_tuple e in
      lps_list CutHint (lazy_expr c) es
    | _ -> lazy_expr c e
  (* many instructions need to wrap the same way *)
  in let wrap e = match U.tag_of_expr expr with
    Insert | Iterate | Map | FilterMap | Flatten | Send | Delete | Update |
    Aggregate | GroupByAggregate -> wrap_indent e
    | _ -> id_fn e
  in let out = match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id -> lps id
  | Tuple -> let es = U.decompose_tuple expr in
    lazy_paren @: lps_list CutHint (lazy_expr c) es 
  | Just -> let e = U.decompose_just expr in
    lps "just " <| lazy_expr c e
  | Empty vt -> lazy_collection_vt c vt [] 
  | Singleton vt -> let e = U.decompose_singleton expr in
    lazy_collection_vt c vt @: lazy_expr c e
  | Combine -> 
    let rec assemble_list c e =  (* print out in list format *)
      let (l, r) = U.decompose_combine e in
      begin match U.tag_of_expr l, U.tag_of_expr r with
        | Singleton _, Combine -> let l2 = U.decompose_singleton l in
            lazy_expr c l2 <| lps ";" <| lsp () <| assemble_list c r
        | Singleton _, Singleton _ -> let l2 = U.decompose_singleton l in
          let r2 = U.decompose_singleton r in
          expr_pair ~sep:(lps ";" <| lsp ()) (l2, r2)
        | _ -> error () (* type error *)
      end in
    let (e1, e2) = U.decompose_combine expr in
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
      | Singleton vt, Combine 
      | Singleton vt, Singleton _ -> 
        lazy_collection_vt c vt @: assemble_list c expr
      | _ -> expr_pair ~sep:(lcut() <| lps "++" <| lcut()) (e1, e2) 
    end
  | Range ct -> let t = U.decompose_range expr in
    lazy_collection c ct @: expr_triple ~sep:(fun () -> lps ":") t
  | Add -> let (e1, e2) = U.decompose_add expr in
    let wrapl = arith_paren e1 in
    begin match U.tag_of_expr e2 with
      | Neg -> let e3 = U.decompose_neg e2 in
        let wrapr = arith_paren e3 in
        expr_pair ~sep:(lsp () <| lps "-" <| lsp ()) ~wl:wrapl ~wr:wrapr 
          (e1, e3)
      | _   -> let wrapr = arith_paren e2 in
        expr_pair ~sep:(lsp () <| lps "+" <| lsp ()) ~wl:wrapl ~wr:wrapr 
          (e1, e2)
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
    if is_neg then
      lps "-" <| lazy_expr c e2
    else
      let wrapl = arith_paren e1 in
      let wrapr = arith_paren e2 in
      expr_pair ~sep:(lsp () <| lps "*" <| lsp ()) ~wl:wrapl ~wr:wrapr (e1,e2)
  | Neg -> let e = U.decompose_neg expr in
    let sym = begin match T.type_of_expr e with
      | TValue x -> begin match T.base_of x with
        | TBool -> "~"
        | _ -> "-"
      end
      | _ -> "-"
    end in
    begin match U.tag_of_expr e with
      | Var _ 
      | Const _ -> lps sym <| lazy_expr c e
      | Lt -> let p = U.decompose_lt e in
        expr_pair ~sep:(lps " >= ") p
      | Leq -> let p = U.decompose_leq e in
        expr_pair ~sep:(lps " > ") p
      | _ -> lps sym <| lazy_paren @: lazy_expr c e
    end
  | Eq -> let p = U.decompose_eq expr in
    expr_pair ~sep:(lsp () <| lps "==" <| lsp ()) p
  | Lt -> let p = U.decompose_lt expr in
    expr_pair ~sep:(lsp () <| lps "<" <| lsp ()) p
  | Neq -> let p = U.decompose_neq expr in
    expr_pair ~sep:(lsp () <| lps "!=" <| lsp ()) p
  | Leq -> let p = U.decompose_leq expr in
    expr_pair ~sep:(lsp () <| lps "<=" <| lsp ()) p
  | Lambda arg -> let e = U.decompose_lambda expr in
    wrap_indent (lps "\\" <| lazy_arg c false arg <| lps " ->") <| lsp () <| 
    wrap_indent (lazy_expr c e)
  | Apply -> let (e1, e2) = U.decompose_apply expr in
    let modify_arg = begin match U.tag_of_expr e2 with
      | Tuple -> id_fn
      | _ -> lazy_paren end
    in begin match U.tag_of_expr e1 with (* can be let *)
      | Var _ -> wrap_indent (lazy_expr c e1 <|
          modify_arg @: lazy_expr c e2)
      | Lambda arg -> let body = U.decompose_lambda e1 in
        wrap_hov 2 (lps "let " <| lazy_arg c false arg <|
          lps " =" <| lsp () <| lazy_expr c e2 <| lsp () ) <| lps "in"
          <| lsp () <| lazy_expr c body
      | _ -> error () (* type error *)
    end
  | Block -> let es = U.decompose_block expr in
    lps "do {" <| lind () <| 
    wrap_hv 0 (lps_list ~sep:";" CutLine (lazy_expr c) es <| lsp ()) 
      <| lps "}"
  | Iterate -> let (e, col) = U.decompose_iterate expr in
    lps "iterate" <|
      lazy_paren (lazy_expr c e <| lps ", " <| lcut () <| lazy_expr c col)
  | IfThenElse -> let (e1, e2, e3) = U.decompose_ifthenelse expr in
    lhov 0 <| 
    lps "if " <| lazy_expr c e1 <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3) <| lcut () <|
    lcb ()
  | Map -> let p = U.decompose_map expr in
    lps "map" <| lazy_paren @: expr_pair p
  | FilterMap -> let t = U.decompose_filter_map expr in
    lps "filtermap" <| lazy_paren @: expr_triple t
  | Flatten -> let e = U.decompose_flatten expr in
    lps "flatten" <| lcut () <| lazy_paren @: lazy_expr c e
  | Aggregate -> let t = U.decompose_aggregate expr in
    lps "fold" <| lazy_paren @: expr_triple t
  | GroupByAggregate -> let q = U.decompose_gbagg expr in
    lps "groupby" <| lcut () <| lazy_paren @: expr_quad q
  | Sort -> let p = U.decompose_sort expr in
    lps "sort" <| lazy_paren @: expr_pair p
  | Peek -> let col = U.decompose_peek expr in
    lps "peek" <| lazy_paren @: lazy_expr c col
  | Slice -> let (col, pat) = U.decompose_slice expr in
    lazy_expr c col <| lazy_bracket @: lazy_expr c pat
  | Insert -> let (e1, e2) = U.decompose_insert expr in
    lps "insert" <| lazy_paren  
      (lazy_expr c e1 <| lps "," <| lsp () <| tuple_no_paren c e2)
  | Delete -> let p = U.decompose_delete expr in
    lps "delete" <| lazy_paren @: expr_pair p
  | Update -> let t = U.decompose_update expr in
    lps "update" <| lazy_paren @: expr_triple t
  | Assign -> let (l, r) = U.decompose_assign expr in
    lazy_expr c l <| lps " <- " <| lazy_expr c r
  | Deref -> let e = U.decompose_deref expr in
    lps "!" <| lazy_expr c e
  | Send -> let (e1, e2, es) = U.decompose_send expr in
    lps "send" <| lazy_paren (expr_pair (e1, e2) <| lps ", " <| 
      lps_list CutHint (tuple_no_paren c) es)
  in
  wrap out

let lazy_trigger c id arg vars expr = 
  let is_block expr = match U.tag_of_expr expr with
    | Block -> true
    | _ -> false
  in
  let indent f = if is_block expr 
               then lps " " <| f
               else lind () <| lbox (lhov 0) f in
  lps @: "trigger "^id <|
  lazy_paren @: lazy_arg c true arg <| lps " " <|
  lazy_trig_vars c vars <|
  lps " =" <| indent (lazy_expr c expr) <| lcut ()

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

let lazy_resource c = function
  | Handle(t, chan_t, chan_f) -> 
    lazy_type c t <| lps " = " <| lazy_channel c chan_t chan_f
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

(* print a K3 program in syntax *)
let string_of_program prog = 
  let c = {dummy=0} in
  let print_fn () =
    let l = lps_list ~sep:"" CutHint (lazy_declaration c |- fst) prog in
    obx 0;  (* vertical box *)
    force_list l;
    cb
  in wrap_formatter ~margin:80 (fun () -> print_fn ())

