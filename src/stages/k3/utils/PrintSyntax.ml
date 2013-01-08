(* Module to print AST into syntax *)

open Util
open Printing
open Lazy

module U = K3Util

type config = {dummy:int}

let lbox = lobc 2 (* default box tab *)

let lazy_id f = f
let lazy_paren f = lps "(" f; lps ")"
let lazy_bracket f = lps "["; f; lps "]"
let lazy_box_brace f = lps "{"; lbox; lpnl; f; lcb; lpnl; lps "}"

let lazy_base_type c = function
  | TUnit -> lps "()"
  | TBool -> lps "bool"
  | TByte -> lps "byte"
  | TInt -> lps "int"
  | TFloat -> lps "float"
  | TString -> lps "string"
  | TMaybe(vt) -> lps "maybe "; lazy_value_type c vt
  | TTuple(vts) -> lps "("; lps_list CutHint (lazy_value_type c) vts; lps ")"
  | TCollection(TSet, vt) -> lps "{"; lazy_value_type c vt; lps "}"
  | TCollection(TBag, vt) -> lps "{|"; lazy_value_type c vt; lps "|}"
  | TCollection(TList, vt) -> lps "["; lazy_value_type c vt; lps "]"
  | TAddress -> lps "address" (* ? *)
  | TTarget -> lps "target"

(* TODO: annotations *)
let lazy_mutable_type c = function
  | TMutable (bt, a) -> lazy_base_type c bt
  | TImmutable (bt, a) -> lazy_base_type c bt

let lazy_value_type c = function
  | TIsolated mt -> lazy_mutable_type c mt
  | TContained mt -> lazy_mutable_type c mt

let lazy_type c = function
  | TFunction(f,t) -> lazy_value_type c; lps " -> "; lazy_value_type t
  | TValue vt -> lazy_value_type vt

let rec lazy_arg c = function
  | AIgnored -> lps "_"
  | AVar (id, vt) -> lps id^":"^lazy_value_type c vt
  | AMaybe(arg) -> lps "maybe "; lazy_arg c arg
  | ATuple(args) -> lps "("; lps_list CutHint (lazy_arg c) args; lps ") "

let lazy_trig_vars c = function
  | [] -> ()
  | vars -> lps "{"; lpnl (); lbox ();
      lps_list ?sep:",\n" CutHint (lazy_id_type c) vars;
      lcb (); lpnl (); lps "} = "

let lazy_const c = function
  | CUnknown -> lps "_"
  | CUnit -> lps "()"
  | CBool(true) -> lps "true"
  | CBool(false) -> lps "false"
  | CInt(i) -> lps @: string_of_int i
  | CFloat(f) -> lps @: string_of_float f
  | CString(s) -> lps s
  | CAddress(s, i) -> lps s^":"^string_of_int i
  | CTarget(id) -> lps id
  | CNothing -> lps "nothing"

let lazy_collection c ct eval = match ct with
    | TSet -> lps "{"; eval; lps "}"
    | TBag -> lps "{|"; eval; lps "|}"
    | TList -> lps "["; eval; lps "]"

let lazy_collection_vt c vt exp = match vt with
  | TCollection(ct, _) -> lazy_collection c ct exp
  | _ -> () (* type error *)

let rec lazy_expr c expr = 
  let expr_pair ?sep=", " (e1, e2) =
    lazy_expr c e1; lps sep; lazy_expr c e2; in
  let expr_triple ?sep=", " (e1,e2,e3) = 
    lazy_expr c e1; lps sep; lazy_expr c e2; lps sep; lazy_expr c e3 in
  let expr_quad ?sep=", " (e1,e2,e3,e4) = 
    lazy_expr c e1; lps sep; lazy_expr c e2; lps sep; lazy_expr c e3; lps sep;
    lazy_expr c e4 in
  match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id -> lps "id"
  | Tuple -> let es = U.decompose_tuple expr in
    lps "("; 
    lps_list CutHint (lazy_expr c) es;
    lps ")";
  | Just -> let e = U.decompose_just expr in
    lps "just "; lazy_expr c e
  | Empty vt -> lazy_collection_vt c vt ()
  | Singleton vt -> let e = U.decompose_singleton in
    lazy_collection_vt c vt @: lazy_expr c e
  | Combine -> let p = U.decompose_combine expr in
    expr_pair ?sep:"++" p
  | Range ct -> let t = U.decompose_range expr in
    lazy_collection c ct @: expr_triple ?sep:":" t
  | Add -> let p = U.decompose_add expr in
    lazy_paren @: expr_pair ?sep:" + " p
  | Mult -> let p = U.decompose_mult expr in
    expr_pair ?sep:" * " p
  | Neg -> let e = U.decompose_neg expr in
    lps "-"; lazy_paren @: lazy_expr c e
  | Eq -> let p = U.decompose_eq expr in
    expr_pair ?sep:" == " p
  | Lt -> let p = U.decompose_lt expr in
    expr_pair ?sep:" < " p
  | Neq -> let p = U.decompose_neq expr in
    expr_pair ?sep:" != " p
  | Leq -> let p = U.decompose_leq expr in
    expr_pair ?sep:" <= " p
  | Lambda arg -> let e = U.decompose_lambda expr in
    lps "\"; lazy_arg arg; -> "; lcut CutHint; lazy_expr c e
  | Apply -> let (e1, e2) = U.decompose_apply expr in
    let modify_arg = begin match U.tag_of_expr e2 with
      | Tuple -> lazy_id
      | _ -> lazy_paren end
    in begin match U.tag_of_expr e1 with (* can be let *)
      | Var -> lazy_expr c e1; 
        modify_arg @: lazy_expr c e2
      | Lambda arg -> lps "let "; lazy_arg c arg;
        lps " = "; lazy_expr c e2; lps " in"; lpnl ();
        lazy_expr c e1 
    end
  | Block -> let es = U.decompose_block expr in
    lps "do {"; lbox (); lpnl ();
    lps_list ?sep:";" CutLine (lazy_expr c) es
    lcb (); lpnl (); lps "}"; lpnl ();
  | Iterate -> let (e, col) = U.decompose_iterate expr in
    lps "iterate"; 
    lazy_paren @: lazy_expr c e; lps ", "; lcut CutHint; 
      lazy_expr c col
  | IfThenElse -> let (e1, e2, e3) = U.decompose_ifthenelse expr in
    lps "if "; lazy_expr c e1; lpnl();
    lps "then"; lbox (); lazy_expr c e2; lcb(); lpnl ();
    lps "else "; lbox (); lazy_expr c e3; lcb(); lpnl ();
  | Map -> let p = U.decompose_map expr in
    lps "map"; lazy_paren @: expr_pair p
  | FilterMap -> let t = U.decompose_filter_map expr in
    lps "filtermap"; lazy_paren @: expr_triple t
  | Flatten -> let e = U.decompose_flatten expr in
    lps "flatten"; lazy_paren @: lazy_expr c e
  | Aggregate -> let t = U.decompose_aggregate expr in
    lps "fold"; lazy_paren @: expr_triple t
  | GroupByAggregate -> let q = U.decompose_gbagg expr in
    lps "groupby"; lazy_paren @: expr_quad q
  | Sort -> let p = U.decompose_sort expr in
    lps "sort"; lazy_paren @: expr_pair p
  | Peek -> let col = U.decompose_peek in
    lps "peek"; lazy_paren @: lazy_expr c col
  | Slice -> let (col, pat) = U.decompose_slice expr in
    lazy_expr c col; lazy_bracket @: lps_list CutHint (lazy_expr c) pat
  | Insert -> let p = U.decompose_insert in
    lps "insert"; lazy_paren @: expr_pair p
  | Delete -> let p = U.decompose_delete in
    lps "delete"; lazy_paren @: expr_pair p
  | Update -> let t = U.decompose_update in
    lps "delete"; lazy_paren @: expr_triple t
  | Assign -> let (l, r) = U.decompose_assign in
    lazy_expr c l; lps " <- "; lazy_expr c r
  | Deref -> let e = U.decompose_deref in
    lps "!"; lazy_expr c e
  | Send -> let t = U.decompose_send in
    lps "send"; lazy_paren @: expr_triple t

let lazy_trigger c id arg vars expr = 
  lps "trigger "^id;
  lazy_paren @: lazy_arg c arg;
  lazy_trig_vars c vars;
  lps " = "; lbox ();
  lazy_expr c expr;
  lcb (); lpnl (); lps "}"

let channel_format c = function
  | CSV  -> "csv"
  | JSON -> "json"

let lazy_channel c chan_t chan_f = match chan_t with
  | File s -> lps @: "file(\""^s^"\", "^channel_format c chan_f
  | Network(str, port) -> "socket(\""^str^"\":"^string_of_int port^")"

let rec lazy_resource_pattern c = function
  | Terminal id -> lps id 
  | Choice rps  -> lps_list ?sep:"| " CutHint (lazy_resource_pattern c) rps
  | Sequence rps -> lps_list ?sep:" " CutHint (lazy_resource_pattern c) rps
  | Optional rp -> lazy_resource_pattern c rp; lps "?"
  | Repeat(rp, _) -> lazy_resource_pattern c rp; lps "*"

let lazy_resource c = function
  | Handle(t, chan_t, chan_f) -> 
    lazy_type c t; lps " = "; lazy_channel chan_t chan_f; lpnl;
  | Pattern(pat) -> lps "pattern "; lazy_resource_pattern c pat

let lazy_flow c = function
  | Source(Code(id, arg, vars, expr)) -> lazy_trigger c id arg vars expr
  | Source(Resource(id, r)) -> 
      lps "source "^id^" : ";
      lazy_resource c r
  | Sink(Resource(id, r)) ->
      lps "sink "^id^" : ";
      lazy_resource c r
  | Bind(id1, id2) -> lps "bind "^id1^" -> "^id2
  | Instruction(Consume id) -> lps "consume "^id
  | _ -> ()

let lazy_flow_program c fas = lps_list ?sep:"" CutLine (lazy_flow c |- fst) fas

let lazy_declaration c = function
  | Global(id, t, expr) -> lps "declare "^id^" : "; lazy_type c t;
    begin match expr with 
      | None -> ()
      | Some e -> lps " = "; lazy_expr e
    end
  | Role(id, fprog) -> 
      lps "role "^id^" "; lazy_box_brace @: lazy_flow_program c fprog
  | Flow fprog -> lazy_flow_program fprog

(* print a K3 program in syntax *)
let print_program prog = 
  let c = config {dummy=0} in
  let l = lps_list ?sep:"" CutLine (lazy_declaration c |- fst) prog in
  force l

