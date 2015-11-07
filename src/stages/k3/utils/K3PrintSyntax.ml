(* Module to print AST into syntax *)

open Util
open Printing
open Lazy
open K3.AST
open K3.Annotation

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
let lps_list ?(sep=", ") cut_t f l =
  [lazy (ps_list ~sep:sep cut_t (force_list |- f) l)]
let str_list l = String.concat ", " l

(* type we pass all the way down for configuring behaviors *)
type config = {
               verbose_types:bool; (* more verbose type printing *)
               uuid:int option;    (* highlight a particular uuid *)
              }

let default_config = {verbose_types=false;
                      uuid=None;
                     }

let verbose_types_config = {default_config with verbose_types=true}

(* low precedence joining of lists *)
let (<|) a b = a @ b

let lazy_wrap l r f = lps l <| f <| lps r
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
  List.flatten @@ list_intersperse (List.map f l) l2

(* separator for lazy_concat *)
let lcomma () = lps "," <| lsp ()

let error s = lps @@ "???: "^s

let lazy_annos ps =
  let lazy_anno = function | Property(_,s) -> lps s | _ -> assert false in
  let props = List.filter U.is_property ps in
  let annos = List.filter U.is_annotation ps in
  let printer sym = function
    | [] -> []
    | xs -> lps sym <| lazy_wrap "&" "&" @@ lps_list ~sep:"; " NoCut lazy_anno xs
  in
  printer "@ " props <| printer "@@ " annos

let string_of_int_set s  = String.concat ", " @@ List.map soi @@ IntSet.elements s
let string_of_int_list s = String.concat ", " @@ List.map soi s

let lazy_keyset  s = lazy_bracket @@ lps @@ string_of_int_set s
let lazy_keylist s = lazy_bracket @@ lps @@ string_of_int_list s

let lps_tag t = lps (sp "\"%s\"" t)

let rec lazy_poly_variant c l =
  lps_list ~sep:"; " CutHint (fun (i,s,t) -> lps (soi i) <| lps ", " <| lps_tag s <| lps ", " <| lazy_type c t) l

and lazy_collection ?(empty=false) c ct eval = match ct with
    | TSet    -> lps "{" <| eval <| lps "}"
    | TBag    -> lps "{|" <| eval <| lps "|}"
    | TList   -> lps "[" <| eval <| lps "]"
    | TVector -> lps "[#" <| eval <| lps "#]"
    | TMap    -> lps "[:" <| eval <| lps ":]"
    | TVMap(Some s) when not empty ->
        lps "[<" <| eval <| lps " | " <| lps (string_of_int_set_set s) <| lsp () <| lps ">]"
    | TVMap _ -> lps "[<" <| eval <| lps ">]"
    | TSortedMap -> lps "{<" <| eval <| lps ">}"
    | TSortedSet -> lps "{:" <| eval <| lps ":}"
    | TPolyQueue(false, x) -> lps "[?" <| lazy_poly_variant c x <| lsp () <| lps "?]"
    | TPolyQueue(true, x)  -> lps "{?" <| lazy_poly_variant c x <| lsp () <| lps "?}"

and lazy_base_type c ~in_col ?(no_paren=false) ?(paren_complex=false) t =
  let wrap_complex x = if paren_complex then lps "(" <| x <| lps ")" else x in
  match t with
  | TUnit      -> lps "unit"
  | TBool      -> lps "bool"
  | TByte      -> lps "byte"
  | TInt       -> lps "int"
  | TDate      -> lps "date"
  | TFloat     -> lps "float"
  | TString    -> lps "string"
  | TMaybe vt  -> wrap_complex (lps "maybe " <| lazy_type c ~in_col ~paren_complex:true vt)
  | TTuple vts ->
      (* if we're top level of a collection, drop the parentheses *)
      (* for verbose types, we leave them in. We also leave them for mutables *)
      let inner () = lps_list NoCut (lazy_type c ~in_col) vts in
      if not c.verbose_types && no_paren then inner ()
      else lps "(" <| inner () <| lps ")"
  | TCollection(ct, vt) -> lazy_collection c ct (lazy_type c ~in_col:true ~no_paren:true vt)
  | TAddress            -> lps "address" (* ? *)
  | TTarget t           -> lps "target" <| lazy_type c ~in_col t
  | TUnknown            -> lps "unknown"
  | TTop                -> lps "top"
  | TIndirect vt        -> wrap_complex (lps "ind " <| lazy_type c ~in_col ~paren_complex:true vt)
  | TFunction(its, ot)   ->
      wrap_complex @@
        lazy_concat ~sep:(fun () -> lsp () <| lps "->" <| lsp ())
        (lazy_type c ~in_col:false ~paren_complex:true) (its@[ot])
  | TAlias s            -> lps s

(* TODO: annotations *)
(* paren_complex: surround by paren if we're a complex type for clarity *)
and lazy_type ?(in_col=false) ?(no_paren=false) ?paren_complex c t =
  let mut = if t.mut then lps "mut " else [] in
  let anno e = if U.filter_prop_annos t.anno <> [] then
      lazy_paren (e <| lazy_annos t.anno)
    else e in
  anno (mut <| lazy_base_type ~in_col ~no_paren ?paren_complex c t.typ)

let rec lazy_arg c drop_tuple_paren a =
  let paren = if drop_tuple_paren then id_fn else lazy_paren in
  match a with
  | AIgnored     -> lps "_"
  | AVar (id, t) -> lps (id^":") <| lazy_type c t
  | AMaybe(arg)  -> lps "just "  <| lazy_arg c false arg
  | ATuple(args) ->
      lhov 0 <| paren (lps_list CutHint (lazy_arg c false) args) <| lcb ()

let lazy_id_type c (id, t) = lps (id^" : ") <| lazy_type c t

let lazy_trig_vars c = function
  | [] -> lps "{}"
  | vars -> lazy_box_brace
      (lps_list ~sep:", " CutHint (fun (id,t,_) -> lazy_id_type c (id,t)) vars)

let lazy_const c = function
  | CUnknown       -> lps "_"
  | CUnit          -> lps "()"
  | CBool(true)    -> lps "true"
  | CBool(false)   -> lps "false"
  | CInt(i)        -> lps @@ string_of_int i
  | CFloat(f)      -> lps @@ string_of_float f
  | CString(s)     -> lps @@ "\""^String.escaped s^"\""
  | CAddress(s, i) -> lps @@ s^":"^string_of_int i
  | CTarget(id)    -> lps id

let lazy_collection_vt ?(empty=false) c bt eval = match bt with
  | TCollection(ct, _)  -> lazy_collection ~empty c ct eval
  | TAlias _ when empty -> lps "[]"
  | _ -> error @@ K3Printing.string_of_base_type bt (* type error *)

let wrap_if_var e = match U.tag_of_expr e with
  | Var _ -> id_fn
  | _     -> lazy_paren

let rec lazy_expr c expr =
  let w = wrap_indent in
  let le = lazy_expr c in
  let sep = fun () -> lps "," <| lsp () in
  let expr_pair ?(sep=sep) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(le e1) <| sep () <| wr(le e2) in
  let expr_sub ?(sep=sep) p =
    expr_pair ~sep ~wl:wrap_indent ~wr:(wrap_hov 0) p in
  let expr_triple ?(sep=sep) (e1,e2,e3) =
    w(le e1) <| sep () <| w(le e2) <| sep () <| w(le e3) in
  let expr_quad ?(sep=sep) (e1,e2,e3,e4) =
    w(le e1) <| sep () <| w(le e2) <| sep () <| w(le e3) <| sep () <| w(le e4) in
  let expr_5 ?(sep=sep) (e1,e2,e3,e4,e5) =
    w(le e1) <| sep () <| w(le e2) <| sep () <| w(le e3) <| sep () <| w(le e4) <| sep () <| w(le e5) in
  let expr_6 ?(sep=sep) (e1,e2,e3,e4,e5,e6) =
    w(le e1) <| sep () <| w(le e2) <| sep () <| w(le e3) <| sep () <| w(le e4) <|
    sep () <| w(le e5) <| sep () <| w(le e6) in
  (* TODO: do comparisons also *)
  (* handle parentheses:
     - If a sub-element is mult or add, we wrap it.
     - If a left sub-element is 'ifthenelse' or a 'let', we wrap it.
     - For a == or !=, we also wrap sub- ==/!= *)
  let arith_paren e = match U.tag_of_expr e with
    | Mult | Add -> lazy_paren
    | _ -> id_fn in
  (* for things like Just *)
  let paren_r e = match U.tag_of_expr e with
    | Nothing _ | Just | Const _ | Tuple | Var _ | Empty _ | Singleton _ -> id_fn
    | _ -> lazy_paren in
(* we're more sensitive for left side *)
 let paren_l e = match U.tag_of_expr e with
    | Just | CaseOf _ | IfThenElse | Let _ -> lazy_paren
    | _          -> id_fn in
 let arith_paren_l e = match U.tag_of_expr e with
    | Just | CaseOf _ | IfThenElse | Let _ -> lazy_paren
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
    expr_pair ~sep:(fun () -> lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let logic_paren_pair sym (el, er) =
    let wrapl = logic_paren_l el in
    let wrapr = logic_paren er in
    expr_pair ~sep:(fun () -> lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let expr_type_is_bool e =
    try begin match (T.type_of_expr e).typ with
        | TBool -> true
        | _     -> false
        end
    with T.TypeError _ -> false (* assume additive *)
  in let tuple_no_paren c e = match U.tag_of_expr e with
    | Tuple -> let es = U.decompose_tuple e in
      lps_list CutHint (lazy_expr c) es
    | _ -> lazy_expr c e
  (* many instructions need to wrap the same way *)
  in let wrap e = match U.tag_of_expr expr with
    | Insert | InsertAt | SetAll | Extend | Iterate | Map | Filter | Flatten | Send | Delete | DeleteAt | DeleteWith | ClearAll
    | Update | UpdateSuffix | UpsertWith | UpsertWithBefore | DeletePrefix | FilterOp _
    | Aggregate | AggregateV | GroupByAggregate | Assign | Combine
    | PolyIter | PolyFold | PolyFoldTag _ | PolyIterTag _ | PolyAt _ | PolyAtWith _ | PolyInsert _ | PolyTagAt | PolyUnpack | PolyReserve | PolySkip _ -> wrap_hov 2 e
    | _ -> id_fn e
  in let out = match U.tag_of_expr expr with
  | Const con  -> lazy_const c con
  | Var id     -> lps id
  | Ignore     -> let e = U.decompose_ignore expr in
                  lps "ignore" <| lazy_paren @@ lazy_expr c e
  | Tuple      -> let es = U.decompose_tuple expr in
    lazy_paren @@ lps_list CutHint (lazy_expr c) es
  | Just       -> let e = U.decompose_just expr in
    lps "just " <| paren_r e (lazy_expr c e)
  | Nothing vt -> lps "nothing:" <| lsp () <| lazy_type c vt
  | Empty t    -> lazy_collection_vt ~empty:true c t.typ [] <| lsp () <|
                  lps ":" <| lsp () <| lazy_type c t
  | Singleton t -> let e = U.decompose_singleton expr in
    lazy_collection_vt c t.typ @@ tuple_no_paren c e
  | Combine     ->
    let rec assemble_list c e =  (* print out in list format *)
      let (l, r) = U.decompose_combine e in
      begin match U.tag_of_expr l, U.tag_of_expr r with
      | Singleton _, Combine     -> let l2 = U.decompose_singleton l in
          tuple_no_paren c l2 <| lps ";" <| lsp () <|
          assemble_list c r
      | Singleton _, Singleton _ -> let l2 = U.decompose_singleton l in
        let r2 = U.decompose_singleton r in
        tuple_no_paren c l2 <| lps ";" <| lsp () <|
          tuple_no_paren c r2
      | Singleton _, Empty _     -> let l2 = U.decompose_singleton l in
        tuple_no_paren c l2
      | _ -> error (K3Printing.string_of_expr l^", "^K3Printing.string_of_expr r) (* type error *)
      end in
    let e1, e2 = U.decompose_combine expr in
    (* wrap the left side of the combine if it's needed *)
    let wrapl = paren_l e1 in
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
    | Singleton t, Combine | Singleton t, Singleton _
    | Singleton t, Empty _ ->
      lazy_collection_vt c t.typ @@ assemble_list c expr
    | _ -> expr_pair ~sep:(fun () -> lcut() <| lps "++" <| lcut()) ~wl:wrapl (e1, e2)
    end
  | Range ct -> let t = U.decompose_range expr in
    lazy_collection c ct @@ expr_triple ~sep:(fun () -> lps "::") t
  | Add      -> let (e1, e2) = U.decompose_add expr in
    begin match U.tag_of_expr e2, expr_type_is_bool e1 with
    | Neg, false -> let e3 = U.decompose_neg e2 in
      arith_paren_pair "-" (e1, e3)
    | _, false   -> arith_paren_pair "+" (e1, e2)
    | _, true    -> arith_paren_pair "||" (e1, e2)
    end
  | Mult     -> let e1, e2 = U.decompose_mult expr in
    let is_neg = begin match U.tag_of_expr e1 with
      | Neg -> let e = U.decompose_neg e1 in
        begin match U.tag_of_expr e with
        | Const(CInt 1) -> true
        | _ -> false
        end
      | Const(CInt(-1)) -> true
      | _ -> false
    end in
    if expr_type_is_bool e1 then arith_paren_pair "&" (e1, e2)
    else if is_neg then lps "-" <| lazy_expr c e2 (* just a minus *)
    else arith_paren_pair "*" (e1, e2)

  | Neg -> let e = U.decompose_neg expr in
    let sym = if expr_type_is_bool e then "!" else "-" in
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
      wrap_indent (lazy_paren (lps "\\" <| lazy_arg c false arg <|
      lps " ->" <| lind () <|
        wrap_hov 0 (lazy_expr c e)))
  | Apply -> let fn, args = U.decompose_apply expr in
    wrap_indent
      (lazy_expr c fn <| lcut () <| lazy_paren
        (lps_list CutHint (lazy_expr c) args))
  | Block -> let es = U.decompose_block expr in
    lps "do {" <| lind () <|
    wrap_hv 0 (lps_list ~sep:";" CutHint (fun e -> wrap_hov 0 (lazy_expr c e)) es <| lsp ())
      <| lps "}"
  | Iterate -> let p = U.decompose_iterate expr in
    lps "iterate" <| lcut () <| lazy_paren @@ expr_sub p
  | IfThenElse -> let e1, e2, e3 = U.decompose_ifthenelse expr in
    wrap_indent (lps "if " <| lazy_expr c e1) <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3)
  | CaseOf x -> let e1, e2, e3 = U.decompose_caseof expr in
    wrap_indent (lps "case" <| lsp () <| lazy_expr c e1 <| lsp () <| lps "of" <| lsp () <|
    wrap_indent (lazy_brace (lps ("just "^x^" ->") <| lsp () <| lazy_expr c e2)) <| lsp () <|
    wrap_indent (lazy_brace (lps "nothing ->" <| lsp () <| lazy_expr c e3)))
  | Map -> let p = U.decompose_map expr in
    lps "map" <| lcut () <| lazy_paren @@ expr_sub p
  | Filter -> let p = U.decompose_filter expr in
    lps "filter" <| lazy_paren @@ expr_sub p
  | Flatten -> let e = U.decompose_flatten expr in
    lps "flatten" <| lcut () <| lazy_paren @@ lazy_expr c e
  | Aggregate -> let t = U.decompose_aggregate expr in
    lps "fold" <| lcut () <| lazy_paren @@ expr_triple t
  | AggregateV -> let t = U.decompose_aggregatev expr in
    lps "vfold" <| lcut () <| lazy_paren @@ expr_triple t
  | GroupByAggregate -> let q = U.decompose_gbagg expr in
    lps "groupby" <| lazy_paren @@ expr_quad q
  | Sort -> let p = U.decompose_sort expr in
    lps "sort" <| lazy_paren @@ expr_sub p
  | Equijoin -> let p = U.decompose_equijoin expr in
    lps "equijoin" <| lazy_paren @@ expr_6 p
  | Size -> let p = U.decompose_size expr in
    lps "csize" <| lazy_paren @@ lazy_expr c p
  | Subscript _ -> let i, te = U.decompose_subscript expr in
    paren_l te (lazy_expr c te) <| lps "." <| lps "[" <| lps (soi i) <| lps "]"
  | AtWith -> let col, idx, lam_none, lam_some = U.decompose_at_with expr in
    lps "at_with" <| lcut () <| lazy_paren @@ expr_quad (col, idx, lam_none, lam_some)
  | At -> let p = U.decompose_at expr in
    lps "at" <| lcut () <| lazy_paren @@ expr_pair p
  | MinWith -> let col, lam_none, lam_some = U.decompose_min_with expr in
    lps "min_with" <| lcut () <| lazy_paren @@ expr_triple (col, lam_none, lam_some)
  | Peek -> let col = U.decompose_peek expr in
    lps "peek" <| lazy_paren @@ lazy_expr c col
  | PeekWithVid -> let col, lam_none, lam_some = U.decompose_peek_with_vid expr in
    lps "peek_with_vid" <| lcut () <| lazy_paren @@ expr_triple (col, lam_none, lam_some)
  | Slice -> let col, pat = U.decompose_slice expr in
    wrap_if_var col (lazy_expr c col) <| lazy_bracket (tuple_no_paren c pat)
  | SliceOp o ->
      let lazy_op o = lps @@ match o with
        | OGt -> ">" | OGeq -> ">=" | OLt  -> "<" | OLeq -> "<=" in
      let _, col, pat = U.decompose_slice_op expr in
      wrap_if_var col (lazy_expr c col) <| lazy_bracket (lazy_op o <| tuple_no_paren c pat)
  | Insert -> let e = U.decompose_insert expr in
    lps "insert" <| lazy_paren (expr_pair e)
  | InsertAt -> let t = U.decompose_insert_at expr in
    lps "insert_at" <| lazy_paren (expr_triple t)
  | SetAll -> let t = U.decompose_set_all expr in
    lps "set_all" <| lazy_paren (expr_pair t)
  | Extend -> let e = U.decompose_extend expr in
    lps "extend" <| lazy_paren (expr_pair e)
  | UpsertWith -> let col, key, lam_no, lam_yes = U.decompose_upsert_with expr in
    lps "upsert_with" <| lazy_paren
      (lazy_expr c col <| lps " ," <| lsp () <| expr_triple (key,lam_no,lam_yes))
  | UpsertWithBefore -> let col, key, lam_no, lam_yes = U.decompose_upsert_with_before expr in
    lps "upsert_with_before" <| lazy_paren
      (lazy_expr c col <| lps " ," <| lsp () <| expr_triple (key,lam_no,lam_yes))
  | Delete -> let e = U.decompose_delete expr in
    lps "delete" <| lazy_paren (expr_pair e)
  | DeleteAt -> let p = U.decompose_delete_at expr in
    lps "delete_at" <| lazy_paren (expr_pair p)
  | DeletePrefix -> let e = U.decompose_delete_prefix expr in
    lps "delete_prefix" <| lazy_paren (expr_pair e)
  | DeleteWith -> let e = U.decompose_delete_with expr in
    lps "delete_with" <| lazy_paren (expr_quad e)
  | Pop -> let e = U.decompose_pop expr in
    lps "pop" <| lazy_paren (lazy_expr c e)
  | ClearAll -> let e = U.decompose_clear_all expr in
    lps "clear_all" <| lazy_paren (lazy_expr c e)
  | Update -> let e = U.decompose_update expr in
    lps "update" <| lazy_paren (expr_triple e)
  | UpdateSuffix -> let e = U.decompose_update_suffix expr in
    lps "update_suffix" <| lazy_paren (expr_triple e)
  | UpdateAtWith -> let e = U.decompose_update_at_with expr in
    lps "update_at_with" <| lazy_paren (expr_triple e)
  | FilterOp o ->
      let str_op o = match o with
        | OGt -> "gt" | OLt -> "lt" | OLeq -> "leq" | OGeq -> "geq" in
      let _, col, filter_val = U.decompose_filter_op expr in
    lps ("filter_"^str_op o) <|
      lazy_paren (lazy_expr c col <| lps " , " <| lazy_expr c filter_val)
  | Indirect -> let x = U.decompose_indirect expr in
    lps "ind" <| lsp () <| paren_l x @@ lazy_expr c x
  | Assign -> let l, r = U.decompose_assign expr in
    lazy_expr c l <| lps " <- " <| lazy_expr c r
  | BindAs _ -> let l, id, r = U.decompose_bind expr in
    wrap_indent (lps "bind" <| lsp () <| lazy_expr c l <| lsp () <| lps "as" <| lsp () <| lps id) <|
      lsp () <| lps "in" <| lsp () <| lazy_expr c r
  | Let _ -> let ids, bound, bexpr = U.decompose_let expr in
    let wrap_paren = match ids with [_] -> id_fn | _ -> lazy_paren in
    wrap_indent
      (lps "let" <| lsp () <| wrap_paren (lps_list NoCut lps ids) <| lps " =" <|
      lsp () <| lazy_expr c bound <| lsp ())
      <| lps "in" <| lsp () <| lcut () <| lazy_expr c bexpr
  | Send -> let e1, e2, es = U.decompose_send expr in
    wrap_indent (lps "send" <| lazy_paren (expr_pair (e1, e2) <| lps ", " <|
      lps_list CutHint (tuple_no_paren c) es))
  | PolyIter -> let p = U.decompose_poly_iter expr in
    lps "poly_iter" <| lazy_paren (expr_quad p)
  | PolyFold -> let p = U.decompose_poly_fold expr in
    lps "poly_fold" <| lazy_paren (expr_triple p)
  | PolyIterTag tag -> let _, e0, e1, e2, e3 = U.decompose_poly_iter_tag expr in
    lps "poly_iter_tag" <| lazy_paren (lps_tag tag <| lcomma () <| expr_quad (e0, e1, e2, e3))
  | PolyFoldTag tag -> let _, e0, e1, e2, e3, e4 = U.decompose_poly_fold_tag expr in
    lps "poly_fold_tag" <| lazy_paren (lps_tag tag <| lcomma () <| expr_5(e0,e1,e2,e3,e4))
  | PolyAt tag -> let _, e0, e1, e2 = U.decompose_poly_at expr in
    lps "poly_at" <| lazy_paren (lps_tag tag <| lcomma () <| expr_triple(e0,e1,e2))
  | PolyAtWith tag -> let _, e0, e1, e2, e3, e4 = U.decompose_poly_at_with expr in
    lps "poly_at_with" <| lazy_paren (lps_tag tag <| lcomma () <| expr_5(e0,e1,e2,e3,e4))
  | PolyInsert tag -> let _, e0, e1 = U.decompose_poly_insert expr in
    lps "poly_insert" <| lazy_paren (lps_tag tag <| lcomma () <| expr_pair(e0, e1))
  | PolyUnpack -> let col = U.decompose_poly_unpack expr in
    lps "poly_unpack" <| lazy_paren (lazy_expr c col)
  | PolyReserve -> let p = U.decompose_poly_reserve expr in
    lps "poly_reserve" <| lazy_paren (expr_quad p)
  | PolyTagAt -> let p = U.decompose_poly_tag_at expr in
    lps "poly_tag_at" <| lazy_paren (expr_pair p)
  | PolySkip _ -> let all, tag, e0, e1, e2 = U.decompose_poly_skip expr in
    let nm = if all then "poly_skip_all" else "poly_skip" in
    lps nm <| lazy_paren (lps_tag tag <| lcomma () <| expr_triple(e0, e1, e2))
  in
  let out = wrap out in
  (* check for annotations *)
  let pas = U.prop_annos_of_expr expr in
  let out =
    if pas <> [] then lazy_paren out <| lazy_annos pas
    else out
  in
  (* if we asked to highlight a uuid, do so now *)
  match c.uuid with
  | Some i when i = U.id_of_expr expr -> lps "%" <| out
  | _ -> out

let lazy_trigger c id arg vars expr =
  let is_block expr = match U.tag_of_expr expr with
    | Block -> true
    | _ -> false
  in
  let indent f = if is_block expr
               then lps " " <| f
               else lind () <| lbox (lhov 0) f in
  lps @@ "trigger "^id <|
  lazy_paren @@ lazy_arg c true arg <| lps " " <|
  lazy_trig_vars c vars <|
  lps " =" <| indent (lazy_expr c expr) <| lcut ()

let channel_format c = function
  (* deliberately don't implement bin *)
  | CSV -> "csv"
  | BIN -> "binary"
  | JSON -> "json"

let lazy_channel c chan_t chan_f = match chan_t with
  (* deliberately don't implement polyfile *)
  | File s -> lps @@ "sfile(\""^s^"\", "^channel_format c chan_f^")"
  | PolyFileSeq(f, i) -> lps @@ sp "spolyfileseq(%s, %s)" f i
  | Network(str, port) -> lps @@ "socket(\""^str^"\":"^string_of_int port^")"

let rec lazy_resource_pattern c = function
  | Terminal id -> lps id
  | Choice rps  -> lps_list ~sep:"| " CutHint (lazy_resource_pattern c) rps
  | Sequence rps -> lps_list ~sep:" " CutHint (lazy_resource_pattern c) rps
  | Optional rp -> lazy_resource_pattern c rp <| lps "?"
  | Repeat(rp, _) -> lazy_resource_pattern c rp <| lps "*"

let lazy_stream c = function
  | RandomStream i -> lps "srandom" <| lazy_paren (lps @@ string_of_int i)
  | ConstStream e  -> lps "stream" <| lazy_paren (lazy_expr c e)

let lazy_resource c r =
  let common t = lazy_type c t <| lps " = " in
  match r with
  | Handle(t, chan_t, chan_f) -> common t <| lazy_channel c chan_t chan_f
  | Stream(t, st) -> common t <| lazy_stream c st
  | Pattern(pat) -> lps "spattern " <| lazy_resource_pattern c pat

let lazy_flow c = function
  | Source(Code(id, arg, vars, expr))
  | Sink(Code(id, arg, vars, expr)) -> lazy_trigger c id arg vars expr
  | Source(Resource(id, r)) ->
      lps ("ssource "^id^" : ") <| lazy_resource c r
  | Sink(Resource(id, r)) ->
      lps ("ssink "^id^" : ") <| lazy_resource c r
  | BindFlow(id1, id2) -> lps @@ "bindflow "^id1^" -> "^id2
  | Instruction(Consume id) -> lps @@ "sconsume "^id

let lazy_flow_program c fas = lps_list ~sep:"" CutHint (lazy_flow c |- fst) fas

let lazy_declaration c d =
  let out = match d with
  | Global(id, t, expr) -> let end_part = begin match expr with
      | None -> []
      | Some e -> lps " =" <| lsp () <| lazy_expr c e
    end in
    wrap_hov 2 (lps @@ "declare "^id^" :" <| lsp () <| lazy_type c t <| end_part)
  | Role(id, fprog) ->
      lps ("srole "^id^" ") <| lazy_box_brace (lazy_flow_program c fprog)
  | Flow fprog -> lazy_flow_program c fprog
  | DefaultRole id -> lps ("sdefault srole "^id)
  | Foreign(id, t) -> lps ("foreign "^id^" :") <| lsp () <| lazy_type c t
  | Typedef(id, t) -> lps ("typedef "^id^" =") <| lsp () <| lazy_type c t
  in
  wrap_hv 0 out <| lcut ()

let wrap_f = wrap_formatter ~margin:80

(* print a K3 type in syntax *)
let string_of_base_type t = wrap_f @@ fun () ->
  force_list @@ lazy_base_type verbose_types_config false t

(* print a K3 type in syntax *)
let string_of_type t = wrap_f @@ fun () ->
  force_list @@ lazy_type verbose_types_config t

(* print a K3 expression in syntax *)
let string_of_expr ?uuid_highlight e =
  let config = default_config in
  let config = match uuid_highlight with
    | None   -> config
    | _      -> {config with uuid=uuid_highlight}
  in
  wrap_f @@ fun () -> force_list @@ lazy_expr config e

(* print a K3 program in syntax *)
let string_of_program ?uuid_highlight prog =
  let config = default_config in
  let config = match uuid_highlight with
    | None -> config
    | _    -> {config with uuid=uuid_highlight}
  in
  wrap_f @@ fun () ->
    let l = lps_list ~sep:"" CutHint (lazy_declaration config |- fst) prog in
    obx 0;  (* vertical box *)
    force_list l;
    cb

(* print a k3 program with test expressions *)
let string_of_program_test ?uuid_highlight ptest =
  (* print a check_expr *)
  let string_of_check_expr = function
      | FileExpr s -> "file "^s
      | InlineExpr (nm,e) -> nm^" : "^string_of_expr ?uuid_highlight e
  in
  (* print a test expression *)
  let string_of_test_expr (e, check_e) =
    sp "(%s) = %s"
      (string_of_expr ?uuid_highlight e) @@
      string_of_check_expr check_e
  in
  match ptest with
  | NetworkTest(p, checklist) ->
      sp "%s\n\nsnetwork sexpected\n\n%s"
        (string_of_program ?uuid_highlight p)
        (String.concat ",\n\n" @@ list_map string_of_test_expr checklist)
  | ProgTest(p, checklist) ->
      sp "%s\n\nsexpected\n\n%s"
        (string_of_program ?uuid_highlight p)
        (String.concat ",\n\n" @@ list_map string_of_test_expr checklist)
  | ExprTest _ -> failwith "can't print an expression test"

