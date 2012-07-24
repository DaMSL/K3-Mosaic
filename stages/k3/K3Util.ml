open Util
open Format
open Lazy
open ListAsSet
open Tree
open K3

(* TODO: AST constructors *)


(* AST accessors *)
let id_of_expr (e:'a expr_t) = fst (fst_data e) 
let tag_of_expr (e:'a expr_t) = snd (fst_data e)
let meta_of_expr (e:'a expr_t) = snd_data e

(* Variable id extraction *)
let vars_of_arg =
  function | AVar(v,_) -> [v] | ATuple(vt_l) -> List.map fst vt_l
  
let typed_vars_of_arg =
  function | AVar(v,t) -> [v,t] | ATuple(vt_l) -> vt_l

let id_of_var e = match tag_of_expr e with
  | Var id -> id | _ -> failwith "invalid variable"

(* Predicates *)
let is_const e = match tag_of_expr e with | Const _ -> true | _ -> false
let is_var e = match tag_of_expr e with | Var _ -> true | _ -> false
let is_var_match id e = is_var e && (id_of_var e) = id

(* Bindings *)
let lambda_bindings f tag = match tag with | Lambda x -> f x | _ -> []

let arg_of_lambda e =
  match lambda_bindings (fun x -> [x]) (tag_of_expr e) with
    | [] -> None
    | [x] -> Some(x)
    | _ -> failwith "invalid lambda arg"

let vars_of_lambda e = lambda_bindings vars_of_arg (tag_of_expr e)

let typed_vars_of_lambda e =
  lambda_bindings typed_vars_of_arg (tag_of_expr e)


(*************************
 * Stringification
 **************************)
let quote s = "\""^s^"\""

let string_opt string_f a = match a with
  | Some b -> [string_f b]
  | None -> []

let tag_str ?(extra="") t ch_t =
  t ^ "("
    ^(if extra = "" then "" else extra ^ ", ")
    ^ String.concat ", " ch_t
    ^ ")"

let string_of_container_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"
    | TMap  -> "TMap"

(* Flat stringification *)
let rec flat_string_of_base_type t = match t with
    | TUnknown  -> "TUnknown"
    | TUnit     -> "TUnit"
    | TBool     -> "TBool"
    | TByte     -> "TByte"
    | TInt      -> "TInt"
    | TFloat    -> "TFloat"
    | TString   -> "TString"

    | TMaybe(t) -> tag_str "TMaybe" [flat_string_of_value_type t]

    | TTuple(t_l) -> tag_str "TTuple" (List.map flat_string_of_value_type t_l)

    | TCollection(t_c, t_e) ->
        tag_str "TCollection"
          [string_of_container_type t_c; flat_string_of_value_type t_e]

    | TAddress -> "TAddress"

    | TTarget(t) ->
        tag_str "TTarget" [flat_string_of_base_type t]

and flat_string_of_mutable_type mt = match mt with
    | TMutable(bt) -> "TMutable("^flat_string_of_base_type bt^")"
    | TImmutable(bt) -> "TImmutable("^flat_string_of_base_type bt^")"

and flat_string_of_value_type vt = match vt with
    | TIsolated(mt) -> "TIsolated("^flat_string_of_mutable_type mt^")"
    | TContained(mt) -> "TContained("^flat_string_of_mutable_type mt^")"

and flat_string_of_type t = match t with
    | TFunction(t_a, t_r) ->
        tag_str "TFunction"
          [flat_string_of_value_type t_a; flat_string_of_value_type t_r]
    
    | TValue(t) -> flat_string_of_value_type t

let string_of_const c = match c with
    | CUnit        -> "CUnit"
    | CUnknown     -> "CUnknown"
    | CBool(b)     -> "CBool("^string_of_bool(b)^")"
    | CInt(i)      -> "CInt("^string_of_int(i)^")"
    | CFloat(f)    -> "CFloat("^string_of_float(f)^")"
    | CString(s)   -> "CString(\""^s^"\")"
    | CAddress(ip,p) -> "CAddress("^ip^":"^string_of_int p^")"
    | CTarget(id)  -> "CTarget("^id^")"
    | CNothing     -> "CNothing"

let string_of_stop_behavior_t s = match s with
    | UntilCurrent -> "UntilCurrent"
    | UntilEmpty -> "UntilEmpty"
    | UntilEOF -> "UntilEOF"

let flat_string_of_arg a = match a with
    | AVar(i, t) -> tag_str "AVar" [i; flat_string_of_type (TValue t)]
    | ATuple(its) ->
        tag_str "ATuple"
          (List.map (fun (i, t) -> i^": "^flat_string_of_type(TValue t)) its)

let flat_string_of_expr_tag tag children =
  let my_tag ?(extra="") t = tag_str ~extra:extra t children
  in match tag with
    | Const(c)  -> tag_str "Const" [string_of_const c]
    | Var(i)    -> tag_str "Var" [i]
    | Tuple     -> my_tag "Tuple"

    | Just      -> my_tag "Just"

    | Empty t     -> my_tag "Empty" ~extra:(flat_string_of_value_type t)
    | Singleton t -> my_tag "Singleton" ~extra:(flat_string_of_value_type t)
    | Combine     -> my_tag "Combine"
    | Range(ct)   -> my_tag "Range" ~extra:(string_of_container_type ct)

    | Add   -> my_tag "Add"
    | Mult  -> my_tag "Mult"
    | Neg   -> my_tag "Neg"
    | Eq    -> my_tag "Eq"
    | Neq   -> my_tag "Neq"
    | Lt    -> my_tag "Lt"
    | Leq   -> my_tag "Leq"

    | Lambda a -> my_tag "Lambda" ~extra:(flat_string_of_arg a)
    | Apply    -> my_tag "Apply"

    | Block      -> my_tag "Block"
    | IfThenElse -> my_tag "IfThenElse"

    | Map              -> my_tag "Map"
    | Iterate          -> my_tag "Iterate"
    | FilterMap        -> my_tag "FilterMap"
    | Flatten          -> my_tag "Flatten"
    | Aggregate        -> my_tag "Aggregate"
    | GroupByAggregate -> my_tag "GroupByAggregate"
    | Sort             -> my_tag "Sort"
    
    | Slice      -> my_tag "Slice"
    | Insert     -> my_tag "Insert"
    | Update     -> my_tag "Update"
    | Delete     -> my_tag "Delete"
    | Peek       -> my_tag "Peek"

    | Assign     -> my_tag "Assign"
    | Deref      -> my_tag "Deref"

    | Send       -> my_tag "Send"

(* TODO: Why can't this function be point-free? *)
let flat_string_of_expr expr =
  string_of_tree (fun ((id, tag), _) -> flat_string_of_expr_tag tag) expr

let rec flat_string_of_consumable c =
  let rcr_list l = List.map flat_string_of_consumable l in
  match c with
    | Source(i, t) -> tag_str "Source" [i; flat_string_of_type t]
    | Loop(id, c)  -> tag_str "Loop" [id; flat_string_of_consumable c]
    | Choice(cs)   -> tag_str "Choice" (rcr_list cs)
    | Sequence(cs) -> tag_str "Sequence" (rcr_list cs)

    | Optional(c) -> tag_str "Optional" [flat_string_of_consumable c]
    | Repeat(c, s) ->
        tag_str "Repeat" 
          [flat_string_of_consumable c; string_of_stop_behavior_t s]

let flat_string_of_declaration d =
  let string_of_id_and_vtype (id,vt) =
    "("^id^", "^flat_string_of_value_type vt^")"
  in
  match d with
    | Global(i, t, init) ->
      tag_str "Global"
        ([i; flat_string_of_type t]@(string_opt flat_string_of_expr init))
      
    | Foreign(i, t) -> tag_str "Foreign" [i; flat_string_of_type t]

    | Trigger(i, arg, ds, e) ->
      let trig_decls = "["^
        (String.concat ", " (List.map string_of_id_and_vtype ds))^"]"
      in
      tag_str "Trigger"
        [i; flat_string_of_arg arg; trig_decls; flat_string_of_expr e]

    | Bind(i, i2) -> tag_str "Bind" [i; i2]
    | Consumable(c) -> tag_str "Consumable" [flat_string_of_consumable c]

let flat_string_of_instruction i = match i with
    | Consume(id) -> tag_str "Consume" [id]

let flat_string_of_statement s = match s with
    | Declaration(d) -> flat_string_of_declaration d
    | Instruction(i) -> flat_string_of_instruction i

let flat_string_of_program ss =
  tag_str "K3" (List.map flat_string_of_statement ss)


(****************************
 * Pretty stringification
 *****************************)

(* Pretty printing helpers *)
type cut_type = NoCut | Hint | Line
 
let ob () = pp_open_hovbox str_formatter 2
let cb () = pp_close_box str_formatter ()
let pc () = pp_print_cut str_formatter ()
let ps s = pp_print_string str_formatter s
let psp () = pp_print_space str_formatter ()
let fnl () = pp_force_newline str_formatter ()

let cut c = match c with
  | NoCut -> ()
  | Hint -> pc ()
  | Line -> fnl ()

let ps_list ?(sep=", ") cut_t f l =
  let n = List.length l in
  ignore(List.fold_left
    (fun cnt e ->
      f e;
      (if cnt < n then ps sep);
      cut cut_t;
      cnt+1)
    1 l)

let pretty_tag_term_str t = ob(); ps t; cb()

let pretty_tag_str cut_t extra t ch_lazy_t =
  begin
    ob();
    ps (t ^ "("); cut cut_t;
    if extra = "" then () else (ps (extra ^ ", "); cut cut_t);
    ps_list cut_t force ch_lazy_t;
    ps ")";
    cb()
  end

(* Lazy variants *)
let lps s = lazy (ps s)

let lazy_string_opt string_f a = match a with
  | Some b -> [lazy (string_f b)]
  | None -> []

let print_expr_id id = ps ("<"^string_of_int id^"> ")
  
let rec lazy_base_type bt  = lazy (print_base_type bt)
and     lazy_value_type vt = lazy (print_value_type vt)
and     lazy_type t        = lazy (print_type t)
and     lazy_arg a         = lazy (print_arg a)
and     lazy_expr ?(print_id=false) e = lazy (print_expr e ~print_id:print_id)

and print_base_type t =
  let my_tag t lazy_ch_t = pretty_tag_str Hint "" t lazy_ch_t in
  let term_tag t = pretty_tag_term_str t in
  match t with
    | TUnknown  -> term_tag "TUnknown"
    | TUnit     -> term_tag "TUnit"
    | TBool     -> term_tag "TBool"
    | TByte     -> term_tag "TByte"
    | TInt      -> term_tag "TInt"
    | TFloat    -> term_tag "TFloat"
    | TString   -> term_tag "TString"

    | TMaybe(t) -> my_tag "TMaybe" [lazy_value_type t]

    | TTuple(t_l) -> my_tag "TTuple" (List.map lazy_value_type t_l)

    | TCollection(t_c, t_e) ->
        my_tag "TCollection"
          [lps (string_of_container_type t_c); lazy_value_type t_e]

    | TAddress -> term_tag "TAddress"

    | TTarget(t) ->
        my_tag "TTarget" [lazy_base_type t]

and print_mutable_type mt =
  let my_tag t bt = pretty_tag_str Hint "" t [lazy_base_type bt]
  in match mt with
    | TMutable(bt) -> my_tag "TMutable" bt
    | TImmutable(bt) -> my_tag "TImmutable" bt

and print_value_type vt =
  let my_tag t mt =
    pretty_tag_str Hint "" t [lazy (print_mutable_type mt)]
  in match vt with
    | TIsolated(mt) -> my_tag "TIsolated" mt
    | TContained(mt) -> my_tag "TContained" mt

and print_type t =
  let my_tag t lazy_ch_t = pretty_tag_str Hint "" t lazy_ch_t in
  match t with
    | TFunction(t_a, t_r) ->
        my_tag "TFunction" (List.map lazy_value_type [t_a; t_r])
    
    | TValue(t) -> my_tag "TValue" [lazy_value_type t]

and print_arg a =
  let my_tag t lazy_ch_t = pretty_tag_str Hint "" t lazy_ch_t in
  let print_id_t (id,vt) =
    lazy (ps (id^": "); print_type (TValue vt))
  in
  match a with
    | AVar(i, t)  -> my_tag "AVar" [lps i; lazy_type (TValue t)]
    | ATuple(its) -> my_tag "ATuple" (List.map print_id_t its)

and print_expr_tag tag lazy_children =
  let my_tag ?(extra="") t = pretty_tag_str Hint extra t lazy_children in
  let ch_tag cut_t t ch = pretty_tag_str cut_t "" t ch in
  let extra_tag t extra = pretty_tag_str Hint "" t (extra@lazy_children) in
  match tag with
    | Const(c)  -> ch_tag NoCut "Const" [lps (string_of_const c)]
    | Var(i)    -> ch_tag NoCut "Var" [lps i]
    | Tuple     -> my_tag "Tuple"

    | Just      -> my_tag "Just"

    | Empty t     -> extra_tag "Empty" [lazy_value_type t]
    | Singleton t -> extra_tag "Singleton" [lazy_value_type t]
    | Combine     -> my_tag "Combine"
    | Range(ct)   -> my_tag "Range" ~extra:(string_of_container_type ct)

    | Add   -> my_tag "Add"
    | Mult  -> my_tag "Mult"
    | Neg   -> my_tag "Neg"
    | Eq    -> my_tag "Eq"
    | Neq   -> my_tag "Neq"
    | Lt    -> my_tag "Lt"
    | Leq   -> my_tag "Leq"

    | Lambda a -> extra_tag "Lambda" [lazy_arg a]
    | Apply    -> my_tag "Apply"

    | Block      -> my_tag "Block"
    | IfThenElse -> my_tag "IfThenElse"

    | Map              -> my_tag "Map"
    | Iterate          -> my_tag "Iterate"
    | FilterMap        -> my_tag "FilterMap"
    | Flatten          -> my_tag "Flatten"
    | Aggregate        -> my_tag "Aggregate"
    | GroupByAggregate -> my_tag "GroupByAggregate"
    | Sort             -> my_tag "Sort"
    
    | Slice      -> my_tag "Slice"
    | Insert     -> my_tag "Insert"
    | Update     -> my_tag "Update"
    | Delete     -> my_tag "Delete"
    | Peek       -> my_tag "Peek"

    | Assign     -> my_tag "Assign"
    | Deref      -> my_tag "Deref"

    | Send       -> my_tag "Send"

and print_expr ?(print_id=false) expr =
  let id_pr e = if print_id then print_expr_id @: id_of_expr e else () in
  let lazy_e = 
    fold_tree (fun _ _ -> ())
      (fun _ lazy_ch e ->
        [lazy (id_pr e; print_expr_tag (tag_of_expr e) (List.flatten lazy_ch))])
      () [] expr
  in force (List.hd lazy_e)

let rec print_consumable c =
  let my_tag = pretty_tag_str Hint "" in
  let lazy_rcr c = lazy (print_consumable c) in 
  let rcr_list l = List.map lazy_rcr l in
  match c with
    | Source(i, t) -> my_tag "Source" [lps i; lazy_type t]
    | Loop(id, c)  -> my_tag "Loop" [lps id; lazy_rcr c]
    | Choice(cs)   -> my_tag "Choice" (rcr_list cs)
    | Sequence(cs) -> my_tag "Sequence" (rcr_list cs)

    | Optional(c) -> my_tag "Optional" [lazy_rcr c]
    | Repeat(c, s) ->
        my_tag "Repeat" [lazy_rcr c; lps (string_of_stop_behavior_t s)]

let print_declaration ?(print_id=false) d =
  let my_tag = pretty_tag_str Line "" in
  let lazy_list l = [lps "["]@l@[lps "]"] in
  let print_id_vt (id,vt) =
    lazy (ps ("("^id^", "); print_value_type vt; ps ")")
  in
  match d with
    | Global(i, t, init) ->
      my_tag "Global"
        ([lps (quote i); lazy_type t]@
        (lazy_string_opt (print_expr ~print_id:print_id) init))

    | Foreign(i, t) -> my_tag "Foreign" [lps (quote i); lazy_type t]

    | Trigger(i, arg, ds, e) ->
      let trig_decls = lazy (
        ps_list Line force (lazy_list (List.map print_id_vt ds)))
      in
      my_tag "Trigger" 
        [lps (quote i); lazy_arg arg; trig_decls; lazy_expr e ~print_id:print_id]

    | Bind(i, i2) -> my_tag "Bind" [lps i; lps i2]
    | Consumable(c) -> my_tag "Consumable" [lazy (print_consumable c)]

let print_instruction i = match i with
    | Consume(id) -> pretty_tag_str Line "" "Consume" [lps id]

let print_statement ?(print_id=false) s =
  let my_tag = pretty_tag_str Line "" in
  match s with
    | Declaration d -> 
        my_tag "Declaration" [lazy (print_declaration d ~print_id:print_id)]
    | Instruction i -> my_tag "Instruction" [lazy (print_instruction i)]

let wrap_formatter print_fn =
  pp_set_margin str_formatter 120;
  print_fn ();
  flush_str_formatter ()


let string_of_base_type bt  = wrap_formatter (fun () -> print_base_type bt)
let string_of_value_type vt = wrap_formatter (fun () -> print_value_type vt)
let string_of_type t        = wrap_formatter (fun () -> print_type t)

let string_of_arg a         = wrap_formatter (fun () -> print_arg a)
let string_of_expr e        = wrap_formatter (fun () -> print_expr e)

let string_of_consumable c  = wrap_formatter (fun () -> print_consumable c)
let string_of_declaration d = wrap_formatter (fun () -> print_declaration d)
let string_of_instruction i = wrap_formatter (fun () -> print_instruction i)
let string_of_statement s   = wrap_formatter (fun () -> print_statement s)

let string_of_program ?(print_id=false) ss =
  wrap_formatter (fun () ->
    ps "[";
    ps_list ~sep:";" Line (print_statement ~print_id:print_id) ss;
    ps "]")
  

(* AST constructors / destructors *)
let decompose_apply e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1)

let decompose_ifthenelse e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1, n 2)

let decompose_aggregate e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1, n 2)

let decompose_gbagg e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1, n 2, n 3)

let decompose_ifthenelse e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1, n 2)

(* Expression extraction *)

(* Returns all subexpressions matching a given predicate *)
let filter_expr f e =
  fold_tree
    (fun x _ -> x)
    (fun _ acc e -> (List.flatten acc)@(if f e then [e] else []))
    None [] e

(* Returns all variables in an expression *)
let vars_of_expr e = uniq (filter_expr is_var e)

(* Returns the free variables in an expression *)
let free_vars_of_expr e =
  let add_bindings env e = (vars_of_lambda e)@env in
  let not_bound_var env acc e =
    (List.flatten acc)@
    (if is_var e && not(List.mem (id_of_var e) env) then [e] else []) 
  in uniq (fold_tree add_bindings not_bound_var [] [] e)

(* Returns whether e2 is directly contained in e1 *)
let contains_expr e1 e2 =
  let contains_aux _ contained e =
    (List.exists (fun x -> x) contained) || (e = e2)
  in fold_tree (fun x _ -> x) contains_aux None false e1

(* Substitutes any occurrences of the given bindings in an expression,
 * in a bottom-up, capture-avoiding fashion.
 * Assumes substitution function domain and range are (subtree) disjoint.
 *)
let substitute_expr subs e =
  let remove_var subs e =
    let vars = vars_of_lambda e in
    if vars = [] then subs
    else
      List.fold_left (fun acc (src, dest) -> 
        if is_var src && List.mem (id_of_var src) vars then acc
        else acc@[src,dest]) [] subs
  in
  let sub_aux subs parts_w_sub_ids e =
    let parts, sub_ids =
      let x,y = List.split parts_w_sub_ids in x, List.flatten y
    in
    let new_e = recompose_tree e parts in
    if List.mem_assoc new_e subs then
      let sub_e = List.assoc new_e subs 
      in (sub_e, sub_ids@[id_of_expr new_e, id_of_expr sub_e])
    else (new_e, sub_ids)
  in fold_tree remove_var sub_aux subs (e, []) e
  
(* Linearizes (i.e. flattens) an expression tree to its constituent
 * subexpressions, in an order given by its first argument.
 * The first argument linearizes a single node and is of the form:
 *   child linearizations -> node -> linearization  *)
let linearize_expr f e = fold_tree (fun x _ -> x) (fun _ -> f) None [] e

(* Common linearizations *)
let pre_order_linearization children node = node::(List.flatten children)
let post_order_linearization children node = (List.flatten children)@[node]
