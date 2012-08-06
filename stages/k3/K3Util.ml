open Format
open Lazy
open ListAsSet
open Util
open Printing
open Tree
open K3


(* Extra type definitions for the parser *)
type 'a expression_test = 'a program_t * 'a expr_t * 'a expr_t


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

let wrap_unless_empty lb rb s = if s = "" then s else (lb^s^rb)

let string_opt string_f a = match a with
  | Some b -> [string_f b]
  | None -> []

let tag_str ?(extra="") t ch_t =
  t ^ "("
    ^(if extra = "" then "" else extra ^ ", ")
    ^ String.concat ", " ch_t
    ^ ")"

(* Terminals *)
let string_of_address (ip,p) = ip^":"^string_of_int p

let string_of_container_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"

let string_of_const c = match c with
    | CUnit          -> "CUnit"
    | CUnknown       -> "CUnknown"
    | CBool(b)       -> "CBool("^string_of_bool(b)^")"
    | CInt(i)        -> "CInt("^string_of_int(i)^")"
    | CFloat(f)      -> "CFloat("^string_of_float(f)^")"
    | CString(s)     -> "CString(\""^s^"\")"
    | CAddress(addr) -> "CAddress("^string_of_address addr^")"
    | CTarget(id)    -> "CTarget("^id^")"
    | CNothing       -> "CNothing"

let string_of_stop_behavior_t s = match s with
    | UntilCurrent -> "UntilCurrent"
    | UntilEmpty -> "UntilEmpty"
    | UntilEOF -> "UntilEOF"

let string_of_tag_type tag = match tag with
    | Const(c)  -> "Const"
    | Var(i)    -> "Var"
    | Tuple     -> "Tuple"
    | Just      -> "Just"

    | Empty t     -> "Empty"
    | Singleton t -> "Singleton"
    | Combine     -> "Combine"
    | Range(ct)   -> "Range"

    | Add   -> "Add"
    | Mult  -> "Mult"
    | Neg   -> "Neg"
    | Eq    -> "Eq"
    | Neq   -> "Neq"
    | Lt    -> "Lt"
    | Leq   -> "Leq"

    | Lambda a -> "Lambda"
    | Apply    -> "Apply"

    | Block      -> "Block"
    | IfThenElse -> "IfThenElse"

    | Map              -> "Map"
    | Iterate          -> "Iterate"
    | FilterMap        -> "FilterMap"
    | Flatten          -> "Flatten"
    | Aggregate        -> "Aggregate"
    | GroupByAggregate -> "GroupByAggregate"
    | Sort             -> "Sort"
    
    | Slice   -> "Slice"
    | Insert  -> "Insert"
    | Update  -> "Update"
    | Delete  -> "Delete"
    | Peek    -> "Peek"

    | Assign  -> "Assign"
    | Deref   -> "Deref"

    | Send    -> "Send"


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
    | TTarget(t) -> tag_str "TTarget" [flat_string_of_base_type t]

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
  flat_string_of_tree (fun ((id, tag), _) -> flat_string_of_expr_tag tag) expr

let rec flat_string_of_stream_pattern p =
  let rcr = flat_string_of_stream_pattern in
  let rcr_list l = List.map rcr l in
  match p with
    | Terminal(id)  -> tag_str "Terminal" [id]
    | Choice(ps)    -> tag_str "Choice" (rcr_list ps)
    | Sequence(ps)  -> tag_str "Sequence" (rcr_list ps)
    | Optional(p)   -> tag_str "Optional" [rcr p]
    | Repeat(p, s) -> tag_str "Repeat" [rcr p; string_of_stop_behavior_t s]

let flat_string_of_stream s =
  match s with
    | Source(i, t, _)    -> tag_str "Source" [i; flat_string_of_type t]
    | Sink(i, t, _)      -> tag_str "Sink" [i; flat_string_of_type t]
    | Derived(id, p)     -> tag_str "Derived" [id; flat_string_of_stream_pattern p]

let flat_string_of_instruction i = match i with
    | Consume(id) -> tag_str "Consume" [id]

let flat_string_of_stream_statement ss = match ss with
    | Stream(s)      -> tag_str "Stream" [flat_string_of_stream s]
    | Bind(i, i2)    -> tag_str "Bind" [i; i2]
    | Instruction(i) -> tag_str "Instruction" [flat_string_of_instruction i]

let flat_string_of_stream_program sp =
  "[ "^(String.concat "," (List.map flat_string_of_stream_statement sp))^" ]"

let flat_string_of_declaration d =
  let string_of_id_and_vtype (id,vt,_) =
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
        
    | Role (id, sp)    -> tag_str "Role" [id; flat_string_of_stream_program sp]
    
    | DefaultRole (id) -> tag_str "DefaultRole" [id]

let flat_string_of_program ss =
  tag_str "K3" (List.map (fun (d,_) -> flat_string_of_declaration d) ss)


(****************************
 * Pretty stringification
 *****************************)

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

and     lazy_expr ?(print_id=false) string_of_meta e =
            lazy (print_expr ~print_id:print_id string_of_meta e)

and print_base_type t =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
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

    | TTarget(t) -> my_tag "TTarget" [lazy_base_type t]

and print_mutable_type mt =
  let my_tag t bt = pretty_tag_str CutHint "" t [lazy_base_type bt]
  in match mt with
    | TMutable(bt) -> my_tag "TMutable" bt
    | TImmutable(bt) -> my_tag "TImmutable" bt

and print_value_type vt =
  let my_tag t mt =
    pretty_tag_str CutHint "" t [lazy (print_mutable_type mt)]
  in match vt with
    | TIsolated(mt) -> my_tag "TIsolated" mt
    | TContained(mt) -> my_tag "TContained" mt

and print_type t =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
  match t with
    | TFunction(t_a, t_r) ->
        my_tag "TFunction" (List.map lazy_value_type [t_a; t_r])
    
    | TValue(t) -> my_tag "TValue" [lazy_value_type t]

and print_arg a =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
  let print_id_t (id,vt) =
    lazy (ps (id^": "); print_type (TValue vt))
  in
  match a with
    | AVar(i, t)  -> my_tag "AVar" [lps i; lazy_type (TValue t)]
    | ATuple(its) -> my_tag "ATuple" (List.map print_id_t its)

and print_expr_tag tag lazy_children =
  let my_tag ?(lb="(") ?(rb=")") ?(sep=", ") ?(extra="") t =
    pretty_tag_str ~lb:lb ~rb:rb ~sep:sep CutHint extra t lazy_children
  in
  let my_tag_list = my_tag ~lb:"([" ~rb:"])" ~sep:"; " in
  let ch_tag cut_t t ch = pretty_tag_str cut_t "" t ch in
  let extra_tag t extra = pretty_tag_str CutHint "" t (extra@lazy_children) in
  match tag with
    | Const(c)  -> ch_tag NoCut "Const" [lps (string_of_const c)]
    | Var(i)    -> ch_tag NoCut "Var" [lps i]
    | Tuple     -> my_tag_list "Tuple"

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

    | Block      -> my_tag_list "Block"
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

and print_expr ?(print_id=false) string_of_meta expr =
  let id_pr e = if print_id then print_expr_id @: id_of_expr e else () in
  let print_meta e =  
    let m_str = wrap_unless_empty "<" ">" (string_of_meta (meta_of_expr e))
    in pc(); ps m_str; pc()
  in
  let print lazy_ch e =
    id_pr e;
    print_expr_tag (tag_of_expr e) (List.flatten lazy_ch);
    print_meta e
  in
  let lazy_e = 
    fold_tree (fun _ _ -> ()) (fun _ lazy_ch e -> [lazy (print lazy_ch e)]) () [] expr
  in force (List.hd lazy_e)

let rec print_stream_pattern p =
  let my_tag = pretty_tag_str CutHint "" in
  let lazy_rcr p = lazy (print_stream_pattern p) in 
  let rcr_list l = List.map lazy_rcr l in
  match p with
    | Terminal(id)  -> my_tag "Terminal" [lps id]
    | Choice(ps)    -> my_tag "Choice" (rcr_list ps)
    | Sequence(ps)  -> my_tag "Sequence" (rcr_list ps)
    | Optional(p)   -> my_tag "Optional" [lazy_rcr p]
    | Repeat(p, s)  -> my_tag "Repeat" [lazy_rcr p; lps (string_of_stop_behavior_t s)]

let print_stream s =
  let my_tag = pretty_tag_str CutHint "" in
  match s with
    | Source(i, t, _) -> my_tag "Source" [lps i; lazy_type t]
    | Sink(i, t, _)   -> my_tag "Sink" [lps i; lazy_type t]
    | Derived(id, p)  -> my_tag "Derived" [lps id; lazy (print_stream_pattern p)]

let print_instruction i = match i with
    | Consume(id) -> pretty_tag_str CutLine "" "Consume" [lps id]

let print_stream_statement ss =
  let my_tag = pretty_tag_str CutLine "" in
  match ss with
    | Stream(s) -> my_tag "Stream" [lazy (print_stream s)]
    | Bind(i, i2)   -> my_tag "Bind" [lps i; lps i2]
    | Instruction i -> my_tag "Instruction" [lazy (print_instruction i)]
 
let print_stream_program sp = List.iter print_stream_statement sp

let print_declaration ?(print_id=false) ?(print_expr_fn=lazy_expr) string_of_meta d =
  let my_tag = pretty_tag_str CutLine "" in
  let print_id_vt (id,vt,_) =
    lazy (ps ("("^id^", "); print_value_type vt; ps ")")
  in
  match d with
    | Global(i, t, init) ->
      my_tag "Global"
        ([lps (quote i); lazy_type t]@
        (lazy_string_opt (print_expr ~print_id:print_id string_of_meta) init))

    | Foreign(i, t) -> my_tag "Foreign" [lps (quote i); lazy_type t]

    | Trigger(i, arg, ds, e) ->
      let trig_decls = 
        lazy(ps "["; ps_list CutLine force (List.map print_id_vt ds); ps "]")
      in
      my_tag "Trigger" 
        [lps (quote i); lazy_arg arg; trig_decls; print_expr_fn ~print_id:print_id string_of_meta e]
        
    | Role (id, sp)   -> my_tag "Role" [lps (quote id); lazy(print_stream_program sp)]

    | DefaultRole id  -> my_tag "DefaultRole" [lps (quote id)] 


let string_of_base_type bt  = wrap_formatter (fun () -> print_base_type bt)
let string_of_value_type vt = wrap_formatter (fun () -> print_value_type vt)
let string_of_type t        = wrap_formatter (fun () -> print_type t)

let string_of_arg a         = wrap_formatter (fun () -> print_arg a)

let string_of_expr string_of_meta e =
  wrap_formatter (fun () -> print_expr string_of_meta e)

let string_of_stream_pattern p    = wrap_formatter (fun () -> print_stream_pattern p)
let string_of_stream s            = wrap_formatter (fun () -> print_stream s)
let string_of_instruction i       = wrap_formatter (fun () -> print_instruction i)
let string_of_stream_statement ss = wrap_formatter (fun () -> print_stream_statement ss) 
let string_of_stream_program sp   = wrap_formatter (fun () -> print_stream_program sp)

let string_of_declaration string_of_meta d =
  wrap_formatter (fun () -> print_declaration string_of_meta d)

let string_of_program ?(print_id=false) ?(print_expr_fn=lazy_expr) string_of_meta ss =
  let print_fn (d, meta) =
    let m_str = wrap_unless_empty "<" ">" (string_of_meta meta) in
    print_declaration 
      ~print_id:print_id ~print_expr_fn:print_expr_fn string_of_meta d;
    (if m_str <> "" then (pc (); ps m_str; pc ()))
  in wrap_formatter (fun () ->
    ps "["; ps_list ~sep:";" CutLine print_fn ss; ps "]")
  

(* AST constructors / destructors *)
let decompose_lambda e = List.nth (sub_tree e) 0

let decompose_apply e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1)

let decompose_ifthenelse e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1, n 2)

let decompose_iterate e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1)

let decompose_map e =
  let n i = List.nth (sub_tree e) i in (n 0, n 1)

let decompose_filter_map e =
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
