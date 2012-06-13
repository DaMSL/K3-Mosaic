open ListAsSet
open Tree
open K3

(* Stringification *)

let string_of_address a = match a with
    | Local(i) -> "Local("^i^")"
    | Remote(i, h, p) -> "Remote("^i^"@"^h^":"^string_of_int p^")"

let string_of_container_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"

let rec string_of_base_type t = match t with
    | TUnknown  -> "TUnknown"
    | TUnit     -> "TUnit"
    | TBool     -> "TBool"
    | TByte     -> "TByte"
    | TInt      -> "TInt"
    | TFloat    -> "TFloat"
    | TString   -> "TString"

    | TMaybe(t) -> "TMaybe("^string_of_value_type(t)^")"

    | TTuple(t_l) -> "TTuple("^(String.concat ", " (List.map string_of_value_type t_l))^")"

    | TCollection(t_c, t_e)
        -> "TCollection("
            ^string_of_container_type t_c^", "
            ^string_of_mutable_type t_e
            ^")"

    | TTarget(a, t) -> "TTarget("^string_of_address(a)^", "^string_of_base_type(t)^")"

and string_of_mutable_type mt = match mt with
    | TMutable(bt) -> "TMutable("^string_of_base_type bt^")"
    | TImmutable(bt) -> "TImmutable("^string_of_base_type bt^")"

and string_of_value_type vt = match vt with
    | TIsolated(mt) -> "TIsolated("^string_of_mutable_type mt^")"
    | TContained(mt) -> "TContained("^string_of_mutable_type mt^")"

and string_of_type t = match t with
    | TFunction(t_a, t_r)
        -> "TFunction("^string_of_value_type t_a ^", "^ string_of_value_type t_r ^")"
    | TValue(t) -> string_of_value_type(t)

let string_of_const c = match c with
    | CUnit      -> "CUnit"
    | CUnknown   -> "CUnknown"
    | CBool(b)   -> "CBool("^string_of_bool(b)^")"
    | CInt(i)    -> "CInt("^string_of_int(i)^")"
    | CFloat(f)  -> "CFloat("^string_of_float(f)^")"
    | CString(s) -> "CString(\""^s^"\")"
    | CNothing   -> "CNothing"

let string_of_arg a = match a with
    | AVar(i, t) -> "AVar("^i^": "^string_of_type(TValue(t))^")"
    | ATuple(its)
        -> "ATuple("^(String.concat ", "
                (List.map (function (i, t) -> i^": "^string_of_type(TValue(t))) its))
        ^")"

let string_of_expr_tag tag children =
  let append_str delim s1 s2 = s1^(if s1 = "" then "" else delim)^s2 in
  let tag_str ?(extra="") n t =
    let rec aux extra n t acc = match n with
      | 0 -> t^"("^extra^","^acc^")"
      | _ -> aux extra (n-1) t (append_str "," acc (List.nth children n))
    in aux extra n t ""
  in
  match tag with
    | Const(c)  -> "Const("^string_of_const(c)^")"
    | Var(i) -> "Var("^i^")"
    | Tuple     -> "Tuple("^(String.concat ", " children)^")"

    | Just -> "Just("^(List.hd children)^")"

    | Empty t     -> tag_str 0 "Empty" ~extra:(string_of_value_type t)
    | Singleton t -> tag_str 1 "Singleton" ~extra:(string_of_value_type t)
    | Combine     -> tag_str 2 "Combine"
    | Range(ct)   -> tag_str 3 "Range" ~extra:(string_of_container_type(ct))

    | Add   -> tag_str 2 "Add"
    | Mult  -> tag_str 2 "Mult"
    | Neg   -> tag_str 1 "Neg"
    | Eq    -> tag_str 2 "Eq"
    | Neq   -> tag_str 2 "Neq"
    | Lt    -> tag_str 2 "Lt"
    | Leq   -> tag_str 2 "Leq"

    | Lambda a -> tag_str 1 "Lambda" ~extra:(string_of_arg a)
    | Apply    -> tag_str 2 "Apply"

    | Block      -> tag_str (List.length children) "Block"
    | IfThenElse -> tag_str 3 "IfThenElse"

    | Map              -> tag_str 2 "Map"
    | Iterate          -> tag_str 2 "Iterate"
    | FilterMap        -> tag_str 3 "FilterMap"
    | Flatten          -> tag_str 1 "Flatten"
    | Aggregate        -> tag_str 3 "Aggregate"
    | GroupByAggregate -> tag_str 4 "GroupByAggregate"
    | Sort             -> tag_str 2 "Sort"
    
    | Slice   -> tag_str 2 "Slice"
    | Insert  -> tag_str 2 "Insert"
    | Update  -> tag_str 3 "Update"
    | Delete  -> tag_str 2 "Delete"
    | Peek    -> tag_str 1 "Peek"

    | Assign  -> tag_str 2 "Assign"
    | Deref   -> tag_str 1 "Deref"

    | Send    -> tag_str 2 "Send"

(* TODO: Why can't this function be point-free? *)
let string_of_expr expr =
  string_of_tree (function (_, tag) -> string_of_expr_tag tag) expr

let string_of_stop_behavior_t s = match s with
    | UntilCurrent -> "UntilCurrent"
    | UntilEmpty -> "UntilEmpty"
    | UntilEOF -> "UntilEOF"

let rec string_of_consumable c = match c with
    | Source(i, t) -> "Source("^i^", "^string_of_type(t)^")"
    | Loop(id, c) -> "Loop("^id^", "^string_of_consumable(c)^")"
    | Choice(cs)
        -> "Choice("^String.concat ", "
            (List.map string_of_consumable cs)
        ^")"

    | Sequence(cs)
        -> "Sequence("^String.concat ", "
            (List.map string_of_consumable cs)
        ^")"

    | Optional(c) -> "Optional("^string_of_consumable c^")"
    | Repeat(c, s)
        -> "Repeat("^string_of_consumable c^", "^string_of_stop_behavior_t s^")"

let string_of_declaration d = match d with
    | Global(i, t)  -> "Global("^i^", "^string_of_type(t)^")"
    | Foreign(i, t) -> "Foreign("^i^", "^string_of_type(t)^")"

    | Trigger(i, arg, ds, e)
        -> "Trigger("^i^", "^string_of_arg(arg)^", ["
            ^String.concat ", " (List.map (fun (id, t) -> "("^id^", "^string_of_value_type t^")") ds)^"], "
            ^string_of_expr(e)
        ^")"

    | Bind(i, i') -> "Bind("^i^", "^i'^")"
    | Consumable(c) -> "Consumable("^string_of_consumable c^")"

let string_of_instruction i = match i with
    | Consume(id) -> "Consume("^id^")"

let string_of_statement s = match s with
    | Declaration(d) -> string_of_declaration(d)
    | Instruction(i)   -> string_of_instruction(i)

let string_of_program ss
    = "K3(["^(String.concat ", " (List.map string_of_statement ss))^"])"


(* AST constructors *)


(* AST accessors *)

let meta_of_expr e = fst_data e
let tag_of_expr e = snd_data e

(* Predicates *)
let is_const e = match tag_of_expr e with | Const _ -> true | _ -> false
let is_var e = match tag_of_expr e with | Var _ -> true | _ -> false

(* Variable id extraction *)
let vars_of_arg =
  function | AVar(v,_) -> [v] | ATuple(vt_l) -> List.map fst vt_l
  
let typed_vars_of_arg =
  function | AVar(v,t) -> [v,t] | ATuple(vt_l) -> vt_l

let id_of_var e = match tag_of_expr e with
  | Var id -> id | _ -> failwith "invalid variable"

(* Bindings *)
let lambda_bindings f tag = match tag with | Lambda x -> f x | _ -> []

let vars_of_lambda e = lambda_bindings vars_of_arg (tag_of_expr e)

let typed_vars_of_lambda e =
  lambda_bindings typed_vars_of_arg (tag_of_expr e)


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
    let vars = List.map (fun id ->
      mk_tree ((meta_of_expr e, Var id), [])) (vars_of_lambda e) in
    if vars = [] then subs
    else List.fold_left (fun acc v -> List.remove_assoc v acc) subs vars
  in
  let sub_aux subs parts_w_sub_ids e =
    let parts, sub_ids =
      let x,y = List.split parts_w_sub_ids in x, List.flatten y
    in
    let new_e = recompose_tree e parts in
    if List.mem_assoc new_e subs
    then (List.assoc new_e subs, sub_ids@[0,0])
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
