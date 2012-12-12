open Util
open ListAsSet
open Tree

open K3.AST
open K3.Annotation

(* AST accessors *)
let id_of_expr e = fst (fst_data e) 
let tag_of_expr e = snd (fst_data e)
let meta_of_expr e = snd_data e

(* Variable id extraction *)
let rec vars_of_arg arg =
    match arg with
    | AIgnored -> []
    | AVar(v,_) -> [v]
    | AMaybe(a') -> vars_of_arg a'
    | ATuple(vt_l) -> List.concat (List.map vars_of_arg vt_l)
  
let rec typed_vars_of_arg arg =
    match arg with
    | AIgnored -> []
    | AVar(v,t) -> [v,t]
    | AMaybe(a') -> typed_vars_of_arg a'
    | ATuple(vt_l) -> List.concat (List.map typed_vars_of_arg vt_l)

let id_of_var e = match tag_of_expr e with
  | Var id -> id | _ -> failwith "invalid variable"

let tuple_type_of_args args_t =
	if List.length args_t = 1 then TValue (List.hd args_t) 
	else TValue (TIsolated(TImmutable(TTuple(args_t),[])))

let tuple_type_of_arg arg =
  tuple_type_of_args (List.map snd (typed_vars_of_arg arg)) 
   

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

(***************
 * Type tags
 ***************)

(* Compute a readable type signature for a K3 type.
 * Type signatures do not preserve annotations. *)
let signature_of_type t =
  let tag d s l = (*(string_of_int d)^*)s^(String.concat "" l) in
  let tag_t d s l = tag d s ((string_of_int (List.length l))::l) in
  let rec sig_ct d ct = match ct with
    | TSet  -> tag d "S" []
    | TBag  -> tag d "B" []
    | TList -> tag d "L" []
  and sig_bt d bt = match bt with
    | TUnknown              -> tag   d "k" []
    | TUnit                 -> tag   d "n" []
    | TBool                 -> tag   d "b" []
    | TByte                 -> tag   d "y" []
    | TInt                  -> tag   d "i" []
    | TFloat                -> tag   d "d" []
    | TString               -> tag   d "s" []
    | TMaybe vt             -> tag   d "o" [sig_vt (d+1) vt]
    | TTuple vtl            -> tag_t d "t" (List.map (sig_vt (d+1)) vtl)
    | TCollection (ct,et)   -> tag   d "c" [sig_ct (d+1) ct; sig_vt (d+1) et]
    | TAddress              -> tag   d "a" []
    | TTarget arg_t         -> tag   d "h" [sig_bt (d+1) arg_t]
  and sig_mt d mt = match mt with
    | TMutable    (bt,_) -> tag d "M" [sig_bt (d+1) bt]
    | TImmutable  (bt,_) -> tag d "U" [sig_bt (d+1) bt]
  and sig_vt d vt = match vt with
    | TIsolated   mt -> tag d "I" [sig_mt (d+1) mt]
    | TContained  mt -> tag d "C" [sig_mt (d+1) mt]
  and sig_t d t = match t with
	  | TFunction (arg_t,ret_t) -> tag d "F" [sig_vt (d+1) arg_t; sig_vt (d+1) ret_t] 
	  | TValue vt -> tag d "V" [sig_vt (d+1) vt]
  in sig_t 0 t

(* Reconstruct a type from a signature, with empty annotations. *)
let type_of_signature s =
  let string_of_char c = String.make 1 c in
  let (>>) f g = fun a -> g (f a) in 
  let (>>=) f g = fun a -> let b,c = f a in b, (g c) in
  let (>>>) f g = fun h a -> let b,c = f a in let d,e = g b in d, (h c e) in
  let n i t = i+1, t in
  let rec ct_sig s i = match s.[i] with
    | 'S' -> n i TSet
    | 'B' -> n i TBag
    | 'L' -> n i TList
    | _ -> failwith "invalid tag for collection type"

  and bt_sig s i =
    match s.[i] with
    | 'k' -> n i TUnknown
    | 'n' -> n i TUnit
    | 'b' -> n i TBool
    | 'y' -> n i TByte
    | 'i' -> n i TInt
    | 'd' -> n i TFloat
    | 's' -> n i TString
    | 'o' -> ((vt_sig s) >>= (fun vt -> TMaybe(vt))) (i+1)
    | 't' -> 
      let nf = int_of_string (string_of_char s.[i+1])
      in ((vt_sig_l s (i+2) nf) >> (fun (i,vtl) -> i, TTuple(vtl))) []

    | 'c' -> ((ct_sig s) >>> (vt_sig s)) (fun ct et -> TCollection(ct,et)) (i+1)
    | 'a' -> n i TAddress
    | 'h' -> ((bt_sig s) >>= (fun bt -> TTarget(bt))) (i+1) 
    | _ -> failwith "invalid tag for base type"

  and mt_sig s i = match s.[i] with
    | 'M' -> ((bt_sig s) >>= (fun bt -> TMutable(bt,[]))) (i+1)
    | 'U' -> ((bt_sig s) >>= (fun bt -> TImmutable(bt,[]))) (i+1)
    | _ -> failwith "invalid tag for mutable type"

  and vt_sig s i = match s.[i] with
    | 'I' -> ((mt_sig s) >>= (fun mt -> TIsolated(mt))) (i+1)
    | 'C' -> ((mt_sig s) >>= (fun mt -> TContained(mt))) (i+1)
    | _ -> failwith "invalid tag for value type"

  and vt_sig_l s i n acc = 
    if n = 0 then i, acc
    else ((vt_sig s) >> (fun (ni,nt) -> vt_sig_l s (n-1) ni (acc@[nt]))) i 
  
  and t_sig s i = match s.[i] with
    | 'F' -> 
      ((vt_sig s) >>> (vt_sig s)) (fun arg_t ret_t -> TFunction(arg_t,ret_t)) (i+1)

    | 'V' -> ((vt_sig s) >>= (fun vt -> TValue(vt))) (i+1)
    | _ -> failwith "invalid tag for type"
  in snd (t_sig s 0)


(* TODO: AST constructors from Yotam *)

(* AST testers *)
let is_peek e = match tag_of_expr e with Peek -> true | _ -> false

(* AST destructors *)
let nth e i = List.nth (sub_tree e) i

let decompose_aggregate e = (nth e 0, nth e 1, nth e 2)
let decompose_apply e = (nth e 0, nth e 1)
let decompose_block e = sub_tree e
let decompose_filter_map e = (nth e 0, nth e 1, nth e 2)
let decompose_ifthenelse e = (nth e 0, nth e 1, nth e 2)
let decompose_insert e = (nth e 0, nth e 1)
let decompose_iterate e = (nth e 0, nth e 1)
let decompose_gbagg e = (nth e 0, nth e 1, nth e 2, nth e 3)
let decompose_lambda e = nth e 0
let decompose_map e = (nth e 0, nth e 1)
let decompose_peek e = nth e 0
let decompose_send e = 
  let rec rest i acc = if i = 1 then acc else rest (i-1) ((nth e i)::acc)
  in (nth e 0, nth e 1, rest ((List.length (sub_tree e))-1) [])
let decompose_tuple e = sub_tree e


let match_declaration id match_f l =
  let m = List.filter match_f l
  in match m with
    | [] -> raise Not_found
    | [x] -> x
    | _ -> failwith ("Multiple matches found for "^id)

(* Declaration accessors *)
let is_global (d,a) = match d with Global _ -> true | _ -> false
let is_flow (d,a)   = match d with Flow _ -> true | _ -> false

let globals_of_program p = List.filter is_global p
let flows_of_program p   = List.filter is_flow p

let global_of_program id p = 
  match_declaration id (fun (d,a) -> match d with
      | Global (n,_,_) when n = id -> true
      | _ -> false 
    ) (globals_of_program p)

(* Flow program accesors *)
let is_source (fs,_)    = match fs with Source _ -> true | _ -> false
let is_sink (fs,_)      = match fs with Sink _ -> true | _ -> false
let is_generator (fs,_) = match fs with Source(Code _) -> true | _ -> false
let is_trigger (fs,_)   = match fs with Sink(Code _) -> true | _ -> false

let endpoints_of_flow match_f fp = 
  let endpoints = List.filter match_f fp in 
  let unwrap (fs,_) = match fs with Source ep -> [ep] | Sink ep -> [ep] | _ -> []
  in List.flatten (List.map unwrap endpoints) 

let endpoints_of_program match_f p = 
  let flow_program (d,_) = match d with Flow p -> p | _ -> [] in
  let flow_statements = List.flatten (List.map flow_program (flows_of_program p))
  in endpoints_of_flow match_f flow_statements

let sources_of_flow p    = endpoints_of_flow is_source p
let sinks_of_flow p      = endpoints_of_flow is_sink p 
let generators_of_flow p = endpoints_of_flow is_generator p
let triggers_of_flow p   = endpoints_of_flow is_trigger p 

let sources_of_program p    = endpoints_of_program is_source p
let sinks_of_program p      = endpoints_of_program is_sink p 
let generators_of_program p = endpoints_of_program is_generator p
let triggers_of_program p   = endpoints_of_program is_trigger p 

let trigger_of_program id p =
  match_declaration id (fun fs -> match fs with
      | Code (n,_,_,_) when n = id -> true
      | _ -> false 
    ) (triggers_of_program p)


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

(* Produce the same tree with new ids top down *)
let renumber_ast_ids (exp:K3.AST.expr_t) num_ref =
  let modify i acc_children t =
    mk_tree @: (((i, tag_of_expr t), meta_of_expr t), acc_children)
  in
  fold_tree1 (fun _ _ -> num_ref := !num_ref + 1; !num_ref) modify 0 exp

(* renumber ids for a whole program *)
let renumber_program_ids prog =
  let num = ref 0 in
  let handle_dec acc = function
    | (Global(x, y, Some e),a) -> 
        (Global(x, y, Some (renumber_ast_ids e num)),a)::acc
    | (Trigger(x, y, z, e),a) -> 
        (Trigger(x, y, z, renumber_ast_ids e num),a)::acc
    | x -> x::acc
  in List.rev @: List.fold_left handle_dec [] prog


    
