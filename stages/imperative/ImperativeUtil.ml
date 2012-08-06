open Format
open Lazy
open Printing
open Tree
open K3Util
open Imperative

(* AST accessors *)
let id_of_expr e = fst (fst_data e) 
let tag_of_expr e = snd (fst_data e)
let meta_of_expr e = snd_data e

let id_of_cmd c = fst (fst_data c) 
let tag_of_cmd c = snd (fst_data c)
let meta_of_cmd c = snd_data c 


(* Pretty printing helpers *)
let option_as_list f opt = match opt with | Some x -> [f x] | _ -> []

let lps s = lazy (ps s)

let lazy_string_opt string_f a = match a with
  | Some b -> [lazy (string_f b)]
  | None -> []
  
(* Type and program grammar pretty printing *)

let string_of_collection_fn_tag coll_fn = match coll_fn with
  | Peek -> "Peek" | Slice -> "Slice"
  | Insert -> "Insert" | Update -> "Update" | Delete -> "Delete" 
  | Combine -> "Combine"
  | Range -> "Range"
  | Sort -> "Sort"
  | Contains -> "Contains"
  | Find -> "Find"

let string_of_composite_fn_tag comp_fn = match comp_fn with
  | Position i -> "Position("^(string_of_int i)^")"
  | Field id -> "Field("^id^")"

let print_tree print_node tree =
  let lazy_root = 
    fold_tree (fun _ _ -> ())
      (fun _ lazy_ch e -> [lazy (print_node lazy_ch e)])
      () [] tree
  in force (List.hd lazy_root)

let rec lazy_type t        = lazy (print_type t)
and     lazy_op o          = lazy (print_op o)
and     lazy_arg a         = lazy (K3Util.print_arg a)
and     lazy_decl d        = lazy (print_decl d)
and     lazy_decl_arg da   = lazy (print_decl_arg da)
and     lazy_expr e        = lazy (print_expr e)
and     lazy_cmd c         = lazy (print_cmd c)

and print_type t = match t with
  | TInternal it ->
    pretty_tag_str CutHint "" "TInternal" [lazy (K3Util.print_type it)]

  | TNamed id -> pretty_tag_str NoCut "" "TNamed" [lps id]

and print_type_env env =
  ob(); ps "[";
  ps_list ~sep:";" CutHint (fun (id,t) -> ps (id^", "); print_type t) env;
  ps "]"; cb()

and print_op op =
  let pop t = ob(); ps t; cb() in 
  match op with
  | Add  -> pop "Add" 
  | Mult -> pop "Mult"
  | Neg  -> pop "Neg"
  | Eq   -> pop "Eq"
  | Neq  -> pop "Neq"
  | Lt   -> pop "Lt"
  | Leq  -> pop "Leq"
  | Ternary  -> pop "Ternary"

and print_decl_arg da = 
  let my_tag = pretty_tag_str CutHint "" in
  match da with
    Constructor expr_args -> my_tag "Constructor" (List.map lazy_expr expr_args) 
  | Init expr -> my_tag "Init" [lazy_expr expr]

and print_decl d =
  let my_tag = pretty_tag_str CutHint "" in
  match d with
  | DType  (id,t) -> my_tag "DType" [lps id; lazy_type t]
  | DVar (id,t,da_opt) ->
    my_tag "DVar" ([lps id; lazy_type t]@(option_as_list lazy_decl_arg da_opt))

  | DFn (id,a,rt,body) ->
    my_tag "DFn" ([lps id; lazy_arg a; lazy_type rt]@(List.map lazy_cmd body))

and print_fn_tag fn_tag = match fn_tag with
  | Collection coll_fn -> ps (string_of_collection_fn_tag coll_fn)
  | Composite comp_fn -> ps (string_of_composite_fn_tag comp_fn)
  | Named id -> ps id
  | Send id -> ps ("send_"^id)
 
and print_expr_tag tag lazy_children =
  let my_tag t = pretty_tag_str CutHint "" t lazy_children in
  let ch_tag t ch = pretty_tag_str CutHint "" t ch in
  match tag with
    Const  c  -> ch_tag "Const" [lps (string_of_const c)]
  | Var    id -> ch_tag "Var" [lps id]
  | Tuple     -> my_tag "Tuple"
  | Just      -> my_tag "Just"
  | Op     op -> ch_tag "Op" ([lazy_op op]@lazy_children)
  | Fn     fn_tag -> ch_tag "Fn" ([lazy (print_fn_tag fn_tag)]@lazy_children)

and print_expr e =
  print_tree (fun lazy_ch e ->
    print_expr_tag (tag_of_expr e) (List.flatten lazy_ch)) e

and print_cmd_tag tag lazy_children =
  let my_tag ?(lb="(") ?(rb=")") ?(sep=", ") ?(cut=CutHint) t =
    pretty_tag_str ~lb:lb ~rb:rb ~sep:sep cut "" t lazy_children
  in
  let my_tag_list = my_tag ~lb:"([" ~rb:"])" ~sep:"; " ~cut:CutLine in
  let ch_tag ?(cut=CutHint) t ch = pretty_tag_str cut "" t ch in
  match tag with
    Assign (id, e)  -> ch_tag "Assign" [lps id; lazy_expr e] 
  | Decl   d        -> ch_tag "Decl" [lazy_decl d]
  | Expr   e        -> ch_tag "Expr" [lazy_expr e]
  | Block           -> my_tag_list "Block"
  
  | For    (id,t,e) ->
      let lazy_for_args = lazy (
        ps "("; ps id; ps " : "; print_type t;
        pc(); ps " in "; pc(); print_expr e; ps ")")
      in ch_tag ~cut:CutLine "For" (lazy_for_args :: lazy_children)
  
  | IfThenElse pred ->
      ch_tag "IfThenElse" ([lazy_expr pred]@lazy_children)

and print_cmd c =
  print_tree (fun lazy_ch c ->
    print_cmd_tag (tag_of_cmd c) (List.flatten lazy_ch)) c

let print_program p =
  ob(); ps "["; fnl();
  List.iter (fun d -> print_decl d; ps ";"; fnl()) p;
  ps "]"; cb()

let wrap_formatter print_fn =
  pp_set_margin str_formatter 120;
  print_fn ();
  flush_str_formatter ()

let string_of_type t    = wrap_formatter (fun () -> print_type t)
let string_of_decl d    = wrap_formatter (fun () -> print_decl d)
let string_of_expr e    = wrap_formatter (fun () -> print_expr e)
let string_of_cmd c     = wrap_formatter (fun () -> print_cmd c)
let string_of_program p = wrap_formatter (fun () -> print_program p)


let var_ids_of_expr e =
  let add_var _ subvars e = (List.flatten subvars)@(match tag_of_expr e with
    | Var id -> [id]
    | _ -> [])
  in ListAsSet.no_duplicates (fold_tree (fun _ _ -> None) add_var None [] e) 

let rec var_ids_of_decl d = match d with
  | DVar   (id,_,da_opt) ->
    id::(match da_opt with
         | Some(Constructor e) -> List.flatten (List.map var_ids_of_expr e)
         | Some(Init e) -> var_ids_of_expr e
         | _ -> [])
  | DFn    (id,_,_,body) -> id::(List.flatten (List.map var_ids_of_cmd body))
  | _ -> []

and var_ids_of_cmd c =
  let add_var _ subvars c = (List.flatten subvars)@(match tag_of_cmd c with
    | Decl d -> var_ids_of_decl d
    | Block -> []
    | Assign (id, e) | For (id,_,e) -> id::(var_ids_of_expr e)
    | Expr e | IfThenElse e -> var_ids_of_expr e)
  in ListAsSet.no_duplicates  (fold_tree (fun _ _ -> None) add_var None [] c)
  
