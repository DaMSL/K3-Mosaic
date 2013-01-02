(* An extended AST for K3's C++ backend *)
open Format
open Lazy
open Printing
open Tree
open K3.AST
open K3Typechecker
open Imperative

(* A CPP AST with exposed constructors.
 * This must be a subtype of Imperative.TargetLanguage *)
module type CPPAST = sig
  
  (* Local module definition to allow AST types to be used in exposed extensions *)
  module ASTImport : Imperative.IAST
  open ASTImport

  type stl_variants_t =
    | TAllocator of id_t option
    | TMulti
    | TOrdered   of id_t option
      (* hash fnobject id * predicate fnobject id *)
    | TUnordered of id_t option * id_t option

  type stl_collection_t = TSTLSet of type_t | TSTLMap of type_t * type_t

	type ext_type_t =
	  | TIterator of type_t
	  | TPair     of type_t * type_t
      (* STL collection type * variant description *)
    | TSTLCollection of stl_collection_t * stl_variants_t list
	
	type bmi_extractor_t =
    | BMIIdentity  of type_t
	  | BMIMember    of type_t * id_t
	  | BMIComposite of bmi_extractor_t list
	
	type ext_type_decl_t =
	    (* Element type * (index tag type ids * unique * ordered * extractor) list *)
	  | TBoostMultiIndex of type_t * (id_t * bool * bool * bmi_extractor_t) list 

	type ext_collection_fn_t =
	  | GetIndex  of id_t     (* index tag type id *)
	  | BeginIterator
	  | EndIterator
	  | Copy
	  | Clear
	
	type ext_fn_t =
	  | IteratorElement
	  | IteratorIncrement
	  | IteratorDecrement
    | MakePair
	  | PairFirst
	  | PairSecond
	
	type 'a ext_cmd_t = 
	    (* Three place for loop, with init, test and advance expressions.
	     * The loop body is an imperative child of this command. *) 
	  | For of 'a expr_t option * 'a expr_t option * 'a expr_t option
	
	    (* A do-while loop with a post-test predicate. The loop body is a child
	     * of this imperative command. *)
	  | DoWhile of 'a expr_t

  val named_ext_types : ext_type_t -> id_t list

  val print_ext_type            : ext_type_t -> unit
  val print_ext_type_decl       : ext_type_decl_t -> unit
  val print_ext_collection_fn   : ext_collection_fn_t -> (unit lazy_t) list -> unit
  val print_ext_fn              : ext_fn_t -> (unit lazy_t) list -> unit
  val print_ext_cmd             : ('a -> string) -> 'a ext_cmd_t -> (unit lazy_t) list -> unit

end

(* Using mutually recursive modules allows us to extend the imperative
 * AST and simultaneously use its types as part of our extension *) 
module rec CPPImpl : Imperative.Export
     with module AST = Imperative.AST(CPPTarget)
 = struct module AST = Imperative.AST(CPPTarget) end 

(* A CPP AST implementation, with base AST types exposed as an imperative AST *)
and CPPTarget : CPPAST with module ASTImport = CPPImpl.AST = struct

(* Open AST types for use internally in this module *)
module ASTImport = CPPImpl.AST
open ASTImport

(* Open AST utility functions for use internally in this module *)
module U = ImperativeUtil.Util(CPPTarget)
open U
  
type stl_variants_t =
  | TAllocator of id_t option
  | TMulti
  | TOrdered   of id_t option
    (* hash fnobject id * predicate fnobject id *)
  | TUnordered of id_t option * id_t option

type stl_collection_t = TSTLSet of type_t | TSTLMap of type_t * type_t

type ext_type_t =
  | TIterator of type_t
  | TPair     of type_t * type_t
    (* STL collection type * variant description *)
  | TSTLCollection of stl_collection_t * stl_variants_t list

type bmi_extractor_t =
  | BMIIdentity of type_t
  | BMIMember of type_t * id_t
  | BMIComposite of bmi_extractor_t list

type ext_type_decl_t =
    (* Element type * (index tag type ids * unique * ordered * extractor) list *)
  | TBoostMultiIndex of type_t * (id_t * bool * bool * bmi_extractor_t) list 

type ext_collection_fn_t =
  | GetIndex  of id_t     (* index tag type id *)
  | BeginIterator
  | EndIterator
  | Copy
  | Clear

type ext_fn_t =
  | IteratorElement
  | IteratorIncrement
  | IteratorDecrement
  | MakePair
  | PairFirst
  | PairSecond

type 'a ext_cmd_t = 
    (* Three place for loop, with init, test and advance expressions.
     * The loop body is an imperative child of this command. *) 
  | For of 'a expr_t option * 'a expr_t option * 'a expr_t option

    (* A do-while loop with a post-test predicate. The loop body is a child
     * of this imperative command. *)
  | DoWhile of 'a expr_t

let rec named_ext_types ext_type = match ext_type with
  | TIterator ct -> named_types ct
  | TPair (l,r) -> (named_types l)@(named_types r)
  | TSTLCollection (TSTLSet t, _) -> named_types t
  | TSTLCollection (TSTLMap (k,v), _) -> (named_types k)@(named_types v)

let lps s = lazy (ps s)

let print_stl_collection c =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match c with
  | TSTLSet e_t -> ptag "TSTLSet" [lazy (print_type e_t)]
  | TSTLMap (k_t, v_t) -> ptag "TSTLMap" [lazy (ps_list NoCut print_type [k_t; v_t])]

let print_variant v =
  let unwrap o = match o with Some id -> id | None -> "" in
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match v with
  | TAllocator alloc -> ptag "TAllocator" [lps (unwrap alloc)]
  | TMulti -> ptag "TMulti" []
  | TOrdered cmp -> ptag "TOrdered" [lps (unwrap cmp)]
  | TUnordered (hash,eq) -> ptag "TUnordered" [lps (unwrap hash); lps (unwrap eq)]

let print_ext_type t =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match t with
  | TIterator t -> ptag "TIterator" [lazy (print_type t)]
  | TPair (l,r) -> ptag "TPair" [lazy (print_type l); lazy (print_type r)]
  | TSTLCollection (c,v) ->
    ptag "TSTLCollection" [lazy (print_stl_collection c); lazy (ps_list NoCut print_variant v)]

let rec print_extractor e = match e with
  | BMIIdentity t -> pretty_tag_str CutHint "" "Identity" [lazy (print_type t)]
  | BMIMember (t,id) -> pretty_tag_str CutHint "" "Member" [lps id]
  | BMIComposite l -> pretty_tag_str CutHint "" "Composite" [lazy (ps_list CutHint print_extractor l)]

let print_index (id, uniq, ordered, extractor) =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  ptag "Index" [lps id;
                lps ("uniq="^string_of_bool uniq);
                lps ("ordr="^string_of_bool ordered);
                lazy(print_extractor extractor)]

let print_ext_type_decl d =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match d with
  | TBoostMultiIndex (elem_t, indexes) ->
    ptag "TBoostMultiIndex" ([lazy (print_type elem_t)]@(List.map (fun e -> lazy (print_index e)) indexes))

let print_ext_collection_fn f lazy_children =
  let ptag ?(cut=CutHint) ?(ch=[]) t = pretty_tag_str CutHint "" t ch in
  match f with
  | GetIndex tag_id_t -> ptag "GetIndex" ~ch:[lazy (ps tag_id_t)] 
  | BeginIterator -> ptag "BeginIterator"
  | EndIterator -> ptag "EndIterator"
  | Copy -> ptag "Copy"
  | Clear -> ptag "Clear"

let print_ext_fn f lazy_children = 
  let ptag ?(cut=CutHint) t = pretty_tag_str CutHint "" t [] in
  match f with
  | IteratorElement -> ptag "IteratorElement"
  | IteratorIncrement -> ptag "IteratorIncrement"
  | IteratorDecrement -> ptag "IteratorDecrement"
  | MakePair -> ptag "MakePair"
  | PairFirst -> ptag "PairFirst"
  | PairSecond -> ptag "PairSecond"

let print_ext_cmd string_of_meta c lazy_children =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  let lazy_expr_opt e_opt = lazy (match e_opt with 
    | None -> ptag "None" []
    | Some e -> print_expr string_of_meta e)
  in
  match c with
  | For (init_e_opt, test_e_opt, adv_e_opt) ->
      ptag "For" ((List.map lazy_expr_opt [init_e_opt; test_e_opt; adv_e_opt]))

  | DoWhile test_e -> ptag "DoWhile" ([lazy (print_expr string_of_meta test_e)])

end

type type_t = CPPTarget.ASTImport.type_t
type 'a program_t = 'a CPPTarget.ASTImport.program_t 


(* C++ source code generation *)

module CPPGenerator : Imperative.Generator
       with type program_t = (type_t * annotation_t) program_t
= struct
  open CPPTarget
  open CPPTarget.ASTImport

  module R = Runtime.Make(CPPTarget)
  module U = ImperativeUtil.Util(CPPTarget)
  
  open R
  open U

  type format_box_type = HBox | VBox | HVBox | HOVBox
  type program_t = (type_t * annotation_t) CPPTarget.ASTImport.program_t

  (* Typing and AST helpers *)
  let type_of_expr e = fst (meta_of_expr e)

  let is_internal t = match t with TInternal _ -> true | _ -> false

  (* TODO: add references here when they are implemented *)
  let is_ptr t = match t with TNamed _ -> true | _ -> false
  
  let pointed_type t = match t with
    | TNamed id -> id
    | _ -> failwith ("invalid pointer type "^(string_of_type t)) 

  let is_func_or_class d = match d with DFn _ | DClass _ -> true | _ -> false 

  let is_leaf_expr e =
    match tag_of_expr e with | Const _ | Var _ -> true | _ -> false

  let is_block_cmd c = match tag_of_cmd c with Block _ -> true | _ -> false

  let extract_option_type string_fn t =
    let error = "invalid option type" in
    if is_internal t then match bi_type t with 
      | TMaybe(e_t) -> string_fn (iv_type e_t)
      | _ -> failwith error
    else failwith error

  let print_tree print_node tree =
	  let lazy_root = 
	    fold_tree (fun _ _ -> ())
	      (fun _ lazy_ch e -> [lazy (print_node lazy_ch e)])
	      () [] tree
	  in force (List.hd lazy_root)
  
  let print_list ?(hint=NoCut) ?(sep=", ") print_fn l =
    if l = [] then () else
    let last = List.nth l ((List.length l)-1) in
    List.iter (fun e -> print_fn e; if e <> last then (ps sep; cut hint) else ()) l

  let print_lazy_list ?(hint=NoCut) ?(sep=", ") l =
    print_list ~hint:hint ~sep:sep force l

  (* Stringification helpers *)
  let escape s = Str.global_replace (Str.regexp_string "\"") "\\\"" s 
  let mk_string s = "\""^(escape s)^"\""

  let wrap ?(hint = NoCut) ?(box = false) ?(hvbox=false)
           ?(cut_before=true) ?(cut_after=false) ?(cut_last=true)
           ?(space = true) l lazy_fn r =
    let o () = if box then (if hvbox then obc 2 else obx 2) else () in
    let c () = if box then cb() else () in
    let cb () = if not cut_before then () else cut hint in
    let ca () = if cut_after then cut hint else () in
    let cl () = if cut_last then cut hint else () in
    let s = if space then " " else "" in 
    cb(); o(); ps (l^s); ca(); force lazy_fn; c(); cl(); ps (s^r)

  let wrap_child_unless_leaf ?(space=false) f ch i =
    if is_leaf_expr (f i) then force (List.nth ch i)
    else wrap ~space:space "(" (List.nth ch i) ")" 

  let concat_template tid1 tid2 =
    tid1^"<"^tid2^(if tid2.[(String.length tid2)-1] = '>' then " " else "")^">"

  let sep_type_list ?(sep=",") string_fn l =
    String.concat sep (List.map string_fn l)
  
  let sep_id_type_list ?(sep_elem="; ") ?(sep_id=" ") string_fn l =
    String.concat sep_elem (List.map (fun (id,t) -> (string_fn t)^sep_id^id) l)
	
	let call_fn lazy_id args =
    force lazy_id; wrap ~space:false "(" (lazy (print_lazy_list args)) ")"

  (* Control flow and failure helpers *)
  let check_with_pred tag pred n l =
	  if (pred (List.length l) n) then ()
	  else failwith ("invalid "^tag^", expected "^(string_of_int n)^" children")

	let check_length ?(tag="operation") n l = check_with_pred tag (=) n l
  let check_at_least n l = check_with_pred "operation" (>=) n l
  let check_leaf tag l = check_length ~tag:tag 0 l
	let check_unary l = check_length 1 l
	let check_binary l = check_length 2 l
	let check_ternary l = check_length 3 l

  (* Start of stringification *)
  
  (* K3-CPP printing *)
  let rec comma_types types = sep_type_list string_of_type types

  (* TODO: for now, ignore TImmutable and TMutable. Think about whether these
   * should be implement as const types and shared_ptrs *) 
  and print_k3_value_type vt = match base_of vt with
    | TBool  -> ps "bool"
    | TByte  -> ps "char"
    | TInt   -> ps "int"
    | TFloat -> ps "double"
    | TString -> ps "string"
    | TMaybe vt -> ps (concat_template "shared_ptr" (string_of_k3_value_type vt))
    | TTuple vt_fields -> 
      let field_types = sep_type_list string_of_k3_value_type vt_fields
      in ps (concat_template "tuple" field_types)

    | TCollection (ct, et) ->
      let ct_str = match ct with
        | TSet -> "set"
        | TBag -> "multiset"
        | TList -> "list"
      in ps (concat_template ct_str (string_of_k3_value_type et))

    | TAddress -> ps "address"
     
      (* Print target types directly as integers *)
    | TTarget target_bt -> ps "int"

    | TUnit -> ps "void" 
    | TUnknown -> failwith "no C++ repr for TUnknown"

  and print_k3_type t =
    let error = "invalid function type, expected a value" in
    print_k3_value_type (value_of t (fun () -> failwith error))
    
  and string_of_k3_value_type vt = wrap_formatter (fun () -> print_k3_value_type vt)

  and print_const c_type c =
    match c with
    | CBool b    -> ps (if b then "true" else "false")
    | CInt i     -> ps (string_of_int i)
    | CFloat f   -> ps (string_of_float f)
    | CString s  -> ps (mk_string s) 
    | CNothing   -> ps ((concat_template "shared_ptr" (string_of_type c_type))^"()")

    (* Targets and address should be desugared *)
    | CTarget _  -> failwith "invalid sugared target constant in C++"
    | CAddress _ -> failwith "invalid sugared address constant in C++"

    | CUnit      -> failwith "unit constant unsupported in C++"
    | CUnknown   -> failwith "unknown constant unsupported in C++"

  (* Imperative-CPP printing *)
  and print_stl_collection c vl =
    let unwrap o = match o with Some(id) -> id | _ -> "" in
    let wrap_type t = 
      let a, (h, e), c, m = List.fold_left (fun (aid,uid,oid,m) v -> match v with
          | TAllocator a -> unwrap a, uid, oid, m
          | TOrdered c -> aid, uid, unwrap c, m
          | TUnordered (h,e) -> aid, (unwrap h, unwrap e), oid, m
          | TMulti -> aid, uid, oid, true
        ) ("", ("", ""), "", false) vl
      in
      let t = 
        if h <> "" && e <> "" && c <> "" then 
          failwith "invalid collection variant (both ordered and unordered)"
        else (if h <> "" && e <> "" then "unordered_" else "")^(if m then "multi" else "")^t
      in 
      let template_args = String.concat "," (List.filter ((<>) "") [c; h; e; a])
      in t, template_args
    in
    let print_template t_id arg_t_l = 
      let t, template_args = wrap_type t_id
      in ps (concat_template t (String.concat "," [comma_types arg_t_l; template_args]))
    in match c with
      | TSTLSet e_t -> print_template "set" [e_t]
      | TSTLMap (k_t, v_t) -> print_template "map" [k_t; v_t]

  and print_ext_type t =
    match t with
    | TIterator t -> ps ((string_of_type ~named_as_shared:false t)^"::iterator")
    | TPair (l,r) -> ps (concat_template "pair" (comma_types [l; r]))
    | TSTLCollection (c, v) -> print_stl_collection c v

  and print_ext_type_decl d =
    let lazy_list ?(hint=NoCut) l = lazy (print_lazy_list ~hint:hint l) in
    let wrap_template tag lazy_f = wrap ~space:false (tag^"<") lazy_f " >" in
    let wrap_template_box ?(cut_before=true) ?(cut_last=false) tag lazy_f =
      wrap ~hint:CutHint ~box:true ~cut_before:false ~cut_after:true ~cut_last:true
        ~space:false (tag^"<") lazy_f " >"
    in
    let rec print_extractor elem_t e = 
      let rcr = print_extractor elem_t in
      match e with
      | BMIIdentity t -> ps (concat_template "identity" (string_of_type t))
      
      | BMIMember (t,id) ->
        wrap_template "member" (lazy_list [lazy (print_type elem_t); lazy(print_type t); lazy(ps id)])
      
      | BMIComposite e_l ->
        let l = List.map (fun e -> lazy (rcr e)) e_l
        in wrap_template "composite" (lazy (print_lazy_list l))
    in
    match d with
    | TBoostMultiIndex (elem_t, indexes) ->
      let print_index indexes =
        let l = List.map (fun (id, uniq, ordered, extractor) -> 
            let t = match uniq, ordered with
              | true, true -> "ordered_unique"
              | true, false -> "hashed_unique"
              | false, true -> "ordered_non_unique"
              | false, false -> "hashed_non_unique"
            in
            let body = [lazy(ps (concat_template "tag" id));
                        lazy(print_extractor elem_t extractor)]
            in lazy (wrap_template t (lazy_list body))
          ) indexes
        in wrap_template_box  "indexed_by" (lazy_list ~hint:CutHint l)
      in 
      let container_body =
        let l = [lazy(print_type elem_t)]@(if indexes <> [] then [lazy(print_index indexes)] else [])
        in lazy_list ~hint:CutHint l
      in wrap_template_box "multi_index_container" container_body

  and print_ext_collection_fn cf lazy_children =
    let p_fn fn_tag =
      check_at_least 1 lazy_children;
      call_fn (lazy (force (List.hd lazy_children); ps "."; ps fn_tag)) (List.tl lazy_children)
    in
    match cf with
    | GetIndex(id) -> p_fn (concat_template "get" id)
    | BeginIterator -> p_fn "begin"
    | EndIterator -> p_fn "end"
    | Copy -> p_fn "copy"
    | Clear -> p_fn "clear"

  and print_ext_fn f lazy_children =
    let print tag = call_fn (lazy (ps tag)) lazy_children in
    let p_pair tag =
      check_unary lazy_children;
      (* TODO: wrap only if this is not a leaf *)
      wrap ~space:false "(" (List.hd lazy_children) (")."^tag)
    in 
    match f with
    | IteratorElement -> print "*"
    | IteratorIncrement -> print "++"
    | IteratorDecrement -> print "--"
    | MakePair -> print "make_pair"
    | PairFirst -> p_pair "first"
    | PairSecond -> p_pair "second"

  and print_ext_cmd string_of_meta c lazy_children =
    match c with
    | For (init_e_opt, test_e_opt, advance_e_opt) ->
      let local_children = 
        let p_opt e_opt = match e_opt with
          | None -> [lazy (ps "")]
          | Some(e) -> [lazy (print_expr e)]
        in
        List.flatten (List.map p_opt [init_e_opt; test_e_opt; advance_e_opt])
      in
      wrap "for(" (lazy (print_lazy_list ~sep:"; " local_children)) ")";
      wrap ~hint:CutHint ~cut_after:true ~box:true "{" 
        (lazy (print_lazy_list ~hint:CutHint ~sep:"" lazy_children))
      "}"

    | DoWhile (cond_expr) ->
      wrap ~hint:CutHint ~cut_after:true "do {"
        (lazy (print_lazy_list ~hint:CutHint ~sep:"" lazy_children))
      "}";
      wrap ~hint:CutHint ~cut_after:true" while (" (lazy (print_expr cond_expr)) ")"

  and print_type ?(named_as_shared=true) t =
	  match t with
	  | TInternal it -> print_k3_type it
	  | TTop         -> ps "shared_ptr<boost::any>"
	  | TNamed id    -> ps (if named_as_shared then (concat_template "shared_ptr" id) else id)
	  
	  | TImpFunction (args_t, rt) ->
	    let fn_type = (string_of_type rt)^" ("^(comma_types args_t)^")"
	    in ps (concat_template "boost::function" fn_type)
	    
	  | TExt e  -> print_ext_type e

  and print_decl_args da = match da with
    | Constructor(cstr_args) -> wrap "(" (lazy (print_list print_expr cstr_args)) ")"
    | Init(expr) -> ps " = "; print_expr expr
  
  and print_decl ?(init_ptrs=false) decl =
    let pad_decl ?(after=false) lazy_fn =
      pc(); force lazy_fn; if after then pc() else () in
    let finish_decl() = ps ";" in
    let print_typedef id lazy_fn = wrap "typedef" lazy_fn id in
    let print_struct id_t_l = 
      let l = List.map (fun (id,t) -> lazy (print_type t; ps (" "^id))) id_t_l in
      ps "struct";
      wrap ~box:true ~hint:CutHint ~cut_after:true ~cut_last:true
        "{" (lazy (print_lazy_list ~hint:CutHint ~sep:"; " l)) "}"
    in
    match decl with
    | DType (id, type_decl) ->
      print_typedef id (match type_decl with
        | TExpr t -> lazy (print_type t)  
        | TComposite id_t_l -> lazy (print_struct id_t_l)
        | TExtDecl e -> lazy (print_ext_type_decl e));
      finish_decl()

    | DVar (id, t, dargs) ->
      let print_var lazy_init = print_type t; ps (" "^id); force lazy_init in
      let default_init dargs = match dargs with
        | None -> ()
        | Some a -> print_decl_args a
      in
      let print_init t dargs =
        if is_ptr t && init_ptrs then
          let f args =
	          let base_t = pointed_type t in
	          let constructor = lazy (ps (concat_template "make_shared" base_t))
	          in (ps " = "; call_fn constructor args)
          in
            match dargs with
            | None -> f []
            | Some (Constructor(args)) -> f (List.map (fun e -> lazy(print_expr e)) args)
            | Some(Init e) -> print_decl_args (Init e)
        else default_init dargs
      in
      print_var (lazy (print_init t dargs));
      finish_decl()

    | DFn (id, args, ret_t, body) ->
      let sep_arg_decls = sep_id_type_list ~sep_elem:", " string_of_type in
      pad_decl ~after:true (lazy (
      print_type ret_t; ps (" "^id);
      wrap ~space:false "(" (lazy (ps (sep_arg_decls args))) ") ";
      wrap ~hint:CutHint ~cut_after:true ~box:true
        "{" (lazy (print_list ~hint:CutHint ~sep:"" (print_cmd ~tlb:true) body)) "}"))

    | DClass (id, parent_opt, members_with_meta) ->
      let parent_str = match parent_opt with
        | None -> ""
        | Some t -> " : public "^(string_of_type ~named_as_shared:false t) in
      let m = List.map fst members_with_meta in 
      pad_decl (lazy (
      ps ("struct "^id^parent_str);
      wrap ~hint:CutHint ~cut_after:true ~box:true
        "{" (lazy (print_list ~hint:CutHint ~sep:"" (print_decl ~init_ptrs:false) m)) "}"))

  and print_op op e ch =
    let e_ch = sub_tree e in
    let wrap_unless_leaf i = wrap_child_unless_leaf (List.nth e_ch) ch i in
    let pb op_str = 
      check_binary ch;
      wrap_unless_leaf 0; ps (" "^op_str^" "); wrap_unless_leaf 1
    in  
    match op with
	  | Add  -> pb "+" 
	  | Mult -> pb "*"
	  | And  -> pb "&&"
	  | Or   -> pb "||"
	  | Not  -> pb "!"
	  | Eq   -> pb "=="
	  | Neq  -> pb "!="
	  | Lt   -> pb "<"
	  | Leq  -> pb "<="
    | Neg  -> check_unary ch; ps "-"; wrap_unless_leaf 0
	  | Ternary  ->
      begin
	      check_ternary ch;
	      let predicate () = wrap_unless_leaf 0; ps " ?" in
	      let branches () = wrap_unless_leaf 1; ps " : "; wrap_unless_leaf 2
	      in wrap "(" (lazy (predicate(); branches ())) ")"
      end
  
  and print_collection_fn (e_type, e) coll_fn ch =
    let desugar_error tag =
      failwith ("invalid collection operation ("^
                    tag^"), expected it to be desugared") 
    in
    let call_w_coll_arg lazy_id_fn args =
      check_at_least 1 args;
      let coll, fn_args = List.hd args, List.tl args in
      call_fn (lazy_id_fn (coll)) fn_args
    in
    match coll_fn with
	  | Peek ->
      let id_fn lazy_coll = lazy (ps "*"; force lazy_coll; ps ".begin")
      in call_w_coll_arg id_fn ch

	  | Insert -> call_w_coll_arg (fun lazy_c -> lazy (force lazy_c; ps ".insert")) ch
    | Delete -> call_w_coll_arg (fun lazy_c -> lazy (force lazy_c; ps ".erase")) ch 
	  | Find -> (call_w_coll_arg (fun lazy_c -> lazy (force lazy_c; ps ".find")) ch)
	  
    | CFExt f -> print_ext_collection_fn f ch

    | Update   -> desugar_error "Update" 
    | Contains -> desugar_error "Contains"
    | Slice    -> desugar_error "Slice"
	  | Combine  -> desugar_error "Combine"
	  | Range    -> desugar_error "Range"
	  | Sort     -> desugar_error "Sort"

  and print_fn (e_type, e) fn_tag ch =
    let ch_e, ch_types = let x = sub_tree e in x, List.map type_of_expr x in
    let ce i = List.nth ch_e i in
    let ct i = List.nth ch_types i in
	  match fn_tag with
	  | Collection coll_fn -> print_collection_fn (e_type, e) coll_fn ch

      (* Tuple field access *)
	  | Member (Position i) -> 
      check_unary ch; wrap ("get<"^(string_of_int i)^">(") (List.hd ch) ")"
	  
    | Member (Field f) ->
      check_unary ch; 
      let accessor = if is_ptr (ct 0) then "->" else "." in
      if is_leaf_expr (ce 0) then force (List.hd ch) else wrap "(" (List.hd ch) ")";
      ps accessor; ps f
      
	  | Member (Method id)  ->
      check_at_least 1 ch;
      let obj, args = List.hd ch, List.tl ch in
      let accessor = if is_ptr (ct 0) then "->" else "." in
      begin force obj; ps accessor; call_fn (lazy (ps id)) args end

	  | Named id -> call_fn (lazy (ps id)) ch
	  
    | Cast to_type -> 
      check_unary ch;
      let from_type = ct 0 in
      let cast_op =
        let as_internal t = if is_internal t then bi_type t else TUnknown in 
        match as_internal from_type, as_internal to_type with
        | TString, _ | _, TString -> 
          concat_template "boost::lexical_cast" (string_of_type to_type)
        | _, _ -> concat_template "static_cast" (string_of_type to_type)
      in wrap ~space:false (cast_op^"(") (List.hd ch) ")"

	  | Send type_tag  -> call_fn (lazy (ps ("send_"^type_tag))) ch 
	  | FExt f         -> print_ext_fn f ch
  
  and print_expr_tag ch e =
    let check_leaf() = check_leaf "expression" ch in
    let e_type, e_tag = type_of_expr e, tag_of_expr e in
    match e_tag with
	  | Const  c  -> check_leaf(); print_const e_type c
	  | Var    id -> check_leaf(); ps id
	  | Tuple     -> wrap ~space:false "make_tuple(" (lazy (print_lazy_list ch)) ")"
	  | Just      -> 
      let extract_fn = extract_option_type string_of_type in
      let value_type = extract_fn e_type
      in wrap ((concat_template "make_shared" value_type)^"(") (List.hd ch) ")"

	  | Op     op -> print_op op e ch
	  | Fn     fn_tag -> print_fn (e_type, e) fn_tag ch

  and print_expr e =
    print_tree (fun cll e -> print_expr_tag (List.flatten cll) e) e
  
  and print_cmd_tag ?(tlb=false) ch c =
    let check_leaf() = check_leaf "command" ch in
    let desugar_error tag = failwith ("invalid command ("^tag^"), expected it to be desugared") in
    let sep() = ps ";" in
    let cc i = let s = sub_tree c in List.nth s i in
    let print_as_block i =
      if is_block_cmd (cc i) then force (List.nth ch i)
      else 
        wrap ~hint:CutHint ~cut_after:true ~box:true "{" (List.nth ch i) "}"
    in
    let c_tag = tag_of_cmd c in
    match c_tag with
	  | Assign (id, e)  -> check_leaf(); ps (id^" = "); print_expr e; sep()
	  | Decl   d        -> check_leaf(); print_decl ~init_ptrs:true d
	  | Expr   e        -> check_leaf(); print_expr e; sep()
	
    | IfThenElse pred ->
      let skip_else =
        let else_branch = List.nth (sub_tree c) 1
        in match tag_of_cmd else_branch with Block -> sub_tree else_branch = [] | _ -> false
      in
      ps "if "; wrap "(" (lazy (print_expr pred)) ") ";
      print_as_block 0; if not skip_else then (pc(); ps "else "; print_as_block 1)
	
	  | Block ->
      if ch = [] then ps "{}" else
      wrap ~hint:CutHint ~cut_before:(not tlb) ~cut_after:true ~box:true
        "{" (lazy (print_lazy_list ~hint:CutHint ~sep:"" ch)) "}"
	  
	  | While e  ->
      ps "while "; wrap "(" (lazy (print_expr e)) ")";
      wrap ~hint:CutHint "{" (List.hd ch) "}"
	  
	  | Return e -> ps "return "; print_expr e; sep()
	
	  | CExt c -> let string_of_meta m = "" in print_ext_cmd string_of_meta c ch
    
    | Foreach (id,t,e) -> desugar_error "Foreach"
    
  and print_cmd ?(tlb=false) c =
    print_tree (fun cll c2 -> 
      let b = if c2 = c then tlb else false
      in print_cmd_tag ~tlb:b (List.flatten cll) c2) c
  
  and string_of_type ?(named_as_shared=true) t =
    wrap_formatter (fun () -> print_type ~named_as_shared:named_as_shared t)

  and string_of_decl d = 
    wrap_formatter (fun () -> obx 0; print_decl ~init_ptrs:true d; cb())
  
  let rec generate_component prog_file c = match c with
    | Include (id, None, None, _) | Include (id, _, _, true) ->
      [prog_file, "#include <"^id^">"]

    | Include (id, None, Some(c), false) -> [id, c]

    | Include (id, Some(prog), None, false) -> 
      [prog_file, "#include <"^id^">"]@(generate_named_program id prog)

    | Include (_, Some(_), Some(_), _) ->
      failwith "invalid module, modules cannot contain both programs and raw code"

    | Component decls_with_meta ->
      [prog_file, 
       (String.concat "\n"
         (List.map string_of_decl (List.map fst decls_with_meta)))]

  and generate_named_program prog_file p =
    let append a b = a^"\n"^b in
    List.fold_left (fun acc c ->
      List.fold_left (fun acc (fn,fc) -> 
          if not (List.mem_assoc fn acc) then acc@[fn,fc]
          else [fn, append (List.assoc fn acc) fc]@(List.remove_assoc fn acc))
        acc (generate_component prog_file c)) [] p
      
  let generate_program p =
    let prog_file = "k3_program.cpp"
    in generate_named_program prog_file p

end
