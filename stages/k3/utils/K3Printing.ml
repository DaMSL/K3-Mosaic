open Format
open Lazy
open Util
open Printing
open Tree

open K3.AST
open K3.Annotation
open K3Util

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

let lps s = lazy (ps s)

let lazy_string_opt string_f a = match a with
  | Some b -> [lazy (string_f b)]
  | None -> []

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
    | TMutable(bt,_) -> "TMutable("^flat_string_of_base_type bt^")"
    | TImmutable(bt,_) -> "TImmutable("^flat_string_of_base_type bt^")"

and flat_string_of_value_type vt = match vt with
    | TIsolated(mt) -> "TIsolated("^flat_string_of_mutable_type mt^")"
    | TContained(mt) -> "TContained("^flat_string_of_mutable_type mt^")"

and flat_string_of_type t = match t with
    | TFunction(t_a, t_r) ->
        tag_str "TFunction"
          [flat_string_of_value_type t_a; flat_string_of_value_type t_r]
    
    | TValue(t) -> flat_string_of_value_type t

let rec flat_string_of_arg a = match a with
    | AIgnored -> tag_str "AIgnored" []
    | AVar(i, t) -> tag_str "AVar" [i; flat_string_of_type (TValue t)]
    | AMaybe(a') -> tag_str "AMaybe" [flat_string_of_arg a']
    | ATuple(args) -> tag_str "ATuple" (List.map flat_string_of_arg args)

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

let flat_string_of_channel_type ct = match ct with
  | File fp -> tag_str "File" [fp]
  | Network addr -> tag_str "Network" [string_of_address addr]

let flat_string_of_channel_format cf = match cf with
  | CSV -> "CSV"
  | JSON -> "JSON"
 
let rec flat_string_of_resource_pattern p =
  let rcr = flat_string_of_resource_pattern in
  let rcr_list l = List.map rcr l in
  match p with
    | Terminal(id)  -> tag_str "Terminal" [id]
    | Choice(ps)    -> tag_str "Choice" (rcr_list ps)
    | Sequence(ps)  -> tag_str "Sequence" (rcr_list ps)
    | Optional(p)   -> tag_str "Optional" [rcr p]
    | Repeat(p, s) -> tag_str "Repeat" [rcr p; string_of_stop_behavior_t s]

let flat_string_of_instruction i = match i with
    | Consume(id) -> tag_str "Consume" [id]

let flat_string_of_flow_resource r = match r with
    | Handle(t,ct,cf)  ->
        tag_str "Handle" [flat_string_of_type t; 
                          flat_string_of_channel_type ct;
                          flat_string_of_channel_format cf]

    | Pattern(p)       -> tag_str "Pattern" [flat_string_of_resource_pattern p]

let flat_string_of_flow_endpoint e =
  let string_of_id_and_vtype (id,vt,_) = "("^id^", "^flat_string_of_value_type vt^")" in
  match e with
  | Resource (id,r) -> tag_str "Resource" [id; flat_string_of_flow_resource r]
  | Code(i, arg, ds, e) ->  
	  let decls = "["^(String.concat ", " (List.map string_of_id_and_vtype ds))^"]" in
	  tag_str "Code"
	    [i; flat_string_of_arg arg; decls; flat_string_of_expr e]

let flat_string_of_flow_statement fs = match fs with
    | Source ep      -> tag_str "Source" [flat_string_of_flow_endpoint ep]
    | Sink ep        -> tag_str "Sink" [flat_string_of_flow_endpoint ep]
    | Bind(i, i2)    -> tag_str "Bind" [i; i2]
    | Instruction(i) -> tag_str "Instruction" [flat_string_of_instruction i]

let flat_string_of_flow_program sp =
  "[ "^(String.concat ","
         (List.map (fun (fs,_) -> flat_string_of_flow_statement fs) sp))^" ]"

let flat_string_of_declaration d =
  match d with
    | Global(i, t, init) ->
      tag_str "Global"
        ([i; flat_string_of_type t]@(string_opt flat_string_of_expr init))
      
    | Foreign(i, t) -> tag_str "Foreign" [i; flat_string_of_type t]

    | Flow fp -> tag_str "Flow" [flat_string_of_flow_program fp]
        
    | Role (id, sp)    -> tag_str "Role" [id; flat_string_of_flow_program sp]
    
    | DefaultRole (id) -> tag_str "DefaultRole" [id]

let flat_string_of_program ss =
  tag_str "K3" (List.map (fun (d,_) -> flat_string_of_declaration d) ss)


(*************************
 * Stringification API
 **************************)

(* Module types *)
module type StringifyAST = sig
  val print_base_type    : base_type_t -> unit
  val print_mutable_type : mutable_type_t -> unit
  val print_value_type   : value_type_t -> unit
  val print_type         : type_t -> unit
  
	val print_arg : arg_t -> unit
	val print_expr : ?print_id:bool -> expr_t -> unit
	
	val print_resource_pattern : resource_pattern_t -> unit
	val print_flow_resource    : flow_resource_t -> unit

  val print_flow_statement :
    ?print_id:bool ->
    ?print_expr_fn:(?print_id:bool -> expr_t -> unit Lazy.t)
    -> flow_statement_t -> unit

  val print_flow_program :
    ?print_id:bool ->
    ?print_expr_fn:(?print_id:bool -> expr_t -> unit Lazy.t)
    -> flow_program_t -> unit
    
  val print_declaration :
    ?print_id:bool ->
    ?print_expr_fn:(?print_id:bool -> expr_t -> unit Lazy.t)
    -> declaration_t -> unit

	val string_of_base_type: base_type_t -> string
	val string_of_value_type: value_type_t -> string
	val string_of_type: type_t -> string
	
  val string_of_arg: arg_t -> string
	val string_of_expr: expr_t -> string
	
	val string_of_resource_pattern : resource_pattern_t -> string
	val string_of_flow_resource    : flow_resource_t -> string
	val string_of_flow_statement   : flow_statement_t -> string
	val string_of_flow_program     : flow_program_t -> string
	val string_of_declaration: declaration_t -> string
	
	val string_of_program:
	  ?print_id:bool ->
	  ?print_expr_fn:(?print_id:bool -> expr_t -> unit Lazy.t)
    -> program_t -> string
end

module type StringifyAnnotations = sig
  val print_annotation : annotation_t -> unit
  val string_of_annotation : annotation_t -> string
end


(* Module implementations *)
module rec ASTStrings : StringifyAST = struct

open AnnotationStrings

(* Lazy variants *)
let print_expr_id id = ps ("<"^string_of_int id^"> ")
  
let rec lazy_annotation a  = lazy (print_annotation a)
and     lazy_base_type bt  = lazy (print_base_type bt)
and     lazy_value_type vt = lazy (print_value_type vt)
and     lazy_type t        = lazy (print_type t)
and     lazy_arg a         = lazy (print_arg a)

and     lazy_expr ?(print_id=false) e = lazy (print_expr ~print_id:print_id e)

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
  let my_tag t bt a =
    pretty_tag_str CutHint "" t
      ([lazy_base_type bt]@(if a = [] then [] else [lazy_annotation a]))
  in match mt with
    | TMutable(bt,a) -> my_tag "TMutable" bt a
    | TImmutable(bt,a) -> my_tag "TImmutable" bt a

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
  match a with
    | AIgnored -> my_tag "AIgnored" []
    | AVar(i, t)  -> my_tag "AVar" [lps i; lazy_type (TValue t)]
    | AMaybe(a') -> my_tag "AMaybe" [lazy_arg a']
    | ATuple(args) -> my_tag "ATuple" (List.map lazy_arg args)

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

and print_expr ?(print_id=false) expr =
  let id_pr e = if print_id then print_expr_id @: id_of_expr e else () in
  let print_meta e =  
    let m_str = wrap_unless_empty "<" ">" (string_of_annotation (meta_of_expr e))
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

let print_instruction i = match i with
    | Consume(id) -> pretty_tag_str CutHint "" "Consume" [lps id]

let rec print_resource_pattern p =
  let my_tag = pretty_tag_str CutHint "" in
  let lazy_rcr p = lazy (print_resource_pattern p) in 
  let rcr_list l = List.map lazy_rcr l in
  match p with
    | Terminal(id)  -> my_tag "Terminal" [lps id]
    | Choice(ps)    -> my_tag "Choice" (rcr_list ps)
    | Sequence(ps)  -> my_tag "Sequence" (rcr_list ps)
    | Optional(p)   -> my_tag "Optional" [lazy_rcr p]
    | Repeat(p, s)  -> my_tag "Repeat" [lazy_rcr p; lps (string_of_stop_behavior_t s)]

let print_flow_resource r =
  let my_tag = pretty_tag_str CutHint "" in
  match r with
    | Handle (t,ct,cf) ->
      my_tag "Handle" [lazy_type t;
                       lps (flat_string_of_channel_type ct);
                       lps (flat_string_of_channel_format cf)]

    | Pattern p        -> my_tag "Pattern" [lazy (print_resource_pattern p)]

let print_flow_endpoint ?(print_id=false) ?(print_expr_fn=lazy_expr) ep =
	let my_tag = pretty_tag_str CutHint "" in
  let print_id_vt (id,vt,_) = lazy (ps ("("^id^", "); print_value_type vt; ps ")") in
	match ep with
  | Resource (id,r) -> my_tag "Resource" [lps id; lazy (print_flow_resource r)]
  | Code(i, arg, ds, e) ->
	  let decls = lazy(ps "["; ps_list CutLine force (List.map print_id_vt ds); ps "]") in
	  my_tag "Code" 
	    [lps (quote i); lazy_arg arg; decls; print_expr_fn ~print_id:print_id e]

let print_flow_statement ?(print_id=false) ?(print_expr_fn=lazy_expr) fs =
  let my_tag = pretty_tag_str CutHint "" in
  let print_endpoint = print_flow_endpoint ~print_id:print_id ~print_expr_fn:print_expr_fn in
  match fs with
    | Source ep     -> my_tag "Source" [lazy (print_endpoint ep)]
    | Sink ep       -> my_tag "Sink" [lazy (print_endpoint ep)]
    | Bind(i, i2)   -> my_tag "Bind" [lps i; lps i2]
    | Instruction i -> my_tag "Instruction" [lazy (print_instruction i)]
 
let print_flow_program ?(print_id=false) ?(print_expr_fn=lazy_expr) fp =
  let print_fn (fs, _) =
    print_flow_statement ~print_id:print_id ~print_expr_fn:print_expr_fn fs
  in
    ps "["; ps_list ~sep:";" CutLine print_fn fp; ps "]"

let print_declaration ?(print_id=false) ?(print_expr_fn=lazy_expr) d =
  let my_tag ?(cut=CutLine) = pretty_tag_str cut "" in
  match d with
    | Global(i, t, init) ->
      my_tag "Global"
        ([lps (quote i); lazy_type t]@
        (lazy_string_opt (print_expr ~print_id:print_id) init))

    | Foreign(i, t) -> my_tag "Foreign" [lps (quote i); lazy_type t]

    | Flow fp -> my_tag "Flow" [lazy (print_flow_program fp)]
        
    | Role (id, sp)   -> my_tag "Role" [lps (quote id); lazy(print_flow_program sp)]

    | DefaultRole id  -> my_tag ~cut:CutHint "DefaultRole" [lps (quote id)] 

let string_of_base_type bt  = wrap_formatter (fun () -> print_base_type bt)
let string_of_value_type vt = wrap_formatter (fun () -> print_value_type vt)
let string_of_type t        = wrap_formatter (fun () -> print_type t)

let string_of_resource_pattern p = wrap_formatter (fun () -> print_resource_pattern p)
let string_of_flow_resource r    = wrap_formatter (fun () -> print_flow_resource r)
let string_of_flow_statement fs  = wrap_formatter (fun () -> print_flow_statement fs) 
let string_of_flow_program fp    = wrap_formatter (fun () -> print_flow_program fp)

let string_of_arg a         = wrap_formatter (fun () -> print_arg a)
let string_of_expr e        = wrap_formatter (fun () -> print_expr e)
let string_of_declaration d = wrap_formatter (fun () -> print_declaration d)

let string_of_program ?(print_id=false) ?(print_expr_fn=lazy_expr) ss =
  let print_fn (d, meta) =
    let m_str = wrap_unless_empty "<" ">" (string_of_annotation meta) in
    print_declaration 
      ~print_id:print_id ~print_expr_fn:print_expr_fn d;
    (if m_str <> "" then (pc (); ps m_str; pc ()))
  in wrap_formatter (fun () ->
    ps "["; ps_list ~sep:";" CutLine print_fn ss; ps "]")

end  

(* Annotations *)
and AnnotationStrings : StringifyAnnotations = struct

open ASTStrings

let string_of_rigidity r = match r with Constraint -> "Constraint" | _ -> "Hint"

let string_of_data_annotation da =
  let string_of_positions p = "["^(String.concat ";" (List.map string_of_int p))^"]" in
  let my_tag tag p = tag^"("^(string_of_positions p)^")" in
  match da with
  | FunDep  (s,d) -> (string_of_positions s)^"->"^(string_of_positions d)
  | Index   p -> my_tag "Index" p
  | Unique  p -> my_tag "Unique" p
  | Ordered p -> my_tag "Ordered" p
  | Sorted  p -> my_tag "Sorted" p

let string_of_control_annotation ca = match ca with
  | Effect ids -> "Effect("^(String.concat "," ids)^")"
  | Parallel deg -> "Parallel("^string_of_int deg^")" 

(* TODO: expose controls for printing type annotations on expressions *)
let print_ast_annotation ?(show_type=false) a =
  let my_tag = pretty_tag_str CutHint "" in
  match a with
  | Data (r,da) -> my_tag "Data" [lps (string_of_rigidity r); lps (string_of_data_annotation da)]
  | Control (r,ca) -> my_tag "Control" [lps (string_of_rigidity r); lps (string_of_control_annotation ca)]
  | Type t when show_type -> my_tag "Type" [lazy (print_type t)]
  | _ -> ()

let print_annotation ann = ps_list CutHint print_ast_annotation ann

let string_of_annotation ann = wrap_formatter (fun () -> print_annotation ann)

end

include ASTStrings
include AnnotationStrings
