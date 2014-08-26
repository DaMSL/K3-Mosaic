open Format
open Lazy
open Util
open Printing
open Tree

open K3.AST
open K3.Annotation
open K3Util

type config_t = {print_id : bool;
                 verbose : bool;
                 print_expr_fn : config_t -> expr_t -> unit Lazy.t }

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

let string_of_address_and_role (addr, role_opt, alias_opt) =
  (match alias_opt with None -> "" | Some a -> a^":")^
    string_of_address addr^
    (match role_opt with None -> "" | Some r -> "/"^r)

let string_of_container_type t_c = match t_c with
    | TSet  -> "TSet"
    | TBag  -> "TBag"
    | TList -> "TList"
    | TMap  -> "TMap"

let string_of_const cn = match cn with
    | CUnit          -> "CUnit"
    | CUnknown       -> "CUnknown"
    | CBool(b)       -> "CBool("^string_of_bool(b)^")"
    | CInt(i)        -> "CInt("^string_of_int(i)^")"
    | CFloat(f)      -> "CFloat("^string_of_float(f)^")"
    | CString(s)     -> "CString(\""^s^"\")"
    | CAddress(addr) -> "CAddress("^string_of_address addr^")"
    | CTarget(id)    -> "CTarget("^id^")"

let string_of_stop_behavior_t s = match s with
    | UntilCurrent -> "UntilCurrent"
    | UntilEmpty -> "UntilEmpty"
    | UntilEOF -> "UntilEOF"

let string_of_tag_type tag = match tag with
    | Const c  -> "Const"
    | Var i    -> "Var"
    | Tuple     -> "Tuple"
    | Just      -> "Just"
    | Nothing t -> "Nothing"

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
    | MapSelf          -> "MapSelf"
    | Iterate          -> "Iterate"
    | Filter           -> "Filter"
    | Flatten          -> "Flatten"
    | Aggregate        -> "Aggregate"
    | GroupByAggregate -> "GroupByAggregate"
    | Sort             -> "Sort"

    | Slice   -> "Slice"
    | Insert  -> "Insert"
    | Update  -> "Update"
    | Delete  -> "Delete"
    | Peek    -> "Peek"

    | Indirect -> "Indirect"
    | Assign   -> "Assign"
    | Deref    -> "Deref"

    | Send    -> "Send"

(* Flat stringification *)
let rec flat_string_of_base_type t = match t with
    | TUnknown  -> "TUnknown"
    | TUnit     -> "TUnit"
    | TBool     -> "TBool"
    | TByte     -> "TByte"
    | TInt      -> "TInt"
    | TDate     -> "TDate"
    | TFloat    -> "TFloat"
    | TString   -> "TString"

    | TMaybe(t) -> tag_str "TMaybe" [flat_string_of_value_type t]

    | TTuple(t_l) -> tag_str "TTuple" (List.map flat_string_of_value_type t_l)

    | TCollection(t_c, t_e) ->
        tag_str "TCollection"
          [string_of_container_type t_c; flat_string_of_value_type t_e]

    | TAddress -> "TAddress"
    | TTarget(t) -> tag_str "TTarget" [flat_string_of_base_type t]
    | TIndirect(ind) -> tag_str "TIndirect" [flat_string_of_value_type ind]

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
    | Const(cns)  -> tag_str "Const" [string_of_const cns]
    | Var(i)    -> tag_str "Var" [i]
    | Tuple     -> my_tag "Tuple"

    | Just      -> my_tag "Just"
    | Nothing t -> my_tag "Nothing" ~extra:(flat_string_of_value_type t)

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
    | MapSelf          -> my_tag "MapSelf"
    | Iterate          -> my_tag "Iterate"
    | Filter           -> my_tag "Filter"
    | Flatten          -> my_tag "Flatten"
    | Aggregate        -> my_tag "Aggregate"
    | GroupByAggregate -> my_tag "GroupByAggregate"
    | Sort             -> my_tag "Sort"

    | Slice      -> my_tag "Slice"
    | Insert     -> my_tag "Insert"
    | Update     -> my_tag "Update"
    | Delete     -> my_tag "Delete"
    | Peek       -> my_tag "Peek"

    | Indirect   -> my_tag "Indirect"
    | Assign     -> my_tag "Assign"
    | Deref      -> my_tag "Deref"

    | Send       -> my_tag "Send"

(* TODO: Why can't this function be point-free? *)
let flat_string_of_expr expr =
  flat_string_of_tree (fun ((id, tag), _) -> flat_string_of_expr_tag tag) expr

let flat_string_of_channel_type ct = match ct with
  | File fp -> tag_str "File" [fp]
  | Network addr -> tag_str "Network" [string_of_address addr]

let flat_string_of_stream_type = function
  | RandomStream i -> tag_str "RandomStream" [string_of_int i]
  | ConstStream e -> tag_str "ConstStream" [flat_string_of_expr e]

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
    | Stream(t,st)  ->
        tag_str "Stream" [flat_string_of_type t;
                          flat_string_of_stream_type st]

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

    | Foreign(i, t) -> tag_str "Foreign" [i; flat_string_of_type  t]

    | Flow fp -> tag_str "Flow" [flat_string_of_flow_program fp]

    | Role (id, sp)    -> tag_str "Role" [id; flat_string_of_flow_program sp]

    | DefaultRole (id) -> tag_str "DefaultRole" [id]

let flat_string_of_program ss =
  tag_str "K3" (List.map (fun (d,_) ->
    flat_string_of_declaration d) ss)


(*************************
 * Stringification API
 **************************)

(* Module types *)
module type StringifyAST = sig
  val def_c : config_t
  val print_base_type    : config_t -> base_type_t -> unit
  val print_mutable_type : config_t -> mutable_type_t -> unit
  val print_value_type   : config_t -> value_type_t -> unit
  val print_type         : config_t -> type_t -> unit

	val print_arg : config_t -> arg_t -> unit
	val print_expr : config_t -> expr_t -> unit

	val print_resource_pattern : config_t -> resource_pattern_t -> unit
	val print_flow_resource    : config_t -> flow_resource_t -> unit

  val print_flow_statement : config_t -> flow_statement_t -> unit

  val print_flow_program : config_t -> flow_program_t -> unit

  val print_declaration : config_t -> declaration_t -> unit

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

	val string_of_program: ?verbose:bool -> ?print_id:bool ->
    ?print_expr_fn:(config_t -> expr_t -> unit Lazy.t) ->
    program_t -> string
end

module type StringifyAnnotations = sig
  val print_annotation : annotation_t -> unit
  val string_of_annotation : annotation_t -> string
end


(* Module implementations *)
module rec ASTStrings : StringifyAST = struct

open AnnotationStrings

(* Lazy variants *)
let print_expr_id c id = ps ("<"^string_of_int id^"> ")

let rec lazy_annotation a  = lazy (print_annotation a)
and     lazy_base_type c bt  = lazy (print_base_type c bt)
and     lazy_value_type c vt = lazy (print_value_type c vt)
and     lazy_type c t        = lazy (print_type c t)
and     lazy_arg c a         = lazy (print_arg c a)

and     lazy_expr c e = lazy (print_expr c e)

and print_base_type c t =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
  let term_tag t = pretty_tag_term_str t in
  match t with
    | TUnknown  -> term_tag "TUnknown"
    | TUnit     -> term_tag "TUnit"
    | TBool     -> term_tag "TBool"
    | TByte     -> term_tag "TByte"
    | TInt      -> term_tag "TInt"
    | TDate     -> term_tag "TDate"
    | TFloat    -> term_tag "TFloat"
    | TString   -> term_tag "TString"
    | TIndirect vt -> my_tag "TIndirect" [lazy_value_type c vt]

    | TMaybe(t) -> my_tag "TMaybe" [lazy_value_type c t]

    | TTuple(t_l) -> my_tag "TTuple" @: List.map (lazy_value_type c) t_l

    | TCollection(t_c, t_e) ->
        my_tag "TCollection"
          [lps (string_of_container_type t_c); lazy_value_type c t_e]

    | TAddress -> term_tag "TAddress"

    | TTarget(t) -> my_tag "TTarget" [lazy_base_type c t]

and print_mutable_type c mt =
  if c.verbose then
    let my_tag t bt a =
      pretty_tag_str CutHint "" t
        ([lazy_base_type c bt]@(if a = [] then [] else [lazy_annotation a]))
    in match mt with
      | TMutable(bt,a) -> my_tag "TMutable" bt a
      | TImmutable(bt,a) -> my_tag "TImmutable" bt a
  else match mt with
    | TMutable(bt,_) | TImmutable(bt,_) -> print_base_type c bt

and print_value_type c vt =
  if c.verbose then
    let my_tag t mt =
      pretty_tag_str CutHint "" t [lazy (print_mutable_type c mt)]
    in match vt with
      | TIsolated(mt) -> my_tag "TIsolated" mt
      | TContained(mt) -> my_tag "TContained" mt
  else match vt with
      | TIsolated(mt) | TContained(mt) -> print_mutable_type c mt

and print_type c t =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
  match t with
    | TFunction(t_a, t_r) ->
        my_tag "TFunction" (List.map (lazy_value_type c) [t_a; t_r])

    | TValue(t) -> my_tag "TValue" [lazy_value_type c t]

and print_arg c a =
  let my_tag t lazy_ch_t = pretty_tag_str CutHint "" t lazy_ch_t in
  match a with
    | AIgnored -> my_tag "AIgnored" []
    | AVar(i, t)  -> my_tag "AVar" [lps i; lazy_type c (TValue t)]
    | AMaybe(a') -> my_tag "AMaybe" [lazy_arg c a']
    | ATuple(args) -> my_tag "ATuple" (List.map (lazy_arg c) args)

and print_expr_tag c tag lazy_children =
  let my_tag ?(lb="(") ?(rb=")") ?(sep=", ") ?(extra="") t =
    pretty_tag_str ~lb:lb ~rb:rb ~sep:sep CutHint extra t lazy_children
  in
  let my_tag_list = my_tag ~lb:"([" ~rb:"])" ~sep:"; " in
  let ch_tag cut_t t ch = pretty_tag_str cut_t "" t ch in
  let extra_tag t extra = pretty_tag_str CutHint "" t (extra@lazy_children) in
  match tag with
    | Const(ct)  -> ch_tag NoCut "Const" [lps (string_of_const ct)]
    | Var(i)    -> ch_tag NoCut "Var" [lps i]
    | Tuple     -> my_tag_list "Tuple"

    | Just      -> my_tag "Just"
    | Nothing t -> extra_tag "Nothing" [lazy_value_type c t]

    | Empty t     -> extra_tag "Empty" [lazy_value_type c t]
    | Singleton t -> extra_tag "Singleton" [lazy_value_type c t]
    | Combine     -> my_tag "Combine"
    | Range(ct)   -> my_tag "Range" ~extra:(string_of_container_type ct)

    | Add   -> my_tag "Add"
    | Mult  -> my_tag "Mult"
    | Neg   -> my_tag "Neg"
    | Eq    -> my_tag "Eq"
    | Neq   -> my_tag "Neq"
    | Lt    -> my_tag "Lt"
    | Leq   -> my_tag "Leq"

    | Lambda a -> extra_tag "Lambda" [lazy_arg c a]
    | Apply    -> my_tag "Apply"

    | Block      -> my_tag_list "Block"
    | IfThenElse -> my_tag "IfThenElse"

    | Map              -> my_tag "Map"
    | MapSelf          -> my_tag "MapSelf"
    | Iterate          -> my_tag "Iterate"
    | Filter           -> my_tag "Filter"
    | Flatten          -> my_tag "Flatten"
    | Aggregate        -> my_tag "Aggregate"
    | GroupByAggregate -> my_tag "GroupByAggregate"
    | Sort             -> my_tag "Sort"

    | Slice      -> my_tag "Slice"
    | Insert     -> my_tag "Insert"
    | Update     -> my_tag "Update"
    | Delete     -> my_tag "Delete"
    | Peek       -> my_tag "Peek"

    | Indirect   -> my_tag "Indirect"
    | Assign     -> my_tag "Assign"
    | Deref      -> my_tag "Deref"

    | Send       -> my_tag "Send"

and print_expr c expr =
  let id_pr e = if c.print_id then print_expr_id c @: id_of_expr e else () in
  let print_meta e =
    let m_str = wrap_unless_empty "<" ">" (string_of_annotation (meta_of_expr e))
    in pc(); ps m_str; pc()
  in
  let print lazy_ch e =
    id_pr e;
    print_expr_tag c (tag_of_expr e) (List.flatten lazy_ch);
    print_meta e
  in
  let lazy_e =
    fold_tree (fun _ _ -> ()) (fun _ lazy_ch e -> [lazy (print lazy_ch e)]) () [] expr
  in force (List.hd lazy_e)

let print_instruction c i = match i with
    | Consume(id) -> pretty_tag_str CutHint "" "Consume" [lps id]

let rec print_resource_pattern c p =
  let my_tag = pretty_tag_str CutHint "" in
  let lazy_rcr p = lazy (print_resource_pattern c p) in
  let rcr_list l = List.map lazy_rcr l in
  match p with
    | Terminal(id)  -> my_tag "Terminal" [lps id]
    | Choice(ps)    -> my_tag "Choice" (rcr_list ps)
    | Sequence(ps)  -> my_tag "Sequence" (rcr_list ps)
    | Optional(p)   -> my_tag "Optional" [lazy_rcr p]
    | Repeat(p, s)  -> my_tag "Repeat" [lazy_rcr p;
      lps (string_of_stop_behavior_t s)]

let print_flow_resource c r =
  let my_tag = pretty_tag_str CutHint "" in
  match r with
    | Handle (t,ct,cf) ->
      my_tag "Handle" [lazy_type c t;
                       lps (flat_string_of_channel_type ct);
                       lps (flat_string_of_channel_format cf)]

    | Stream (t,st) ->
      my_tag "Stream" [lazy_type c t; lps @: flat_string_of_stream_type st]


    | Pattern p        -> my_tag "Pattern" [lazy (print_resource_pattern c p)]

let print_flow_endpoint c ep =
  let my_tag = pretty_tag_str CutHint "" in
  let print_id_vt (id,vt,_) = lazy (ps ("("^id^", "); print_value_type c vt;
      ps ")")  in
  match ep with
  | Resource (id,r) -> my_tag "Resource" [lps id; lazy (print_flow_resource c r)]
  | Code(i, arg, ds, e) ->
    let decls = lazy(ps "[";
                     ps_list CutLine force (List.map print_id_vt ds); ps "]") in
    my_tag "Code"
      [lps (quote i); lazy_arg c arg; decls; c.print_expr_fn c e]

let print_flow_statement c fs =
  let my_tag = pretty_tag_str CutHint "" in
  let print_endpoint = print_flow_endpoint c in
  match fs with
    | Source ep     -> my_tag "Source" [lazy (print_endpoint ep)]
    | Sink ep       -> my_tag "Sink" [lazy (print_endpoint ep)]
    | Bind(i, i2)   -> my_tag "Bind" [lps i; lps i2]
    | Instruction i -> my_tag "Instruction" [lazy (print_instruction c i)]

let print_flow_program c fp =
  let print_fn (fs, _) =
    print_flow_statement c fs
  in
    ps "["; ps_list ~sep:";" CutLine print_fn fp; ps "]"

let def_c = {print_id=false; verbose=true; print_expr_fn=lazy_expr}

let print_declaration c d =
  let my_tag ?(cut=CutLine) = pretty_tag_str cut "" in
  let print_flow_fn p = print_flow_program c p in
  match d with
    | Global(i, t, init) ->
      my_tag "Global"
        ([lps (quote i); lazy_type c t]@
        (lazy_string_opt (print_expr c) init))

    | Foreign(i, t) -> my_tag "Foreign" [lps (quote i); lazy_type c t]

    | Flow fp -> my_tag "Flow" [lazy (print_flow_fn fp)]

    | Role (id, sp)   -> my_tag "Role" [lps (quote id); lazy(print_flow_fn sp)]

    | DefaultRole id  -> my_tag ~cut:CutHint "DefaultRole" [lps (quote id)]

let string_of_base_type bt  = wrap_formatter (fun () -> print_base_type def_c bt)
let string_of_value_type vt = wrap_formatter (fun () -> print_value_type def_c vt)
let string_of_type t        = wrap_formatter (fun () -> print_type def_c t)

let string_of_resource_pattern p =
  wrap_formatter (fun () -> print_resource_pattern def_c p)
let string_of_flow_resource r =
    wrap_formatter (fun () -> print_flow_resource def_c r)
let string_of_flow_statement fs =
    wrap_formatter (fun () -> print_flow_statement def_c fs)
let string_of_flow_program fp  =
    wrap_formatter (fun () -> print_flow_program def_c fp)

let string_of_arg a         = wrap_formatter (fun () -> print_arg def_c a)
let string_of_expr e        = wrap_formatter (fun () -> print_expr def_c e)
let string_of_declaration d = wrap_formatter (fun () -> print_declaration def_c d)

let string_of_program ?(verbose=true) ?(print_id=false) ?(print_expr_fn=lazy_expr) ss =
  let c = {print_id=print_id; print_expr_fn=print_expr_fn; verbose=verbose} in
  let print_fn (d, meta) =
    let m_str = wrap_unless_empty "<" ">" (string_of_annotation meta) in
    print_declaration c d;
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
  let string_of_dependency d = match d with
    | Element -> "*"
    | Positions p -> string_of_positions p
  in
  let my_tag tag p = tag^"("^(string_of_positions p)^")" in
  match da with
  | FunDep    (s,d) -> (string_of_positions s)^"->"^(string_of_dependency d)
  | MVFunDep  (s,d) -> (string_of_positions s)^"=>"^(string_of_dependency d)
  | Unique  p     -> my_tag "Unique" p
  | Ordered p     -> my_tag "Ordered" p
  | Sequential    -> "Sequential"
  | RandomAccess  -> "RandomAccess"

let string_of_control_annotation ca = match ca with
  | Effect ids -> "Effect("^(String.concat "," ids)^")"
  | Parallel deg -> "Parallel("^string_of_int deg^")"

(* TODO: expose controls for printing type annotations on expressions *)
let print_ast_annotation ?(show_type=false) a =
  let my_tag = pretty_tag_str CutHint "" in
  match a with
  | Data (r,da) -> my_tag "Data" [lps (string_of_rigidity r); lps (string_of_data_annotation da)]
  | Control (r,ca) -> my_tag "Control" [lps (string_of_rigidity r); lps (string_of_control_annotation ca)]
  | Type t when show_type -> my_tag "Type" [lazy (print_type def_c t)]
  | _ -> ()

let print_annotation ann = ps_list CutHint print_ast_annotation ann

let string_of_annotation ann = wrap_formatter (fun () -> print_annotation ann)

end

include ASTStrings
include AnnotationStrings
