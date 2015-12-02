open Util
open Printing
open Lazy
open K3.AST
open K3.Annotation

module U = K3Util
module T = K3Typechecker
module KH = K3Helpers
module D = K3Dist
module KP = K3PrintSyntax

exception MissingType of int * string

let indent = ref 0

let force_list = List.iter force

let def_a = [], false

let r_underscore = Str.regexp "_"

(* lazy functions *)
let lhv i = [lazy (obc i)]     (* open box *)
let lhov i = [lazy (obv i)]
let lv i = [lazy (obx i)]   (* open vertical box *)
let lcb () = [lazy (cb ())]     (* close box *)
(*let lhv 2 = [lazy (obc 2)]*)
let lcut () = [lazy (pbsi 0 0)] (* print nothing or split line *)
let lind () = [lazy (indent := !indent + 2; pbsi 1 !indent)] (* print break with indent or space *)
let undo_indent () = [lazy (indent := !indent - 2)]
let lsp () = [lazy (pbsi 1 !indent)] (* print space or split line *)
let lps s = [lazy (ps s)]      (* print string *)
let lps_list ?(sep=", ") cut_t f l =
  [lazy (ps_list ~sep:sep cut_t (force_list |- f) l)]

(* low precedence joining of lists *)
let (<|) a b = a @ b

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
(* let wrap_indent f = f (* lind () <| f <| undo_indent () *) *)

let lazy_concat ?(sep=lsp) f l =
  let len = List.length l - 1 in
  let l2 = list_populate (fun _ -> sep ()) 0 len in
  List.flatten @@ list_intersperse (List.map f l) l2

(* separator for lazy_concat *)
let lcomma () = lps "," <| lsp ()

(* type we pass all the way down for configuring behaviors *)
type config = {
                env:T.type_bindings_t;  (* type bindings for environment *)
                trig_env:T.type_bindings_t;
                tenv:T.type_bindings_t; (* type aliases *)
                map_to_fold:bool;       (* convert maps and ext to fold, due to k3new limitations *)
                project:StrSet.t;       (* need to project further down *)
                singleton_id:string;
                use_filemux:bool;
                safe_writes:bool;
                use_intmap:bool;
              }

let default_config = {
                       env = [];
                       trig_env = [];
                       tenv = [];
                       map_to_fold = false;
                       project = StrSet.empty;
                       singleton_id="elem";
                       use_filemux=false;
                       safe_writes=false;
                       use_intmap=false;
                     }

let verbose_types_config = default_config

let typecheck e =
  try
    T.type_of_expr e
  with T.TypeError _ ->
    (* try to deduce expression type if missing *)
    T.type_of_expr @@ T.deduce_expr_type ~override:false [] [] [] e

(* light type checking for expressions we create *)
let light_type c e =
  try
     T.deduce_expr_type ~override:false c.trig_env c.env c.tenv e
  with T.TypeError (uuid, name, err) as exc ->
    prerr_string @@ sp "Typechecker Error @%d: %s\n%s\n\n%s" uuid name
      (K3TypeError.string_of_error err) (K3PrintSyntax.string_of_expr ~uuid_highlight:uuid e);
    raise exc

(* Get a binding id from a number *)
let id_of_num i = sp "b%d" i

let abc_str = "abcdefghijklmnopqrstuvwxyz"

let str_op o = match o with
  | OGt -> "gt" | OLt -> "lt" | OLeq -> "leq" | OGeq -> "geq"

(* Get a record id from a number
 * we use a scheme of a..z, za, zb..zz, zza..
 *)
let record_id_of_num ?(prefix="r") i =
  let rec s_of_i acc i =
    if i <= String.length abc_str then
      sp "%s%c" acc (abc_str.[i-1])
    else
      s_of_i (acc ^ "z") (i-String.length abc_str)
  in
  prefix ^ (s_of_i "" i)

(* Add record ids to a list *)
let add_record_ids c ?prefix l =
  match l with
  | []    -> failwith "No list to add record ids to"
  | [x]   -> [c.singleton_id, x]
  | [x;y] -> ["key", x; "value", y] (* to make gbaggs easy *)
  | _     ->
    let i_l = insert_index_fst ~first:1 l in
    List.map (fun (i, x) -> record_id_of_num ?prefix i, x) i_l

let lazy_properties ?(symbol="@:") props f =
  let lazy_anno = function Property(_,s) -> lps s | _ -> assert false in
  let ps = List.filter U.is_property props in
  let annos = List.filter U.is_annotation props in
  let printer symbol l f = match l with
    | []  -> f
    | [p] -> lazy_paren (lazy_paren f <| lps (sp " %s " symbol) <| lazy_anno p)
    | ps  -> lazy_paren (lazy_paren f <| lps (sp " %s{" symbol) <| lps_list NoCut lazy_anno ps <| lps "}")
  in
  printer "@" annos (printer symbol ps f)

(* Add record ids to a string *)
let concat_record_str ?(sep=":") l =
  List.map (fun (s,x) -> sp "%s%s%s" s sep x) @@
  (* remove any ignored fields *)
  List.filter (fun (_,x) -> x <> "_") l

let error () = lps "???"

let lazy_bracket_list f l = lazy_bracket (lps_list NoCut f l)

let has_int_key t = (hd @@ KH.unwrap_ttuple t).typ = TInt

(* print out multi_index. we only support hash indices *)
let rec lazy_multi_index c ss elem_t =
  (* unwrap all the tuples, and use the relevant record ids *)
  let elem_t = KH.unwrap_ttuple elem_t in
  let elem_t = List.map KH.unwrap_ttuple elem_t in
  let no_extract = List.length (List.nth elem_t 0) = 1 in
  let record_ids = List.map (add_record_ids c) elem_t in
  let record_ids = add_record_ids c record_ids in
  let record_ids = List.flatten @@ List.map (fun (nm, l) ->
    if List.length l = 1 then [nm, snd @@ hd l]
    else l) record_ids in

  let index_ls = List.map (List.sort (-)) @@ list_of_intsetset ss in
  (* convert indices to record ids/types *)
  let record_idxs = List.map (List.map (List.nth record_ids)) index_ls in

  lps "{ MultiIndexVMap" <| lps "," <| lsp () <|
  lps_list CutHint (fun idx ->
    lps (if no_extract then "VMapIndex" else "VMapIndexE") <|
    lazy_paren
      (lps "key=[:> " <|
        lps_list CutHint (fun (key, typ) -> lps key <| lps "=>" <| lazy_type c typ) idx <|
        lps "]" <|
       if no_extract then [] else
       (lps "," <| lsp () <|
        lps "extractors=[$#>" <| lsp () <|
        lps_list CutHint (fun (key, _) -> lps key <| lps "=>" <| lps "\"key." <| lps key <|
        lps "\"") idx <| lps "]")
       ))
  record_idxs <| lps "}"

and lazy_mape c elem_t =
  match KH.unwrap_ttuple elem_t with
  | [k; v] ->
    lazy_paren
      (lps "key=[:> key=>" <| lazy_type c k <| lps "]," <| lsp () <|
      lps "value=[:> value=>" <| lazy_type c v <| lps "]")
  | _ -> error ()

and lazy_poly_tags c tags =
  lps "variants=[:#>" <| lsp () <|
  lps_list CutHint (fun (i,s,t) ->
      lps s <| lps " => " <| lazy_type c ~in_col:true t <| lps " : " <| lps (soi i)) tags <|
  lps "]"

and lazy_col c col_t elem_t = match col_t with
  | TSet        -> lps "{Set}"
  | TBag        -> lps "{Collection}"
  | TList       -> lps "{Seq}"
  | TVector     -> lps "{Vector}"
  | TMap when c.use_intmap && has_int_key elem_t -> lps "{IntMap}"
  | TMap        -> lps "{MapE" <| lazy_mape c elem_t <| lps "}"
  | TVMap None  -> lps "{MultiIndexVMap}"
  | TVMap(Some ss) -> lazy_multi_index c ss elem_t
  | TSortedMap  -> lps "{SortedMapE" <| lazy_mape c elem_t <| lps "}"
  | TSortedSet  -> lps "{SortedSet}"
  | TPolyQueue(unique, tag) ->
    let nm = if unique then "Unique" else "Flat" in
    lps (sp "{%sPolyBuffer" nm) <| lazy_paren (lazy_poly_tags c tag) <| lps "}"

and lazy_type ?(brace=true) ?(in_col=false) c t =
  let wrap_props f = lazy_properties ~symbol:"@::" t.anno f in
  let wrap_mut f = if t.mut then lps "mut " <| f else f in
  let wrap_single f =
    let wrap = if brace then lazy_brace else id_fn in
    if in_col then wrap(lps (c.singleton_id^":") <| f) else f
  in
  let wrap = wrap_props |- wrap_single |- wrap_mut in
  match t.typ with
  | TUnit        -> wrap @@ lps "()"
  | TBool        -> wrap @@ lps "bool"
  | TByte        -> wrap @@ lps "byte"
  | TInt
  | TDate        -> wrap @@ lps "int"
  | TFloat       -> wrap @@ lps "real"
  | TString      -> wrap @@ lps "string"
  | TMaybe(vt)   -> lazy_paren (wrap (lps "option " <| lazy_type c ~in_col vt))
  | TAddress     -> wrap @@ lps "address" (* ? *)
  | TTarget bt   -> wrap (lps "target" <| lazy_type c ~in_col bt)
  | TUnknown     -> wrap @@ lps "()"
  | TTop         -> wrap @@ lps "top"
  | TAlias id    -> wrap @@ lps id
  | TIndirect vt -> lazy_paren (wrap (lps "ind " <| lazy_type c ~in_col vt))
  | TTuple(vts)  -> (* tuples become records *)
      let rec_vts = add_record_ids c vts in
      let inner = lazy_concat ~sep:lcomma (fun (id, vt) ->
        lps (id^":") <| lazy_type c vt) rec_vts in
      let wrap_tup = if brace then lazy_brace else id_fn in
      wrap_props @@ wrap_mut (wrap_tup (lsp () <| inner <| lsp ()))
  | TCollection(ct, vt) -> wrap (
    lps "collection " <| lazy_type c ~in_col:true vt <| lps " @ " <| lazy_col c ct vt)
  | TFunction(itl, ot) ->
      lps_list ~sep:" -> " CutHint (lazy_type c) (itl@[ot])

let rec lazy_arg c drop_tuple_paren = function
  | AIgnored      -> lps "_"
  | AVar (id, vt) -> lps id
  | _             -> failwith "shouldn't be here"

let lazy_id_type c (id,t) = lps (id^" : ") <| lazy_type c t

let lazy_const c v =
  let add_float_zero s =
    let l = String.length s in
    if s.[l - 1] = '.' then s^"0" else s
  in
  match v with
  | CUnknown       -> lps "_"
  | CUnit          -> lps "()"
  | CBool true     -> lps "true"
  | CBool false    -> lps "false"
  | CInt i         -> lps @@ string_of_int i
  | CFloat f       -> lps @@ add_float_zero @@ string_of_float f
  | CString s      -> lps @@ sp "\"%s\"" (String.escaped s)
  | CAddress(s, i) -> lps @@ sp "%s:%d" s i
  | CTarget id     -> lps id

(* wrap a const collection expression with collection notation *)
let lazy_collection_vt c vt eval = match (T.repr c.tenv vt).typ with
  | TCollection(ct, et) ->
      (* preceding list of element types *)
      let lazy_elem_list = lazy_type ~brace:false ~in_col:true c et in
      let sep, l, r =
        if eval = [] then [], "{", "}"
        else (lps "|" <| lsp () <| eval, "{|", "|}")
      in
      lps l <| lazy_elem_list <| sep <| lps r <| lps " @ " <| lazy_col c ct et
  | _ -> error () (* type error *)

(* arg type with numbers included in tuples and maybes *)
type arg_num
    = NIgnored
    | NVar      of int * id_t * type_t
    | NMaybe    of int * arg_num
    | NTuple    of int * arg_num list

let rec string_of_anum = function
  | NIgnored        -> "(_)"
  | NVar(i, id, t)  -> sp "NVar(%d, %s)" i id
  | NMaybe(i, arg)  -> sp "NMaybe(%d, %s)" i (string_of_anum arg)
  | NTuple(i, args) -> sp "NTuple(%d, %s)" i (strcatmap string_of_anum args)

(* get an id for the argument at the shallow level for trigger or lambda *)
let shallow_bind_id c ~in_record = function
  | NIgnored        -> "_"
  (* A case of a single variable in an in_record (map etc) *)
  | NVar (i, id, vt) when in_record ->
      begin match (T.repr c.tenv vt).typ with
      | TTuple _ -> id (* we have a single variable representing a tuple -- don't bind *)
      (*| TCollection _ -> id [> single variable representing collection <]*)
      | _        -> id_of_num i
      end
  | NVar (_, id, _) -> id
  | NMaybe (i,_)
  | NTuple (i,_)    -> id_of_num i

(* convert args to an arg type containing numbers for binding *)
(* toplevel args are always a1, a2 etc *)
let arg_num_of_arg a =
  let i = ref 0 in
  let rec loop n = function
    | AIgnored     -> NIgnored
    (* We assign to variables as well for record unpacking *)
    | AVar(id, v)  -> NVar(n, id, v)
    | AMaybe x     -> incr i; NMaybe(n, loop !i x)
    | ATuple xs    ->
      let len = List.length xs in
      let rng = create_range ~first:!i len in
      i := !i + len;
      NTuple(n, List.map2 loop rng xs)
  in loop !i a

(* retrieve the ids from one level of arg_num *)
let get_id_of_arg = function
  | NIgnored     -> "_"
  | NVar(_,id,_) -> id
  | NMaybe(i,_)
  | NTuple(i,_)  -> id_of_num i

(* convert an arg_num to a value type *)
let rec value_type_of_arg_num = function
  | NIgnored        -> KH.canonical TUnknown
  | NVar(_, _, t)      -> t
  | NMaybe(_, a')   -> KH.canonical (TMaybe(value_type_of_arg_num a'))
  | NTuple(_, args) -> KH.canonical (TTuple(List.map value_type_of_arg_num args))

(* this is how we translate tuples to arguments *)
let list_of_top_args = function
  (*| NTuple(_, [_]) as x -> [x]*)
  | NTuple(_, xs)  -> xs
  | x              -> [x]

(* code to unwrap an option type *)
(* project: add a projection out of a record *)
let unwrap_option c ?(project=false) f =
  let p = if project then "."^c.singleton_id else "" in
  lps "case " <| f <| lps " of" <| lsp () <|
  lps (sp "{ Some x -> x%s }" p) <| lsp () <|
  lps "{ None -> error () }"

(* A slice can have other statements inside it. We need to get the inner tuple
 * out, and to make a function that will construct everything inside the lambda
 * once given a bottomed-out inner expression
 *)
let rec extract_slice e =
  match U.tag_of_expr e with
  | Tuple -> U.decompose_tuple e, id_fn
  | Let _ ->
      let ids, bound, expr = U.decompose_let e in
      let tup, sub_expr    = extract_slice expr in
      tup, (KH.mk_let ids bound) |- sub_expr
  | _ -> failwith "extract_slice unhandled expression"

(* create string representing types of a slice and a record *)
let slice_names c pat =
  (* focus on the relevant pattern *)
  let pat' = U.unwrap_tuple @@ hd @@ pat in
  let pat =
    (* if we have a 1-member key, then we want to add record_ids
     * to the full key,value pair. Otherwise, we want to add them
     * to the internal tuple pattern *)
    if List.length pat' = 1 then pat
    else pat'
  in
  let pat = add_record_ids c pat in
  let pat = List.filter (not |- D.is_unknown |- snd) pat in
  String.concat "" (fst_many pat)

let is_tuple e = match U.tag_of_expr e with Tuple -> true | _ -> false

(* get the important part of the pattern out: for a [key, value] it's
 * the id function, but for [[a,b,c], value]] it's the inner tuple.
 * The asymmetry is caused by the fact that we need to preserve the full
 * tuple (including value in the simple case) to get the right record ids *)
let meaningful_pat c pat =
  if is_tuple @@ hd pat then hd pat
  else light_type c @@ KH.mk_tuple pat

(* for vmaps, we encode many of our functions with just a vid inside a tuple. Extract
  * this for the API *)
let maybe_vmap c col pat fun_no fun_yes =
  let col, _ = KH.unwrap_tcol @@ T.type_of_expr col in
  match col with
  | TVMap _ -> begin match U.decompose_tuple pat with
    | vid::rest -> fun_yes vid (light_type c @@ KH.mk_tuple rest)
    | _         -> failwith "missing vid in pattern for vmap"
    end
  | _ -> fun_no pat

(* try pattern matching a bunch of expressions *)
let try_matching e l =
  let rec loop acc_fail = function
    | x::xs -> begin match x () with
                | None -> loop acc_fail xs
                | Some x -> x
                | exception (Failure s) -> loop (acc_fail^"; "^s) xs
                | exception _ -> loop acc_fail xs
               end
    | [] -> failwith @@ sp "No match found for %s: %s" (K3Printing.string_of_expr e) acc_fail
  in loop "" l

(* check if a collection is a vmap *)
let is_vmap c col = match fst @@ KH.unwrap_tcol @@ T.repr c.tenv @@ T.type_of_expr col with
                  | TVMap _ -> true | _ -> false

let verify_vmap c col = if is_vmap c col then () else failwith "Not a vmap"

let is_map c col = match fst @@ KH.unwrap_tcol @@ T.repr c.tenv @@ T.type_of_expr col with
                  | TSortedMap | TMap -> true | _ -> false

let verify_map c col = if is_map c col then () else failwith "Not a map"

let is_sorted_map c col = match fst @@ KH.unwrap_tcol @@ T.repr c.tenv @@ T.type_of_expr col with
                  | TSortedMap -> true | _ -> false

let is_intmap c col =
  if not c.use_intmap then false else
  let t = T.repr c.tenv @@ T.type_of_expr col in
  try
    let t_c, t_e = KH.unwrap_tcol t in
    t_c = TMap && has_int_key t_e
  with Failure _ -> false

let may_v_or_intmap c col pat fn fn_intmap fn_vmap =
  if is_intmap c col then
    match U.decompose_tuple pat with
    | k::_ -> fn_intmap k
    | _ -> failwith "oops"
  else maybe_vmap c col pat fn fn_vmap

let maybe_intmap c col pat fn fn_intmap =
  if is_intmap c col then
    match U.decompose_tuple pat with
    | k::_ -> fn_intmap k
    | _ -> failwith "oops"
  else fn pat

let verify_sorted_map c col = if is_sorted_map c col then () else failwith "Not a sorted map"

let verify_intmap c col = if is_intmap c col then () else failwith "Not an intmap"

let verify_lookup_pat p = if D.is_lookup_pat p then () else failwith "Not a lookup pattern"

(* We return the pattern breakdown: a list, and a lambda forming the internal structure *)
let breakdown_pat pat = match U.tag_of_expr pat with
  | Tuple -> U.decompose_tuple pat, id_fn
  (* if we have a let, we need a proper tuple extraction *)
  | Let _ -> extract_slice pat
  | _     -> [pat], id_fn

(* identify if a lambda is an id lambda *)
let is_id_lambda e =
  (* get a flat list of only the first level of ids *)
  let id_of_arg = function
    | AVar(v,_) -> Some v
    | _         -> None
  in
  let flat_ids_of_arg = function
    | AVar(v,_)  -> [v]
    | ATuple(vs) ->
        let vs' = List.map id_of_arg vs in
        if List.mem None vs' then []
        else flatten_some vs'
    | _          -> []
  in
  let args, body = U.decompose_lambda e in
  let ids = flat_ids_of_arg args in
  if null ids then false
  else match U.tag_of_expr body, ids with
    | Var id, [id'] when id = id' -> true
    | Tuple, _ -> ids = KH.vars_to_ids @@ U.decompose_tuple body
    | _, _     -> false

(* Variable names to translate *)
module StringMap = Map.Make(struct type t = string let compare = String.compare end)
let var_translate = List.fold_left (fun acc (x,y) -> StringMap.add x y acc) StringMap.empty @@
  ["int_of_float", "truncate";
   "float_of_int", "real_of_int";
   "string_of_int", "itos";
   "string_of_float", "rtos";
   "peers", "my_peers2";
   "role", "my_role";
   "parse_sql_date", "tpch_date";
   "maxi", "max";
   "maxif", "max"]

  (* descriptions of how to pass variables. {In,Out}Rec implies that even if we see a non-tuple
   * value, we should turn it into a record (with an 'i' label). This is necessary because of the
   * record restriction in newk3, which states that everything in a collection must be a record *)
let vid_out_arg = [], K3Dist.is_vid_tuple
let vid_in_arg  = K3Dist.is_vid_tuple

(* change a pattern to have a default value (last element) *)
(* since k3new can't handle unknowns *)
(* @map_e: replace all unknowns *except* the value, since we need to drop it *)
let lookup_pat_of_slice ?(vid=false) ~col pat =
  let col_t, elem_t = KH.unwrap_tcol @@ T.type_of_expr col in
  let elem_t = KH.unwrap_ttuple elem_t in
  let elem_t = if vid then KH.t_vid :: elem_t else elem_t in
  (* since this is a lookup, we can turn pat into a list. drop the value *)
  let pat = fst @@ breakdown_pat pat in
  let pat', elem_t =
    if vid then pat, elem_t else list_drop_end 1 pat, list_drop_end 1 elem_t in
  let pat' = List.map2 (fun x t ->
    if D.is_unknown x then KH.default_value_of_t t else x) pat' elem_t in
  if vid then pat' else pat'@[list_last pat]

(* remove the value of a pattern and replace with unknown *)
(* if we're referencing a variable, we need to just project out the key *)
let map_mk_unknown c get_key pat =
  let p = U.unwrap_tuple pat in
  light_type c @@ KH.mk_tuple @@ match p with
  | [_]   when get_key -> [KH.mk_subscript 1 pat; KH.mk_cunknown]
  | [_]                -> [KH.mk_cunknown; KH.mk_subscript 2 pat]
  | [x;_] when get_key -> [x; KH.mk_cunknown]
  | [_;x]              -> [KH.mk_cunknown; x]
  | _ -> failwith "bad pattern"

(* wrap with projection if needed *)
let wrap_project c col x =
  let ts = KH.unwrap_ttuple @@ snd @@ KH.unwrap_tcol @@ T.type_of_expr col in
  if List.length ts = 1 then (lazy_paren x) <| lps ("."^c.singleton_id) else x

(* create a deep bind for lambdas, triggers, and let statements
 * -in_record indicates that the first level of binding should be a record *)
let rec deep_bind ~in_record c arg_n =
  let rec loop d a =
    let record = in_record && d=0 in (* do we want a record now *)
    match a with
      (* pretend to unwrap a record *)
    | NVar(i, id, vt) when record ->
        begin match (T.repr c.tenv vt).typ with
        | TTuple _  -> []    (* don't bind if we have an id representing a record *)
        | _         ->
          (* force bind a variable that comes in as a pretend record *)
          lps "bind " <| lps (id_of_num i) <| lps (" as {"^c.singleton_id^":") <| lps id
          <| lps "} in " <| lcut ()
        end
    | NIgnored
    | NVar _      -> [] (* no binding needed *)
    | NTuple(i, args) ->
        (* only produce binds if we're deeper than specified depth *)
        (if d < 0 then [] else
          let args_id = List.map get_id_of_arg args in
          let rec_ids = List.filter ((<>) "_" |- snd) @@ add_record_ids c args_id in
          (* filter out all the ignored ids *)
          begin match rec_ids with
          | []      -> [] (* if there's nothing to bind, skip it *)
          | rec_ids ->
            let args_rec = concat_record_str rec_ids in
            let sub_ids = lazy_concat ~sep:lcomma lps args_rec in
            lps "bind " <| lps @@ id_of_num i <| lps " as {" <| sub_ids <| lps "} in "
            <| lcut () end)  <|
        (* rest of the binds *)
        List.flatten @@ List.map (loop @@ d+1) args
  | NMaybe(i, arg)  ->
      if d < 0 then [] else
        lps "let " <| lps (get_id_of_arg arg) <| lps " = " <| unwrap_option c (lps @@ id_of_num i) <|
        lps " in" <| lsp () <| loop (d+1) arg
  in loop 0 arg_n

(* Apply a method -- the lambda part *)
and apply_method_nocol ?(dot=true) ?prefix_fn c ~name ~args ~arg_info =
  let wrap_if_big e = match U.tag_of_expr e with
      | Var _ | Const _ | Tuple | Empty _ -> id_fn
      | _ -> lazy_paren
  in
  let args' = list_zip args arg_info in
  let dot_s = if dot then "." else "" in
  lps (dot_s^name) <| lsp () <|
    lazy_concat (fun (e, info) ->
      wrap_if_big e @@ lazy_expr ~expr_info:info ?prefix_fn c e) args'

(* Apply a method to a collection *)
and apply_method ?prefix_fn c ~name ~col ~args ~arg_info =
  (* we only need parens if we're not applying to a variable *)
  let f = match U.tag_of_expr col with
  | Var _ -> id_fn
  | _     -> lazy_paren
    in f @@ lazy_expr c col <| apply_method_nocol c ~name ~args ~arg_info ?prefix_fn

(* common pattern for lookup_with *)
and handle_lookup_with
  ?(vmap=false) ?(intmap=false) ?some_lam_args c ~id
  name col pat e_none e_some =
    (* NOTE: we don't check for lookup_pat. We assume that's already been done *)
    (* we CAN use lookup_with4 *)
    let _, t_elem = KH.unwrap_tcol @@ T.type_of_expr col in
    let pat =
      if intmap then [pat]
      else lookup_pat_of_slice ~vid:vmap ~col pat in
    let lam_args = maybe [id, t_elem] id_fn some_lam_args in
    let arg' =
      [light_type c @@ KH.mk_lambda' ["_", KH.t_unit] e_none;
       light_type c @@ KH.mk_lambda' lam_args e_some] in
    let args, arg_info =
      if vmap then
        [hd pat; light_type c @@ KH.mk_tuple @@ tl pat] @ arg',
          [vid_out_arg; def_a; def_a; [0], false]
      else
        [light_type c @@ KH.mk_tuple pat] @ arg',
          [def_a; def_a; [0], false] in
    some @@ apply_method c ~name ~col ~args ~arg_info

(* apply a function to arguments *)
and function_application c fun_e l_e =
  (* Check if we need to modify the function *)
  (* for example, hash gets only one function in the new k3 *)
  let id, tag, anns, children = U.details_of_expr fun_e in
  let fun_e = match tag with
  | Var x when str_take 4 x = "hash" ->
         U.expr_of_details id (Var "hash") anns children
  | _ -> fun_e in
  let print_fn e = match U.tag_of_expr e with
    | Var _ | Const _ | Tuple   -> lazy_expr c e
    | _                         -> lazy_paren @@ lazy_expr c e
  in
  wrap_indent (lazy_expr c fun_e <| lsp () <| lazy_concat print_fn l_e)

(* handle the printing of a lambda *)
and handle_lambda c ?(expr_info=([],false)) ~prefix_fn arg e =
  (* create a numbering of each argument pattern match *)
  let arg_n = arg_num_of_arg arg in
  let arg_l = list_of_top_args arg_n in
  let in_recs, out_rec = expr_info in
  (* loop over the lambda arguments *)
  (* bindings are lazy binds which we only write at the end *)
  let rec loop i bindings arg =
    let in_record = List.mem i in_recs in
    let write_lambda x =
      lps "\\" <| lps @@ shallow_bind_id c ~in_record x <| lps " ->" <| lsp ()
    in
    match arg with
    | x::xs ->
        (* accumulate deep bindings for this argument *)
        let binds = bindings <| deep_bind ~in_record c x in
        (* write the actual lambda expression for this argument *)
        lazy_paren (write_lambda x <| loop (i+1) binds xs)
        (* write the rest of the expression *)
        (* for the final expr, we may need to wrap the output in a record *)
    | []    -> bindings <| lazy_expr c (prefix_fn e) ~expr_info:([], out_rec)
  in loop 0 [] arg_l


(* create a fold instead of a map or ext (for new k3 typechecking reasons) *)
(* expects a lambda expression, and collection expression inside a map/flattenMap *)
and fold_of_map_ext c expr =
  let open KH in
  let t_out = T.type_of_expr expr in
  (* customize for the different operations *)
  let (lambda, col), suffix = match U.tag_of_expr expr with
    | Map     -> U.decompose_map expr, "map"
    | Flatten -> U.decompose_map @@ U.decompose_flatten expr, "ext"
    | _       -> failwith "Can only convert flatten-map or map to fold"
  in
  let args, body = U.decompose_lambda lambda in
  let acc_id = "_acc"^suffix in
  let acc_arg = AVar (acc_id, t_out) in
  (* because map needs a deep tuple match, we need to remove that for fold *)
  let args' = match args with
    | ATuple xs -> xs
    | x         -> [x] in
  let args' = ATuple(acc_arg :: args') in
  let body' = match U.tag_of_expr expr with
    | Map     -> mk_insert_block acc_id [body]
    | Flatten -> mk_extend_block acc_id body
    | _       -> failwith "Can only convert flatten-map or map to fold"
  in
  lazy_expr c @@ light_type c @@
    KH.mk_agg (KH.mk_lambda args' body') (mk_empty t_out) col

(* convert a slice to a filter function.
 * @frontier: For a vmap, change to a frontier lookup
 *            A vmap cannot ever have a regular slice
 * NOTE: a precise lookup should be picked up by Peek(Slice(x,_)) matching
 *)
and filter_of_slice ~frontier c col pat =
  let tup_t = snd @@ KH.unwrap_tcol @@ T.type_of_expr col in
  let es, lam_fn = breakdown_pat pat in
  let vid_e, es = if frontier then some @@ hd es, tl es else None, es in

  (* prepare record info, and allow deep slice patterns *)
  let rec loop es : ((expr_t -> expr_t) * expr_t) list =
    let i_e = insert_index_fst ~first:1 es in
    (* find the non-unknown slices *)
    let i_e = List.filter (not |- D.is_unknown |- snd) i_e in
    let s_e = List.map (first KH.mk_subscript) i_e in
    (* find if there are any tuples inside the pattern *)
    let tups, no_tups = List.partition (is_tuple |- snd) s_e in
    (* handle the tuples by recursing over them *)
    let tups = List.flatten @@ List.map (fun (subst,e) ->
      let deep = loop @@ U.unwrap_tuple e in
      List.map (fun (s,e) -> s |- subst , e) deep
    ) tups
    in
    no_tups @ tups
  in
  let s_es = loop es in
  (* obvious optimization - no slice needed *)
  if null s_es && not frontier then lazy_expr c col else
  let do_eq e (sub_fn, v) = KH.mk_eq (sub_fn e) v in
  let nm = "_t" in
  let lambda = light_type c @@
    KH.mk_lambda' [nm, tup_t] @@
      lam_fn @@ (* apply an inner lambda constructor *)
      List.fold_right
        (fun x acc -> KH.mk_and acc @@ do_eq (KH.mk_var nm) x)
        (try tl s_es
          with Invalid_argument _ -> [])
        (try do_eq (KH.mk_var nm) @@ hd s_es
          with Invalid_argument _ -> light_type c KH.mk_ctrue)
  in
  let args, arg_info =
    if frontier then [unwrap_some vid_e; lambda], [vid_out_arg; [0], false]
    else [lambda], [[0], false] in
  apply_method c ~name:"filter" ~col ~args ~arg_info

(* printing expressions *)
(* argnums: lambda only   -- number of expected arguments *)
(* prefix_fn: lambda only -- modify the lambda with a prefix *)
(* expr_info: additional info about the expression in the form of a pair of a list
 * of parameters that must be converted to records even if they're not tuples, and a boolean
 * signifying whether the result needs to be converted to a record the same way. In general,
 * only collection members ever need this functionality *)
and lazy_expr ?(prefix_fn=id_fn) ?(expr_info=([],false)) c expr =
  let recur = lazy_expr ~prefix_fn ~expr_info c in

  let expr_pair ?(sep=lps "," <| lsp ()) ?(wl=id_fn) ?(wr=id_fn) (e1, e2) =
    wl(lazy_expr c e1) <| sep <| wr(lazy_expr c e2) in
(* for a stmt of a block *)
 let paren_stmt e = match U.tag_of_expr e with
   | Var _ | Const _ -> id_fn
   | _ -> lazy_paren in
 let is_neg e = match U.tag_of_expr e with
   | Const(CInt i) when i < 0 -> true
   | Const(CFloat f) when f < 0. -> true
   | _ -> false
 in
(* for things like .map *)
 let paren_l e = match U.tag_of_expr e with
    | Var _ | Const _ -> id_fn
    | _ -> lazy_paren in
  let paren_r e = match U.tag_of_expr e with
    | Nothing _ | Just | Const _ | Tuple | Var _
    | Empty _ | Singleton _ -> id_fn
    | _                     -> lazy_paren in
 let arith_paren e = match U.tag_of_expr e with
    | Mult | Add -> lazy_paren
    | Apply -> let e1, _ = U.decompose_apply e in
               begin match U.tag_of_expr e1 with
               | Var "divf" -> lazy_paren
               | Var "mod"  -> lazy_paren
               | _          -> lazy_paren
               end
    | Const x when is_neg e -> lazy_paren
    | Const _ | Var _ -> id_fn
    | _ -> lazy_paren
  (* for == and != *)
  in let logic_paren e = match U.tag_of_expr e with
    | Eq | Neq -> lazy_paren
    | _        -> arith_paren e (* arith ast is used for logic *)
  in let logic_paren_l e = match U.tag_of_expr e with
    | Eq | Neq -> lazy_paren
    | _        -> arith_paren e
  in let arith_paren_pair sym (el, er) =
    let wrapl = arith_paren el in
    let wrapr = arith_paren er in
    expr_pair ~sep:(lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let logic_paren_pair sym (el, er) =
    let wrapl = logic_paren_l el in
    let wrapr = logic_paren er in
    expr_pair ~sep:(lsp () <| lps sym <| lsp ()) ~wl:wrapl ~wr:wrapr (el, er)
  in let expr_type_is_bool e =
    try begin match (T.repr c.tenv @@ T.type_of_expr e).typ with
        | TBool -> true
        | _     -> false
        end
    with T.TypeError(_,_,_) -> false (* assume additive *)
  in
  (* many instructions need to wrap the same way *)
  (* in let wrap e = match U.tag_of_expr expr with
    | Insert _ | Iterate | Map | Filter | Flatten | Send | Delete _ | Update _
    | Aggregate | GroupByAggregate -> wrap_hov 2 e
    | IfThenElse -> wrap_hov 0 e
    | _ -> id_fn e
  in *)
  (* begin analysis of tags *)
  let analyze () = match U.tag_of_expr expr with
  | Const con -> lazy_const c con
  | Var id    ->
      begin try lps @@ StringMap.find id var_translate
      with Not_found ->
        (* check if we need to do projection *)
        if StrSet.mem id c.project then lps id <| lps ("."^c.singleton_id)
        else lps id
      end
  | Tuple     -> let es = U.decompose_tuple expr in
    let id_es = add_record_ids c es in
    (* since we're never supposed to have unknowns here in newk3, we can just
     * filter out unknowns after we added record ids *)
    let id_es = List.filter (not |- D.is_unknown |- snd) id_es in
    let inner = lazy_concat ~sep:lcomma (fun (id, e) ->
        lps (id^":") <| lazy_expr c e) id_es
    in lazy_brace inner
  | Ignore -> let e = U.decompose_ignore expr in
      lps "ignore" <| lsp () <| lazy_paren @@ recur e
  | Just -> let e = U.decompose_just expr in
    lps "Some " <| paren_r e (lazy_expr c e)
  | Nothing vt -> lps "None " <| if vt.mut then lps "mut" else lps "immut"
  | Empty vt   -> lps "empty " <| lazy_collection_vt c vt []
  | Singleton _ ->
    (* Singletons are sometimes typed with unknowns (if read from a file) *)
    let e = U.decompose_singleton expr in
    let t = typecheck expr in
    (* for vmaps, we need to convert to a sequence with empty *)
    if is_vmap c expr then
      let e' =
        let open KH in
        mk_let ["x"] (mk_empty t) @@
          mk_block [
            mk_insert "x" [e];
            mk_var "x" ] in
      lazy_expr c @@ light_type c e'
    else
      lazy_collection_vt c t @@ lazy_expr c e
  | Combine ->
    let rec assemble_list c e =  (* print out in list format *)
      let l, r = U.decompose_combine e in
      begin match U.tag_of_expr l, U.tag_of_expr r with
        | Singleton _, Combine -> let l2 = U.decompose_singleton l in
            lazy_expr c l2 <| lps "," <| lsp () <|
            assemble_list c r
        | Singleton _, Singleton _ -> let l2 = U.decompose_singleton l in
          let r2 = U.decompose_singleton r in
          lazy_expr c l2 <| lps "," <| lsp () <|
            lazy_expr c r2
        | Singleton _, Empty _ -> let l2 = U.decompose_singleton l in
          lazy_expr c l2
        | _ -> error () (* type error *)
      end in
    let e1, e2 = U.decompose_combine expr in
    (* wrap the left side of the combine if it's needed *)
    (* for vmaps, we can't use sugared syntax. Must have ugly expressions *)
    begin match U.tag_of_expr e1, U.tag_of_expr e2 with
      | Singleton vt, Combine
      | Singleton vt, Singleton _
      | Singleton vt, Empty _ when not @@ is_vmap c e1 && not @@ is_vmap c e2 ->
          let t = T.type_of_expr expr in
          lazy_collection_vt c t @@ assemble_list c expr
      | _ -> apply_method c ~name:"combine" ~col:e1 ~args:[e2] ~arg_info:[def_a]
    end

  | Range ct -> let st, str, num = U.decompose_range expr in
    (* newk3 range only has the last number *)
    let range_type = KH.canonical @@ TFunction([KH.t_int], KH.wrap_tlist KH.t_int) in
    function_application c (U.attach_type range_type @@ KH.mk_var "range") [num]
  | Add -> let (e1, e2) = U.decompose_add expr in
    begin match U.tag_of_expr e2, expr_type_is_bool e1 with
      | Neg, false -> let e3 = U.decompose_neg e2 in
        arith_paren_pair "-" (e1, e3)
      | _, false -> arith_paren_pair "+" (e1, e2)
      | _, true -> arith_paren_pair "or" (e1, e2)
    end
  | Mult -> let e1, e2 = U.decompose_mult expr in
    let is_neg = begin match U.tag_of_expr e1 with
      | Neg -> let e = U.decompose_neg e1 in
        begin match U.tag_of_expr e with
          | Const(CInt(1)) -> true
          | _ -> false
        end
      | Const(CInt(-1)) -> true
      | _ -> false
    end in
    begin match expr_type_is_bool e1, is_neg with
      | true, _ -> arith_paren_pair "and" (e1, e2)
      | false, true -> lps "-" <| lazy_expr c e2 (* just a minus *)
      | _ -> arith_paren_pair "*" (e1, e2)
    end
  | Neg -> let e = U.decompose_neg expr in
    let sym = if expr_type_is_bool e then "not " else "-" in
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
      handle_lambda c ~prefix_fn ~expr_info arg e
  | Apply -> let fn, args = U.decompose_apply expr in
    let do_pair_paren sym = match args with
      | [x; y] -> arith_paren_pair sym (x, y)
      | _      -> failwith "not a pair"
    in
    begin match U.tag_of_expr fn, args with (* can be let *)
      (* divide becomes an infix operator *)
      | Var "divf", _     -> do_pair_paren "/"
      | Var "divi", _     -> do_pair_paren "/"
      | Var "mod", _      -> do_pair_paren "%"
      | Var "reciprocali", [e] -> arith_paren_pair "/" (light_type c @@ KH.mk_cfloat 1.0, e)
      | Var "reciprocal", [e]  -> arith_paren_pair "/" (light_type c @@ KH.mk_cint 1, e)
      | Var "concat", _ -> do_pair_paren "++"
      (* convert load_csv to right format *)
      | Var s, _ when s = K3StdLib.csv_loader_name^"2" ->
          begin match List.map U.tag_of_expr args with
          (* get name of var to extract table name *)
          | [Var x; col] ->
              begin match Str.split r_underscore x with
              | [table; _] ->
                  let open KH in
                  let args =
                    List.map (light_type c) @@
                    [ mk_singleton (wrap_tbag t_string) [mk_var x];
                      (* get the witness collection *)
                      list_last args] in
                  let loader = String.uppercase table^"LoaderRP" in
                  apply_method_nocol {c with singleton_id = "path"}
                    ~dot:false ~name:loader ~args ~arg_info:[def_a; def_a]
              | _ -> failwith "bad arg to load_csv_col"
              end
          | _ -> failwith "bad arg to load_csv_col2"
          end
      (* function application *)
      | _ -> function_application c fn args
    end
  | Block -> let es = U.decompose_block expr in
    lazy_paren @@ wrap_indent (wrap_hv 0 @@
      lps_list ~sep:";" CutHint (fun e ->
        wrap_hov 0 @@ paren_stmt e @@ lazy_expr c e) es)

  | IfThenElse -> let e1, e2, e3 = U.decompose_ifthenelse expr in
    wrap_indent (lps "if " <| lazy_expr c e1) <| lsp () <|
    wrap_indent (lps "then" <| lsp () <| lazy_expr c e2) <| lsp () <|
    wrap_indent (lps "else" <| lsp () <| lazy_expr c e3)

  | CaseOf id ->
    let e1, e_some, e_none = U.decompose_caseof expr in

    let normal () = (* normal case printout *)
      Some(
        lps "case" <| lsp () <| lazy_expr c e1 <| lsp () <| lps "of" <| lsp () <|
        wrap_indent (lazy_brace (lps ("Some "^id^" ->") <| lsp () <| lazy_expr c e_some)) <|
        wrap_indent (lazy_brace (lps "None ->" <| lsp () <| lazy_expr c e_none)))
    in
    (* helper function to apply lookup_with on a secondary index *)
    let handle_slice_lookup_with
      col pat e_none e_some =
        let _, t_elem = KH.unwrap_tcol @@ T.type_of_expr col in
        let fn_pat = meaningful_pat c @@ tl @@ U.unwrap_tuple pat in
        let name =
          "lookup_before_by_" ^ (slice_names c @@ U.unwrap_tuple fn_pat) in
        let arg' =
          [light_type c @@ KH.mk_lambda' ["_", KH.t_unit] e_none;
            light_type c @@ KH.mk_lambda' [id, t_elem] e_some] in
        let args, arg_info =
            [hd @@ U.unwrap_tuple pat; light_type c fn_pat] @ arg',
              [vid_out_arg; def_a; def_a; [0], false] in
        some @@ apply_method c ~name ~col ~args ~arg_info
      in

      (* check for a case(peek(slice(...)) *)
      try_matching expr [
        (fun () ->
          let col = U.decompose_peek e1 in
          some @@ try_matching col [

            (* Slice on VMap and full lookup *)
            (fun () ->
              let col, pat = U.decompose_slice col in
              verify_vmap c col;
              verify_lookup_pat pat;
              handle_lookup_with c ~vmap:true ~id "lookup" col pat e_none e_some);

            (* Slice frontier on VMap and full lookup *)
            (fun () ->
              let col, pat = U.decompose_slice_lt col in
              verify_vmap c col;
              verify_lookup_pat pat;
              handle_lookup_with c ~vmap:true ~id "lookup_before"
                col pat e_none e_some);

            (* Slice op on sortedmap -- full lookup *)
            (fun () ->
              let op, col, pat = U.decompose_slice_op col in
              verify_sorted_map c col;
              verify_lookup_pat pat;
              handle_lookup_with c ("lookup_"^str_op op) ~id
                col pat e_none e_some);

            (* Slice on int_map and full lookup *)
            (fun () ->
              let col, pat = U.decompose_slice col in
              verify_intmap c col;
              verify_lookup_pat pat;
              let key = hd @@ U.decompose_tuple pat in
              handle_lookup_with c ~intmap:true ~id "lookup_key" col key e_none e_some);

            (* Slice on map and full lookup *)
            (fun () ->
              let col, pat = U.decompose_slice col in
              verify_map c col;
              verify_lookup_pat pat;
              handle_lookup_with c "lookup" ~id col pat e_none e_some);

            (* Slice on VMap with partial key*)
            (fun () ->
              let col, pat = U.decompose_slice col in
              verify_vmap c col;
              handle_slice_lookup_with col pat e_none e_some);

            (* Slice frontier on VMap with partial lookup *)
            (fun () ->
              let col, pat = U.decompose_slice_lt col in
              verify_vmap c col;
              handle_slice_lookup_with col pat e_none e_some);

            (* AggregateV -> lookup_before *)
            (* In order for an aggregatev to be here, it must have a full key *)
            (fun () ->
              (* we happen to have a bind here, so special case it *)
              let _, t_elem = KH.unwrap_tcol @@ T.type_of_expr col in
              let e0, x, e1 = U.decompose_bind col in
              let lambda, acc, col = U.decompose_aggregatev e1 in
              let col, pat = U.decompose_slice_lt col in
              let e_some' =
                KH.mk_let [id]
                  (KH.mk_apply' (D.flatten_fn_nm @@ KH.unwrap_ttuple t_elem)
                  [KH.mk_var id]) e_some
              in
              (* do bind here *)
              Some(
                lps "bind" <| lsp () <| lazy_expr c e0 <| lsp () <| lps "as"
                <| lsp () <| lps "ind" <| lsp () <| lps x <| lsp () <|
                lps "in" <| lsp () <|
                (unwrap_some @@
                  handle_lookup_with c ~vmap:true ~id
                  "lookup_before" col pat e_none e_some')));

            (fun () ->
              let _, t_elem = KH.unwrap_tcol @@ T.type_of_expr col in
              (* peek_with *)
              let args =
                [light_type c @@ KH.mk_lambda' ["_", KH.t_unit] e_none;
                 light_type c @@ KH.mk_lambda' [id, t_elem] e_some] in
              let arg_info = [def_a; [0], false] in
              Some(apply_method c ~name:"peek" ~col ~args ~arg_info));
          ]);

        normal]

  | BindAs _ -> let bind, id, r = U.decompose_bind expr in
    let c = {c with project=StrSet.remove id c.project} in
    lps "bind" <| lsp () <| wrap_indent(lazy_expr c bind) <| lsp () <| lps "as" <| lsp () <|
    lps "ind" <| lsp () <| lps id <| lsp () <| lps "in" <| lsp () <|
    wrap_indent (lazy_expr c r)

  | Let _ ->
      (* let can be either bind (destruct tuples) or let in new k3 *)
      let ids, bound, bexpr = U.decompose_let expr in
      (* remove ids that need projection *)
      let c =
        {c with project=List.fold_left (fun acc x -> StrSet.remove x acc) c.project ids}
      in
      begin match ids with
        | [id] -> (* let *)
          lps "let" <| lsp () <| lps id <| lsp () <| lps "=" <| lsp () <|
          wrap_indent (lazy_expr c bound) <| lsp () <| lps "in" <| lsp () <|
          wrap_indent (lazy_expr c bexpr)
        | _   ->  (* bind deconstruct *)
            let ids' = concat_record_str @@ add_record_ids c ids in
            lps "bind" <| lsp () <| wrap_indent(lazy_expr c bound) <| lsp () <|
            lps "as" <| lsp () <| lps "{" <| lps_list NoCut lps ids' <|
            lps "}" <| lsp () <| lps "in" <| lsp () <|
            wrap_indent (lazy_expr c bexpr)
      end

  | Iterate -> let lambda, col = U.decompose_iterate expr in
    apply_method c ~name:"iterate" ~col ~args:[lambda] ~arg_info:[[0], false]

  | Map ->
      if c.map_to_fold then fold_of_map_ext c expr
      else (* normal map *)
        let lambda, col = U.decompose_map expr in
        apply_method c ~name:"map" ~col ~args:[lambda] ~arg_info:[[0], true]

  | Filter -> let lf, col = U.decompose_filter expr in
    apply_method c ~name:"filter" ~col ~args:[lf] ~arg_info:[[0], false]

  (* flatten(map(...)) becomes ext(...) *)
  | Flatten ->
      if c.map_to_fold then
        (* both map and ext become folds *)
        fold_of_map_ext c expr
      else (* normal ext *)
        let e = U.decompose_flatten expr in
        (* ext needs an empty type right now to know what to do if the result is empty
        * flatten should always return a bag type since we can't guarantee uniqueness*)
        let t = T.type_of_expr expr in
        let t = match (T.repr c.tenv t).typ with
          | TCollection(TList, x) -> KH.canonical @@ TCollection(TList, x)
          | TCollection(_, x)     -> KH.canonical @@ TCollection(TBag, x)
          | _                     -> failwith "not a collection"
        in
        let empty_c = light_type c @@ KH.mk_empty t in
        begin match U.tag_of_expr e with
        | Map ->
            let lambda, col = U.decompose_map e in
            apply_method c ~name:"ext" ~col ~args:[lambda; empty_c]
              ~arg_info:[[0], false; def_a]
        | _   -> failwith "Unhandled Flatten without map"
        end

  | Aggregate -> let lambda, acc, col = U.decompose_aggregate expr in
    let args, arg_info = match U.unwrap_tuple acc with
      | vid::acc when is_vmap c col -> [vid; lambda] @ acc, [def_a; [1], false; def_a]
      | _ when is_vmap c col -> failwith "Aggregate: missing vid in vmap fold"
      | _ -> [lambda; acc], [[1], false; def_a]
    in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"fold" ~col ~args ~arg_info

  | Equijoin -> let c1, c2, prj1, prj2, f, zero = U.decompose_equijoin expr in
    apply_method c ~name:"equijoinkf_kv" ~col:c1
      ~args:[c2; prj2; f; zero]
      ~arg_info:[def_a; [0], false; [1; 2], false; def_a]

  | AggregateV ->
    let lambda, acc, col = U.decompose_aggregatev expr in
    let normal () =
      let in_recs = if vid_in_arg then [1;2] else [2] in
      some @@ apply_method c ~name:"fold_all" ~col ~args:[lambda; acc]
        ~arg_info:[in_recs, false; def_a]
    in
    (* handle a case of fold_all(slice(...)) *)
    let handle_slice () =
      let col, pat = U.decompose_slice_lt col in
      let vid = hd @@ U.unwrap_tuple pat in
      let pat_no_vid = tl @@ U.unwrap_tuple pat in
      let pat' = meaningful_pat c pat_no_vid in
      (* check if we can just do a lookup *)
      if D.is_lookup_pat pat then
        handle_lookup_with c ~vmap:true ~id:"x" "lookup_before"
        col pat
          (light_type c @@ KH.mk_empty @@ T.type_of_expr acc)
          (KH.mk_singleton (T.type_of_expr acc) [KH.mk_var "x"])
      else
        (* if we have only unknowns, then we must access the full map (ie. a fold) *)
        let as_fold = List.filter (not |- D.is_unknown) (U.unwrap_tuple pat') = [] in
        let name =
          if as_fold then "fold_vid"
          else "fold_slice_vid_by_"^slice_names c pat_no_vid in

        let args, arg_info =
          if as_fold then
            [vid; lambda; acc], [def_a; [2], false; def_a]
          else
            [vid; light_type c pat'; lambda; acc],
            [def_a; [], true; [2], false; def_a] in
        some @@ apply_method c ~name ~col ~args ~arg_info
    in
    try_matching expr [
      handle_slice;
      normal
    ]

  | GroupByAggregate -> let lam1, lam2, acc, col = U.decompose_gbagg expr in
    (* find out if our accumulator is a collection type *)
    apply_method c ~name:"group_by" ~col ~args:[lam1; lam2; acc]
      ~arg_info:[[0], false; [1], false; def_a]

  | Sort -> let lambda, col = U.decompose_sort expr in
    apply_method c ~name:"sort" ~col ~args:[lambda] ~arg_info:[[0; 1], false]
      ~prefix_fn:(fun e -> light_type c @@ KH.mk_if e (KH.mk_cint (-1)) @@ KH.mk_cint 1)

  | Size -> let col = U.decompose_size expr in
    let name = if is_vmap c col then "total_size" else "size" in
    apply_method c ~name ~col ~args:[light_type c KH.mk_cunit]
      ~arg_info:[def_a]

  | AtWith -> let col, idx, lam_none, lam_some = U.decompose_at_with expr in
    if c.safe_writes then
      apply_method c ~name:"safe_at" ~col
        ~args:[idx; lam_none; lam_some] ~arg_info:[def_a; def_a; [0], false]
    else apply_method c ~name:"unsafe_at" ~col
        ~args:[idx; lam_some] ~arg_info:[def_a; [0], false]

  | At -> let col, idx = U.decompose_at expr in
    (* we need to project since we return a value *)
    wrap_project c col (apply_method c ~name:"at" ~col ~args:[idx] ~arg_info:[def_a])

  | MinWith -> let col, lam_none, lam_some = U.decompose_min_with expr in
    apply_method c ~name:"min" ~col
      ~args:[lam_none; lam_some] ~arg_info:[def_a; [0], false]

  | PeekWithVid ->
      try_matching expr [
        (fun () ->
          let col, lam_none, lam_some = U.decompose_peek_with_vid expr in
          let col, pat = U.decompose_slice_lt col in
          let name = "lookup_before_with_vid" in
          if not (D.is_lookup_pat pat) then None else
          let pat = lookup_pat_of_slice ~vid:true ~col pat in
          let vid = hd pat in
          let pat = light_type c @@ KH.mk_tuple @@ tl pat in
          let args = [vid; pat; lam_none; lam_some] in
          let arg_info = [def_a; def_a; def_a; [1], false] in
          some @@ apply_method c ~name ~col ~args ~arg_info);
      ]

  | Peek -> let col = U.decompose_peek expr in
    (* normal peek applications *)
    let normal () =
      let name = if is_vmap c col then "peek_now" else "peek" in
      wrap_project c col (apply_method c ~name ~col ~args:[light_type c @@ KH.mk_cunit] ~arg_info:[def_a])
    in

    (* to handle the case where we have a full slice over a vmap, we need to look ahead *)
    let tag = U.tag_of_expr col in
    let col_t, _ = KH.unwrap_tcol @@ T.type_of_expr col in

    (* common patterns for lookups *)
    let handle_lookup ?(decomp_fn=U.decompose_slice) ?(vmap=false) name col =
      let col, pat = decomp_fn col in
      (* check if we have a specific value rather than an open slice pattern *)
      if D.is_lookup_pat pat then
        (* create a default value for the last member of the slice *)
        let pat = lookup_pat_of_slice ~vid:vmap ~col pat in
        let args, arg_info =
          if vmap then [hd pat; light_type c @@ KH.mk_tuple @@ tl pat], [vid_out_arg; def_a]
          else [light_type c @@ KH.mk_tuple pat], [def_a] in
        apply_method c ~name ~col ~args ~arg_info
      else normal ()
    in
    begin match col_t, tag with
    | TVMap _, Slice              -> handle_lookup ~vmap:true "lookup" col
    | TVMap _, SliceOp(OLt)       -> handle_lookup ~vmap:true
        ~decomp_fn:U.decompose_slice_lt "lookup_before" col
    | (TMap | TSortedMap),  Slice -> handle_lookup "lookup" col
    | TSortedMap,  SliceOp(OGeq)  -> handle_lookup "upper_bound" col
    | _                           -> normal ()
    end

  | Subscript _ -> let i, tup = U.decompose_subscript expr in
      let t = KH.unwrap_ttuple @@ T.type_of_expr tup in
      let id_t = add_record_ids c t in
      let id = fst @@ at id_t (i-1) in
      (paren_l tup @@ lazy_expr c tup) <| lps "." <| lps id

  | SliceOp(OLt) ->
      let col, pat = U.decompose_slice_lt expr in
      if is_vmap c col then
        filter_of_slice ~frontier:true c col pat
      else error ()

  | SliceOp(_) -> error ()

  | Slice ->
      let col, pat = U.decompose_slice expr in
      filter_of_slice ~frontier:false c col pat

  | Extend -> let col, col' = U.decompose_extend expr in
    apply_method c ~col ~name:"extend" ~args:[col'] ~arg_info:[def_a]

  | Insert -> let col, x = U.decompose_insert expr in
    maybe_vmap c col x
      (fun x -> lazy_expr c col <| apply_method_nocol c ~name:"insert" ~args:[x]
          ~arg_info:[[], true])
      (fun vid x -> lazy_expr c col <| apply_method_nocol c ~name:"insert" ~args:[vid;x]
          ~arg_info:[vid_out_arg; [], true])

  | InsertAt -> let col, idx, x = U.decompose_insert_at expr in
    apply_method c ~name:"insert_at" ~args:[idx; x] ~arg_info:[def_a; [], true] ~col

  | SetAll -> let col, x = U.decompose_set_all expr in
    apply_method c ~name:"set_all" ~args:[x] ~arg_info:[[], true] ~col

  | Delete -> let col, x = U.decompose_delete expr in
    (* get rid of the value for maps *)
    let x = if is_map c col then map_mk_unknown c true x else x in
    may_v_or_intmap c col x
      (fun x -> apply_method c ~col ~name:"erase" ~args:[x]
        ~arg_info:[[],true])
      (fun x -> apply_method c ~col ~name:"erase_key" ~args:[x]
        ~arg_info:[def_a])
      (fun vid x -> apply_method c ~col ~name:"erase" ~args:[vid;x]
        ~arg_info:[vid_out_arg; [], true])

  | DeleteAt -> let col, n = U.decompose_delete_at expr in
    (* project output if needed since we return a value *)
    wrap_project c col (apply_method c ~col ~name:"erase_at" ~args:[n] ~arg_info:[def_a])

  | DeleteWith -> let col, key, lam_none, lam_some = U.decompose_delete_with expr in
    maybe_intmap c col key
      (fun _ -> apply_method c ~col ~name:"erase_with"
        ~args:[key; lam_none; lam_some] ~arg_info:[[], true; def_a; [0], false])
      (fun k -> apply_method c ~col ~name:"erase_with"
        ~args:[k; lam_none; lam_some] ~arg_info:[def_a; def_a; [0], false])

  | Pop -> let col = U.decompose_pop expr in
    apply_method c ~col ~name:"pop" ~args:[KH.mk_cunit] ~arg_info:[def_a]

  | DeletePrefix -> let col, x = U.decompose_delete_prefix expr in
    maybe_vmap c col x
      (fun x -> lazy_expr c col <| apply_method_nocol c ~name:"erase_before" ~args:[x]
        ~arg_info:[[], true])
      (fun vid x -> lazy_expr c col <| apply_method_nocol c ~name:"erase_before" ~args:[vid;x]
        ~arg_info:[vid_out_arg; [], true])

  | DeleteAllPrefix -> let col, vid = U.decompose_delete_all_prefix expr in
      lazy_expr c col <| apply_method_nocol c ~name:"erase_all_before" ~args:[vid]
        ~arg_info:[vid_out_arg]

  | ClearAll -> let col = U.decompose_clear_all expr in
      apply_method c ~col ~name:"clear" ~args:[KH.mk_cunit] ~arg_info:[def_a]

  | Update -> let col, oldx, newx = U.decompose_update expr in
    (* get rid of the value for maps *)
    let oldx = if is_map c col then map_mk_unknown c true oldx else oldx in
    if is_intmap c col then
      maybe_intmap c col oldx
        (fun _ -> [])
        (fun o ->
          lazy_expr c col <| apply_method_nocol c ~name:"update_key" ~args:[o;newx]
            ~arg_info:[def_a; [], true])
    else
    maybe_vmap c col newx
      (fun newx ->
        let newx = if is_map c col then map_mk_unknown c false newx else newx in
        lazy_expr c col <| apply_method_nocol c ~name:"update" ~args:[oldx;newx]
        ~arg_info:[[], true; [], true])
      (fun vid newx ->
        lazy_expr c col <| apply_method_nocol c ~name:"update" ~args:[vid;oldx;newx]
        ~arg_info:[vid_out_arg; [], true; [], true])

  | UpdateSuffix -> let col, key, lambda = U.decompose_update_suffix expr in
    begin match U.decompose_tuple key with
    | vid::key ->
        lazy_expr c col <| apply_method_nocol c ~name:"update_after"
        ~args:[vid; light_type c @@ KH.mk_tuple key; lambda]
        ~arg_info:[vid_out_arg; [], true; (if vid_in_arg then [0] else []) @ [1], true]
    | _ -> failwith "UpdateSuffix: bad key"
    end

  | UpdateAtWith -> let col, key, lambda = U.decompose_update_at_with expr in
    apply_method c ~col ~name:"update_at" ~args:[key; lambda] ~arg_info:[def_a; [0], true]

  | UpsertWith -> let col, key, lam_no, lam_yes = U.decompose_upsert_with expr in
    let key = lookup_pat_of_slice ~vid:(is_vmap c col) ~col key in
    may_v_or_intmap c col (light_type c @@ KH.mk_tuple key)
      (fun key -> lazy_expr c col <| apply_method_nocol c ~name:"upsert_with" ~args:[key; lam_no; lam_yes]
        ~arg_info:[[], true; [], true; [0], true])
      (fun key -> lazy_expr c col <| apply_method_nocol c ~name:"upsert_with_key" ~args:[key; lam_no; lam_yes]
        ~arg_info:[def_a; [], true; [0], true])
      (fun vid key -> lazy_expr c col <| apply_method_nocol c ~name:"upsert_with" ~args:[vid; key; lam_no; lam_yes]
        ~arg_info:[vid_out_arg; [], true; [], true; [0], true])

  | UpsertWithBefore -> let col, key, lam_no, lam_yes = U.decompose_upsert_with_before expr in
    let key = lookup_pat_of_slice ~vid:(is_vmap c col) ~col key in
    maybe_vmap c col (light_type c @@ KH.mk_tuple key)
      (fun key -> lazy_expr c col <| apply_method_nocol  c ~name:"upsert_before" ~args:[key; lam_no; lam_yes]
        ~arg_info:[[], true; [], true; [0], true])
      (fun vid key -> lazy_expr c col <| apply_method_nocol  c ~name:"upsert_before" ~args:[vid; key; lam_no; lam_yes]
        ~arg_info:[vid_out_arg; [], true; [], true; [0], true])

  | FilterOp _ ->
    let op, col, filter_val = U.decompose_filter_op expr
    in apply_method c ~name:("filter_"^str_op op) ~col ~args:[filter_val] ~arg_info:[[], true]

  | Assign -> let l, r = U.decompose_assign expr in
    lazy_expr c l <| lsp () <| lps "=" <| lsp () <| lazy_expr c r

  | Indirect -> let x = U.decompose_indirect expr in
    lps "ind" <| lsp () <| lazy_expr c x

  | Send -> let target, addr, args = U.decompose_send expr in
    wrap_indent @@ lazy_paren (expr_pair (target, addr)) <| lps "<- " <|
      lps_list CutHint (lazy_expr c) args

  | PolyIter -> let idx, offset, fn, col = U.decompose_poly_iter expr in
    apply_method c ~name:"traverse2" ~col ~args:[idx; offset; fn] ~arg_info:[def_a; def_a; [], true]
  | PolyFold -> let fn, acc, col = U.decompose_poly_fold expr in
    apply_method c ~name:"foldl" ~col ~args:[fn; acc] ~arg_info:[def_a; def_a]
  | PolyIterTag tag -> let _, idx, offset, fn, col = U.decompose_poly_iter_tag expr in
    apply_method c ~name:("iterate_"^tag) ~col ~args:[idx; offset; fn] ~arg_info:[def_a; def_a; [2], false]
  | PolyFoldTag tag -> let _, idx, offset, fn, acc, col = U.decompose_poly_fold_tag expr in
    apply_method c ~name:("foldl_"^tag) ~col ~args:[idx; offset; fn; acc] ~arg_info:[def_a; def_a; [3], false; def_a]
  | PolyAt tag -> let _, col, idx, offset = U.decompose_poly_at expr in
    apply_method c ~name:(tag^"_at") ~col ~args:[idx; offset] ~arg_info:[def_a; def_a]
  | PolyAtWith tag -> let _, col, idx, offset, lam_none, lam_some = U.decompose_poly_at_with expr in
    apply_method c ~name:(tag^"_safe_at") ~col ~args:[idx; offset; lam_none; lam_some] ~arg_info:[def_a; def_a; def_a; [0], false]
  | PolyInsert tag -> let _, col, v = U.decompose_poly_insert expr in
    apply_method c ~name:("append_"^tag) ~col ~args:[v] ~arg_info:[[], true]
  | PolyTagAt   -> let col, i = U.decompose_poly_tag_at expr in
    apply_method c ~name:("tag_at") ~col ~args:[i] ~arg_info:[def_a]
  | PolyUnpack   -> let col = U.decompose_poly_unpack expr in
    apply_method c ~name:"unpack" ~col ~args:[KH.mk_cunit] ~arg_info:[def_a]
  | PolyReserve   -> let col, x, y, z = U.decompose_poly_reserve expr in
    apply_method c ~name:"reserve" ~col ~args:[x; y; z] ~arg_info:[def_a; def_a; def_a]
  | PolySkip _ -> let all, tag, col, idx, offset = U.decompose_poly_skip expr in
    let nm = if all then "skip_all_" else "skip_" in
    apply_method c ~name:(nm^tag) ~col ~args:[idx; offset] ~arg_info:[def_a; def_a]
  in
  (* check if we need to write a property *)
  let props = U.meta_of_expr expr in
  let analyze () = lazy_properties props (analyze ()) in
  (* check if we need to wrap our output in a tuple (record) *)
  if snd expr_info then
    match U.tag_of_expr expr with
      (* don't wrap a lambda itself *)
    | Lambda _ -> analyze ()
    | _ ->
        (* check the type of the expression *)
        let t =
          try T.type_of_expr expr
          with T.TypeError(id, _, T.UntypedExpression) ->
              raise @@ MissingType(id, K3Printing.string_of_expr expr)
        in
        begin match (T.repr c.tenv t).typ with
        (* unknown is for error function *)
        | TTuple _
        | TUnknown -> analyze () (* no need to wrap *)
        | _        -> lazy_expr ~expr_info:(fst expr_info, false)
                        ~prefix_fn c @@ light_type c @@ KH.mk_tuple ~force:true [expr]
        end
  else analyze ()

let lazy_trigger c id arg vars expr =
  (*let is_block expr = match U.tag_of_expr expr with Block -> true | _ -> false in
  let indent f = if is_block expr
               then lps " " <| f
               else wrap_indent f in*)
  wrap_indent (lps ("trigger "^id) <| lps " : " <|
  lazy_type ~in_col:false c @@ KH.wrap_ttuple @@ KH.type_of_arg arg <| lsp () <|
  lps "=" <| lsp () <| lazy_expr c @@ KH.mk_lambda arg expr <| lcut ())

let channel_format c = function
  | CSV  -> "psv"
  | JSON -> "json"
  | BIN  -> "binary"

let lazy_channel c chan_t chan_f = match chan_t with
  | PolyFileSeq(files, inorder) ->
      lps @@ sp "polyfileseq %s binary raw %s rebatch" files inorder
  | File _ ->
      lps @@ sp "file switch_path text %s" (channel_format c chan_f)
  | Network(str, port) -> lps @@ "socket(\""^str^"\":"^string_of_int port^")"

let rec lazy_resource_pattern c = function
  | Terminal id -> lps id
  | Choice rps  -> lps_list ~sep:"| " CutHint (lazy_resource_pattern c) rps
  | Sequence rps -> lps_list ~sep:" " CutHint (lazy_resource_pattern c) rps
  | Optional rp -> lazy_resource_pattern c rp <| lps "?"
  | Repeat(rp, _) -> lazy_resource_pattern c rp <| lps "*"

let lazy_stream c = function
  | RandomStream i -> lps "random" <| lazy_paren (lps @@ string_of_int i)
  (* k3o must put a collection here, and k3 expects a value, so extract it *)
  | ConstStream e  ->
      begin match KH.list_of_k3_container e with
      | [e] -> lps "value" <| lazy_paren (lazy_expr c e)
      | _   -> failwith "cannot translate stream to k3new"
      end

let lazy_resource c r =
  let common t = lazy_type c t <| lps " = " in
  match r with
  | Handle(t, chan_t, chan_f) -> common t <| lazy_channel c chan_t chan_f
  | Stream(t, st) -> common t <| lazy_stream c st
  | Pattern(pat) -> lps "pattern " <| lazy_resource_pattern c pat

let lazy_flow c e =
  let out =
    match e with
    | Source(Code(id, arg, vars, expr))
    | Sink(Code(id, arg, vars, expr)) -> lazy_trigger c id arg vars expr
    | Source(Resource(id, r)) ->
        lps ("source "^id^" : ") <| lazy_resource c r
    | BindFlow(id1, id2) -> lps @@ "feed "^id1^" |> "^id2
    | _ -> []
  in out <| lcut () <| lcut ()

let lazy_flow_program c fas = lps_list ~sep:"" CutLine (lazy_flow c |- fst) fas

let lazy_declaration c d =
  let out = match d with
  | Global(id, t, expr) ->
      let end_part = match expr with
        | None   -> []
        | Some e -> lps " =" <| lsp () <| lazy_expr c e
      in
      wrap_indent (lps @@ "declare "^id^" :" <| lsp () <| lazy_type c t <| end_part)
  | Role(id, fprog) -> lazy_flow_program c fprog
  | Flow fprog -> lazy_flow_program c fprog
  | DefaultRole id -> []
  | Foreign(id, t) -> lps ("declare "^id^" :") <| lsp () <| lazy_type c t
  | Typedef(id, t) -> lps "typedef " <| lps id <| lps " = " <| lazy_type c t
  in
  wrap_hov 0 out <| lcut ()

let wrap_f = wrap_formatter ~margin:80

(* print a K3 type in syntax *)
let string_of_base_type t = wrap_f @@ fun () ->
  force_list @@ lazy_type verbose_types_config t

(* print a K3 type in syntax *)
let string_of_value_type t = wrap_f @@ fun () ->
  force_list @@ lazy_type verbose_types_config t

(* print a K3 expression in syntax *)
let string_of_expr e = wrap_f @@ fun () ->
  force_list @@ lazy_expr verbose_types_config e

module StringSet = Set.Make(struct type t=string let compare=String.compare end)

(* remove/convert functions that are renamed in k3new *)
let filter_incompatible prog =
  let drop_globals = List.fold_left (flip StringSet.add) StringSet.empty
    ["error"; "divf"; "mod"; "float_of_int"; "int_of_float"; "get_max_int"; "parse_sql_date"; "peers"; ]
  in
  filter_map (fun ((d,a) as dec) ->
    (* we don't want the monomorphic hash functions *)
    match d with
    | Foreign(id, _)   -> None
    | Global(id, _, _) when StringSet.mem id drop_globals -> None
    | DefaultRole _ -> None
    | _             -> Some dec
  ) prog

(* print a K3 program in syntax *)
(* We get the typechecking environments so we can do incremental typechecking where needed *)
let string_of_program ?(map_to_fold=false) ?(use_filemux=false) ?(safe_writes=false) prog (env, tenv, trig_env) =
  let config = {default_config with env; trig_env; tenv; map_to_fold; use_filemux; safe_writes} in
  wrap_f @@ fun () ->
    let l = wrap_hv 0 (lps_list ~sep:"" CutHint (lazy_declaration config |- fst) prog) in
    force_list l

(* print a new k3 program with added sources and feeds *)
(* envs are the typechecking environments to allow us to do incremental typechecking *)
let string_of_dist_program ?(file="default.txt") ~map_to_fold ~use_filemux ~safe_writes warmup_maps wrelnames (p, envs) =
  let p' = filter_incompatible p in
  let map_template (nm, ty) =
"\
declare warmup_map_inpath_"^nm^" : mut string
"
  in
  let map_load_fn wm =
"\
declare load_warmup_maps : () -> () = \\_ -> (
  "^(List.fold_left (fun acc (nm,_) ->
      (if acc = "" then "    " else acc^";\n    ")^
        "    openFile me \"chan"^nm^"\" warmup_map_inpath_"^nm^" \"k3\" false \"r\";\n"
       ^"    "^nm^" = doRead me \"chan"^nm^"\"") "" wm)^"
)
"
  in
"\
include \"Core/Builtins.k3\"
include \"Core/Log.k3\"
include \"Annotation/Map.k3\"
include \"Annotation/Maps/MapE.k3\"
include \"Annotation/Maps/IntMap.k3\"
include \"Annotation/Maps/SortedMap.k3\"
include \"Annotation/Maps/SortedMapE.k3\"
include \"Annotation/Maps/VMap.k3\"
include \"Annotation/MultiIndex/MultiIndexVMap.k3\"
include \"Annotation/Set.k3\"
include \"Annotation/Seq.k3\"
include \"Annotation/Sets/SortedSet.k3\"
include \"Annotation/Vector.k3\"
include \"Annotation/FlatPolyBuffer.k3\"
include \"Annotation/UniquePolyBuffer.k3\"
include \"Core/MosaicBuiltins.k3\"
include \"Core/Profile.k3\"

@:CArgs 2
declare NATIONLoaderRP : collection {path: string} @Collection -> collection {ra:int, rb:string, rc:int, rd:string} @Collection -> collection {ra:int, rb:string, rc:int, rd:string} @Collection
  with effects \\_ -> \\_ -> io

@:CArgs 2
declare REGIONLoaderRP : collection {path: string} @Collection -> collection {ra:int, rb:string, rc:string} @Collection -> collection {ra:int, rb:string, rc:string} @Collection
  with effects \\_ -> \\_ -> io

typedef filechunks = collection {path: string} @Collection
declare switch_mux_inputs : collection {seq:filechunks} @Collection

declare my_peers2 : collection { elem:address } @ {Collection} =
  peers.fold (\\acc -> (\\x -> (acc.insert {elem:x.addr}; acc))) empty { elem:address} @ Collection

declare my_role : collection { elem:string } @ {Collection} =
  role.fold (\\acc -> (\\x -> (acc.insert {elem:x.i}; acc))) empty { elem:string} @ Collection

declare files       : collection {path: string} @Collection
declare seqfiles    : collection {seq: collection {path: string} @Collection} @Collection
declare inorder     : mut string = \"in_order.csv\"
declare eventlog    : mut string = \"events.csv\"
declare msgcountlog : mut string = \"msgcount.csv\"
declare routelog    : mut string = \"routelog.csv\"

sink mosaic_event_sink    : {etag: int, evid: int, pcomponent: int, t: int}     = file eventlog text csv
sink mosaic_msgcount_sink : {etag: int, evid: int, msgempty: int, msgfull: int} = file msgcountlog text csv
sink mosaic_routelog_sink : {etag: int, evid: int, pcomponent: int, rkey: string, rrbucket: int, rrnode: int} = file routelog text csv

declare peer_masters : collection {key: address, value: address} @Map

control IfMachineMaster {
  ?e => (peer_masters.lookup {key: me, value: me} (\\x -> ()) (\\r -> ((if me == r.value then ((print (atos me)); $.[e]) else ()))))
     +> {}
}

declare rebatch : mut int = 0

" ^ (string_of_program ~map_to_fold ~use_filemux ~safe_writes p' envs)^"\n"
  ^ (match warmup_maps with
     | Some(wm) -> String.concat "\n" @@ List.map map_template wm @ [map_load_fn wm]
     | _ -> "")


let string_of_dist_warmup_program ?(file="default.txt") ~map_to_fold ~use_filemux ~safe_writes warmup_maps wrelnames (p, envs) =
  let p' = filter_incompatible p in
  let map_template (nm, ty) =
"\
declare warmup_map_outpath_"^nm^" : mut string

sink sink_"^nm^" : "^(string_of_base_type @@ KH.immut ty)^" = file warmup_map_outpath_"^nm^" binary k3
"
  in
  let map_save_fn wm =
"\
trigger save_warmup_maps : () = \\_ -> (
  (
  "^(List.fold_left (fun acc (nm,_) -> (if acc = "" then "  " else acc^";\n    ")^"(sink_"^nm^", me) <- "^nm) "" wm)^";
    (halt, me) <- ()
  ) @OnCounter(id=[# shutdown], eq=[$ "^(string_of_int @@ List.length wm)^"], reset=[$ false], profile=[$ false])
)

trigger load_inputs : () = \\_ -> (
  "^(List.fold_left (fun acc nm -> acc^nm^"LoaderMosaic "^nm^"Paths "^nm^";\n") "" wrelnames)^"
  (compute_warmup_maps, me) <- ()
)

trigger compute_warmup_maps : () = \\_ -> (
  "^(List.fold_left (fun acc (nm,_) ->
      (if acc = "" then "  " else acc^";\n    ")
        ^"bootstrap_"
        ^(str_drop (String.length "bs_") nm)^" ()") "" wm)^"
)

trigger halt : () = \\_ -> haltEngine()

source go : () = value ()
feed go |> load_inputs
"
  in
"\
include \"Core/Barrier.k3\"
include \"Core/Builtins.k3\"
include \"Core/Log.k3\"
include \"Core/MosaicBuiltins.k3\"
include \"Core/Optimization.k3\"
include \"Core/Profile.k3\"
include \"Annotation/Maps/MapE.k3\"
include \"Annotation/MultiIndex/MultiIndexVMap.k3\"
include \"Distributed/Mosaic.k3\"

@:CArgs 2
declare ORDERSLoaderMosaic : collection {path: string} @Collection
  -> collection { ra:int, rb:int, rc:string, rd:real, re:int, rf:string, rg:string, rh:int, ri:string, rj:int } @ {Collection}
  -> ()
  with effects \\_ -> \\_ -> io

@:CArgs 2
declare CUSTOMERLoaderMosaic : collection {path: string} @Collection
  -> collection { ra:int, rb:string, rc:string, rd:int, re:string, rf:real, rg:string, rh:string, ri:int } @ {Collection}
  -> ()
  with effects \\_ -> \\_ -> io

@:CArgs 2
declare LINEITEMLoaderMosaic : collection {path: string} @Collection
  -> collection { ra:int, rb:int, rc:int, rd:int, re:real, rf:real, rg:real, rh:real, ri:string, rj:string, rk:int, rl:int, rm:int, rn:string, ro:string, rp:string, rq:int } @ {Collection}
  -> ()
  with effects \\_ -> \\_ -> io

declare ORDERSPaths : collection {path: string} @Collection

declare CUSTOMERPaths : collection {path: string} @Collection

declare LINEITEMPaths : collection {path: string} @Collection

" ^(string_of_program ~map_to_fold ~use_filemux ~safe_writes p' envs)^"\n"
  ^(match warmup_maps with
    | Some(wm) -> String.concat "\n" @@ List.map map_template wm @ [map_save_fn wm]
    | _ -> "")

