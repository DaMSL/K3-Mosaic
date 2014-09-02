open Util
open Printing

open K3.AST
open K3Util
open K3Printing
open K3Helpers

exception RuntimeError of int * string

(* Interpreter representation of values *)

module IdMap = Map.Make(struct type t = id_t let compare = String.compare end)

(* add to the id_map from a list *)
let add_from_list map l =
  List.fold_left (fun acc (k, v) ->
    IdMap.add k v acc
  ) map l

let map_modify f key map =
  let oldval =
    try
      Some(IdMap.find key map)
    with Not_found -> None
  in
  match f oldval with
  | None   -> IdMap.remove key map
  | Some v -> IdMap.add key v map

module rec ValueMap : NearMap.S with type key = Value.value_t =
  NearMap.Make(
    struct type t = Value.value_t let compare = compare end)

and ValueMMap : IMultimap.S with type elt = Value.value_t =
  IMultimap.Make(
    struct type t = Value.value_t let compare = compare end)

and Value : sig
  type eval_t = VDeclared of value_t ref
              | VTemp of value_t

  and foreign_func_t = env_t -> env_t * eval_t

  (* arguments to a function/trigger *)
  and frame_t = (id_t * value_t) list

  (* mutable environment, frame environment *)
  and env_t = (value_t ref) IdMap.t * (frame_t list)

  and value_t
      = VUnknown
      | VUnit
      | VBool of bool
      | VInt of int
      | VFloat of float
      | VByte of char
      | VString of string
      | VTuple of value_t list
      | VOption of value_t option
      | VSet of value_t ISet.t
      | VBag of value_t IBag.t
      | VList of value_t IList.t
      | VMap of value_t ValueMap.t
      | VMultimap of ValueMMap.t
      | VFunction of arg_t * expr_t
      | VForeignFunction of arg_t * foreign_func_t
      | VAddress of address
      | VTarget of id_t
      | VIndirect of value_t ref
  end = Value

open Value

(* trigger env is where we store the trigger functions. These functions take the
 * address,
 * scheduler_state (parametrized here to prevent circular inclusion), the
 * environment, value_t of arguments, and produce unit *)
type trigger_env_t = (address -> env_t -> value_t -> unit) IdMap.t
type program_env_t = trigger_env_t * env_t

(* Value stringification *)
let unwrap opt = match opt with Some v -> v | _ -> failwith "invalid option unwrap"

let v_to_list = function
  | VBag m -> IBag.to_list m
  | VSet m -> ISet.to_list m
  | VList m -> IList.to_list m
  | VMap vs -> List.map (fun (k,v) -> VTuple[k;v]) @: ValueMap.to_list vs
  | VMultimap vs -> List.map (fun x -> VTuple x) @: ValueMMap.to_list vs
  | _ -> failwith "(v_to_list): not a collection"

let tag = function
	| VUnknown           -> "VUnknown"
	| VUnit              -> "VUnit"
	| VBool _            -> "VBool"
	| VInt _             -> "VInt"
	| VFloat _           -> "VFloat"
	| VByte _            -> "VByte"
	| VString _          -> "VString"
	| VTuple _           -> "VTuple"
  | VOption _          -> "VOption"
	| VSet _             -> "VSet"
	| VBag _             -> "VBag"
	| VList _            -> "VList"
  | VMap _             -> "VMap"
  | VMultimap _        -> "VMultimap"
	| VFunction _        -> "VFunction"
  | VForeignFunction _ -> "VForeignFunction"
	| VAddress _         -> "VAddress"
	| VTarget _          -> "VTarget"
  | VIndirect _        -> "VIndirect"

let rec repr_of_value v =
  let s_of_col m = String.concat "; " @: List.map repr_of_value @: v_to_list m in
  let paren s = Printf.sprintf "(%s)" s in
  tag v ^
  match v with
	| VBool b                 -> paren @: string_of_bool b
	| VInt i                  -> paren @: string_of_int i
	| VFloat f                -> paren @: string_of_float f
	| VByte c                 -> paren @: string_of_int (Char.code c)
	| VString s               -> paren s
	| VTuple vs               -> paren @: String.concat ", " @: List.map repr_of_value vs
  | VOption None            -> paren "None"
  | VOption(Some x)         -> paren @: repr_of_value x
	| VSet _
	| VBag _
	| VList _
  | VMap _
  | VMultimap _             -> paren @: s_of_col v
	| VFunction (a, b)        -> paren @: Printf.sprintf "%s -> %s" (string_of_arg a) (string_of_expr b)
  | VForeignFunction (a, _) -> paren @: string_of_arg a
	| VAddress (ip, port)     -> paren @: ip^":"^ string_of_int port
	| VTarget id              -> paren @: id
  | VIndirect ind           -> paren @: repr_of_value !ind
  | _                       -> ""


(* mark_points are optional sorted counts of where we want markings *)
let rec print_value ?(mark_points=[]) v =
  let count = ref 0 in
  let rec loop ~mark_points v =
    incr count;
    (* mark if we're at the right point *)
    let mark_points = match mark_points with
      | x::xs when !count = x -> ps "%"; xs
      | xs                    -> xs
    in
    let lazy_value v = lazy(loop v ~mark_points) in
    let print_collection lb rb vs =
      pretty_tag_str ~lb:lb ~rb:rb ~sep:"; " CutHint "" ""
        (List.map lazy_value @: v_to_list vs)
    in
    match v with
    | VUnknown                -> ps "??"
    | VUnit                   -> ps "()"
    | VBool b                 -> ps @: string_of_bool b
    | VInt i                  -> ps @: string_of_int i
    | VFloat f                -> ps @: string_of_float f
    | VByte c                 -> ps @: string_of_int (Char.code c)
    | VString s               -> ps @: Printf.sprintf "\"%s\"" @:
                                   String.escaped s
    | VTuple vs               -> pretty_tag_str CutHint "" ""
                                   (List.map lazy_value vs)
    | VOption None            -> ps "None"
    | VOption vopt            -> pretty_tag_str CutHint "" "Some"
                                   [lazy_value (unwrap vopt)]
    | VSet _ as vs            -> print_collection "{" "}" vs
    | VBag _ as vs            -> print_collection "{|" "|}" vs
    | VList _ as vs           -> print_collection "[" "]" vs
    | VMap _ as vs            -> print_collection "[|" "|]" vs
    | VMultimap _ as vs       -> print_collection "[||" "||]" vs
    | VFunction (a, b)        -> ps "<fun>"
    | VForeignFunction (a, _) -> ps "<foreignfun>"
    | VAddress (ip,port)      -> ps (ip^":"^ string_of_int port)
    | VTarget id              -> ps ("<"^id^">")
    | VIndirect ind           -> pretty_tag_str CutHint "" "ind" [lazy_value !ind]
  in loop ~mark_points v

let string_of_value ?mark_points v = wrap_formatter (fun () -> print_value ?mark_points v)

(* Environment stringification *)
let print_binding (id,v) = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()

(* for a map structure *)
let print_binding_m id v = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()

let print_frame frame = List.iter print_binding frame

let print_env skip_functions (globals, (frames:frame_t list)) =
  let filter_m e = IdMap.filter
    (fun _ -> function
      | VFunction _        -> false
      | VForeignFunction _ -> false
      | _                  -> true)
    e in
  let filter_l l = List.filter
    (function
      | _, VFunction _        -> false
      | _, VForeignFunction _ -> false
      | _                     -> true)
    l in
  ps @: Printf.sprintf "----Globals(%i)----" @: IdMap.cardinal globals; fnl();
  let global_m = IdMap.map (!) globals in
  let global_m' = if not skip_functions then global_m
                  else filter_m global_m in
  IdMap.iter print_binding_m global_m';
  fnl();
  ps @: Printf.sprintf "----Frames(%i)----" @: List.length frames; fnl();
  let frames' = List.map filter_l frames in
  List.iter print_frame frames'

let print_trigger_env env =
  ps @: Printf.sprintf "----Triggers(%i)----" @: IdMap.cardinal env; fnl();
  IdMap.iter (fun id _ -> ps id; fnl()) env

let print_program_env (trigger_env, val_env) =
  print_trigger_env trigger_env;
  print_env false val_env

let string_of_env ?(skip_functions=true) (env:env_t) =
  wrap_formatter (fun () -> print_env skip_functions env)

let string_of_program_env env = wrap_formatter (fun () -> print_program_env env)

(* conversion of things to values *)
let value_of_const = function
  | CUnknown -> VUnknown
  | CUnit -> VUnit
  | CBool b -> VBool b
  | CInt i -> VInt i
  | CFloat f -> VFloat f
  | CString s -> VString s
  | CAddress (ip,port) -> VAddress (ip,port)
  | CTarget id -> VTarget id

(* limited conversion between expr and values *)
let rec value_of_const_expr e = match tag_of_expr e with
    | Tuple   -> let es = decompose_tuple e in
      VTuple(list_map value_of_const_expr es)
    | Const c -> value_of_const c
    | Neg     ->
        begin match tag_of_expr @: decompose_neg e with
        | Const(CInt i)   -> VInt (-i)
        | Const(CFloat f) -> VFloat (-.f)
        | _ -> failwith @: "Negative can only have int or float"
        end
    | t -> failwith @: "value is too complex: "^soi @: Obj.tag @: Obj.repr t

(* Global collection functions for values *)

let map_to_tuple (k,v) = VTuple[k;v]
let mmap_to_tuple x = VTuple x

type 'a t_err_fn = (string -> string -> 'a)

let v_peek err_fn c = match c with
  | VSet m      -> ISet.peek m
  | VBag m      -> IBag.peek m
  | VList m     -> IList.peek m
  | VMap m      -> maybe None (some |- map_to_tuple) @: ValueMap.peek m
  | VMultimap m -> maybe None (some |- mmap_to_tuple) @: ValueMMap.peek m
  | _ -> err_fn "v_peek" "not a collection"

let v_combine err_fn x y = match x, y with
  | VSet m,  VSet m'          -> VSet(ISet.combine m m')
  | VBag m,  VBag m'          -> VBag(IBag.combine m m')
  | VList m, VList m'         -> VList(IList.combine m m')
  | VMap m,  VMap m'          -> VMap(ValueMap.combine m m')
  | VMultimap m, VMultimap m' -> VMultimap(ValueMMap.combine m m')
  | _ -> err_fn "v_combine" "mismatch in collections"

let v_fold err_fn f acc = function
  | VSet m      -> ISet.fold f acc m
  | VBag m      -> IBag.fold f acc m
  | VList m     -> IList.fold f acc m
  | VMap m      -> ValueMap.fold (fun k v acc -> f acc @: VTuple[k;v]) m acc
  | VMultimap m -> ValueMMap.fold (fun acc x -> f acc @: VTuple x) acc m
  | _ -> err_fn "v_fold" "not a collection"

let v_iter err_fn f = function
  | VSet m      -> ISet.iter f m
  | VBag m      -> IBag.iter f m
  | VList m     -> IList.iter f m
  | VMap m      -> ValueMap.iter (fun k v -> f (VTuple[k;v])) m
  | VMultimap m -> ValueMMap.iter (fun x -> f @: VTuple x) m
  | _ -> err_fn "v_iter" "not a collection"

let v_insert err_fn x m = match x, m with
  | _, VSet m             -> VSet(ISet.insert x m)
  | _, VBag m             -> VBag(IBag.insert x m)
  | _, VList m            -> VList(IList.insert x m)
  | VTuple[k;v], VMap m   -> VMap(ValueMap.add k v m)
  | VTuple x, VMultimap m -> VMultimap(ValueMMap.insert x m)
  | _ -> err_fn "v_insert" "invalid input"

let v_delete err_fn x m = match x, m with
  | _, VSet m             -> VSet(ISet.delete x m)
  | _, VBag m             -> VBag(IBag.delete x m)
  | _, VList m            -> VList(IList.delete x m)
  | VTuple [k; v], VMap m -> VMap(ValueMap.remove k m)
  | VTuple x, VMultimap m -> VMultimap(ValueMMap.delete x m)
  | _ -> err_fn "v_delete" "invalid input"

let v_update err_fn oldv newv c = match oldv, newv, c with
  | _,_,VSet m                         -> VSet(ISet.update oldv newv m)
  | _,_,VBag m                         -> VBag(IBag.update oldv newv m)
  | _,_,VList m                        -> VList(IList.update oldv newv m)
  | VTuple[k;v], VTuple[k';v'], VMap m -> VMap(ValueMap.update k v k' v' m)
  | VTuple x, VTuple x', VMultimap m   -> VMultimap(ValueMMap.update x x' m)
  | _ -> err_fn "v_update" "not a collection"

let v_empty err_fn ?(no_multimap=false) = function
  | VSet _      -> VSet(ISet.empty)
  | VBag _      -> VBag(IBag.empty)
  | VList _     -> VList(IList.empty)
  | VMap _      -> VMap(ValueMap.empty)
  | VMultimap m -> if no_multimap then VBag(IBag.empty)
                   else VMultimap(ValueMMap.from_mmap m)
  | _ -> err_fn "v_empty" "not a collection"

let v_empty_of_t = function
  | TSet        -> VSet(ISet.empty)
  | TBag        -> VBag(IBag.empty)
  | TList       -> VList(IList.empty)
  | TMap        -> VMap(ValueMap.empty)
  | TMultimap m -> VMultimap(ValueMMap.init m)

(* sort only applies to list *)
let v_sort err_fn f = function
  | VList m -> VList(IList.sort f m)
  | _ -> err_fn "v_sort" "not a list"

let v_singleton err_fn elem c = match elem, c with
  | _,TSet                   -> VSet(ISet.singleton elem)
  | _,TBag                   -> VBag(IBag.singleton elem)
  | _,TList                  -> VList(IList.singleton elem)
  | VTuple[k;v], TMap        -> VMap(ValueMap.singleton k v)
  | VTuple x, TMultimap idxs -> VMultimap(ValueMMap.singleton idxs x)
  | _ -> err_fn "v_singleton" "not a collection"

(* for v_slice *)
let match_pattern pat_v v =
  let match_or_unknown v1 v2 = match v1 with
    | VUnknown -> true
    | _        -> v1 = v2
  in
  match pat_v, v with
  | VTuple pat_f, VTuple v_f ->
    (try List.for_all2 match_or_unknown pat_f v_f
      with Invalid_argument _ -> false)
  | _, _ -> match_or_unknown pat_v v

let v_slice err_fn pat = function
  | VSet m         -> VSet(ISet.filter (match_pattern pat) m)
  | VBag m         -> VBag(IBag.filter (match_pattern pat) m)
  | VList m        -> VList(IList.filter (match_pattern pat) m)
  | VMap m         -> VMap(ValueMap.filter (fun k v ->
                        match_pattern pat @: VTuple[k;v]) m)
  | VMultimap mm   -> VMultimap(ValueMMap.filter (fun x ->
                        match_pattern pat @: VTuple x) mm)
  | _ -> err_fn "v_slice" "not a collection"

(* only for multimap *)
let v_slice_idx = fun err_fn idxs comps pat c -> match c, comps, pat with
  | VMultimap mm, VTuple comps', VTuple pat' ->
      let comp_fn = function
        | VInt (-1) -> `LT
        | VInt 1    -> `GT
        | _         -> `EQ
      in
      let comps'' = List.map comp_fn comps' in
      VBag(IBag.map mmap_to_tuple @: ValueMMap.slice idxs comps'' pat' mm)
  | _ -> err_fn "v_slice_idx" "bad format"

let v_iter2 err_fn f xs ys = match xs, ys with
  | VList xs, VList ys -> IList.iter2 f xs ys
  | VSet xs, VSet ys -> ISet.iter2 f xs ys
  | VBag xs, VBag ys -> IBag.iter2 f xs ys
  | VMap xs, VMap ys -> ValueMap.iter2 (fun k v v' -> f (VTuple[k;v]) (VTuple[k;v'])) xs ys
  | VMultimap xs, VMultimap ys -> ValueMMap.iter2 (fun x y -> f (VTuple x) (VTuple y)) xs ys
  | _ -> err_fn "v_iter2" "mismatching or no collections"

(* Value comparison. *)
let equal_values ?(neq=false) a b =
  let res = ref true in
  let dummy_err _ _ = res := false in
  let rec check2 left right = match left, right with
    | (VList _ | VSet _ | VBag _ | VMap _ | VMultimap _), _->
        (try v_iter2 dummy_err check2 left right
        with Invalid_argument _ -> res := false)
    | VFloat x, VFloat y ->
      let e = 0.0001 in
      if abs_float(x -. y) > e then res := false
    | x, y -> if x != y then res := false
  in
  check2 a b;
  if neq then not !res else !res

(* Value comparison. Returns a partial list of inequality positions if there are any *)
let find_inequality a b =
  let count = ref 0 in
  let res = ref [] in
  let add_cur () = res := !count :: !res in
  let dummy_err _ _ = add_cur () in
  let rec check2 left right =
    incr count;
    match left, right with
    | (VList _ | VSet _ | VBag _ | VMap _ | VMultimap _), _->
        (try v_iter2 dummy_err check2 left right
        with Invalid_argument _ -> add_cur ())
    | VFloat x, VFloat y ->
      let e = 0.0001 in
      if abs_float(x -. y) > e then add_cur ()
    | x, y -> if x != y then add_cur ()
  in
  check2 a b;
  nub @: !res

let rec type_of_value uuid value =
  let get_typ v = type_of_value uuid v in
  let dummy_err _ _ = None in
  let col_get () = maybe t_unit get_typ @: v_peek dummy_err value in
  match value with
  | VUnknown           -> canonical TUnknown
  | VUnit              -> t_unit
  | VBool b            -> t_bool
  | VInt _             -> t_int
  | VFloat _           -> t_float
  | VByte _            -> t_string
  | VString _          -> t_string
  | VAddress (_,_)     -> t_addr
  | VTarget id         -> canonical @: TTarget(TUnknown) (* We don't have the ids *)
  | VOption None       -> wrap_tmaybe @: canonical TUnknown
  | VOption (Some v)   -> wrap_tmaybe @: type_of_value uuid v
  | VTuple vs          -> wrap_ttuple @: List.map (type_of_value uuid) vs
  | VBag _             -> wrap_tbag @: col_get ()
  | VSet _             -> wrap_tset @: col_get ()
  | VList _            -> wrap_tlist @: col_get ()
  | VMap _             -> wrap_tmap @: col_get ()
  | VMultimap mm       -> wrap_tmmap (ValueMMap.get_idxs mm) @: col_get ()
  | VFunction _
  | VForeignFunction _ -> raise (RuntimeError (uuid, "type_of_value: cannot apply to function"))
  | VIndirect ind      -> type_of_value uuid !ind

let rec expr_of_value uuid value =
  let handle_cols vs =
    let l = List.map (expr_of_value uuid) @: v_to_list vs in
    k3_container_of_list (type_of_value uuid value) l
  in
  match value with
  | VUnknown -> mk_const CUnknown
  | VUnit -> mk_const CUnit
  | VBool b -> mk_const @: CBool b
  | VInt i -> mk_const @: CInt i
  | VFloat f -> mk_const @: CFloat f
  | VByte b -> mk_const @: CString(string_of_int @: Char.code b)
  | VString s -> mk_const @: CString s
  | VAddress (ip,port) -> mk_const @: CAddress (ip,port)
  | VTarget id -> mk_const @: CTarget id
  | VOption(None) -> mk_nothing t_unknown
  | VOption(Some v) -> mk_just @: expr_of_value uuid v
  | VTuple vs -> mk_tuple @: List.map (expr_of_value uuid) vs
  | VSet vs -> handle_cols value
  | VList vs -> handle_cols value
  | VBag vs -> handle_cols value
  | VMap vs -> handle_cols value
  | VMultimap vs -> handle_cols value
  | VIndirect ind -> mk_ind @: expr_of_value uuid !ind
  | VFunction _ | VForeignFunction _ -> raise (RuntimeError (uuid,
      "expr_of_value: cannot apply to function"))

