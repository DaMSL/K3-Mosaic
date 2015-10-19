open Util
open Printing

open K3.AST
open K3Util
open K3Printing
open K3Helpers

exception RuntimeError of int * string * string

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

module rec OrderedKey : ICommon.OrderedKeyType = struct
    type t = Value.value_t
    let compare a b = ValueComp.compare_v a b
    let hash a = ValueComp.hash a
    let to_string = ValueUtils.repr_of_value
end

and ValueMap : NearMap.S with type key = Value.value_t = NearMap.Make(OrderedKey)

and ValueVMap : IVMap.S with type key = Value.value_t
                        and  type vid = Value.value_t = IVMap.Make(OrderedKey)(OrderedKey)

and ValueHashMap : HashMap.S with type key = Value.value_t = HashMap.Make(OrderedKey)

and ValueBag : IBag.S with type elt = Value.value_t
                      and type t = int HashMap.Make(OrderedKey).t
                         = IBag.Make(OrderedKey)


and ValueSet : ISet.S with type elt = Value.value_t
                      and type t = unit HashMap.Make(OrderedKey).t
                         = ISet.Make(OrderedKey)

and ValueSSet : NearSet.S with type elt = Value.value_t = NearSet.Make(OrderedKey)

and ValueComp : (sig val compare_v : Value.value_t -> Value.value_t -> int
                     val hash : Value.value_t -> int
                     val reset_counter : unit -> unit
                     val get_counter : unit -> int
                 end) = struct
    open Value
    exception Mismatch of int
    let counter = ref 0 (* for pinpointing errors *)

    let precision = 2. (* for floats *)
    let hash_mult = 10. ** 2.
    let comp_diff = 1. /. hash_mult

    let get_counter () = !counter
    let reset_counter () = counter := 0

    (* get a difference between float numbers *)
    let float_diff f f' =
      let d = f -. f' in
      if d > comp_diff then 1
      else if d < -. comp_diff then -1
      else 0

    let rec compare_v a b =
      incr counter;
      match a,b with
      | _,    VMax -> -1
      | VMax, _    -> 1
      | _,    VMin -> 1
      | VMin, _    -> -1
      | VTuple vs, VTuple vs' ->
          begin try
            List.iter2 (fun x y -> let r = compare_v x y in
                         if r <> 0 then raise (Mismatch r)) vs vs'; 0
          with Mismatch r -> r end
      | VOption(Some v), VOption(Some v') -> compare_v v v'
      | VSet v, VSet v' -> ValueSet.compare v v'
      | VBag v, VBag v' -> ValueBag.compare v v'
      | VList v, VList v' -> IList.compare compare_v v v'
      | VVector(v,_,_), VVector(v',_,_) -> IntMap.compare compare_v v v'
      | VMap v, VMap v' -> ValueMap.compare compare_v v v'
      | VSortedMap v, VSortedMap v' -> ValueMap.compare compare_v v v'
      | VSortedSet v, VSortedSet v' -> ValueSSet.compare v v'
      | VVMap v, VVMap v' -> ValueVMap.compare compare_v v v'
      | VIndirect v, VIndirect v' -> compare_v !v !v'
      | VInt v, VInt v' -> v - v'
      | VInt i, VFloat f -> float_diff (foi i) f
      | VFloat f, VInt i -> float_diff f (foi i)
      | VFloat v, VFloat v' ->
          let (f, i), (f', i') = frexp v, frexp v' in
          let d = i - i' in
          if d <> 0 then d
          else float_diff f f'
      | x, y -> compare x y (* generic comparison *)

    (* try to get a consistent hashing scheme based on members *)
    let rec hash (v:value_t) =
      (* hash members only *)
      let col_hash fold_fn v = fold_fn (fun acc x -> hash x lxor acc) 0 v in
      let col_hashrev fold_fn v = fold_fn (fun x acc -> hash x lxor acc) v 0 in
      let map_hash fold_fn v = fold_fn (fun k v acc -> hash k lxor hash v lxor acc) v 0 in
      let vmap_hash fold_fn v = fold_fn (fun t k v acc -> hash t lxor hash k lxor hash v lxor acc) v 0 in
      match v with
      | VTuple vs       -> col_hash List.fold_left vs
      | VOption(Some v) -> hash v
      | VIndirect v     -> hash !v
      | VSet v          -> col_hash ValueSet.fold v
      | VBag v          -> col_hash ValueBag.fold v
      | VList v         -> col_hash IList.fold v
      | VVector(v,_,_)  -> IntMap.fold (fun k (v:value_t) acc -> Hashtbl.hash k lxor hash v lxor acc) v 0
      | VMap v          -> map_hash ValueMap.fold v
      | VSortedMap v    -> map_hash ValueMap.fold v
      | VSortedSet v    -> col_hashrev ValueSSet.fold v
      | VVMap v         -> vmap_hash ValueVMap.fold_all v
      (* floats need to be hashed in a way that won't make them impossible to distinguish *)
      | VFloat v        -> let f, i = frexp v in
                           Hashtbl.hash i lxor (Hashtbl.hash @@ floor @@ f *. hash_mult)
      | x               -> Hashtbl.hash x

  end

and Value : sig
  type eval_t = VDeclared of value_t ref
              | VTemp of value_t

  and foreign_func_t = env_t -> env_t * eval_t

  (* global environment, frame environment *)
  (* the global env needs to be separate for closures *)
  and local_env_t = value_t list IdMap.t
  and global_env_t = (value_t ref) IdMap.t
  (* trigger env is where we store the trigger functions. These functions take the
  * address,
  * scheduler_state (parametrized here to prevent circular inclusion), the
  * environment, value_t of arguments, and produce unit *)
  and trigger_env_t = (address -> env_t -> value_t list -> unit) IdMap.t
  (* keep track of what was modified *)
  and env_t = {
        triggers: trigger_env_t;
        globals:  global_env_t;
        locals:   local_env_t;
        accessed: StrSet.t ref;
        type_aliases:(id_t, type_t) Hashtbl.t;
      }
  and fun_typ = FLambda | FGlobal of id_t | FTrigger of id_t

  and value_t
      = VMax
      | VMin
      | VUnknown
      | VUnit
      | VBool of bool
      | VInt of int
      | VFloat of float
      | VByte of char
      | VString of string
      | VTuple of value_t list
      | VOption of value_t option
      | VSet of ValueSet.t
      | VBag of ValueBag.t
      | VList of value_t IList.t
      | VVector of value_t IntMap.t * int (* size *) * value_t (* for empty values *)
      | VMap of value_t ValueMap.t
      | VSortedMap of value_t ValueMap.t
      | VSortedSet of ValueSSet.t
      | VVMap of value_t ValueVMap.t
      | VPolyQueue of (value_t * string * value_t) IntMap.t * poly_tags
                      (* itag, stag, value *)
      | VFunction of fun_typ * arg_t * local_env_t * expr_t (* closure *)
      | VForeignFunction of id_t * arg_t * foreign_func_t
      | VAddress of address
      | VTarget of id_t
      | VIndirect of value_t ref
  end = Value

and ValueUtils : (sig val v_to_list : Value.value_t -> Value.value_t list
                      val tag : Value.value_t -> string
                      val repr_of_value : Value.value_t -> string
                  end) = struct
  open Value

  let map_to_tuple (k,v)    = VTuple[k;v]
  let vmap_to_tuple (t,k,v) = VTuple[t;k;v]
  let mmap_to_tuple x       = VTuple x

    (* Value stringification *)
    let v_to_list = function
      | VBag m  -> ValueBag.to_list m
      | VSet m  -> ValueSet.to_list m
      | VSortedSet m  -> ValueSSet.to_list m
      | VList m -> IList.to_list m
      | VVector(m,sz,v_def) ->
        List.map (fun i -> try IntMap.find i m with Not_found -> v_def) @@ create_range sz
      | VMap m | VSortedMap m -> List.map map_to_tuple @@ ValueMap.to_list m
      | VVMap m -> List.map vmap_to_tuple @@ ValueVMap.to_list m
      | VPolyQueue(m,_) -> List.map (fun (i,(v,s,v')) -> VTuple[VInt i; v; VString s; v']) @@
          IntMap.to_list m
      | _ -> failwith "(v_to_list): not a collection"

    let tag = function
      | VMax               -> "VMax"
      | VMin               -> "VMin"
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
      | VVector _          -> "VVector"
      | VMap _             -> "VMap"
      | VSortedMap _       -> "VSortedMap"
      | VSortedSet _       -> "VSortedSet"
      | VVMap _            -> "VVMap"
      | VPolyQueue _       -> "VPolyQueue"
      | VFunction _        -> "VFunction"
      | VForeignFunction _ -> "VForeignFunction"
      | VAddress _         -> "VAddress"
      | VTarget _          -> "VTarget"
      | VIndirect _        -> "VIndirect"

    let rec repr_of_value v =
      let s_of_col m = String.concat "; " @@ List.map repr_of_value @@
        List.sort ValueComp.compare_v @@
        v_to_list m in
      let paren s = sp "(%s)" s in
      tag v ^
      match v with
      | VBool b                 -> paren @@ string_of_bool b
      | VInt i                  -> paren @@ string_of_int i
      | VFloat f                -> paren @@ string_of_float f
      | VByte c                 -> paren @@ string_of_int (Char.code c)
      | VString s               -> paren s
      | VTuple vs               -> paren @@ String.concat ", " @@ List.map repr_of_value vs
      | VOption None            -> paren "None"
      | VOption(Some x)         -> paren @@ repr_of_value x
      | VSet _ | VBag _ | VList _ | VVector _ | VMap _ | VVMap _ | VSortedMap _ | VSortedSet _
                                -> paren @@ s_of_col v
      | VFunction (_, a, _, b)  -> paren @@ sp "%s -> %s" (string_of_arg a) (string_of_expr b)
      | VForeignFunction (i, a, _) -> paren @@ string_of_arg a
      | VAddress (ip, port)     -> paren @@ ip^":"^ string_of_int port
      | VTarget id              -> paren @@ id
      | VIndirect ind           -> paren @@ repr_of_value !ind
      | _                       -> ""
  end

open Value

include ValueUtils

let rec v_col_of_t ?elem t_col = match t_col with
  | TSet        -> VSet(ValueSet.empty)
  | TBag        -> VBag(ValueBag.empty)
  | TList       -> VList(IList.empty)
  | TVector     -> begin match elem with
    | None   -> failwith "vector requires elem type"
    | Some (ta_env, t) -> VVector(IntMap.empty, 0, v_of_t ta_env t)
    end
  | TMap        -> VMap(ValueMap.empty)
  | TVMap _     -> VVMap(ValueVMap.empty)
  | TSortedMap  -> VSortedMap(ValueMap.empty)
  | TSortedSet  -> VSortedSet(ValueSSet.empty)
  | TPolyQueue tags -> VPolyQueue(IntMap.empty, tags)

and v_of_t ta_env ?id t =
  let v_of_t = v_of_t ta_env ?id in
  match t.typ with
  | TTop | TUnknown  -> VUnknown
  | TUnit            -> VUnit
  | TBool            -> VBool false
  | TInt | TDate     -> VInt 0
  | TFloat           -> VFloat 0.
  | TString          -> VString ""
  | TMaybe _         -> VOption None
  | TTuple l         -> VTuple(List.map v_of_t l)
  | TAddress         -> VAddress("0.0.0.0", 0)
  | TIndirect t      -> VIndirect(ref @@ v_of_t t)
  | TCollection(col,elem) -> v_col_of_t col ~elem:(ta_env, elem)
  | TTarget _        -> failwith @@ "No default value for target "^unwrap_option "" id
  | TFunction _      -> failwith @@ "No default value for function "^unwrap_option "" id
  | TByte            -> failwith "Bytes are not implemented"
  | TAlias id        -> v_of_t @@ Hashtbl.find ta_env id


let matching_collections v v' = match v, v' with
  | VList _, VList _
  | VVector _, VVector _
  | VBag _, VBag _
  | VSet _, VSet _
  | VMap _, VMap _
  | VSortedMap _, VSortedMap _
  | VSortedSet _, VSortedSet _
  | VVMap _, VVMap _ -> true
  | _ -> false

let unwrap_vtuple = function VTuple x -> x | x -> [x]
let encode_tuple (k,v)    = VTuple[k;v]
let mencode_tuple x       = VTuple x
(* split key and value properly *)
let split_kv_raw kv =
  let rec loop = function
  | []   -> failwith "empty kv values"
  | [v]  -> [], v
  | x::y -> let l, r = loop y in
            x::l, r
  in loop kv

let wrap_vtuple = function
  | []  -> failwith "empty vtuple"
  | [k] -> k
  | ks  -> VTuple ks

let split_kv kv = first (wrap_vtuple) @@ split_kv_raw kv

let split_tkv = function
  | VTuple(t::kv) ->
      let k, v = split_kv kv in
      t, k
  | _ -> failwith "not a tuple"

let default_env = {
  triggers=IdMap.empty;
  globals=IdMap.empty;
  locals=IdMap.empty;
  accessed=ref StrSet.empty;
  type_aliases=Hashtbl.create 10;
}

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
    let print_collection ?(sort=false) lb rb vs =
      let sort_fn = if sort then List.sort ValueComp.compare_v else id_fn in
      pretty_tag_str ~lb:lb ~rb:rb ~sep:"; " CutHint "" ""
        (List.map lazy_value @@ sort_fn @@ ValueUtils.v_to_list vs)
    in
    let sort = true in
    match v with
    | VUnknown                -> ps "??"
    | VMax                    -> ps "VMax"
    | VMin                    -> ps "VMin"
    | VUnit                   -> ps "()"
    | VBool b                 -> ps @@ string_of_bool b
    | VInt i                  -> ps @@ string_of_int i
    | VFloat f                -> ps @@ string_of_float f
    | VByte c                 -> ps @@ string_of_int (Char.code c)
    | VString s               -> ps @@ sp "\"%s\"" @@
                                   String.escaped s
    | VTuple vs               -> pretty_tag_str CutHint "" ""
                                   (List.map lazy_value vs)
    | VOption None            -> ps "None"
    | VOption vopt            -> pretty_tag_str CutHint "" "Some"
                                   [lazy_value (unwrap_some vopt)]
    | VSet _ as vs            -> print_collection ~sort "{" "}" vs
    | VBag _ as vs            -> print_collection ~sort "{|" "|}" vs
    | VList _ as vs           -> print_collection "[" "]" vs
    | VVector _ as vs         -> print_collection "[#" "#]" vs
    | VMap _ as vs            -> print_collection ~sort "[:" ":]" vs
    | VSortedMap _ as vs      -> print_collection ~sort "{<" ">}" vs
    | VSortedSet _ as vs      -> print_collection ~sort "{:" ":}" vs
    | VVMap _ as vs           -> print_collection ~sort "[<" ">]" vs
    | VPolyQueue _ as vs      -> print_collection "[?" "?]" vs
    | VFunction _             -> ps "<fun>"
    | VForeignFunction (_, a, _) -> ps "<foreignfun>"
    | VAddress (ip,port)      -> ps (ip^":"^ string_of_int port)
    | VTarget id              -> ps ("<"^id^">")
    | VIndirect ind           -> pretty_tag_str CutHint "" "ind" [lazy_value !ind]
  in loop ~mark_points v

let string_of_value ?mark_points v = wrap_formatter (fun () -> print_value ?mark_points v)
let sov = string_of_value

(* Environment stringification *)
let print_binding (id,v) = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()

(* Value comparison. *)
let equal_values a b = ValueComp.compare_v a b = 0

let compare_values f a b = f (ValueComp.compare_v a b) 0

(* Value comparison. Returns a partial list of inequality positions if there are any *)
let find_inequality a b =
  ValueComp.reset_counter ();
  if ValueComp.compare_v a b <> 0 then
    Some(ValueComp.get_counter ())
  else None

let is_vmap = function VVMap _ -> true | _ -> false

let strip_vid = function
  | VTuple [t;k;v'] -> VTuple [k;v']
  | v -> v

let v_peek ?(vid=false) err_fn c = match c with
  | VSet m -> ValueSet.peek m
  | VSortedSet m -> ValueSSet.peek m
  | VBag m  -> ValueBag.peek m
  | VList m -> IList.peek m
  | VVector(m,s,v) when s > 0 -> begin try some @@ IntMap.find 0 m with Not_found -> Some v end
  | VVector(_,_,_) -> None
  | VMap m | VSortedMap m -> maybe None (some |- encode_tuple)  @@ ValueMap.peek m
  | VVMap m -> maybe None (fun (t,k,v) -> some @@ if vid then VTuple[t;k;v] else VTuple[k;v]) @@ ValueVMap.peek m
  | v -> err_fn "v_peek" @@ sp "not a collection: %s" @@ sov v

let v_at ?(extend=false) ?tag ?(get_stag=false) ?(get_itag=false) err_fn c idx = match c, idx with
  | VVector _ as v, _ when get_itag || get_stag || tag <> None ->
        err_fn "v_at" @@ "not a polyqueue: "^sov v

  | VVector(m,sz,t), VInt i ->
      begin try some @@ IntMap.find i m
      with Not_found -> if (i >= 0 && i < sz) || extend then some t else None end

  | VVector _, v -> err_fn "v_at" @@ sp "not an integer: %s" @@ sov v

  | VPolyQueue(m,_), VInt i ->
    begin try
      let itag, stag, v = IntMap.find i m in
      if get_itag then some itag
      else if get_stag then some @@ VString stag
      else
        let tag = unwrap_some tag in
          if stag = tag then some v
          else err_fn "v_at" @@ sp "wrong tag in polyqueue: asked %s, got %s" tag stag
    with Not_found -> None end

  | v, i -> err_fn "v_at" @@ sp "improper data: %s, %s" (sov v) (sov i)

let v_min err_fn c = match c with
  | VSortedMap m ->
      begin try
        let k,v = ValueMap.min_binding m in
        some @@ VTuple [k;v]
    with Not_found -> None end
  | v -> err_fn "v_min" @@ sp "not a sorted data structure: %s" @@ sov v

let v_is_empty err_fn = function
  | VSet m       -> VBool(ValueSet.is_empty m)
  | VSortedSet m -> VBool(ValueSSet.is_empty m)
  | VBag m       -> VBool(ValueBag.is_empty m)
  | VList m      -> VBool(IList.is_empty m)
  | VVector(_,s,_) -> VBool(s = 0)
  | VMap m
  | VSortedMap m -> VBool(ValueMap.is_empty m)
  | VVMap m      -> VBool(ValueVMap.is_empty m)
  | v -> err_fn "v_is_empty" @@ sp "not a collection: %s" @@ sov v

let drop_vars = ["route_memo_"]

(* for a map structure *)
let print_binding_m ?(skip_functions=true) ?(skip_empty=true) id v =
  let dummy x y = None in
  (* check for conditions *)
  let rec check_print v' =
    if List.exists (fun x -> str_prefix x id) drop_vars then false else
    match v' with
    | (VFunction _ | VForeignFunction _) when skip_functions -> false
    | (VSet _ | VBag _ | VList _ | VVector _ | VMap _ | VVMap _ | VSortedMap _ | VSortedSet _)
        when skip_empty && v_peek dummy v' = None -> false
    | VIndirect x -> check_print !x
    | _ -> true
  in
  if check_print v then begin ob(); ps (id^" = "); pc(); print_value v; cb(); fnl() end
  else ()

let print_frame frame = IdMap.iter print_binding_m frame

let print_env ?skip_functions ?skip_empty ?(accessed_only=true) env =
  let print id v =
    let action () = print_binding_m ?skip_functions ?skip_empty id v in
    if accessed_only then
      if StrSet.mem id !(env.accessed) then action () else ()
    else action ()
  in
  ps "----Globals----"; fnl();
  IdMap.iter (fun k v -> print k !v) env.globals;
  if not @@ IdMap.is_empty env.locals then begin
      fnl(); ps "----Locals----"; fnl();
      IdMap.iter (fun k v -> print k @@ hd v) env.locals
    end;
  fnl ()

let print_trigger_env env =
  ps @@ sp "----Triggers(%i)----" @@ IdMap.cardinal env.triggers; fnl();
  IdMap.iter (fun id _ -> ps id; fnl()) env.triggers

let string_of_env ?skip_functions ?skip_empty ?accessed_only (env:env_t) =
  wrap_formatter (fun () -> print_env ?skip_functions ?skip_empty ?accessed_only env)

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
        begin match tag_of_expr @@ decompose_neg e with
        | Const(CInt i)   -> VInt (-i)
        | Const(CFloat f) -> VFloat (-.f)
        | _ -> failwith @@ "Negative can only have int or float"
        end
    | t -> failwith @@ "value is too complex: "^soi @@ Obj.tag @@ Obj.repr t

(* Global collection functions for values *)

type 'a t_err_fn = (string -> string -> 'a)

let v_combine err_fn x y = match x, y with
  | VSet m,  VSet m'            -> VSet(ValueSet.combine m m')
  | VBag m,  VBag m'            -> VBag(ValueBag.combine m m')
  | VList m, VList m'           -> VList(IList.combine m m')
  | VVector(m,s,t), VVector(m',_,_) ->
      let m, s =
        IntMap.fold (fun _ v (acc, s) ->
          IntMap.add s v acc, s + 1
        ) m' (m, s) in
      VVector(m,s,t)
  | VMap m,  VMap m'            -> VMap(ValueMap.combine m m')
  | VSortedMap m, VSortedMap m' -> VSortedMap(ValueMap.combine m m')
  | VSortedSet m, VSortedSet m' -> VSortedSet(ValueSSet.union m m')
  | VVMap m, VVMap m'           -> VVMap(ValueVMap.combine m m')
  | _ -> err_fn "v_combine" "mismatch in collections"

let v_fold err_fn f acc = function
  | VSet m       -> ValueSet.fold f acc m
  | VBag m       -> ValueBag.fold f acc m
  | VList m      -> IList.fold f acc m
  | VVector(m,sz,def) ->
    List.fold_left (fun acc i -> try f acc (IntMap.find i m) with Not_found -> f acc def) acc @@ create_range sz
  | VMap m
  | VSortedMap m   -> ValueMap.fold (fun k v acc -> f acc @@ encode_tuple (k,v)) m acc
  | VSortedSet m   -> ValueSSet.fold (fun k acc -> f acc k) m acc
  | VVMap m        -> ValueVMap.fold_all (fun _ k v acc -> f acc @@ VTuple[k;v]) m acc
  | v -> err_fn "v_fold" @@ sp "not a collection: %s" @@ sov v

let v_fold_poly err_fn f acc = function
  | VPolyQueue(m, _) -> IntMap.fold f m acc
  | v -> err_fn "v_fold_poly" @@ sp "not a polyqueue: %s" @@ sov v

let unwrap_vint err_fn = function VInt i -> i | _ -> err_fn "unwrap_vint" "not an int"

(* traverse starting with i, then proceed to the next location returned by f
   also accumulate
*)
let v_traverse_poly err_fn f i acc = function
  | VPolyQueue(m, _) ->
    let rec loop i acc =
      begin try
        let j, acc = f i (IntMap.find i m) acc in
        loop j acc
      with Not_found -> i, acc end
    in
    loop i acc
  | v -> err_fn "v_traverse_poly" @@ sp "not a polyqueue: %s" @@ sov v

let v_fold_poly_tag err_fn idx tag f acc m = match m, idx with
  | VPolyQueue(m, _), VInt(idx) ->
    let rec loop acc i =
      let v = IntMap.find i m in
      if snd3 v = tag then loop (f i v acc) (i + 1)
      else acc
    in
    loop acc idx

  | v, _ -> err_fn "v_fold_poly" @@ sp "not a polyqueue: %s" @@ sov v

let v_set_all err_fn v = function
  | VVector(m,sz,def) ->
      (* use the sparseness if we're resetting to the empty value *)
      if ValueComp.compare_v v def = 0 then VVector(IntMap.empty,sz,def)
      else
        let rec loop acc n =
          if n >= sz then acc
          else loop (IntMap.add n v acc) (n+1)
        in VVector(loop IntMap.empty 0,sz,def)
  | v -> err_fn "v_set_all" @@ sp "not a vector: %s" @@ sov v

(* fold over values *)
let rec fold_val f zero v =
  let recur = fold_val f in
  match v with
  | VTuple l        -> f (List.fold_left recur zero l) v
  | VOption(Some x) -> f (recur zero x) v
  | VIndirect r     -> f (recur zero !r) v
  | VSet _ | VBag _ | VList _ | VVector _
  | VMap _ | VSortedMap _ | VSortedSet _
  | VVMap _         -> v_fold (fun x y -> failwith "oops") recur zero v
  | _               -> f zero v

let v_fold_v vid err_fn f zero = function
  | VVMap m -> ValueVMap.fold vid (fun k v acc -> f acc @@ VTuple[k;v]) m zero
  | v -> err_fn "v_fold_v" @@ sp "not a supported collection: %s" @@ sov v

let v_fold_all err_fn f acc = function
  | VVMap m     -> ValueVMap.fold_all (fun vid k v acc -> f acc vid @@ VTuple[k;v]) m acc
  | v -> err_fn "v_fold_all" @@ sp "not a supported collection: %s" @@ sov v

let has_unknown v = fold_val (fun acc v -> v = VUnknown || acc) false v
let no_unknown v = not @@ has_unknown v

let v_iter err_fn f = function
  | VSet m      -> ValueSet.iter f m
  | VBag m      -> ValueBag.iter f m
  | VList m     -> IList.iter f m
  | VVector(m,sz,def) ->
    List.iter (fun i -> try f (IntMap.find i m) with Not_found -> f def) @@ create_range sz
  | VMap m
  | VSortedMap m   -> ValueMap.iter (fun k v -> f @@ encode_tuple (k,v)) m
  | VSortedSet m   -> ValueSSet.iter f m
  | VVMap m     -> ValueVMap.iter (fun _ k v -> f @@ VTuple[k;v]) m
  | v -> err_fn "v_iter" @@ sp "not a collection: %s" @@ string_of_value v

let v_insert ?vidkey ?tag err_fn x m =
  let error v c = err_fn "v_insert" @@
    sp "invalid input: insert: %s\ninto: %s" (sov v) (sov c)
  in
  match x, m with
  | _, VSet m                 -> VSet(ValueSet.insert x m)
  | _, VBag m                 -> VBag(ValueBag.insert x m)
  | _, VList m                -> VList(IList.insert x m)
  | _, VVector(m,s,t)           -> VVector(IntMap.add s x m, s + 1,t)
  | VTuple[k;v], VMap m       -> VMap(ValueMap.add k v m)
  | VTuple[k;v], VSortedMap m -> VSortedMap(ValueMap.add k v m)
  | _, VSortedSet m           -> VSortedSet(ValueSSet.add x m)
  | VTuple[t;k;v], VVMap m    -> VVMap(ValueVMap.add t k v m)
  | VTuple[k;v] as v', (VVMap m as c') ->
      (* take the vid from the provided key *)
      begin match vidkey with
      | Some(VTuple(t::_)) -> VVMap(ValueVMap.add t k v m)
      | _                  -> error v' c'
      end
  | _, VPolyQueue(m, tags)  ->
    let tag = unwrap_some tag in
    let itag =
      try
        fst3 @@ List.find (fun (i,s,_) -> (s:string) = tag) tags
      with Not_found -> raise @@ RuntimeError(-1, "v_insert",
                                              sp "tag %s not found in %s" tag @@ string_of_poly_variant tags)
    in
    let max_idx = try fst @@ IntMap.max_binding m with Not_found -> -1 in
    VPolyQueue(IntMap.add (max_idx + 1) (VInt itag, tag, x) m, tags)

  | v, c                   -> error v c

let v_insert_at err_fn x i m = match m, i with
  | VVector(m, sz, t), VInt i ->
      let sz' = if i < sz then sz else i + 1 in
      VVector(IntMap.add i x m, sz', t)
  | x, y -> err_fn "v_insert_at" (sp "bad arguments %s, %s" (sov x) @@ sov y)

let v_delete err_fn x m = match x, m with
  | _, VSet m                   -> VSet(ValueSet.delete x m)
  | _, VBag m                   -> VBag(ValueBag.delete x m)
  | _, VList m                  -> VList(IList.delete x m)
  | _, VVector(m,s,t)           ->
      (* expensive operation: shift all after deletion *)
      VVector(
        snd @@ IntMap.fold (fun k v (found, acc) ->
          (* shift down 1 *)
          if found then true, IntMap.add (k-1) v acc
          else
            if ValueComp.compare_v v x = 0 then true, acc
            else false, IntMap.add k v acc)
        m (false, IntMap.empty), s-1, t)

  | VTuple [k; v], VMap m       -> VMap(ValueMap.remove k m)
  | VTuple [k; v], VSortedMap m -> VSortedMap(ValueMap.remove k m)
  | _, VSortedSet m             -> VSortedSet(ValueSSet.remove x m)
  | VTuple [t;k;_], VVMap m     -> VVMap(ValueVMap.remove t k m)
  | v, c                        -> err_fn "v_delete" @@
    sp "invalid input: delete: %s\nfrom: %s" (sov v) (sov c)

(* vidkey: sometimes we need to get the vid from another tuple for vmap *)
let v_update ?vidkey err_fn oldv newv c =
  let error ?(x="") v v' c = err_fn "v_update" @@
    sp "invalid input: update: %s\nfrom: %s\nin: %s,\nextra: %s"
      (sov v) (sov v') (sov c) x
  in
  match oldv, newv, c with
  | _,_,VSet m                               -> VSet(ValueSet.update oldv newv m)
  | _,_,VBag m                               -> VBag(ValueBag.update oldv newv m)
  | _,_,VList m                              -> VList(IList.update oldv newv m)
  | _,_,VVector(m,s,t)                       ->
      let found = ref false in
      VVector(IntMap.map (fun v ->
        if not !found && ValueComp.compare_v v oldv = 0 then (found := true; newv) else v) m, s, t)
  | VTuple[k;v], VTuple[k';v'], VMap m       -> VMap(ValueMap.update k v k' v' m)
  | VTuple[k;v], VTuple[k';v'], VSortedMap m -> VSortedMap(ValueMap.update k v k' v' m)
  | _,_,VSortedSet m                         -> VSortedSet(ValueSSet.add newv @@ ValueSSet.remove oldv m)
  | VTuple[k;v], VTuple[t;k';v'], VVMap m -> VVMap(ValueVMap.update t k v k' v' m)
  | VTuple[k;v] as v1, (VTuple[k';v'] as v2), (VVMap m as c) ->
      begin match vidkey with
      | Some(VTuple(t::_)) -> VVMap(ValueVMap.update t k v k' v' m)
      | Some x             -> error ~x:(sov x) v1 v2 c
      | _                  -> error v1 v2 c
      end
  | v, v',c -> error v v' c

let dummy_tuple = VTuple(List.map (const VUnit) @@ create_range ~first:1 20)

let v_update_suffix err_fn key f col = match key, col with
  | VTuple[t;k;_], VVMap m ->
      let dummy = VInt 0 in
      (* translate the function: impedance mismatch *)
      let f' v =
        let ret = f @@ VTuple [dummy; VTuple [dummy_tuple; v]] in
        list_last @@ unwrap_vtuple ret
      in
      VVMap(ValueVMap.update_suffix t k f' m)
  | _ -> failwith "v_update_suffix: only supported on vmap"

let v_delete_prefix err_fn key col = match key, col with
  | VTuple[t;k;_], VVMap m -> VVMap(ValueVMap.remove_prefix t k m)
  | _ -> failwith "v_update_suffix: only supported on vmap"

let v_empty err_fn = function
  | VSet _         -> VSet(ValueSet.empty)
  | VBag _         -> VBag(ValueBag.empty)
  | VList _        -> VList(IList.empty)
  | VVector(m,_,t) -> VVector(IntMap.empty, 0, t)
  | VMap _         -> VMap(ValueMap.empty)
  | VSortedMap _   -> VSortedMap(ValueMap.empty)
  | VSortedSet _   -> VSortedSet(ValueSSet.empty)
  | VVMap m        -> VVMap(ValueVMap.empty)
  | VPolyQueue(m,t) -> VPolyQueue(IntMap.empty, t)
  | c -> err_fn "v_empty" @@ sp "invalid input: %s" (string_of_value c)

let v_sort err_fn f = function
  | VVector(m,s,t) -> VVector(IntMap.of_list @@ insert_index_fst @@
                        List.sort f @@ snd_many @@ IntMap.bindings m,s,t)
  | VList m -> VList(IList.sort f m)
  | _ -> err_fn "v_sort" "not a list"

let v_size err_fn = function
  | VSet m      -> VInt(ValueSet.size m)
  | VList m     -> VInt(IList.size m)
  | VVector(_,s,_) -> VInt s
  | VMap m
  | VSortedMap m   -> VInt(ValueMap.cardinal m)
  | VSortedSet m   -> VInt(ValueSSet.cardinal m)
  | VVMap m     -> VInt(ValueVMap.size m)
  | VBag m      -> VInt(ValueBag.size m)
  | VPolyQueue(m,_) -> VInt(IntMap.cardinal m)
  | _           -> err_fn "vsize" "not a collection"

let v_singleton err_fn ta_env elem c telem = match elem, c with
  | _,TSet                  -> VSet(ValueSet.singleton elem)
  | _,TBag                  -> VBag(ValueBag.singleton elem)
  | _,TList                 -> VList(IList.singleton elem)
  | _,TVector               -> VVector(IntMap.singleton 0 elem, 1, v_of_t ta_env telem)
  | VTuple[k;v], TMap       -> VMap(ValueMap.singleton k v)
  | VTuple[k;v], TSortedMap -> VSortedMap(ValueMap.singleton k v)
  | _,TSortedSet            -> VSortedSet(ValueSSet.singleton elem)
  | VTuple[t;k;v], TVMap _  -> VVMap(ValueVMap.singleton t k v)
  | _ -> err_fn "v_singleton" "not a collection"

(* for v_slice *)
let match_or_unknown v1 v2 = match v1 with
  | VUnknown -> true | _        -> v1 = v2

let rec match_pattern pat_v v = match pat_v, v with
  | VUnknown, _ -> true
  | VTuple pat_f, VTuple v_f ->
    (try List.for_all2 match_pattern pat_f v_f
      with Invalid_argument _ -> false)
  | x, y -> ValueComp.compare_v x y = 0

let v_slice err_fn pat m = match m, pat with
  | VSet m, _      -> VSet(ValueSet.filter (match_pattern pat) m)
  | VBag m, _      -> VBag(ValueBag.filter (match_pattern pat) m)
  | VList m, _     -> VList(IList.filter (match_pattern pat) m)
  | VVector(m,_,t), _   ->
      let v, sz =
        IntMap.fold (fun _ v ((acc, n) as a) ->
          if match_pattern pat v then IntMap.add n v acc, n + 1 else a)
        m (IntMap.empty, 0) in
      VVector(v, sz, t)
  | VMap m, VTuple[k;_] when no_unknown k ->
      begin try let v = ValueMap.find k m in
          VMap(ValueMap.singleton k v)
      with Not_found -> VMap(ValueMap.empty) end
  | VMap m, _      ->
      VMap(ValueMap.filter (fun k v ->
        match_pattern pat @@ encode_tuple (k,v)) m)
  | VSortedMap m, VTuple[k;_] when no_unknown k   ->
      begin try let v = ValueMap.find k m in
          VSortedMap(ValueMap.singleton k v)
      with Not_found -> VSortedMap(ValueMap.empty) end
  | VSortedMap m, _ ->
      VSortedMap(ValueMap.filter (fun k v ->
        match_pattern pat @@ encode_tuple (k,v)) m)
  | VSortedSet m, _ -> VSortedSet(ValueSSet.filter (match_pattern pat) m)
  | VVMap m, VTuple [t;k;_] when no_unknown k ->
      VVMap(ValueVMap.frontier_point ~op:`EQ t k m)
  | VVMap m, VTuple [t;k;_] ->
      VVMap(ValueVMap.frontier_slice ~op:`EQ
        ~filter_fn:(fun k' -> match_pattern k k') t m)
  | _ -> err_fn "v_slice" "not a collection"


let v_slice_op op err_fn pat m =
  let find_fn = function
    | `LT -> ValueMap.find_lt
    | `LEQ -> ValueMap.find_leq
    | `GT -> ValueMap.find_gt
    | `GEQ -> ValueMap.find_geq
    | `EQ -> failwith "eq not supported here"
  in
  match m, pat with
    (* point lookup or slice lookup? *)
  | VVMap m, VTuple[t;k;_] when no_unknown k ->
      VVMap(ValueVMap.frontier_point ~op t k m)
  | VVMap m, VTuple[t;k;_] ->
      VVMap(ValueVMap.frontier_slice ~op
        ~filter_fn:(fun k' -> match_pattern k k') t m)
    (* slice_op only works with full key on sortedmap *)
  | VSortedMap m, VTuple[k;_] when no_unknown k ->
        begin try
          let k, v = (find_fn op) k m in
          VSortedMap(ValueMap.singleton k v)
        with Not_found -> VSortedMap(ValueMap.empty) end
  | _ -> err_fn "v_slice_lower" (sp "unhandled or bad values: %s" @@ sov pat)

(* gather all entries gt/lt/geq/leq than given key *)
let v_filter_op err_fn op pat m = match m, pat, op with
  | VSortedSet m, _, `GT  -> VSortedSet(ValueSSet.filter_gt pat m)
  | VSortedSet m, _, `GEQ -> VSortedSet(ValueSSet.filter_geq pat m)
  | VSortedSet m, _, `LT  -> VSortedSet(ValueSSet.filter_lt pat m)
  | VSortedSet m, _, `LEQ -> VSortedSet(ValueSSet.filter_leq pat m)
  | VSortedMap m, VTuple[k;_], `GT  -> VSortedMap(ValueMap.filter_gt k m)
  | VSortedMap m, VTuple[k;_], `GEQ -> VSortedMap(ValueMap.filter_geq k m)
  | VSortedMap m, VTuple[k;_], `LT  -> VSortedMap(ValueMap.filter_lt k m)
  | VSortedMap m, VTuple[k;_], `LEQ -> VSortedMap(ValueMap.filter_leq k m)
  | _ -> err_fn "v_filter_op" "not implemented"

let rec type_of_value uuid value =
  let get_typ v = type_of_value uuid v in
  let dummy_err _ _ = None in
  let col_get () = maybe t_unit get_typ @@ v_peek dummy_err value in
  match value with
  | VUnknown           -> t_unknown
  | VUnit              -> t_unit
  | VBool b            -> t_bool
  | VInt _             -> t_int
  | VFloat _           -> t_float
  | VByte _            -> t_string
  | VString _          -> t_string
  | VAddress (_,_)     -> t_addr
  | VTarget id         -> canonical @@ TTarget(t_unknown) (* We don't have the ids *)
  | VOption None       -> wrap_tmaybe @@ t_unknown
  | VOption (Some v)   -> wrap_tmaybe @@ type_of_value uuid v
  | VTuple vs          -> wrap_ttuple @@ List.map (type_of_value uuid) vs
  | VBag _             -> wrap_tbag @@ col_get ()
  | VSet _             -> wrap_tset @@ col_get ()
  | VList _            -> wrap_tlist @@ col_get ()
  | VVector _          -> wrap_tvector @@ col_get ()
  | VMap _             -> wrap_tmap @@ col_get ()
  | VSortedMap _       -> wrap_tsortedmap @@ col_get ()
  | VSortedSet _       -> wrap_tsortedset @@ col_get ()
  | VVMap _            -> wrap_tvmap @@ col_get ()
  | VPolyQueue(_,tag)  -> wrap_tpolyq tag
  | VIndirect ind      -> type_of_value uuid !ind
  | VFunction _
  | VForeignFunction _ -> raise (RuntimeError (uuid, "type_of_value", "cannot apply to function"))
  | VMax | VMin        -> raise (RuntimeError (uuid, "type_of_value", "cannot apply to vmax/vmin"))

(*
let rec expr_of_value uuid value =
  let handle_cols vs =
    let l = List.map (expr_of_value uuid) @@ ValueUtils.v_to_list vs in
    k3_container_of_list (type_of_value uuid value) l
  in
  match value with
  | VUnknown -> mk_const CUnknown
  | VUnit -> mk_const CUnit
  | VBool b -> mk_const @@ CBool b
  | VInt i -> mk_const @@ CInt i
  | VFloat f -> mk_const @@ CFloat f
  | VByte b -> mk_const @@ CString(string_of_int @@ Char.code b)
  | VString s -> mk_const @@ CString s
  | VAddress (ip,port) -> mk_const @@ CAddress (ip,port)
  | VTarget id -> mk_const @@ CTarget id
  | VOption(None) -> mk_nothing t_unknown
  | VOption(Some v) -> mk_just @@ expr_of_value uuid v
  | VTuple vs -> mk_tuple @@ List.map (expr_of_value uuid) vs
  | VSet _ | VList _ | VBag _ | VVector _ | VMap _ | VSortedMap _ | VSortedSet _ | VVMap _ -> handle_cols value
  | VIndirect ind -> mk_ind @@ expr_of_value uuid !ind
  | VFunction _
  | VForeignFunction _ -> raise @@ RuntimeError (uuid,
      "expr_of_value: cannot apply to function")
  | VMax | VMin -> raise @@ RuntimeError (uuid,
      "expr_of_value: cannot apply to vmax/vmin")
*)
