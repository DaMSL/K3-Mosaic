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
      | VMap v, VMap v' -> ValueMap.compare compare_v v v'
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

    module IntMap : (Map.S with type key = int) = Map.Make(struct
      type t = int
      let compare = (-)
    end)

    (* try to get a consistent hashing scheme based on members *)
    let rec hash v =
      (* hash members only *)
      let col_hash fold_fn v = fold_fn (fun acc x -> hash x lxor acc) 0 v in
      let map_hash fold_fn v = fold_fn (fun k v acc -> hash k lxor hash v lxor acc) v 0 in
      let vmap_hash fold_fn v = fold_fn (fun t k v acc -> hash t lxor hash k lxor hash v lxor acc) v 0 in
      match v with
      | VTuple vs       -> col_hash List.fold_left vs
      | VOption(Some v) -> hash v
      | VIndirect v     -> hash !v
      | VSet v          -> col_hash ValueSet.fold v
      | VBag v          -> col_hash ValueBag.fold v
      | VList v         -> col_hash IList.fold v
      | VMap v          -> map_hash ValueMap.fold v
      | VVMap v         -> vmap_hash ValueVMap.fold v
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
  and trigger_env_t = (address -> env_t -> value_t -> unit) IdMap.t
  (* keep track of what was modified *)
  and env_t = {
        triggers: trigger_env_t;
        globals:  global_env_t;
        locals:   local_env_t;
        accessed: StrSet.t ref;
      }

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
      | VMap of value_t ValueMap.t
      | VVMap of value_t ValueVMap.t
      | VFunction of arg_t * local_env_t * expr_t (* closure *)
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
      | VList m -> IList.to_list m
      | VMap m  -> List.map map_to_tuple @@ ValueMap.to_list m
      | VVMap m -> List.map vmap_to_tuple @@ ValueVMap.to_list m
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
      | VMap _             -> "VMap"
      | VVMap _            -> "VVMap"
      | VFunction _        -> "VFunction"
      | VForeignFunction _ -> "VForeignFunction"
      | VAddress _         -> "VAddress"
      | VTarget _          -> "VTarget"
      | VIndirect _        -> "VIndirect"

    let rec repr_of_value v =
      let s_of_col m = String.concat "; " @@ List.map repr_of_value @@
        List.sort ValueComp.compare_v @@
        v_to_list m in
      let paren s = Printf.sprintf "(%s)" s in
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
      | VSet _
      | VBag _
      | VList _
      | VMap _
      | VVMap _                 -> paren @@ s_of_col v
      | VFunction (a, _, b)     -> paren @@ Printf.sprintf "%s -> %s" (string_of_arg a) (string_of_expr b)
      | VForeignFunction (i, a, _) -> paren @@ string_of_arg a
      | VAddress (ip, port)     -> paren @@ ip^":"^ string_of_int port
      | VTarget id              -> paren @@ id
      | VIndirect ind           -> paren @@ repr_of_value !ind
      | _                       -> ""
  end

open Value

include ValueUtils

let matching_collections v v' = match v, v' with
  | VList _, VList _
  | VBag _, VBag _
  | VSet _, VSet _
  | VMap _, VMap _
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
    | VString s               -> ps @@ Printf.sprintf "\"%s\"" @@
                                   String.escaped s
    | VTuple vs               -> pretty_tag_str CutHint "" ""
                                   (List.map lazy_value vs)
    | VOption None            -> ps "None"
    | VOption vopt            -> pretty_tag_str CutHint "" "Some"
                                   [lazy_value (unwrap_some vopt)]
    | VSet _ as vs            -> print_collection ~sort "{" "}" vs
    | VBag _ as vs            -> print_collection ~sort "{|" "|}" vs
    | VList _ as vs           -> print_collection "[" "]" vs
    | VMap _ as vs            -> print_collection ~sort "[:" ":]" vs
    | VVMap _ as vs           -> print_collection ~sort "[<" ">]" vs
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

(* vmaps need to remove unit key and keep the value *)
let remove_unit k v = match k with
  | VUnit -> v
  | _     -> VTuple[k;v]

let v_peek err_fn c = match c with
  | VSet m  -> ValueSet.peek m
  | VBag m  -> ValueBag.peek m
  | VList m -> IList.peek m
  | VMap m  -> maybe None (some |- encode_tuple)  @@ ValueMap.peek m
  | VVMap m -> maybe None (fun (_,k,v) -> some @@ remove_unit k v) @@ ValueVMap.peek m
  | v -> err_fn "v_peek" @@ Printf.sprintf "not a collection: %s" @@ string_of_value v

(* for a map structure *)
let print_binding_m ?(skip_functions=true) ?(skip_empty=true) id v =
  let dummy x y = None in
  (* check for conditions *)
  let rec check_print v' =
    match v' with
    | (VFunction _ | VForeignFunction _) when skip_functions -> false
    | (VSet _ | VBag _ | VList _
    | VMap _ | VVMap _ ) when skip_empty && v_peek dummy v' = None -> false
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
  ps @@ Printf.sprintf "----Triggers(%i)----" @@ IdMap.cardinal env.triggers; fnl();
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
  | VSet m,  VSet m'          -> VSet(ValueSet.combine m m')
  | VBag m,  VBag m'          -> VBag(ValueBag.combine m m')
  | VList m, VList m'         -> VList(IList.combine m m')
  | VMap m,  VMap m'          -> VMap(ValueMap.combine m m')
  | VVMap m, VVMap m'         -> VVMap(ValueVMap.combine m m')
  | _ -> err_fn "v_combine" "mismatch in collections"

let v_fold err_fn f acc = function
  | VSet m      -> ValueSet.fold f acc m
  | VBag m      -> ValueBag.fold f acc m
  | VList m     -> IList.fold f acc m
  | VMap m      -> ValueMap.fold (fun k v acc -> f acc @@ encode_tuple (k,v)) m acc
  | VVMap m     -> ValueVMap.fold (fun _ k v acc -> f acc @@ remove_unit k v) m acc
  | v -> err_fn "v_fold" @@ Printf.sprintf "not a collection: %s" @@ string_of_value v

let v_foldv err_fn f acc = function
  | VVMap m     -> ValueVMap.fold (fun vid k v acc -> f acc vid @@ remove_unit k v) m acc
  | v -> err_fn "v_foldv" @@ Printf.sprintf "not a supported collection: %s" @@ string_of_value v

let v_iter err_fn f = function
  | VSet m      -> ValueSet.iter f m
  | VBag m      -> ValueBag.iter f m
  | VList m     -> IList.iter f m
  | VMap m      -> ValueMap.iter (fun k v -> f @@ encode_tuple (k,v)) m
  | VVMap m     -> ValueVMap.iter (fun _ k v -> f @@ remove_unit k v) m
  | v -> err_fn "v_iter" @@ Printf.sprintf "not a collection: %s" @@ string_of_value v

let v_insert err_fn x m = match x, m with
  | _, VSet m              -> VSet(ValueSet.insert x m)
  | _, VBag m              -> VBag(ValueBag.insert x m)
  | _, VList m             -> VList(IList.insert x m)
  | VTuple[k;v], VMap m    -> VMap(ValueMap.add k v m)
  | VTuple[t;k;v], VVMap m -> VVMap(ValueVMap.add t k v m)
  | VTuple[t;v], VVMap m   -> VVMap(ValueVMap.add t VUnit v m)
  | v, c                   -> err_fn "v_insert" @@
    Printf.sprintf "invalid input: insert: %s\ninto: %s" (sov v) (sov c)

let v_delete err_fn x m = match x, m with
  | _, VSet m               -> VSet(ValueSet.delete x m)
  | _, VBag m               -> VBag(ValueBag.delete x m)
  | _, VList m              -> VList(IList.delete x m)
  | VTuple [k; v], VMap m   -> VMap(ValueMap.remove k m)
  | VTuple [t;k;_], VVMap m -> VVMap(ValueVMap.remove t k m)
  | VTuple [t;_], VVMap m   -> VVMap(ValueVMap.remove t VUnit m)
  | v, c                    -> err_fn "v_delete" @@
    Printf.sprintf "invalid input: delete: %s\nfrom: %s" (sov v) (sov c)

let v_update err_fn oldv newv c = match oldv, newv, c with
  | _,_,VSet m                            -> VSet(ValueSet.update oldv newv m)
  | _,_,VBag m                            -> VBag(ValueBag.update oldv newv m)
  | _,_,VList m                           -> VList(IList.update oldv newv m)
  | VTuple[k;v], VTuple[k';v'], VMap m    -> VMap(ValueMap.update k v k' v' m)
  | VTuple[k;v], VTuple[t;k';v'], VVMap m -> VVMap(ValueVMap.update t k v k' v' m)
  | VTuple[v], VTuple[t;v'], VVMap m      -> VVMap(ValueVMap.update t VUnit v VUnit v' m)
  | v,v',c -> err_fn "v_update" @@ Printf.sprintf
    "invalid input: update: %s\nfrom: %s\nin: %s" (sov v) (sov v') (sov c)

let v_upsert_with err_fn key lam_none lam_some col =
  let update t k v m =
    VVMap(ValueVMap.update_with t k (function
      | None    -> some @@ lam_none VUnit
      | Some v  -> some @@ lam_some v) m)
  in
  (* TODO: implement for other types *)
  match key, col with
  | VTuple [t;k;v], VVMap m -> update t k v m
  | VTuple [t;v], VVMap m           -> update t VUnit v m
  | _ -> failwith "v_upsert_with: unsupported"

let v_update_suffix err_fn key f col = match key, col with
  | VTuple[t;k;_], VVMap m -> VVMap(ValueVMap.update_suffix t k f m)
  | VTuple[t;_], VVMap m           -> VVMap(ValueVMap.update_suffix t VUnit f m)
  | _ -> failwith "v_update_suffix: only supported on vmap"

let v_delete_prefix err_fn key col = match key, col with
  | VTuple[t;_;_], VVMap m -> VVMap(ValueVMap.remove_prefix t m)
  | VTuple[t;_],   VVMap m -> VVMap(ValueVMap.remove_prefix t m)
  | _ -> failwith "v_update_suffix: only supported on vmap"

let v_empty err_fn ?(no_map=false) ?(no_multimap=false) = function
  | VSet _      -> VSet(ValueSet.empty)
  | VBag _      -> VBag(ValueBag.empty)
  | VList _     -> VList(IList.empty)
  | VMap _      when no_map      -> VBag(ValueBag.empty)
  | VMap _      -> VMap(ValueMap.empty)
  | VVMap m     -> VVMap(ValueVMap.empty)
  | c -> err_fn "v_empty" @@ Printf.sprintf "invalid input: %s" (string_of_value c)

let v_empty_of_t = function
  | TSet        -> VSet(ValueSet.empty)
  | TBag        -> VBag(ValueBag.empty)
  | TList       -> VList(IList.empty)
  | TMap        -> VMap(ValueMap.empty)
  | TVMap       -> VVMap(ValueVMap.empty)

(* sort only applies to list *)
let v_sort err_fn f = function
  | VList m -> VList(IList.sort f m)
  | _ -> err_fn "v_sort" "not a list"

let v_size err_fn = function
  | VSet m      -> VInt(ValueSet.size m)
  | VList m     -> VInt(IList.size m)
  | VMap m      -> VInt(ValueMap.cardinal m)
  | VVMap m     -> VInt(ValueVMap.size m)
  | VBag m      -> VInt(ValueBag.size m)
  | _           -> err_fn "vsize" "not a collection"

let v_singleton err_fn elem c = match elem, c with
  | _,TSet                       -> VSet(ValueSet.singleton elem)
  | _,TBag                       -> VBag(ValueBag.singleton elem)
  | _,TList                      -> VList(IList.singleton elem)
  | VTuple[k;v], TMap            -> VMap(ValueMap.singleton k v)
  | VTuple[t;k;v], TVMap -> VVMap(ValueVMap.singleton t k v)
  | VTuple[t;v], TVMap           -> VVMap(ValueVMap.singleton t VUnit v)
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

let v_slice err_fn pat = function
  | VSet m         -> VSet(ValueSet.filter (match_pattern pat) m)
  | VBag m         -> VBag(ValueBag.filter (match_pattern pat) m)
  | VList m        -> VList(IList.filter (match_pattern pat) m)
  | VMap m         -> VMap(ValueMap.filter (fun k v ->
                        match_pattern pat @@ encode_tuple (k,v)) m)
  | VVMap m        -> VVMap(ValueVMap.filter (fun _ _ v ->
                        match_pattern pat v) m)
  | _ -> err_fn "v_slice" "not a collection"

let v_slice_frontier err_fn pat m = match m, pat with
  | VVMap m, VTuple[t;k;v]  ->
      (* point lookup or slice lookup? *)
      if not @@ List.mem VUnknown(unwrap_vtuple k) then
        try
          VVMap(ValueVMap.frontier_point t k m)
        with Not_found -> VVMap(ValueVMap.empty)
      else
        VVMap(
          ValueVMap.filter (fun _ _ v -> match_pattern pat v) @@
            ValueVMap.frontier_slice t m)

  | VVMap m, VTuple[t;_] ->
      begin try
        VVMap(ValueVMap.frontier_point t VUnit m)
      with Not_found -> VVMap(ValueVMap.empty) end

  | _ -> err_fn "v_slice_frontier" "bad input"

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
  | VMap _             -> wrap_tmap @@ col_get ()
  | VVMap _            -> wrap_tvmap @@ col_get ()
  | VIndirect ind      -> type_of_value uuid !ind
  | VFunction _
  | VForeignFunction _ -> raise (RuntimeError (uuid, "type_of_value: cannot apply to function"))
  | VMax | VMin        -> raise (RuntimeError (uuid, "type_of_value: cannot apply to vmax/vmin"))

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
  | VSet vs -> handle_cols value
  | VList vs -> handle_cols value
  | VBag vs -> handle_cols value
  | VMap vs -> handle_cols value
  | VVMap vs -> handle_cols value
  | VIndirect ind -> mk_ind @@ expr_of_value uuid !ind
  | VFunction _
  | VForeignFunction _ -> raise @@ RuntimeError (uuid,
      "expr_of_value: cannot apply to function")
  | VMax | VMin -> raise @@ RuntimeError (uuid,
      "expr_of_value: cannot apply to vmax/vmin")

