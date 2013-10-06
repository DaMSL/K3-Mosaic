(** K3 Scala Generator *)

open M3
open K3
open Database
open Format
open Type
open Constants
open Patterns

exception K3SException of string
;;

(** This module implements a code generator that turns K3 code into
    Scala code.
*)
module K3S =
struct
   (** Describes which type of collection we are dealing with *)
   type collection_type_t = Unknown | Persistent | Intermediate

   (** Holds a nested description of function arguments *)
   type argn_t = 
   | ArgN of string
   | ArgNTuple of argn_t list

   (** This type is used to describe the type of some piece of code *)
   type type_t   = 
   | Float
   | Bool
   | Int
   | String
   | Date
   | Any
   | External of string
   | Unit
   | Fn of argn_t * type_t * type_t
   | ExtFn of string * type_t * type_t 
   | Tuple of type_t list
   | Collection of collection_type_t * type_t list * type_t
   | Trigger of Schema.event_t
   
   (** Code is generated as a string *)
   type source_code_t = string
   (** The code generator produces a tuple of code and its type *)
   type code_t = source_code_t * type_t
   type op_t = (type_t -> type_t -> 
      (source_code_t -> source_code_t -> source_code_t) * 
      type_t * string option)
   type db_t = unit

   (** The line seperator is used to format the generated code *)
   let line_sep = "\n"
   (** The maximum number of elements that are supported in one Scala tuple *)
   let max_tuple_elems = 100
   (** Keeps track of the number of sources that we have seen so far *)
   let source_count = ref 0

   let consts = Hashtbl.create 2
   let nb_consts = ref 0

   (** Splits a list l into two lists at element number n *)
   let list_split l n = 
      let rec _list_split l1 l2 n =
         if(n > 0) then
            match l2 with
            | x :: xs -> _list_split (l1 @ [x]) xs (n - 1)
            | [] -> (l1, [])
         else
            (l1, l2)
      in
      _list_split [] l n

   (** Generates a simple list of variables named prefix plus a number *)
   let list_vars prefix n = 
      let rec _list_vars prefix n = 
         if n = 0 then [] 
      else 
         (prefix ^ (string_of_int n)) :: (_list_vars prefix (n - 1)) in
      List.rev (_list_vars prefix n)

   (** Zips two lists together *)
   let rec list_zip l1 l2 = 
      match l1, l2 with
      | x1 :: xs1, x2 :: xs2 -> (x1, x2) :: (list_zip xs1 xs2)
      | [], [] -> []
      | _ -> raise (K3SException("list_zip expected lists of equal length"))

   (** Produces a string representation of a list *)
   let make_list ?(empty="") ?(parens=("(", ")")) ?(sep=",") l = 
      let rec make_list_ l = 
         match l with
         | [x] -> x
         | x :: xs -> x ^ sep ^ (make_list_ xs)
         | [] -> empty
      in
      (fst parens) ^ (make_list_ l) ^ (snd parens)

   (* Constructs a tuple given a list of strings, nesting it if necessary *)
   let rec make_tuple ?(tpe = "") t : string = 
      let nb_elems = (List.length t) in 
      let elems = 
         if nb_elems > max_tuple_elems then
            let (t1, t2) = (list_split t (max_tuple_elems - 1)) in
            t1 @ [make_tuple t2]
         else
            t
      in
      match t with 
      | []      -> "Unit"
      | x :: [] -> "(" ^ x ^ ")"
      | _       -> (if tpe = "" then "Tuple" ^ 
         (string_of_int (List.length elems)) else tpe) ^ 
         (make_list elems)
   
   (** Takes a tuple t and flattens it recursively *)
   let rec flatten_tuple t = 
      match t with
      | Tuple(ts) -> List.fold_left (fun x t -> x @ (flatten_tuple t)) [] ts
      | _ -> [t]

   (** Returns a string representing a tuple with argument names *)
   let rec string_of_argn ?(prefix = "") a : string = 
      match a with
      | ArgN(n) -> if n = "()" then n else prefix ^ n
      | ArgNTuple(ns) -> make_tuple 
         (List.map (fun x -> string_of_argn ~prefix:prefix x) ns)
   
   (** Returns a string of a tuple and nests it if necessary *) 
   let rec string_of_tuple_type t : string =
      let nb_elems = (List.length t) in 
      let elems = 
         if nb_elems > max_tuple_elems then
            let (t1, t2) = (list_split t (max_tuple_elems - 1)) in
            t1 @ [string_of_tuple_type t2]
         else
            t
      in
      match t with 
      | []      -> "Unit"
      | x :: [] -> "(" ^ x ^ ")"
      | _       -> "Tuple" ^ (string_of_int (List.length elems)) ^ 
      (make_list ~parens:("[", "]") elems)

   (** Returns the string representation of type t *)
   let rec string_of_type t : string = 
      match t with 
      | Float -> "Double"
      | Bool -> "Boolean"
      | Int -> if Debug.active "BIG-INT" then "BigInt" else "Long"
      | String -> "String"
      | Date -> "Date"
      | Any -> "Any"
      | External(s) -> s
      | Unit -> "Unit"
      | Fn(_, arg, ret) -> 
         (string_of_type arg) ^ " => " ^ (string_of_type ret)
      | ExtFn(_, arg, ret) -> 
         (string_of_type arg) ^ " => " ^ (string_of_type ret)
      | Tuple(elems) -> (string_of_tuple_type (List.map string_of_type elems))
      | Collection(tpe, keys_t, val_t) -> 
         let type_string = match tpe with 
         | Intermediate -> "K3IntermediateCollection"
         | Persistent -> "K3PersistentCollection"
         | Unknown -> "K3Collection" in
         type_string ^ "[" ^
         (string_of_tuple_type (List.map string_of_type keys_t)) ^ ", " ^ 
         (string_of_type val_t) ^ "]"
      | Trigger(event_t) -> "Trigger"

   let debug_string ((x, t):code_t) : string =
      (string_of_type t) ^ ": " ^ x

   let debugfail (expr:(K3.expr_t option)) (msg:string) =
      print_string ("Error: " ^ msg ^ "\nExpression: " ^ (
         match expr with 
         | None -> "{Not Available}"
         | Some(s) -> K3.code_of_expr s));
      raise (K3SException("K3Scalagen Internal Error ["^msg^"]"))
   
   (** Takes a base type and maps it to the corresponding Scala code generator
       type *)
   let rec map_base_type bt = 
      match bt with
      | TBool -> Bool
      | TInt -> Int
      | TFloat -> Float
      | TString -> String
      | TDate -> Date
      | TAny -> Any
      | TExternal(s) -> External(s)

   (** Maps a K3 type to a Scala backend type 
       Note: Collections should be annotated *)
   let rec map_type t =
      match t with
      | K3.TUnit -> Unit
      | K3.TBase(bt) -> map_base_type bt
      | K3.TTuple(tlist) -> Tuple(List.map map_type tlist)
      | K3.Collection(c_t, ctype) -> 
         let m_t = 
            match c_t with
            | K3.Intermediate -> Intermediate
            | K3.Unknown -> Unknown
            | K3.Persistent -> Persistent
            in
            begin match (map_type ctype) with 
            | Tuple([]) -> Collection(m_t, [], Unit)
            | Tuple(klist) -> 
               Collection(m_t, (List.rev (List.tl (List.rev klist))), 
               (List.hd (List.rev klist)))
            | vtype -> Collection(m_t, [], vtype)
            end
      | K3.Fn(argtlist,rett) -> Fn(ArgNTuple([]), map_type (List.hd argtlist),
         map_type (K3.Fn(List.tl argtlist, rett)))
         
   (** Returns the return type of a function *)
   let fn_ret_type fnt = 
      match fnt with 
      | Fn(_, _, fnrett) -> fnrett
      | ExtFn(_, _, fnrett) -> fnrett
      | _ -> debugfail None ("Function excpected found: " ^ 
         (string_of_type fnt))
   
   (** Returns a flattened list of nested function arguments *)
   let rec flatten_args args =
      match args with
      | ArgN(n) -> [n]
      | ArgNTuple(ags) -> List.fold_left 
         (fun x a -> x @ (flatten_args a)) [] ags
   
   (** Finds the common supertype of two types **)
   let rec unify_types a b = 
      match a, b with
      | Int, Float | Float, Int -> Float
      | Bool, Int  | Int, Bool  -> Int
      | Collection(_, ak, at), Collection(_, bk, bt) -> 
         Collection(Intermediate, (List.map2 unify_types ak bk), 
            unify_types at bt) 
      | Tuple(at), Tuple(bt) -> Tuple(List.map2 unify_types at bt)
      | a, b when a = b -> a
      | _, _ -> 
         debugfail None 
            ("Cannot unify types: " ^ 
            (string_of_type a) ^ " and " ^ (string_of_type b))

   (** Returns code to cast a Boolean into a Double *)
   let mk_bool_to_float a = "(if(" ^ a ^ ") 1.0 else 0.0)"

   (** Returns code to cast a Boolean into an Int *)
   let mk_bool_to_int a = "(if(" ^ a ^ ") 1L else 0L)"
   let mk_date_to_time a = a ^ ".getTime()"

   (** Generates a list of indices with the same length as the list
       passed to the function *)
   let list_index l = 
      let rec _list_index i l =
         match l with
         | [] -> []
         | x :: xs -> i :: (_list_index (i + 1) xs)
      in
      (_list_index 1 l)

   (** Generates a list of indices correpsonding to the structure
       of the tuple *)
   let tuple_index tp = 
      let rec _tuple_index pfx tp: string list =
         match tp with
         | Tuple([x]) -> (_tuple_index pfx x)
         | Tuple(ls) -> (List.fold_left  
               (fun a (i, x) -> a @ 
                  (_tuple_index (pfx ^ "._" ^ (string_of_int i)) x))
               [] (list_zip (list_index ls) ls))
         | _ -> [pfx]
      in
      (_tuple_index "" tp)

   (** Generates code to cast an expression n of type ot to type t*)
   let rec cast_up n ot t =
      match t, ot with
      | Collection(_, k, v), Collection(_, ko, vo) when k <> ko || v <> vo ->
         let key_length = List.length k in
         let rett = (Tuple((Tuple(k)) :: [v])) in
         let body = (string_of_type rett) ^ "(" ^ 
            (make_list (list_vars "v" key_length)) ^ ", v" ^ 
            (string_of_int (key_length + 1)) ^ ")" 
         in
         n ^ ".map { " ^ (wrap_function_key_val ko vo 
         (Fn(ArgNTuple(List.map (fun x -> ArgN(x)) 
         (list_vars "v" (key_length + 1))), Tuple(k @ [v]), rett)) body) ^ " }"
      | Tuple(ts), Tuple(tso) when ts <> tso ->
         "({ val v = " ^ n ^ "; " ^  
         (make_tuple ~tpe:(string_of_type t)
         (List.map2 (fun n (t, ot) -> cast_up n ot t) 
         (list_vars "v._" (List.length ts)) (list_zip ts tso))) ^ "})"
      | Int, Bool -> mk_bool_to_int n
      | _ -> n 

   and implicit_conversions ?(pfx = None) argsn t1s t2s: string =  
      let fa = flatten_args argsn in
      let ti = tuple_index t1s in 
      (make_list ~parens:("", ";") ~sep:";" 
         (List.map2 (fun (i, v) (t1, t2) -> 
            "val " ^ v ^ ":" ^ (string_of_type t2) ^ " = " ^ (cast_up (
               match pfx with | None -> "_" ^ v | Some(p) -> p ^ i) 
            t1 t2) ^ " /* " ^ (string_of_type t1) ^ " => " ^ 
            (string_of_type t2) ^ " */") (list_zip ti fa)
            (list_zip (flatten_tuple t1s) (flatten_tuple t2s))))

   (** Generates the header of a function *)
   and wrap_function argt fnt f: string = 
      match fnt with
      | Fn(argsn, argst, rett) ->
         "(y:" ^ (string_of_type argt) ^ ") => { y match { case " ^ 
         (string_of_argn argsn) ^ " => {" ^ f ^ "} } }" 
      | ExtFn(n, _, _) -> n ^ " _"
      | _ -> debugfail None "Expected a function"

   (** Generates the header for a function taking a key/value tuple 
       as input. Example the function that is used for the mapping operation *)
   and wrap_function_key_val (kt:type_t list) (vt:type_t) fnt f: string =     
      let ktt, vtt, f_body = 
         match fnt with
         | Fn(argsn1, argst1, Fn(argsn2, argst2, rett)) ->
            ([List.hd kt], List.hd (List.tl kt), 
               wrap_function (fn_ret_type vt) (Fn(argsn2, argst2, rett)) f)
         | _ -> (kt, vt, f) 
      in
      match fnt with
      | Fn(argsn, argst, rett) ->
         "(x:Tuple2[" ^ (string_of_type (Tuple(ktt))) ^ ", " ^ 
         (string_of_type vtt) ^ "]) => { " ^
         (implicit_conversions ~pfx:(Some("x")) argsn 
            (Tuple([Tuple(ktt); vtt])) argst) ^
         f_body ^ " }" 
      | ExtFn(n, _, _) -> n ^ " _"
      | _ -> debugfail None "Expected a function"


   type source_impl_t = source_code_t
  
   (** The identity function *)
   let identity a = a
   
   (** Returns a function generating code for a arithmetic 
       operation with casting if necessary *)
   let num_op ?(conva = identity) ?(convb = identity) opcode = 
      (fun a b -> "(" ^ conva(a) ^ ") " ^ opcode ^ " (" ^ convb(b) ^ ")")

   let cmp_op ?(conva = identity) ?(convb = identity) opcode = 
      (fun a b -> ("(" ^ conva(a) ^ ") " ^ opcode ^ " (" ^ convb(b) ^ ")"))

   (** Returns a function generating code for a certain
       type of operation *)
   let f_op op_f op_b = 
      (fun a b -> 
         match (a, b) with
         | (Float, Float) -> ((num_op op_f), Float, None)
         | (Bool, Bool)   -> ((num_op ~conva:mk_bool_to_int 
                                      ~convb:mk_bool_to_int op_b), Int, None)
         | (Bool, Float)  -> ((num_op op_f), Float, None)
         | (Float, Bool)  -> ((num_op op_f), Float, None)
         | (Float, Int)   -> ((num_op op_f), Float, None)
         | (Int, Float)   -> ((num_op op_f), Float, None)
         | (Int, Int)     -> ((num_op op_f), Int, None)
         | (Int, Bool)    -> ((num_op op_f), Int, None)
         | (Bool, Int)    -> ((num_op op_f), Int, None)
         | (_, _) -> 
            ((num_op op_f), Unit, 
               Some((string_of_type a) ^ " [" ^ op_f ^ "||" ^ op_b ^ "] " ^
               (string_of_type b)))
      )
(*
   let fixed_op (t1:type_t) (t2:type_t) (rett:type_t) 
         (op:string -> string -> string) =
      (fun a b -> if (a = t1) && (b = t2) 
         then (op, rett, None)  
         else (op, Unit, Some((string_of_type a) ^ " [cmp] " ^ 
            (string_of_type b))))*)
   
   (** Generates a function generating code for a compare operation *)
   (* FIX-ME: Daniel: Now K3 treats booleans as ints. When this is fixed in *)
   (* K3, we can also fix it here as well as in 'cmp_op'. *)
   let c_op op = (fun a b -> 
      match (a, b) with
      | (Float, Float) | (String, String) | (Bool, Bool) | (Int, Int)
      | (Int, Float) | (Float, Int) -> 
         ((cmp_op op), Bool, None)
      (* Sometimes there is K3 code that compares booleans to floats, so code
         is being generated to convert the boolean to a float *)
      | (Bool, Float) -> ((cmp_op op ~conva:mk_bool_to_float), Bool, None)
      | (Float, Bool) -> ((cmp_op op ~convb:mk_bool_to_float), Bool, None)
      | (Bool, Int) -> ((cmp_op op ~conva:mk_bool_to_int), Bool, None)
      | (Int, Bool) -> ((cmp_op op ~convb:mk_bool_to_int), Bool, None)
      | (Date, Date) -> 
         ((cmp_op op ~conva:mk_date_to_time ~convb:mk_date_to_time), Bool, 
            None)
      | (_, _) -> 
         ((cmp_op op), Unit, 
            Some((string_of_type a) ^ " [" ^ op ^ "] " ^ (string_of_type b)))
   )

   (** Implementation of arithmetic and comparision operators *)
   let add_op         : op_t = (f_op "+" "+")
   let mult_op        : op_t = (f_op "*" "*")
   let eq_op          : op_t = (c_op "==")
   let neq_op         : op_t = (c_op "!=")
   let lt_op          : op_t = (c_op "<")
   let leq_op         : op_t = (c_op "<=")
   let ifthenelse0_op : op_t = 
      (fun at bt -> 
         match at with 
         | Bool -> 
            ((fun a b -> ("if(" ^ a ^ ") { " ^ b ^ " } else { 0.0 }")), 
               bt, None)
         | Float -> 
            ((fun a b -> ("if(" ^ a ^ " != 0.0) { " ^ b ^ " } else { 0.0 }")),
               bt, None)
         | _ -> 
            ((fun a b -> "()"), Unit, 
               Some("ifthenelse("^(string_of_type at)^")"))
      )

   (** Generates the code for a constant expression *)
   let const ?(expr = None) (c:const_t) : code_t = 
      match c with 
      | CBool(y) -> (string_of_bool y, Bool)
      | CInt(y) -> ((string_of_int y) ^ "L", Int)
      | CFloat(y) -> ((string_of_float y) ^ "0", Float)
      | CString(y) -> ("\"" ^ y ^ "\"", String)
      | CDate(y,m,d) -> 
         let v = 
            "new GregorianCalendar(" ^ (string_of_int y) ^ "," ^ 
            (string_of_int m) ^ " - 1," ^ (string_of_int d) ^ ").getTime();" 
         in
         let cstr = 
            if Hashtbl.mem consts v then
               Hashtbl.find consts v
            else (
               nb_consts := !nb_consts + 1;
               let str = ("c" ^ (string_of_int !nb_consts)) in
               Hashtbl.add consts v str;
               str
            )
         in
         (cstr, Date)
   
   (** Generates the code for a variable *)
   let var ?(expr = None) (v:K3.id_t) (t:K3.type_t) : code_t = 
      ("var_" ^ v, map_type t)

   (** Generates the code for a tuple *)
   let tuple ?(expr = None) (c:code_t list) : code_t =
      if (List.length c) = 0 then 
         debugfail expr "Tuple has 0 elements"
      else 
         let t = Tuple(List.map (fun (_, t)->t) c) in
         (make_tuple ~tpe:(string_of_type t)
         (List.map (fun (s, _) -> "(" ^ s ^ ")") c), t)

   (** Generates code for the projection of a tuple *)
   let project ?(expr = None) ((tuple, tuplet):code_t) 
         (f:int list) : code_t =
      let tuple_members = 
         match tuplet with 
         | Tuple(t) -> t 
         | _ -> debugfail expr "projecting a non-tuple"
      in
      let _, in_list, out_list, t = 
         List.fold_left (fun (i,in_list,out_list,t_accum) t -> 
            (i+1, 
               in_list @
                  [if (List.mem i f) then "p_"^(string_of_int i) else "_"],
               out_list @ 
                  (if (List.mem i f) then ["p_"^(string_of_int i)] else []),
               t_accum@(if (List.mem i f) then [t] else []))) 
            (0,[],[],[]) 
            tuple_members
      in
      ("(" ^ tuple ^ ") match { case " ^ (make_list in_list) ^ " => " ^ 
      (make_list out_list) ^ 
      "; case _ => throw new IllegalArgumentException(\"boom\") }", Tuple(t))

   (** Generates the code for the creation of a singleton collection.
       Note: the new collection will be an intermediate collection *)
   let singleton ?(expr = None) ((d, dt):code_t) (t:K3.type_t) : code_t =
      let kt, vt, v = 
         match (map_type t) with
         | Tuple(t) -> 
            let vars = (list_vars "v._" (List.length t)) in
            let kv, vv = list_split vars ((List.length vars) - 1) in
            let kt, vt = list_split t ((List.length t) - 1) in
            (kt, vt, make_tuple([make_tuple 
               ~tpe:(string_of_type (Tuple(kt))) kv; List.hd vv]))
         | t -> [], [t], "v"
      in
      let tpe = Collection(Intermediate, kt, List.hd vt) in
      ("{ val v = " ^ d ^ "; new " ^ (string_of_type tpe) ^ "(List(" ^ v ^ ")) }", 
         tpe)
      
   (** Generates code to combine two collections
       
       Note: If the collections are of a different type
       it tries to cast the collections to a common supertype *)
   let combine ?(expr = None) (terms:code_t list): code_t =
      if (List.length terms) < 1 
      then debugfail expr "Empty combines are invalid" 
      else 
         let common_type = 
            List.fold_left (fun a (_, b) -> unify_types a b) 
            (match (List.hd terms) with (_, t) -> t) terms 
         in
         let k, v = 
            match common_type with 
            | Collection(_, k, v) -> (k, v) 
            | _ -> debugfail expr "Combine can only combine collections" 
         in
         ("({ val result = Map[" ^ (string_of_type (Tuple k)) ^ "," ^ 
         (string_of_type v) ^ "]();" ^ (make_list ~sep:";" ~parens:("", ";") 
         (List.map (fun (n, t) ->
            let ko, vo = 
               match t with 
               | Collection(_, ko, vo) -> ko, vo 
               | _ -> debugfail None "Expected collection" 
            in
            let key_len = List.length ko in
            let v_var =  "v" ^ (string_of_int (key_len + 1)) in
            let body = "val t = " ^ (make_tuple (list_vars "v" (key_len))) ^ 
               "; result += ((t, (result.get(t) match { case Some(v) => v + " ^
               v_var ^ "; case _ => " ^ v_var ^ " })))" 
            in
            "(" ^ n ^ ").foreach { " ^ (wrap_function_key_val ko vo 
            (Fn(ArgNTuple(List.map (fun x -> ArgN(x)) 
            (list_vars "v" (key_len + 1))), Tuple(k @ [v]), Unit)) body) ^ " }"
         ) terms)) ^ " new " ^ (string_of_type common_type) ^ "(result) })",
         common_type) 

   (** Generates code for an operator *)
   let op ?(expr = None) ((mk_op):op_t) ((a, at):code_t) 
         ((b, bt):code_t) : code_t =
      let (op_f,rt,err) = (mk_op at bt) in
      (match err with 
      | None -> () 
      | Some(err_msg) -> 
         debugfail expr ("Type mismatch in arithmetic: " ^ err_msg));
      ((op_f a b), rt)

   (** Generates code for an if-then-else statement *)
   let ifthenelse ?(expr = None) ((i, it):code_t) ((t, tt):code_t)
         ((e, et):code_t) : code_t =
      if it <> Bool then 
         debugfail expr "Type mismatch: non-bool condition"
      else
         let unified_type = unify_types tt et in
         (* Split long ifthenelse statements in multiple functions to
            avoid the JVM limitation for function lengths *)
         let tmt, emt = 
            begin match (Debug.active "IVC-TIMER"), expr with
            | true, Some(K3.IfThenElse(K3.Member(_, _), _, _)) ->
               "incTimer.time { " ^ t ^ " }", "ivcTimer.time { " ^ e ^ " }"
            | _, _ -> t, e
            end
         in
         ((if (String.length tmt) > 10000 then
            "({ def tb = " ^ (cast_up tmt tt unified_type) ^ "; def fb = " ^
            (cast_up emt et unified_type) ^ ";" ^
            "if(" ^ i ^ ") { tb } else { fb }})"
         else 
            "if(" ^ i ^ ") { " ^ (cast_up tmt tt unified_type) ^ " } else { " ^
            (cast_up emt et unified_type) ^ " }"), unified_type)

   (** Generates a block of statements *)
   let block ?(expr = None) (stmts:code_t list) : code_t =
      (make_list ~parens:("{","}") ~sep:"; " 
         (List.map (fun (s, _) -> s) stmts),
         (if (List.length stmts) < 1 then Unit 
         else (snd (List.hd (List.rev stmts)))))
   
   (** Adds some comment to a piece of code *)
   let comment ?(expr = None) (comment:string) ((c, ct):code_t) : code_t = 
      (" /* " ^ comment ^ " */ " ^ c, ct)
  
   (** Generates to code to iterate over a collection *)
   let iterate ?(expr = None) ((fn, fnt):code_t) 
         ((c, ct):code_t) : code_t =
      match ct with
      | Collection(_, kt, vt) -> ("(" ^ c ^ ").foreach { " ^ 
         (wrap_function_key_val kt vt fnt fn) ^ " }", Unit)
      | _ -> debugfail expr "Iteration of a non-collection"  

   (** Maps argument types to Scala codegen types and names *)
   let map_args_type args =       
      match args with
      | K3.AVar(v, vt) -> (ArgN("var_" ^ v), map_type vt)
      | K3.ATuple(tlist) -> (
         ArgNTuple(List.map (fun (v, _) -> ArgN("var_" ^ v)) tlist),
         Tuple(List.map (fun (_, t) -> map_type t) tlist))

   (** Generates code for a lambda function
   
       Note: Only the code for the body is generated, the header
       gets generated where the function is applied *)
   let lambda ?(expr = None) (args:K3.arg_t) ((b, bt):code_t) : code_t =
      let argsn, argst = map_args_type args in
      (b, Fn(argsn, argst, bt))

   (** Generates code for a lambda with two arguments *)
   let assoc_lambda ?(expr = None) (arg1:K3.arg_t) (arg2:K3.arg_t) 
         (b:code_t) : code_t =
      lambda arg1 (lambda arg2 b)
   
   (** Generates the code that refers to an external function 
   
       Note: some special characters in the function name are replaced *)
   let external_lambda ?(expr = None) (name:K3.id_t) 
         (args:K3.arg_t) (ret:K3.type_t) : code_t =
      let strlen = String.length name in
      let rec clean_name name i buffer: string =
         if (strlen = i) then 
            Buffer.contents buffer
         else (
            match name.[i] with
            | '/' -> 
               Buffer.add_string buffer "div"; 
               clean_name name (i + 1) buffer
            | c -> 
               Buffer.add_char buffer c; 
               clean_name name (i + 1) buffer
         ) 
      in
      let _, argst = map_args_type args in
      ("", ExtFn(clean_name name 0 (Buffer.create 0), argst, map_type ret))

   (** Generates code that applies a function
   
       Note: For lambda functions, a val assignment is generated
       instead of a function to reduce nesting *)
   let apply ?(expr = None) ((fn, fnt):code_t) 
         ((arg, argt):code_t) : code_t = (
      begin match fnt with
      | Fn(argsn, argst, rett) ->
         let str_type = 
            match argt with | Tuple(_) -> "" | _ -> ":" ^ (string_of_type argt) 
         in
         "{ val " ^ (string_of_argn ~prefix:"_" argsn) ^ str_type ^ " = " ^ 
         arg ^ ";" ^ (implicit_conversions argsn argt argst) ^ fn ^ "}"
      | ExtFn(n, a, _) -> 
         let lower_n = String.lowercase n in
         let n_a = List.length (flatten_tuple a) in
         if n_a = 1 then
            "(" ^ lower_n ^ "(" ^ arg ^ "))" 
         else
            "({ val arg = " ^ arg ^ "; " ^ lower_n ^ 
            (make_list (list_vars "arg._" n_a)) ^ "})"
      | _ -> debugfail None "Expected a function"
      end,
      fn_ret_type fnt)

   (** This function generates code to map from one collection to another *)
   let map ?(expr = None) ((fn, fnt):code_t) (exprt:K3.type_t) 
         ((c, ct):code_t) : code_t =
      let (keyt,valt) = 
         match (fn_ret_type fnt) with
         |  Tuple(retlist) ->
            if retlist = []
            then debugfail expr "Mapping function with empty ret"
            else ((List.rev (List.tl (List.rev retlist))),
               (List.hd (List.rev retlist)))
         |  x -> ([], x)
      in
      let kt, vt, wrapped_fn = 
         match ct with 
         | Collection(_, kt, vt) -> 
            (kt, vt, (wrap_function_key_val kt vt fnt fn))
         | _ -> debugfail expr "Mapping of a non-collection" 
      in
      let key_length = (List.length keyt) in
      let convert_to_key_value = 
         match keyt with
         | [] -> make_tuple ("()" :: "v" :: [])
         | x -> (string_of_type (Tuple((Tuple(keyt)) :: [valt]))) ^ "(" ^ 
            (make_list (list_vars "v._" key_length)) ^ ", v._" ^ 
            (string_of_int (key_length + 1)) ^ ")"
      in
      (c ^ ".map((y:Tuple2[" ^ 
      (string_of_tuple_type (List.map string_of_type kt)) ^ 
      "," ^ (string_of_type vt) ^ "]) => { val v = ({ " ^ wrapped_fn ^ 
      " })(y); " ^ 
      convert_to_key_value ^ " })", Collection(Intermediate, keyt,valt))

   (** Generates a code to aggregate elements of a collection (e.g. calculating
       the sum of a certain element *)
   let aggregate ?(expr = None) ((fn, fnt):code_t) ((init, initt):code_t) 
         ((c, ct):code_t) : code_t =
      match ct with
      | Collection(_, kt, vt) ->
         let foldfn = 
            begin match initt with
            | Int when not (Debug.active "NO-SPEC") -> "foldLong"
            | _   -> "fold"
            end
         in
         (c ^ "." ^ foldfn ^ "(" ^ init ^ ", { " ^ 
            (wrap_function_key_val (Tuple(kt) :: [vt]) 
            (fn_ret_type fnt) fnt fn) ^ 
            " })", (fn_ret_type (fn_ret_type fnt)))
      | _ -> 
         debugfail expr ("Aggregate on a non-collection: " ^ c ^ " has type " ^ 
      (string_of_type ct))

   (** Generates code to aggregate elements of a collection in groups *)
   let group_by_aggregate ?(expr = None) ((agg, aggt):code_t) 
         ((init, initt):code_t) ((group, groupt):code_t) 
         ((c, ct):code_t) : code_t =
      let key_type = 
         match (fn_ret_type groupt) with 
         | Tuple(tlist) -> tlist
         | _ -> [(fn_ret_type groupt)]
      in
      match ct with
      | Collection(_, kt, vt) ->
         (c ^ ".groupByAggregate(" ^ init ^ ", { " ^ 
         (wrap_function_key_val kt vt groupt group) ^ " }, { " ^ 
         (wrap_function_key_val (Tuple(kt) :: [vt]) 
            (fn_ret_type aggt) aggt agg) ^ 
         " })", 
            Collection(Intermediate, key_type,(fn_ret_type (fn_ret_type aggt))))
      | _ -> debugfail expr ("groupByAggregate on a non-collection: " ^ c ^ 
         " has type " ^ (string_of_type ct))

   (** Generates code to flatten a collection *)
   let flatten ?(expr = None) ((c, ct):code_t) : code_t =
      match ct with
      | Collection(_, [],Collection(_, ki,t)) ->
         (("(" ^ c ^ ").flatten()"), Collection(Intermediate, ki, t))
      | _ -> debugfail expr ("Flatten on a non-collection: " ^ c ^ 
         " has type " ^ (string_of_type ct))
   
   (** Generates code to check whether a certain key is part of a collection *)
   let exists ?(expr = None) ((c, ct):code_t) (key:code_t list) : code_t =
      match ct with 
      |  Collection(_, kt,vt) when kt <> [] ->
         ("(" ^ c ^ ").contains(" ^ (make_tuple 
            (List.map (fun (s, _) -> s) key)) ^ 
         ")", Bool)
      | _ -> debugfail expr "Existence check on a non-collection"

   (** Generates code to get the value for a certain key in a collection *)
   let lookup ?(expr = None) ((c, ct):code_t) (key:code_t list) : code_t =
      match ct with 
      | Collection(_, kt, vt) when kt <> [] ->
         ("(" ^ c ^ ").lookup(" ^ (make_tuple 
            (List.map (fun (s, _) -> s) key)) ^ ")", vt)
      | _ -> debugfail expr ("Lookup on a non-collection: " ^ c ^ 
         " has type " ^ (string_of_type ct))
      
  (* map, partial key, pattern -> slice *)
  let slice ?(expr = None) ((c, ct):code_t) (pkey:code_t list) 
           (pattern:int list) : code_t =
    if List.length pattern <> List.length pkey then
       debugfail expr ("Error: slice pattern and key length mismatch")
    else
    if List.length pattern = 0 then (c, ct)
    else
    match ct with 
    | Collection(Persistent, kt, vt) when kt <> [] ->
      (c ^ ".slice(" ^ (make_tuple (List.map fst pkey)) ^ ", List" ^ 
        (make_list (List.map string_of_int pattern)) ^ ")", 
      Collection(Intermediate, kt, vt))
    | Collection(_, kt,vt) when kt <> [] ->
      (c ^ ".slice(" ^ (make_tuple (List.map fst pkey)) ^ ", List" ^ 
        (make_list (List.map string_of_int pattern)) ^ ")", 
      Collection(Intermediate, kt, vt))
    | _ -> debugfail expr "Slice on a non-collection"

   (** Generates code to filter the elements of a collection *)
   let filter ?(expr = None) ((fn, fnt):code_t) 
         ((c, ct):code_t) : code_t =
      match ct with
      | Collection(_, kt, vt) ->
         (c ^ ".filter(" ^
            (wrap_function_key_val kt vt fnt fn) ^
            ")", ct)
      | _ -> debugfail expr ("Filter of a non-collection: " ^ c ^ 
         " has type " ^ (string_of_type ct))
   
   (** Generates code to get the value of a persistent value *)
   let get_value ?(expr = None) (t:K3.type_t) (map:K3.coll_id_t): code_t =
      (map ^ ".get()",  map_type t)

   let get_in_map ?(expr = None) (schema:K3.schema) (t:K3.type_t)   
      (map:K3.coll_id_t): code_t =
   (map, Collection(Persistent, (List.map (fun (_,x) -> map_type x) schema),  
      map_type t))

  let get_out_map ?(expr = None) (schema:K3.schema) (t:K3.type_t)
                 (map:K3.coll_id_t): code_t =
    (map, Collection(Persistent, (List.map (fun (_,x) -> map_type x) schema),  
      map_type t))

  let get_map ?(expr = None) ((ins,outs):(K3.schema*K3.schema))  
             (t:K3.type_t) (map:K3.coll_id_t): code_t =
    (map, Collection(Persistent, (List.map (fun (_, x) -> map_type x) ins),
             Collection(Persistent, (List.map (fun (_, x) -> map_type x) outs),
                         map_type t)))

   (** Generates code to update a persistent value *)
   let update_value ?(expr = None) (map:K3.coll_id_t) 
         ((v, vt):code_t): code_t =
      (map ^ ".update(" ^ v ^ ")", Unit)

   (** Gets the representation of zero for a given type **)
   let get_zero (t: type_t): string =
      begin match t with 
      | Int -> "0L"
      | Float -> "0.0"
      | Bool -> "false"
      | _ -> debugfail None ("Expected int or float but found " ^ 
         (string_of_type t))
      end

   (** Generates code to update a value in a persistent in map *)
   let update_in_map_value ?(expr = None) (map:K3.coll_id_t) 
         (key:code_t list) ((v, vt):code_t): code_t =
      let kc = make_tuple (List.map fst key) in
      let nv = "val nv = " ^ v ^ ";" in
      let uc = map ^ ".updateValue(" ^ kc ^ ", nv)" in
      let rc = map ^ ".remove(" ^ kc ^ ")" in
      let c = 
         if Debug.active "DELETE-ON-ZERO" then
            let z = get_zero vt in
            "{" ^ nv ^ "if(nv != " ^ z ^ ") " ^ uc ^ " else " ^ rc ^ "}"
         else "{" ^ nv ^ uc ^ "}"
      in
      (c, Unit)

   (** Generates code to update a value in a persistent out map *)
   let update_out_map_value ?(expr = None) (map:K3.coll_id_t) 
         (key:code_t list) ((v, vt):code_t): code_t =
      let kc = make_tuple (List.map fst key) in
      let nv = "val nv = " ^ v ^ ";" in
      let uc = map ^ ".updateValue(" ^ kc ^ ", nv)" in
      let rc = map ^ ".remove(" ^ kc ^ ")" in
      let c = 
         if Debug.active "DELETE-ON-ZERO" then
            let z = get_zero vt in
            "{" ^ nv ^ "if(nv != " ^ z ^ ") " ^ uc ^ " else " ^ rc ^ "}"
         else "{" ^ nv ^ uc ^ "}"
      in
      (c, Unit)

   (** Generates code to update a value in a persistent full map *)
   let update_map_value ?(expr = None) (map:K3.coll_id_t) (inkey:code_t list)
         (outkey:code_t list) ((v, vt):code_t): code_t =
      let ikc = make_tuple (List.map fst inkey) in
      let okc = make_tuple (List.map fst outkey) in
      let nv = "val nv = " ^ v ^ ";" in
      let uc = map ^ ".updateValue(" ^ ikc ^ ", " ^ okc ^ ", nv)" in
      let rc = map ^ ".remove(" ^ ikc ^ "," ^ okc ^ ")" in
      let c = 
         if Debug.active "DELETE-ON-ZERO" then
            let z = get_zero vt in
            "{" ^ nv ^ "if(nv != " ^ z ^ ") " ^ uc ^ " else " ^ rc ^ "}"
         else "{" ^ nv ^ uc ^ "}"
      in
      (c, Unit)

   (** Generates code to update a persistent in map *)
   let update_in_map ?(expr = None) (map:K3.coll_id_t) (v:code_t): code_t =
      (map ^ ".updateInMap()", Unit)
      
   (** Generates code to update a persistent out map *)
   let update_out_map ?(expr = None) (map:K3.coll_id_t) (v:code_t): code_t =
      (map ^ ".updateOutMap()", Unit)

   (** Generates code to update a map in a persistent full map *)
   let update_map ?(expr = None) (map:K3.coll_id_t) (inkey:code_t list) 
      ((v, vt):code_t): code_t =
      let conversion = match vt with
      | Collection(_, _, _) -> ".toPersistentCollection()"
      | _ -> "" in
      (map ^ ".updateValue(" ^ (make_tuple (List.map fst inkey)) ^ ", " ^ 
         v ^ conversion ^ ")", Unit)
   
   let remove_in_map_element ?(expr = None) (map:K3.coll_id_t) 
         (inkey:code_t list) : code_t =
      (map ^ ".remove(" ^ (make_tuple (List.map fst inkey)) ^ ")", Unit)

   (* persistent collection id, out key -> remove *)
   let remove_out_map_element ?(expr = None) (map:K3.coll_id_t) 
         (outkey:code_t list) : code_t =
      (map ^ ".remove(" ^ (make_tuple (List.map fst outkey)) ^ ")", Unit)

   (* persistent collection id, in key, out key -> remove *)
   let remove_map_element ?(expr = None) (map:K3.coll_id_t) (inkey:code_t list) 
         (outkey:code_t list) : code_t =
      (map ^ ".remove(" ^ (make_tuple (List.map fst inkey)) ^ ", " ^ 
      (make_tuple (List.map fst outkey)) ^ ")", Unit)

   (* Generates code that does nothing *)
   let unit_operation : code_t = ("()", Unit) 

   let lookup_def_val ?(expr = None) ((c, ct):code_t) (key:code_t list) 
         ((v, vt):code_t): code_t =
      match ct with 
      | Collection(_, kt, vt) when kt <> [] ->
         ("(" ^ c ^ ").lookup(" ^ (make_tuple 
            (List.map (fun (s, _) -> s) key)) ^ ", " ^ v ^ ")", vt)
      | _ -> debugfail expr ("Lookup on a non-collection: " ^ c ^ 
         " has type " ^ (string_of_type ct))      
   
   (** Generates code for a trigger function *)
   let trigger (eventt:Schema.event_t) (code:code_t list): code_t =
      let prefix, vars =
         match eventt with
         (* TODO: Implement corrective updates *)
         | Schema.CorrectiveUpdate(_, _, _, _, _) -> 
            debugfail None "Corrective updates not implemented yet"
         | Schema.InsertEvent(rel, vars, tpe) -> ("Insert" ^ rel, vars) 
         | Schema.DeleteEvent(rel, vars, tpe) -> ("Delete" ^ rel, vars)
         | Schema.SystemInitializedEvent -> ("SystemInitialized", []) 
      in
      let var_def = (make_list (List.map (fun (n, t) -> 
         "var_" ^ n ^ ": " ^ (string_of_type (map_base_type t))) vars)) 
      in
      let fn_def = "def on" ^ prefix ^ var_def in
      let stmts = (make_list ~parens:("{","}") ~sep:"; " 
         (List.map (fun (s, _) -> s) code)) 
      in
      (fn_def ^ " = " ^ stmts, Trigger(eventt))

   (** This function generates code for sources *)
   let source (source:Schema.source_t) 
         (adaptors:(Schema.adaptor_t * Schema.rel_t) list):
         (source_impl_t * code_t option * code_t option) =
      let sc = 
         source_count := !source_count + 1; string_of_int !source_count in
      let string_of_framing_type framing = 
         match framing with
         | Schema.FixedSize(i) -> "FixedSize(" ^ (string_of_int i) ^ ")"
         | Schema.Delimited(str) -> "Delimited(\"" ^ str ^ "\")"
      in
      let adaptors_strings = 
         (List.map (fun ((atype, akeys), (rel, schema, _)) -> 
            let args = if (List.length akeys) == 0 then 
               "" else 
            (make_list ~parens:(", ","") 
               (List.map (fun (k, v) -> 
                  match (k, v) with
                  | "delimiter", "|" -> "delimiter = \"\\|\""
                  | k, v -> k ^ " = \"" ^ v ^ "\"") akeys)) 
      in
      match atype with
      | "csv" -> 
         let string_of_schema_type t = 
            match t with
            | TBool -> "BoolColumn"
            | TInt -> "IntColumn"
            | TFloat -> "FloatColumn"
            | TString -> "StringColumn"
            | TDate -> "DateColumn"
            | _ -> debugfail None "Unsupported type in adaptor"
         in
         let schema_str = 
            make_list (List.map (fun (_, t) -> string_of_schema_type t) schema) 
         in
         "new CSVAdaptor(\"" ^ rel ^ "\", List" ^ schema_str ^ args ^ ")"
      | _ ->
         "createAdaptor(\"" ^ atype ^ "\", \"" ^ rel ^ "\", List" ^
         (make_list 
            (List.map (fun (k,v) -> "(\"" ^ k ^ "\", \"" ^ v ^ "\")") akeys)) ^
         ")"
         ) adaptors)
      in
      let source_init = 
         match source with 
         | Schema.FileSource(s, f) -> 
            "createInputStreamSource(new FileReader(\"" ^ s ^ 
            "\"), List" ^ (make_list adaptors_strings) ^ ", " ^ 
            (string_of_framing_type f) ^ ")"
         | Schema.SocketSource(_, _, _) -> 
            debugfail None "Sockets not implemented"
         | Schema.PipeSource(_, _) -> 
            debugfail None "Pipes not implemented"
         | Schema.NoSource -> 
            debugfail None "Empty source not implemented"
      in
      let init_code = 
         let fn_event_to_tuple = 
            "x => x match {" ^ 
            (make_list ~sep:";" ~parens:("",";") (List.map 
            (fun (adaptor, (rel, vars, _)) -> 
               "case StreamEvent(InsertTuple, o, \"" ^ rel ^ "\", List" ^ 
               let k = 
                  (make_list (List.map (fun (n, _) -> "var_" ^ n) vars)) in
               let knt = 
                  (make_list (List.map (fun (n, t) -> "var_" ^ n ^ ":" ^ 
                  string_of_type (map_base_type t)) vars)) 
               in
               knt ^ ") => " ^ 
               "if(" ^ rel ^ ".contains(" ^ k ^ ")) { val count = " ^ rel ^ 
               ".lookup(" ^ k ^ ") + 1; " ^ 
               rel ^ ".updateValue(" ^ k ^ ", count) } else { " ^
               rel ^ ".updateValue(" ^ k ^ ", 1) }" ^
               "case event => throw DBTFatalError(" ^
               "\"Event could not be dispatched: \" + event)"
            ) adaptors)) ^
            "}"
         in
         ".forEachEvent({" ^ fn_event_to_tuple ^ "})"
      in
      let source_n = "s" ^ sc in 
      ("val " ^ source_n ^ " = " ^ source_init, Some(init_code, Unit), 
         Some(source_n, Unit))

   (** Puts all the generated code together in a Scala object *)
   let main (schemas:K3.map_t list) (patterns:pattern_map) 
         (tables:(source_impl_t * code_t option * code_t option) list)
         (streams:(source_impl_t * code_t option * code_t option) list)
         (triggers:code_t list) 
         (tlqs:(string * K3.expr_t * code_t) list): code_t = 
      let str_tlqs = 
         (make_list ~sep:";" ~parens:("",";") (List.map 
            (fun (n, _, (c, ct)) ->
               "def get" ^ n ^ "():" ^ (string_of_type ct) ^ " = {" ^ c ^ "}"
            ) tlqs))
      in
      let print_results = 
         "def printResults(): Unit = { val pp = new PrettyPrinter(8000, 2);" ^
         (make_list ~sep:";" ~parens:("",";") 
            (List.map (fun (x, _, (c, ct)) ->
               match ct with
               | Collection(_, _, _) -> 
                  "println(pp.format(<" ^ x ^ ">{ get" ^ x ^ "().toXML() }" ^ 
                  "</" ^ x ^ ">))"
               | _ -> "println(pp.format(<" ^ x ^ ">{ get" ^ x ^ "() }" ^ 
               "</" ^ x ^ ">))") tlqs)) ^ 
         (if Debug.active "IVC-TIMER" then "ivcTimer.print; incTimer.print;" else "") ^ 
         " }" 
      in
      let str_schema =
         let make_type_list t: string = 
            string_of_tuple_type 
               (List.map (fun (n, t) -> string_of_type (map_base_type t)) t) 
         in
         let sndIdx rel tpe valt =
            try
            (* find patterns for a certain map *)
               let map_patterns = 
                  snd (List.find (fun (n, _) -> n = rel) patterns) in
               let idxs: string list = List.fold_left 
                  (fun agg pat -> 
                     let rec list_project l ids = 
                        match ids with 
                        | [] -> []
                        | x :: xs -> (List.nth l x) :: (list_project l xs)
                     in
                     let map_pattern (vars,ids): string = (
                        let vars = (list_vars "x" (List.length tpe)) in 
                        let sids = (List.map string_of_int ids) in                       
                        (make_list ~sep:"_" ~parens:("\"","\"") sids) ^ 
                        " -> SecondaryIndex[" ^ 
                        (make_type_list (list_project tpe ids)) ^
                        "," ^ (make_type_list tpe) ^ ", " ^ valt ^ 
                        "](x => x match { case " ^ (make_tuple vars) ^ " => " ^
                        (make_tuple (list_project vars ids)) ^ " })")

                     in 
                     match pat with 
                     | Patterns.In(([],[])) -> agg
                     | Patterns.Out(([],[])) -> agg 
                     | Patterns.In(p) -> (map_pattern p) :: agg
                     | Patterns.Out(p) -> (map_pattern p) :: agg
                  ) [] map_patterns 
               in
               match idxs with
               | [] -> "None"
               | _  -> "Some(Map" ^ (make_list idxs) ^ ")"
            with
            | Not_found -> "None"
         in
         (make_list ~parens:("",";") ~sep:";" (List.map 
            (fun (id,ivars,ovars,t) ->
               let str_val_t = string_of_type (map_base_type t) in
               match (ivars, ovars) with
               | ([], []) -> "var " ^ id ^ " = SimpleVal[" ^ str_val_t ^ 
                  "](\"" ^ id ^"\", 0)"
               | ([], os) -> "var " ^ id ^ " = new K3PersistentCollection[" ^
                  (make_type_list os) ^ ", " ^ str_val_t ^ "]" ^
                  "(\"" ^ id ^ "\", Map(), " ^ (sndIdx id os str_val_t) ^ 
                  ") /* out */"
               | (is, []) -> "var " ^ id ^ " = new K3PersistentCollection[" ^ 
                  (make_type_list is) ^ ", " ^ str_val_t ^ "]" ^
                  "(\"" ^ id ^ "\", Map(), " ^ 
                  (sndIdx id is str_val_t) ^ ") /* in */"
               | (is, os) -> "var " ^ id ^ 
                  " = new K3FullPersistentCollection[" ^ (make_type_list is) ^ 
                  "," ^ (make_type_list os) ^ ", " ^ str_val_t ^ "]" ^ 
                  "(\"" ^ id ^ "\", Map(), " ^ 
                  (sndIdx id is str_val_t) ^ ") /* full */") 
            schemas))
      in
      (* The code for the run function *)
      let run = 
         "def act(): Unit =" ^
         "{ fillTables();" ^
         "onSystemInitialized();" ^
         "sources.start;" ^
         "while(true) {" ^
         "receive { " ^
         "case EndOfStream => {" ^
         "supervisor ! DBTDone; " ^
         "exit(); }" ^
         "case e: DBTEvent => dispatcher(e) }}}"
      in
      (* Imports for the Scala code *)
      let imports = 
         (make_list ~sep:";" ~parens:("",";") (List.map 
            (fun x -> "import " ^ x)
            [ 
               "java.io._";
               "org.dbtoaster.dbtoasterlib.StreamAdaptor._";
               "org.dbtoaster.dbtoasterlib.K3Collection._";
               "org.dbtoaster.dbtoasterlib.Source._";
               "org.dbtoaster.dbtoasterlib.DBToasterExceptions._";
               "org.dbtoaster.dbtoasterlib.ImplicitConversions._";
               "org.dbtoaster.dbtoasterlib.StdFunctions._";
               "org.dbtoaster.dbtoasterlib.QueryInterface._";
               "scala.collection.mutable.Map";
               "java.util.Date";
               "java.util.GregorianCalendar";
               "xml._";
               "scala.actors.Actor";
               "scala.actors.Actor._"
            ])
         )
      in
      let dispatcher = 
         "def dispatcher(event: DBTEvent): Unit = { event match { " ^
         (make_list ~parens:("",";") ~sep:";" 
         (List.map (fun x -> (
            match (snd x) with 
            | Trigger(eventt) -> 
               let event_type, trigger_type, rel, vars = 
               match eventt with 
               (* TODO: Implement corrective updates *)
               | Schema.CorrectiveUpdate(_, _, _, _, _) -> 
                  debugfail None "Corrective updates not implemented yet"
               | Schema.InsertEvent(rel, vars, tpe) -> 
                  ("InsertTuple", "Insert", rel, vars)
               | Schema.DeleteEvent(rel, vars, tpe) -> 
                  ("DeleteTuple", "Delete", rel, vars)
               | Schema.SystemInitializedEvent -> 
                  ("SystemInitialized", "SystemInitialized", "", []) 
               in
               "case StreamEvent(" ^ event_type ^ ", o, \"" ^ rel ^ "\", " ^ (
               if (List.length vars) = 0 then 
                  "Nil" 
               else (
                  make_list ~parens:("", "::Nil") ~sep:"::"
                     (List.map (fun (n, t) ->
                        "(var_" ^ n ^ ": " ^ 
                        (string_of_type (map_base_type t)) ^ ")") vars))) ^
               ") => on" ^ 
               trigger_type ^ rel ^ 
               (make_list (List.map (fun (n, t) -> "var_" ^ n) vars))
            | t -> 
               debugfail None ("Trigger expected but found " ^ 
               (string_of_type t))
            )) triggers)) ^ 
         "case EndOfStream => ();" ^
         "case _ => " ^
         "throw DBTFatalError(\"Event could not be dispatched: \" + event)" ^
         "} " ^
         "supervisor ! DBTTupleProcessed; }" 
      in
      let str_sources = 
         (make_list ~parens:("",";") ~sep:";" 
            (List.map (fun (s, _, _) -> s) streams)) ^
         (make_list ~parens:("",";") ~sep:";" 
            (List.map (fun (s, _, _) -> s) tables))
      in
      (* Code to multiplex available sources *)
      let str_streams = 
         "val sources = new SourceMultiplexer(this, List" ^ 
         (make_list (List.map (fun (_, _, os) -> 
            match os with 
            | Some(s, _) -> s 
            | _ -> debugfail None "Missing source name"
            ) streams)) ^ ");"
      in
      (* Code for the triggers *)
      let triggers = 
         (make_list ~parens:("",";") ~sep:";" 
         (List.map (fun (b, bt) -> b) triggers)) 
      in
      (* Code to load the static tables *)
      let filltables = 
         "def fillTables(): Unit = {" ^
         (make_list ~parens:("",";") ~sep:";" (List.map (fun (s, init, _) ->
         match init with 
         | Some(i, _) -> s ^ i
         | None -> ""
         ) tables)) ^
         "}"
      in 
      let set_supervisor = 
         "def setSupervisor(supervisor: Actor) = this.supervisor = supervisor;"
      in
      let print_maps = 
         "def printMapSizes() = {" ^
         (make_list ~parens:("",";") ~sep:";" (List.map 
            (fun (id,ivars,ovars,t) -> id ^ ".printSize") 
            schemas)) ^
         "}"
      in
      imports ^
      " package org.dbtoaster { " ^
      "class Query() " ^ 
      "extends DBTQuery { var supervisor: Actor = null;" ^ (
         if Debug.active "IVC-TIMER" then 
            "val ivcTimer = new DBTTimer(\"IVC\");" ^
            "val incTimer = new DBTTimer(\"INC\");" 
         else "") ^
      set_supervisor ^
      (Hashtbl.fold (fun a b c -> "val " ^ b ^ " = " ^ a ^ ";" ^ c) consts "") ^
      str_sources ^ str_streams ^ str_schema ^ str_tlqs ^ triggers ^ 
      filltables ^ dispatcher ^ run ^ print_results ^ print_maps ^ " }}", 
      Unit

   (** This function formats a piece of scala code 
       Note: it uses a string buffer to store the formatted
       code for performance *)
   let format_code (str:string) = 
      let strlen = String.length str in
      let rec _format_code (pos:int) (ind:int) (newline:bool) buffer: string = 
         if (strlen = pos) then 
            Buffer.contents buffer
         else
            let rec make_indent (len:int): string = 
               if len = 0 then "" else " " ^ (make_indent (len - 1)) in
            let npos = pos + 1 in
            match str.[pos] with
            | '{' -> let nind = ind + 2 in ( 
               if newline then Buffer.add_string buffer (make_indent ind);
               Buffer.add_char buffer '{'; 
               _format_code npos nind true buffer
            )
            | '}' -> let nind = ind - 2 in ( 
               Buffer.add_string buffer (line_sep ^ (make_indent nind) ^ "}"); 
               _format_code npos nind true buffer
            )
            | ';' -> (
               Buffer.add_string buffer  (";" ^ line_sep); 
               _format_code npos ind true buffer
            )
            | x -> if x == ' ' && newline then 
               _format_code npos ind true buffer
            else ( 
               Buffer.add_string buffer ((if newline then line_sep ^ 
               (make_indent ind) else "") ^ (Char.escaped x)); 
               _format_code npos ind false buffer
            )
      in
      _format_code 0 0 true (Buffer.create 1)
   
   (** Writes the formatted code to a channel *)
   let output ((code, _):code_t) (out:out_channel): unit =
      output_string out (format_code code)
      
   (** Returns the formatted code *)
   let to_string ((code, _):code_t): string =
      format_code code

end

module K3CG : K3Codegen.CG = K3S
