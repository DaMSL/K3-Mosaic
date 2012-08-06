(* A simplified imperative programming language *)

open Tree
open K3

type type_t = TInternal of K3.type_t | TNamed of id_t
type type_env_t = id_t * type_t list  

type 'a decl_args_t =
    Constructor of 'a expr_t list
  | Init of 'a expr_t

(* Declarations for imperative programs. *)
and 'a decl_t =
    (* Composite type declaration *) 
    DType  of id_t * type_t

    (* Variable identifier, type and optional constructor/initializer *)
  | DVar   of id_t * type_t * 'a decl_args_t option
  
    (* Function name, arguments, return type and body *)
  | DFn    of id_t * arg_t * type_t * 'a cmd_t list

(* Primitive operations *)
and op_t = Add | Mult | Neg | Eq | Neq | Lt | Leq | Ternary

and collection_fn_t = 
    (* Standard builtins *)
    Peek | Slice | Insert | Update | Delete 
    
    (* Extra builtins *) 
  | Combine | Range | Sort

    (* Map primitives *)
  | Contains | Find

and composite_fn_t = Position of int | Field of id_t

and fn_t =
    Collection of collection_fn_t
  | Composite of composite_fn_t
  | Named of id_t
  | Send  of id_t (* type tag *)

(* Expression types *)
and expr_tag_t =
    Const  of constant_t
  | Var    of id_t
  | Tuple
  | Just
  | Op     of op_t
  | Fn     of fn_t

and 'a expr_t = ((int * expr_tag_t) * 'a) tree_t 

(* Command types *)
and 'a cmd_tag_t =
    Assign     of id_t * 'a expr_t 
  | Decl       of 'a decl_t
  | Expr       of 'a expr_t
  | Block
  | For        of id_t * type_t * 'a expr_t
  | IfThenElse of 'a expr_t

and 'a cmd_t = ((int * 'a cmd_tag_t) * 'a) tree_t

type 'a program_t = 'a decl_t list
