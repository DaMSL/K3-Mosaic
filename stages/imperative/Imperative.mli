(* A simplified imperative programming language *)

open Tree
open K3.AST

(* The interface required of backend implementation languages *)
module type TargetLanguage = sig
  type ext_type_t
  type ext_type_decl_t
  type ext_collection_fn_t
  type ext_fn_t
  type 'a ext_cmd_t
  
  val print_ext_type            : ext_type_t -> unit
  val print_ext_type_decl       : ext_type_decl_t -> unit
  val print_ext_collection_fn   : ext_collection_fn_t -> unit
  val print_ext_fn              : ext_fn_t -> unit
  val print_ext_cmd             : ('a -> string) -> 'a ext_cmd_t -> unit
end

(* Imperative AST implementation *)
module AST(T : TargetLanguage) :
sig
	type type_t =
	    TInternal of K3.AST.type_t
	  | TNamed    of id_t
    | TMap      of type_t * type_t
	  | TExt      of T.ext_type_t
	
	type type_decl_t =
	    TExpr      of type_t
	  | TComposite of (id_t * type_t) list
    | TExtDecl   of T.ext_type_decl_t
	
	type type_env_t = id_t * type_t list
	
	type 'a decl_args_t =
	    Constructor of 'a expr_t list
	  | Init of 'a expr_t
	
	(* Declarations for imperative programs. *)
	and 'a decl_t =
	    (* Type declarations *) 
	    DType  of id_t * type_decl_t
	
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
	
	    (* Associative data structure primitives *)
	  | Contains | Find

    | CFExt of T.ext_collection_fn_t
	
	and member_access_fn_t = Position of int | Field of id_t
	
	and fn_t =
	    Collection of collection_fn_t
	  | Member     of member_access_fn_t
	  | Named      of id_t
	  | Send       of id_t (* type tag *)
    | FExt       of T.ext_fn_t
	
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
    | IfThenElse of 'a expr_t
    | Block
	  | Foreach    of id_t * type_t * 'a expr_t
    | While      of 'a expr_t 
    | Return     of 'a expr_t
    | CExt       of 'a T.ext_cmd_t
	
	and 'a cmd_t = ((int * 'a cmd_tag_t) * 'a) tree_t
	
	type 'a program_t = ('a decl_t * 'a) list

end

(* Module types for AST exports *)
module type IAST = sig
  type    type_t
  type    type_decl_t
  type    type_env_t
  type 'a decl_args_t
  type 'a decl_t
  type    op_t
  type    collection_fn_t
  type    member_access_fn_t
  type    fn_t
  type    expr_tag_t
  type 'a expr_t
  type 'a cmd_tag_t
  type 'a cmd_t
  type 'a program_t
end

module type Export = sig module AST : IAST end
