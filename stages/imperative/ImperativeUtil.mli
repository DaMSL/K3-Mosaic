open K3.AST
open Imperative

module Util : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	(* AST accessors *)
	val id_of_expr : 'a expr_t -> int 
	val tag_of_expr : 'a expr_t -> expr_tag_t
	val meta_of_expr : 'a expr_t -> 'a
	
	val id_of_cmd : 'a cmd_t -> int 
	val tag_of_cmd : 'a cmd_t -> 'a cmd_tag_t
	val meta_of_cmd : 'a cmd_t -> 'a
	
	(* Stringification *)
  val print_type : type_t -> unit
  val print_decl : ('a -> string) -> 'a decl_t -> unit
  val print_expr : ('a -> string) -> 'a expr_t -> unit
  val print_cmd  : ('a -> string) -> 'a cmd_t -> unit
  val print_component : ('a -> string) -> 'a component_t -> unit
  val print_program : ('a -> string) -> 'a program_t -> unit

	val string_of_type: type_t -> string
	val string_of_decl: ('a -> string) -> 'a decl_t -> string
	val string_of_expr: ('a -> string) -> 'a expr_t -> string
	val string_of_cmd: ('a -> string) -> 'a cmd_t -> string
  val string_of_component : ('a -> string) -> 'a component_t -> string
	val string_of_program: ('a -> string) -> 'a program_t -> string
	
	val var_ids_of_expr : 'a expr_t -> id_t list
	val var_ids_of_decl : 'a decl_t -> id_t list
	val var_ids_of_cmd : 'a cmd_t -> id_t list

  (* Symbol helpers *)
  val gen_expr_sym : unit -> int
  val gen_cmd_sym  : unit -> int
  val gen_flow_sym : unit -> int
  
  val gen_expr_name : string -> string
  val gen_cmd_name  : string -> string   
  val gen_flow_name : string -> string

  (* Typing helpers *)
  val vi_type : type_t -> K3.AST.value_type_t
  val bi_type : type_t -> K3.AST.base_type_t
  val ib_type  : K3.AST.base_type_t -> type_t 
	val iv_type  : K3.AST.value_type_t -> type_t
	val i_type   : K3.AST.type_t -> type_t
	val unit_t   : type_t
	val int_t    : type_t
	val bool_t   : type_t
	val string_t : type_t
	val addr_t   : type_t

  (* AST construction *)
  val mk_iexpr : expr_tag_t -> 'a -> 'a expr_t list -> 'a expr_t
  val mk_cmd : 'a cmd_tag_t -> 'a -> 'a cmd_t list -> 'a cmd_t

  (* Declaration constructors *)
  val mk_var_decl : id_t -> type_t -> 'a decl_args_t option -> 'a decl_t

  (* Expression constructors *)
  val mk_const : 'a -> constant_t -> 'a expr_t
  val mk_var   : 'a -> id_t -> 'a expr_t
  val mk_tuple : 'a -> 'a expr_t list -> 'a expr_t
  val mk_op    : 'a -> op_t -> 'a expr_t list -> 'a expr_t
  val mk_fn    : 'a -> fn_t -> 'a expr_t list -> 'a expr_t
 
  (* Command constructors *)
  val mk_assign : 'a -> id_t -> 'a expr_t -> 'a cmd_t
  val mk_decl   : 'a -> 'a decl_t -> 'a cmd_t
  val mk_expr   : 'a -> 'a expr_t -> 'a cmd_t
  val mk_ifelse : 'a -> 'a expr_t -> 'a cmd_t list -> 'a cmd_t
  val mk_block  : 'a -> 'a cmd_t list -> 'a cmd_t
  val mk_for    : 'a -> id_t -> type_t -> 'a expr_t -> 'a cmd_t list -> 'a cmd_t
  val mk_while  : 'a -> 'a expr_t -> 'a cmd_t list -> 'a cmd_t
  val mk_return : 'a -> 'a expr_t -> 'a cmd_t

  (* Higher-level constructors *)
  val call_proc : (unit -> 'a) -> id_t -> (type_t * 'a) expr_t list -> (type_t * 'a) cmd_t
  
  val invoke_method :
    'a -> id_t -> 'a expr_t -> 'a expr_t list -> 'a expr_t

  val call_method_proc :
    (unit -> 'a) -> id_t -> (type_t * 'a) expr_t -> (type_t * 'a) expr_t list
    -> (type_t * 'a) cmd_t

  val cast_to_top : (unit -> 'a) -> (type_t * 'a) expr_t -> (type_t * 'a) expr_t
end