open K3.AST
open K3.Annotation

(* Annotations *)
val string_of_annotation : annotation_t -> string

val string_of_address: address -> string
val string_of_container_type: container_type_t -> string
val string_of_const: constant_t -> string
val string_of_tag_type: expr_tag_t -> string

(* Compact stringification *)
val flat_string_of_base_type: base_type_t -> string
val flat_string_of_mutable_type: mutable_type_t -> string
val flat_string_of_value_type: value_type_t -> string
val flat_string_of_type: type_t -> string
val flat_string_of_arg: arg_t -> string
val flat_string_of_expr_tag: expr_tag_t -> string list -> string
val flat_string_of_expr: expr_t -> string

val flat_string_of_flow_statement : flow_statement_t -> string
val flat_string_of_flow_program   : flow_program_t -> string
val flat_string_of_declaration    : declaration_t -> string

val flat_string_of_program: program_t -> string

(* Pretty stringification, used as the default. *)
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
val string_of_declaration      : declaration_t -> string

val string_of_program:
  ?print_id:bool ->
  ?print_expr_fn:(?print_id:bool -> expr_t -> unit Lazy.t)
  -> program_t -> string