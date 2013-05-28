open K3.AST
open K3.Annotation

val string_of_base_type : base_type_t -> string

val string_of_value_type : value_type_t -> string

val string_of_mutable_type : mutable_type_t -> string

val string_of_type : type_t -> string

val string_of_expr : expr_t -> string

val string_of_program : ?uuid_highlight: int option -> program_t -> string
