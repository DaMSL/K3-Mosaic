open K3.AST
open K3Values

val test_expressions : string -> program_test_t -> unit

val test_program : 
  program_t -> (program_t -> (address * program_env_t) list) -> 
  string -> program_test_t -> unit

val parse_expression_test : string -> program_test_t
val parse_program_test : string -> program_test_t
