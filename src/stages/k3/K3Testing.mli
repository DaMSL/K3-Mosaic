open K3.AST
open K3Values.Value

val test_program :
  K3Global.peer_t list -> K3Global.peer_t list -> program_t -> (program_t -> (address * env_t) list) ->
  string -> program_test_t -> unit
