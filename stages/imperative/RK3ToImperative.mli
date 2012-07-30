open K3
open K3Typechecker
open ReifiedK3
open Imperative

val imperative_of_reified_expr :
  (unit -> 'a) -> (id_t * arg_t) list -> 'a reified_expr_t -> 'a cmd_t list

val imperative_of_program :
  (unit -> 'a) -> 'a tprogram_t -> 'a Imperative.program_t