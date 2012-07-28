open K3
open K3Typechecker
open ReifiedK3
open Imperative

val imperative_of_reified_node :
  (unit -> 'a) -> 
  'a cmd_t option -> 'a cmd_t list list -> 'a reified_expr_t
    -> 'a cmd_t list

val imperative_of_reified_expr : (unit -> 'a) -> 'a reified_expr_t -> 'a cmd_t list

val imperative_of_program : (unit -> 'a) -> 'a tprogram_t -> 'a Imperative.program_t