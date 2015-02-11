open K3.AST

(* code to include when switch sends put *)
val sw_ack_init_code : expr_t
(* code to include when node receives put *)
val nd_ack_send_code : expr_t

val globals : (declaration_t * annotation_t) list
val functions : (declaration_t * annotation_t) list
val triggers : (declaration_t * annotation_t) list
