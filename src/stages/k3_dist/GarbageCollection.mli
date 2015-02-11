open K3.AST

(* code to include when switch sends put *)
val sw_ack_init_code : addr_nm:id_t -> vid_nm:id_t -> expr_t
(* code to include when node receives put *)
val nd_ack_send_code : addr_nm:id_t -> vid_nm:id_t -> expr_t

val global_vars : (declaration_t * annotation_t) list
val triggers : K3Dist.config -> (flow_statement_t * annotation_t) list
