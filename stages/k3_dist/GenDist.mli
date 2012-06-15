exception ProcessingFailed of string
val vid_type : K3.value_type_t
val ip_type : K3.value_type_t
val arg_types_of_t : ProgInfo.prog_data_t -> string -> K3.value_type_t list
val arg_names_of_t : ProgInfo.prog_data_t -> string -> K3.id_t list
val args_of_t_as_vars :
  ProgInfo.prog_data_t ->
  string -> ((int * K3.expr_tag_t) * int) Tree.tree_t list
val args_of_t_with_v :
  ProgInfo.prog_data_t -> string -> (K3.id_t * K3.value_type_t) list
val arg_types_of_t_with_v :
  ProgInfo.prog_data_t -> string -> K3.value_type_t list
val args_of_t_as_vars_with_v :
  ProgInfo.prog_data_t ->
  string -> ((int * K3.expr_tag_t) * int) Tree.tree_t list
val send_fetch_name_of_t : 'a -> string -> string
val rcv_fetch_name_of_t : 'a -> string -> string
val send_push_name_of_t :
  ProgInfo.prog_data_t -> string -> int -> ProgInfo.map_id_t -> string
val rcv_put_name_of_t : 'a -> string -> string
val rcv_put_args_of_t : ProgInfo.prog_data_t -> string -> K3.arg_t
val rcv_push_name_of_t :
  ProgInfo.prog_data_t -> string -> int -> ProgInfo.map_id_t -> string
val do_complete_name_of_t : 'a -> string -> int -> string
val route_for : ProgInfo.prog_data_t -> ProgInfo.map_id_t -> string
val shuffle_for :
  ProgInfo.prog_data_t ->
  int -> ProgInfo.map_id_t -> ProgInfo.map_id_t -> string
val send_fetch_trigger :
  ProgInfo.prog_data_t -> string -> int K3.declaration_t
val send_push_stmt_map_funcs :
  ProgInfo.prog_data_t -> string -> int K3.declaration_t list
val gen_dist : ProgInfo.prog_data_t -> 'a -> int K3.declaration_t list
