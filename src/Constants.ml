open Int64

(* Driver constants *)
let default_search_paths = ["."]
let default_ip           = "localhost"
let default_port         = 10000
let default_node_address = (default_ip, default_port)
let default_role         = None
let default_peers        = []
let default_run_length   = Int64.minus_one
let default_print_types  = false
let default_debug_info   = false