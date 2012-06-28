exception SymbolNotFound of string
exception DuplicateSymbol of string

(* Prefixes and symbol classes *)
let sym_prefix, default_class = "__", "K3"

(* Counters *)
let sym_prefixes = Hashtbl.create 10
let sym_counters = Hashtbl.create 10

let is_sym_prefix class_id = Hashtbl.mem sym_prefixes class_id
let is_sym_counter class_id = Hashtbl.mem sym_counters class_id

let is_symbol class_id =
  (is_sym_prefix class_id) && (is_sym_counter class_id)

let get_symbol_prefix class_id =
  try Hashtbl.find sym_prefixes class_id
  with Not_found -> raise (SymbolNotFound class_id) 
  
let get_symbol_counter class_id =
  try let current = Hashtbl.find sym_counters class_id in
    Hashtbl.replace sym_counters class_id (current+1);
    current
  with Not_found -> raise (SymbolNotFound class_id) 

let register_symbol class_id prefix =
  if not(is_sym_prefix class_id || is_sym_counter class_id) then
    begin
      Hashtbl.replace sym_prefixes class_id prefix;
      Hashtbl.replace sym_counters class_id 0
    end
  else raise (DuplicateSymbol class_id)

let gen_string_sym class_id sym =
  (get_symbol_prefix class_id)^sym^
  (string_of_int (get_symbol_counter class_id))

let gen_int_sym class_id = (get_symbol_counter class_id)

let initialize_k3_symbols () =
  register_symbol default_class sym_prefix
;;

initialize_k3_symbols();;  
