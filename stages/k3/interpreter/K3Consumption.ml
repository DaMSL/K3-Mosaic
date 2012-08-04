(* Dealing with Sources and Consumption. *)
open Symbols
open Printing
open K3
open K3Util
open K3Values
open K3Typechecker

exception StreamError of id_t

type state_id = int

type channel_impl_t =
  | In  of in_channel option
  | Out of out_channel option

type action_t = 
  | VEndpoint   of id_t * type_t * stream_channel_t * channel_impl_t
  | VDerived    of id_t
  | VEOF

and stream_fsm_t = (state_id * (action_t * state_id * state_id)) list

type stream_env_t = (id_t * stream_t) list
type fsm_env_t = (id_t * stream_fsm_t) list


(* State identifier generation *)
let state_sym_class = "FSM"
let gen_state_sym() = gen_int_sym state_sym_class
let _ = register_symbol state_sym_class "__";;

(* Stringification *)
let string_of_action a = match a with 
  | VEndpoint (id, t, s, impl) -> "Endpoint("^id^")" 
  | VDerived id -> "Derived("^id^")"
  | VEOF -> "EOF"

let print_transition (i, (a,n,f)) =
  ps (string_of_int i); psp(); ps ": "; pc();
  ps (String.concat ", " [string_of_action a; string_of_int n; string_of_int f])

let print_fsm fsm =
  ps "[ "; fnl();
  List.iter (fun t -> pb 2; print_transition t; fnl()) fsm;
  ps " ]"; fnl()

let string_of_fsm fsm = wrap_formatter (fun () -> print_fsm fsm)

let print_stream_env stream_env =
  List.iter (fun (id,s) -> print_stream s; fnl()) stream_env

let string_of_stream_env stream_env =
  wrap_formatter (fun () -> print_stream_env stream_env)

let print_fsm_env fsm_env =
  List.iter (fun (id, fsm) -> ps id; ps " = "; pc(); print_fsm fsm; fnl()) fsm_env

let string_of_fsm_env fsm_env = wrap_formatter (fun () -> print_fsm_env fsm_env)

(* Evaluation methods *)
let value_of_string t = match t with
  | TInt -> fun v -> VInt(int_of_string v)
  | TFloat -> fun v -> VFloat(float_of_string v)
  | _ -> fun v -> VString(v)

let pull_source i t s in_chan =
	let signature = 
		match t <| base_of %++ value_of |> (fun () -> raise (StreamError i)) with
		| TTuple(ts) -> List.map base_of ts
		| _ -> raise (StreamError i)
	in
	match s with
	  | File(CSV, _) -> 
	    (try 
         let next_record = Str.split (Str.regexp ",") (input_line in_chan) in
	       Some (VTuple(List.map2 value_of_string signature next_record))
       with End_of_file -> None)
	  | _ -> raise (StreamError i)


(* Given a source and FSM environment, construct a FSM which can be polled for values. *)
let compile stream_env fsm_env p =
	let rec compile_aux stream_env fsm_env ~on_success ~on_fail p =
	  let rcr = compile_aux stream_env fsm_env in
	  let rcr_list state rcr_f l = fst (List.fold_left (fun (state_acc, next) p ->
	      let states = rcr_f next p
	      in (states@state_acc), (fst (List.hd states))
	    ) ([], state) (List.rev l))
	  in
	  let replace_placeholder oid nid l =
	    let sub i = if i = oid then nid else i in
	    List.map (fun (i, (a,n,f)) -> (sub i, (a, sub n, sub f))) l 
	  in
	  let choice_states good bad l = 
	    rcr_list bad (fun next p -> rcr ~on_success:good ~on_fail:next p) l
	  in
	  let sequence_states good bad l =
	    rcr_list good (fun next p -> rcr ~on_success:next ~on_fail:bad p) l
	  in
	  let optional_state state = rcr ~on_success:state ~on_fail:state in
	  let repeat_state state p = 
	    let placeholder_state = gen_state_sym() in
	    let states = rcr ~on_success:placeholder_state ~on_fail:state p
	    in replace_placeholder placeholder_state (fst (List.hd states)) states
	  in
	  let new_state action = [gen_state_sym(), (action, on_success, on_fail)] in
	  match p with 
	  | Terminal id -> 
	    if List.mem_assoc id stream_env then
	      begin match List.assoc id stream_env with
	        | Source (id,t,s) -> new_state (VEndpoint(id, t, s, In(None)))
	        | Sink (id,t,s) -> new_state (VEndpoint(id, t, s, Out(None)))
	        | _ -> failwith "invalid stream type in stream environment"
	      end
	    
	    else if List.mem_assoc id fsm_env then new_state (VDerived id)
	
	    else failwith ("io pattern compile error: could not find channel "^id)
	
	  | Choice (l)   -> choice_states on_success on_fail l
	  | Sequence (l) -> sequence_states on_success on_fail l
	  | Optional (p) -> optional_state on_success p
	  | Repeat (p,_) -> repeat_state on_success p
  in
  let end_state = gen_state_sym() in
  let rfsm = compile_aux stream_env fsm_env end_state end_state p in 
  let eof_state = (end_state, (VEOF, end_state, end_state)) 
  in rfsm@[eof_state]


(* Go through all actions of an FSM, and initialize its channel implementations. *)
let initialize fsm =
  let init_file_impl filename i = match i with
    | In(None) -> In(Some(open_in filename))
    | Out(None) -> Out(Some(open_out filename))
    | _ -> i
  in
  let rec init_aux senv fsm = 
	  List.fold_left (fun (senv, nfsm) (i, (a,n,p)) ->
	    match a with
	      | VEndpoint(id, t, s, impl) ->
	        let nsenv, na = match s with
	          | File(_, filename) ->
	            (try senv, (List.assoc id senv)
	             with Not_found ->
	               let ne = VEndpoint(id, t, s, init_file_impl filename impl)
	               in ((id,ne)::senv), ne)
	
	          | _ -> raise (StreamError id) 
	        in nsenv, (nfsm@[i, (na,n,p)])

	      | _ -> senv, (nfsm@[i, (a,n,p)])
	    ) (senv, []) fsm
  in snd (init_aux [] fsm)


(* Run an FSM for a single event *)
let rec run fsm_env fsm state_opt =
  let run_endpoint id t s impl = match impl with
    | In(Some(c)) -> pull_source id t s c
    | _ -> raise (StreamError id)
  in
  let state = match state_opt with None -> (fst (List.hd fsm)) | Some (x) -> x in
  try
    let action, next, fail = List.assoc state fsm in
    match action with
      | VEndpoint (id, t, s, impl) ->
        let v = run_endpoint id t s impl in
        v, (if v = None then Some fail else Some next)

      | VDerived (id) -> 
        (try run fsm_env (List.assoc id fsm_env) None
         with Not_found -> failwith ("invalid derived state "^id)) 

      | VEOF -> None, None
  with Not_found -> failwith ("invalid state "^(string_of_int state))


(* Returns a stream identifier and either an individual stream or
 * a FSM for a stream pattern. For a source stream, we also compile
 * an FSM defined as repeated consumption of that stream to allow its
 * direct use in a consume instruction. *)
let prepare_stream stream_env fsm_env s = match s with
  | Source (id, t, sc) ->
    let self_pattern = Repeat(Terminal(id), UntilEOF) in
    id, Some(s), Some(compile (stream_env@[id, s]) fsm_env self_pattern)
  | Sink (id, t, sc) -> id, Some(s), None
  | Derived (id, p) -> id, None, Some(compile stream_env fsm_env p)


(* Returns a stream and FSM environment, as well as source bindings
 * from a K3 program *)
let event_loop_of_program k3_program =
  let env_of_declaration (stream_env, fsm_env, source_bindings) d
  = match d with
    | Stream s ->
      let id, ns, nfsm = prepare_stream stream_env fsm_env s in
      let nstream_env = match ns with None -> stream_env | Some(x) -> stream_env@[id,x] in
      let nfsm_env = match nfsm with None -> fsm_env | Some(x) -> fsm_env@[id,x]
      in (nstream_env, nfsm_env, source_bindings)

    | Bind (src_id, trig_id) ->
      (stream_env, fsm_env, (src_id, trig_id)::source_bindings)
      
    | _ -> stream_env, fsm_env, source_bindings 
  in
  let env_of_stmt env k3_stmt = match k3_stmt with
    | Declaration d -> env_of_declaration env d 
    | _ -> env
  in 
  let init_env = ([], [], []) in
  List.fold_left env_of_stmt init_env k3_program
