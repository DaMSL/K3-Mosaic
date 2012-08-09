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
  | AEndpoint   of id_t * type_t * stream_channel_t * channel_impl_t
  | ADerived    of id_t
  | AEOF

and stream_fsm_t = (state_id * (action_t * state_id * state_id)) list

type stream_env_t = (id_t * stream_t) list
type fsm_env_t = (id_t * stream_fsm_t) list

type event_loop_t =
  stream_env_t * fsm_env_t * source_bindings_t * (instruction_t list)

(* State identifier generation *)
let state_sym_class = "FSM"
let gen_state_sym() = gen_int_sym state_sym_class
let _ = register_symbol state_sym_class "__";;

(* Stringification *)
let string_of_action a = match a with 
  | AEndpoint (id, t, s, impl) -> "Endpoint("^id^")" 
  | ADerived id -> "Derived("^id^")"
  | AEOF -> "EOF"

let print_transition (i, (a,n,f)) =
  ps (string_of_int i); ps " : ";
  ps (String.concat ", " [string_of_action a; string_of_int n; string_of_int f])

let print_fsm fsm =
  ps "[ ";
  List.iter (fun t -> ob(); print_transition t; ps "; "; cb()) fsm;
  ps " ]"

let string_of_fsm fsm = wrap_formatter (fun () -> print_fsm fsm)

let print_stream_env stream_env =
  List.iter (fun (id,s) -> print_stream s; fnl()) stream_env

let string_of_stream_env stream_env =
  wrap_formatter (fun () -> print_stream_env stream_env)

let print_fsm_env fsm_env =
  List.iter (fun (id, fsm) -> ps id; ps " = "; pc(); print_fsm fsm; fnl()) fsm_env

let string_of_fsm_env fsm_env = wrap_formatter (fun () -> print_fsm_env fsm_env)

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
            | Source (id,t,s) -> new_state (AEndpoint(id, t, s, In(None)))
            | Sink (id,t,s) -> new_state (AEndpoint(id, t, s, Out(None)))
            | _ -> failwith "invalid stream type in stream environment"
          end
        
        else if List.mem_assoc id fsm_env then new_state (ADerived id)
    
        else failwith ("io pattern compile error: could not find channel "^id)
    
      | Choice (l)   -> choice_states on_success on_fail l
      | Sequence (l) -> sequence_states on_success on_fail l
      | Optional (p) -> optional_state on_success p
      | Repeat (p,_) -> repeat_state on_success p
  in
  let end_state = gen_state_sym() in
  let rfsm = compile_aux stream_env fsm_env end_state end_state p in 
  let eof_state = (end_state, (AEOF, end_state, end_state)) 
  in rfsm@[eof_state]


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


(* For each role in a K3 program, returns a stream and FSM environment,
 * and source bindings *)
let roles_of_program k3_program =
  let event_loop_of_stream_stmt (stream_env, fsm_env, source_bindings, instructions) ss
  = match ss with
    | Stream s ->
      let id, ns, nfsm = prepare_stream stream_env fsm_env s in
      let nstream_env = match ns with None -> stream_env | Some(x) -> stream_env@[id,x] in
      let nfsm_env = match nfsm with None -> fsm_env | Some(x) -> fsm_env@[id,x]
      in (nstream_env, nfsm_env, source_bindings, instructions)

    | Bind (src_id, trig_id) ->
      (stream_env, fsm_env, (src_id, trig_id)::source_bindings, instructions)
      
    | Instruction i -> stream_env, fsm_env, source_bindings, (instructions@[i]) 
  in
  let event_loop_of_role (env, default) (d,_) = match d with
    | Role(id, sp) -> (env@[id, List.fold_left event_loop_of_stream_stmt ([],[],[],[]) sp], default)
    | DefaultRole(id) -> 
      (try env, Some(List.assoc id env) with Not_found -> (env, default))  
    | _ -> env, default
  in 
  List.fold_left event_loop_of_role ([], None) k3_program
