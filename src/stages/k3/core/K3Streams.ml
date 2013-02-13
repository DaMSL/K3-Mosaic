open Symbols
open Printing
open K3.AST
open K3Util
open K3Printing

exception StreamError of id_t

type resource_bindings_t = (id_t * id_t) list
type resource_env_t = (id_t * (bool * flow_resource_t)) list

module type FSMLabels = sig
  type input
  type output
  val string_of_input : input -> string
  val string_of_output : output -> string
end

module type FSMType = sig
  type input
  type output
  type state_id = int
  type action_t = Output of output | Terminate
  type transition_t = action_t * state_id 

    (* Matched transitions and actions, non-match transition and action *)
    type fsm_state = (input * transition_t) * transition_t  
    
    (* state id -> state metadata *)
    type fsm_t = (state_id * fsm_state) list
    
    (* fsm id -> fsm *)
    type fsm_env_t = (id_t * fsm_t) list

  val string_of_input : input -> string
  val string_of_output : output -> string

  val string_of_fsm     : fsm_t -> string
  val string_of_fsm_env : fsm_env_t -> string
end

module FSM = functor(Labels : FSMLabels) ->
struct
  type input = Labels.input
  type output = Labels.output
  
  type state_id = int
  type action_t = Output of output | Terminate
  type transition_t = action_t * state_id 

	(* Matched transitions and actions, non-match transition and action *)
	type fsm_state = (input * transition_t) * transition_t  
	
	(* state id -> state metadata *)
	type fsm_t = (state_id * fsm_state) list
	
	(* fsm id -> fsm *)
	type fsm_env_t = (id_t * fsm_t) list

  let string_of_input  = Labels.string_of_input
  let string_of_output = Labels.string_of_output

	let string_of_action prefix a =
    let my_tag t l = prefix^":"^t^"("^(String.concat "," l)^")" in
    match a with
    | Output o -> my_tag "Output" [string_of_output o]
    | Terminate -> "Terminate"

	let print_transition (i, ((input, (ma,n)), (fa,f))) =
	  ps (string_of_int i); ps " : ";
    ps ("<"^(string_of_input input)^">");
	  ps (String.concat ", " [string_of_action "Match" ma; string_of_int n; 
                            string_of_action "Mismatch" fa; string_of_int f])
	
	let print_fsm fsm =
	  ps "[ ";
	  List.iter (fun t -> ob(); print_transition t; ps "; "; cb()) fsm;
	  ps " ]"
	
  let string_of_fsm fsm = wrap_formatter (fun () -> print_fsm fsm)
  
	let print_fsm_env fsm_env =
	  List.iter (fun (id, fsm) -> ps id; ps " = "; pc(); print_fsm fsm; fnl()) fsm_env
	
	let string_of_fsm_env fsm_env = wrap_formatter (fun () -> print_fsm_env fsm_env)

end

(* Resource pattern FSMs *)
module ResourceActions = struct
  type input = id_t

  type resource_output =
      (* sink dispatch list *) 
    | Dispatch of id_t list
    | Fail

  type output =
      (* Sources have pre-entry metadata as well a resource output *)
    | Source of resource_output * id_t list
    | Sink of resource_output 

  let string_of_input i = i

  let string_of_resource_output r = match r with 
    | Dispatch dispatch -> "Dispatch["^(String.concat "," dispatch)^"]"
    | Fail -> "Fail"

  let string_of_output o = match o with
    | Source (r, p) -> "Source("^(string_of_resource_output r)^", ["^(String.concat "," p)^"])"
    | Sink r -> "Sink("^(string_of_resource_output r)^")"

end

module ResourceFSM : FSMType
      with type input = ResourceActions.input
      and  type output = ResourceActions.output
  = FSM(ResourceActions)

type dispatcher_t = ResourceFSM.fsm_t
type dispatcher_env_t = ResourceFSM.fsm_env_t 

type event_loop_t =
  resource_env_t * dispatcher_env_t * (instruction_t list)

(* State identifier generation *)
let state_sym_class = "FSM"
let gen_state_sym() = gen_int_sym state_sym_class
let _ = register_symbol state_sym_class "__";;

(* Stringification *)
let print_resource_env resource_env =
  let my_tag = pretty_tag_str CutHint "" in
  List.iter (fun (id, (source,r)) -> 
      my_tag (if source then "Source" else "Sink") 
        [lazy (print_flow_resource def_c r)];
      fnl()
    ) resource_env

let string_of_resource_env resource_env =
  wrap_formatter (fun () -> print_resource_env resource_env)

let string_of_resource_bindings bindings = wrap_formatter (fun () ->
  List.iter (fun (src,trig) -> ps (src^" -> "^trig); fnl()) bindings)

let string_of_dispatcher d = ResourceFSM.string_of_fsm d
let string_of_dispatcher_env denv = ResourceFSM.string_of_fsm_env denv

(* FSM accessors *)
let id_of_state s = fst s
let match_action_of_state s = fst (snd (fst (snd s)))
let fail_action_of_state s = fst (snd (snd s))

let pre_entry_of_state state =
  let module A = ResourceActions in
  let module F = ResourceFSM in 
  match match_action_of_state state with
  | F.Output (A.Source (_,p)) -> p
  | _ -> []

(* Accessors *)
let handle_of_resource resource_env id =
    try 
      let resource = List.assoc id resource_env in Some resource
    with Not_found -> None 

let is_net_handle resource_env id = 
  match handle_of_resource resource_env id with
    | Some(_, Handle(_, Network _, _)) -> true
    | _ -> false

let is_file_handle resource_env id = 
  match handle_of_resource resource_env id with
    | Some(_, Handle(_, File _, _)) -> true
    | _ -> false

(* Given a source and FSM environment, construct an FSM continuation. *)
let compile_pattern resource_env resource_bindings fsm_env 
                    post_rcr_f choice_f sequence_f terminal_f p =
  let rec compile_aux resource_env fsm_env ~on_success ~on_fail p =
	  let rcr = compile_aux resource_env fsm_env in
	  let rcr_list state l rcr_f pre_entry_f =
      let rstates, pre_entries, _ =
	      List.fold_left (fun (state_acc, pre_entry_acc, next) p ->
	        let states, pre_entry = pre_entry_f pre_entry_acc (rcr_f next p)
		      in (states@state_acc), pre_entry, (fst (List.hd states))
		    ) ([], [], state) (List.rev l)
      in post_rcr_f pre_entries rstates
	  in
	  let replace_placeholder oid nid l =
	    let sub i = if i = oid then nid else i in
	    List.map (fun (i, ((inp, (ma,n)), (fa,f))) -> (sub i, ((inp, (ma, sub n)), (fa, sub f)))) l 
	  in
	  let choice_states good bad l = 
	    rcr_list bad l
        (fun next p -> rcr ~on_success:good ~on_fail:next p) choice_f
	  in
	  let sequence_states good bad l =
	    rcr_list good l
        (fun next p -> rcr ~on_success:next ~on_fail:bad p) sequence_f
	  in
	  let optional_state state = rcr ~on_success:state ~on_fail:state in
	  let repeat_state state p = 
	    let placeholder_state = gen_state_sym() in
	    let states = rcr ~on_success:placeholder_state ~on_fail:state p
	    in replace_placeholder placeholder_state (fst (List.hd states)) states
	  in
    match p with 
	  | Terminal id -> 
	    if List.mem_assoc id resource_env then
        let bindings = List.map snd (List.filter (fun (x,y) -> x=id) resource_bindings) in
        let resource = List.assoc id resource_env
        in terminal_f id on_success on_fail bindings resource  
	    
	    else if List.mem_assoc id fsm_env then
        (* Inline derived FSMs *)
        List.assoc id fsm_env 
	
	    else failwith ("io pattern compile error: could not find channel "^id)
	
	  | Choice (l)   -> choice_states on_success on_fail l
	  | Sequence (l) -> sequence_states on_success on_fail l
	  | Optional (p) -> optional_state on_success p
	  | Repeat (p,_) -> repeat_state on_success p
  in
  let end_state = gen_state_sym() in
  end_state, compile_aux resource_env fsm_env end_state end_state p 

(* Dispatcher FSM compilation *)
let compile_dispatcher resource_env resource_bindings fsm_env p =
  let module A = ResourceActions in
  let module F = ResourceFSM in
  let replace_entry_ids new_ids fsm = 
    let new_hd = match List.hd fsm with
    | (a, ((b, (F.Output (A.Source(c, _)), d)), e)) ->
      (a, ((b, (F.Output (A.Source(c, new_ids)), d)), e)) 
    | x -> x 
    in new_hd::(List.tl fsm)
  in
  let choice_f pre_entry_acc states =
	  (* Lift up and replace the pre-entry ids at the child *) 
	  let new_entry_acc = (pre_entry_of_state (List.hd states))@pre_entry_acc in
	  let new_states = replace_entry_ids [] states in
	  new_states, new_entry_acc
  in
  let sequence_f pre_entry_acc states = 
	  (* Pass-through the pre-entry ids at the child *)
	  states, pre_entry_of_state (List.hd states)
  in
  let terminal_f id on_success on_fail bindings r = match r with
    | (true, Handle _) | (true, Stream _) ->
      let match_action = F.Output (A.Source(A.Dispatch(bindings), [id])) in
      let fail_action = F.Output (A.Source(A.Fail,[id])) in 
      [gen_state_sym(),
        ((id, (match_action, on_success)), (fail_action, on_fail))]

    | (false, Handle _) ->
      let match_action = F.Output (A.Sink(A.Dispatch(bindings))) in
      let fail_action = F.Output (A.Sink(A.Fail)) in 
      [gen_state_sym(),
        ((id, (match_action, on_success)), (fail_action, on_fail))]

    | _ -> failwith "invalid resource type in multiplexer pattern"
  in
  let end_state, rfsm = compile_pattern resource_env resource_bindings fsm_env
                          replace_entry_ids choice_f sequence_f terminal_f p
  in 
  let end_transition = F.Terminate, end_state in
  let eof_state = (end_state, (("", end_transition), end_transition)) 
  in rfsm@[eof_state]

(* Returns all resources (input and output) associated with this dispatcher *)
let resources_of_dispatcher d =
  List.filter ((<>) "")
    (ListAsSet.no_duplicates
      (List.fold_left (fun racc (_, ((rid, _), _)) -> rid::racc) [] d))

(* Returns the initial resources (i.e. those accessible from the initial state)
 * of a dispatcher *)
let initial_resources_of_dispatcher d =
  let init_state = List.hd d in
  id_of_state init_state, pre_entry_of_state init_state

(* Constructs an event loop for a flow program *)
let event_loop_of_flow fp : event_loop_t =
  let event_loop_of_flow_stmt ((pat_acc, bind_acc), (res_env, d_env, instrs)) (fs,_) =
    (* Binds a flow resource in the given resource and FSM environment.
     * For sources, we also include a self-repetition pattern in the FSM environment. *)
    match fs with
    | Source (Resource (id,r)) ->
      let npats, nrenv, nd_env = match r with 
	      | Handle _ | Stream _ -> 
            [id, Repeat(Terminal(id), UntilEOF)], res_env@[id, (true,r)], d_env
	      | Pattern p -> [id, p], res_env, d_env
      in (pat_acc@npats, bind_acc), (nrenv, nd_env, instrs)

    | Sink (Resource (id,r)) ->
      let npats, nrenv, nd_env = match r with 
	      | Handle (t,ct,cf) -> [], res_env@[id, (false,r)], d_env
	      | Pattern p -> [id, p], res_env, d_env
        | Stream _ -> failwith "Streams not supported as sinks"
      in (pat_acc@npats, bind_acc), (nrenv, nd_env, instrs)

    | Bind (src_id, trig_id) ->
      (pat_acc, (src_id, trig_id)::bind_acc), (res_env, d_env, instrs)
    
    | Instruction i ->
      let compile_fsm (id,p) = id, compile_dispatcher res_env bind_acc d_env p in
      let nd = List.map compile_fsm pat_acc
      in ([], []), (res_env, d_env@nd, (instrs@[i]))

    | _ ->  (pat_acc, bind_acc), (res_env, d_env, instrs)
  in
  snd (List.fold_left event_loop_of_flow_stmt (([],[]), ([],[],[])) fp)


(* Role environment constructor *)
let event_loop_of_role (pr,pde,pi) (role_env, default) (d,_) = match d with
	| Role(id, fp) -> 
	  let r,de,i = event_loop_of_flow fp in
	  (role_env@[id, (pr@r,pde@de,pi@i)], default)
	
	| DefaultRole(id) -> 
	  (try role_env, Some(id, List.assoc id role_env)
     with Not_found -> (role_env, default))  
	
	| _ -> role_env, default

(* Accumulates event loops based on all flow programs in a K3 program *)
let event_loop_of_program (racc,dacc,iacc) (d,_) = match d with
  | Flow fp -> 
  let r,dp,i = event_loop_of_flow fp in racc@r, dacc@dp, iacc@i 
  | _ -> racc,dacc,iacc

(* For each role in a K3 program, returns a stream and FSM environment,
 * and source bindings. *)
let roles_of_program k3_program =
  List.fold_left (event_loop_of_role ([],[],[])) ([], None) k3_program

(* Same as roles_of_program, except with each role prepended with top-level
 * flows defined in the K3 program, making these act as global, common roles. *) 
let extended_roles_of_program k3_program =
  let init = List.fold_left event_loop_of_program ([],[],[]) k3_program
  in List.fold_left (event_loop_of_role init) ([], None) k3_program
