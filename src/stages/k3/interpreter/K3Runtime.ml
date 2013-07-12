(* A single-threaded scheduler and single-site peer simulator *)

open Util
open K3.AST
open K3Streams
open K3Values
open K3Util
open K3Printing

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

(* Error handling *)
type runtime_error_type =
    INVALID_GLOBAL_QUEUE
  | INVALID_TRIGGER_QUEUE
  | INVALID_TRIGGER_TARGET
  | INVALID_SHUFFLE_BUFFER
  | INVALID_BREAKPOINT

let runtime_errors = [
    INVALID_GLOBAL_QUEUE, (-1000, "invalid empty task queue");
    INVALID_TRIGGER_QUEUE, (-1001, "invalid dequeue on trigger");
    INVALID_TRIGGER_TARGET, (-1002, "invalid target for message send");
    INVALID_SHUFFLE_BUFFER, (-1003, "error in shuffle buffer");
    INVALID_BREAKPOINT, (-1004, "invalid breakpoint");
  ]
  
let error t =
  let i, s = try List.assoc t runtime_errors
             with Not_found -> -1, "K3Runtime: unknown error code"
  in raise @: RuntimeError(i, s)

(* Event queues and scheduling *)

(* Two modes of queueing supported:
 * i. global queueing, where all trigger messages are managed on a single
 *    queue. In multithreading evaluation, this would suffer from contention
 *    on the queue.
 * ii. per trigger queue, with trigger dispatch block tasks are placed
 *     on the global queue to indicate work should be done on a trigger-specific
 *     queue
 *)
type queue_granularity_t = Global | PerTrigger

type task_t =
    Background of (program_env_t -> unit)
  | NamedDispatch of id_t * value_t 
  | BlockDispatch of id_t * int

type status_t = NormalExec | BreakPoint
type bp_t = PostBreakPoint | PreBreakPoint

type scheduler_spec = {
    mutable mode : queue_granularity_t;
    mutable events_to_process : int64;
    mutable interleave_period : int;
    mutable shuffle_tasks     : bool;
  }

let default_interleave_period = 10
let default_events_to_process = Int64.minus_one

let default_params = {
    mode = PerTrigger;
    events_to_process = default_events_to_process;
    interleave_period = default_interleave_period;
    shuffle_tasks = false;
  }

(* Breakpoint types *)
type breakpoint_t = {
  trigger : id_t;
  (* specify values for some args, others can be left out *)
  args : value_t;
  (* when the counter reaches 0, the breakpoint is activated *)
  counter : int;
  (* stop pre or post trigger *)
  post_trigger: bool;
}

type node_queue_t = 
    (* node,  (general queue,   (trigger, trig queue of args) *)
    (address, (task_t Queue.t * (id_t, value_t Queue.t) Hashtbl.t)) Hashtbl.t


(* for causing intentional reordering within the simulated network (while
  * still maintaining TCP-like ordering from a single sender *)
type shuffle_buffer_t = 
    (* node * sender,   (trigger * args)) *)
    (address, (address, (id_t * value_t) Queue.t) Hashtbl.t) Hashtbl.t

type scheduler_state = {
  params : scheduler_spec;
  mutable events_processed : Int64.t;
  node_queues : node_queue_t;
  shuffle_buffer : shuffle_buffer_t;
  mutable breakpoints : breakpoint_t list;
 }

let init_scheduler_state ?(shuffle_tasks=false) ?(breakpoints=[])
    ?(run_length=default_events_to_process) () = 
  {
    params = { default_params with shuffle_tasks = shuffle_tasks; 
               events_to_process = run_length };
    events_processed = Int64.zero;
    node_queues = Hashtbl.create 10;
    shuffle_buffer = Hashtbl.create 10;
    breakpoints = breakpoints;
  }

let use_global_queueing s = s.params.mode = Global
let dispatch_block_size s = s.params.interleave_period  

(* Node helpers *)
let is_node s address = Hashtbl.mem s.node_queues address

let get_node_queues s address = Hashtbl.find s.node_queues address

let register_node s address = 
  if not @: is_node s address then
    Hashtbl.add s.node_queues address (Queue.create (), Hashtbl.create 10)
  else invalid_arg @: "duplicate node at "^string_of_address address

let unregister_node s address = Hashtbl.remove s.node_queues address

(* Global queueing helpers *)
let get_global_queue s address = 
  try fst @: get_node_queues s address
  with Not_found -> failwith @: "unknown node "^string_of_address address

(* Per trigger queueing helpers *)
let get_trigger_queues s address = 
  try snd @: get_node_queues s address
  with Not_found -> failwith @: "unknown node "^string_of_address address

let get_trigger_input_queue s address trigger_id =
  Hashtbl.find (get_trigger_queues s address) trigger_id

let register_trigger s address trigger_id =
  let trigger_queues = get_trigger_queues s address in
  if not @: Hashtbl.mem trigger_queues trigger_id then
    Hashtbl.add trigger_queues trigger_id @: Queue.create ()
  else invalid_arg @: "duplicate trigger registration for "^trigger_id

let unregister_trigger s address trigger_id force =
  try let q = get_trigger_input_queue s address trigger_id in
      if (Queue.is_empty q) || force then
        let trigger_queues = get_trigger_queues s address in
        Hashtbl.remove trigger_queues trigger_id
  with Not_found -> () 

(* Scheduling methods *)
let schedule_task s address task = Queue.push task @: get_global_queue s address

let schedule_trigger s v_target v_address args = match v_target, v_address with
  | VTarget trigger_id, VAddress address ->
    if use_global_queueing s then 
      schedule_task s address @: NamedDispatch (trigger_id, args)
    else
      let q = get_trigger_input_queue s address trigger_id in
      Queue.push args q;
      schedule_task s address @: 
        BlockDispatch (trigger_id, dispatch_block_size s)

  | _, _ -> error INVALID_TRIGGER_TARGET

let schedule_event s source_bindings source_id source_address events =
  let schedule_fn trig_id =
    schedule_trigger s (VTarget trig_id) (VAddress source_address) in 
  try
    let trigger_ids = snd_many @: 
      List.filter (fun (x,_) -> x = source_id) source_bindings in 
    List.iter 
      (fun trigger_id -> List.iter (schedule_fn trigger_id) events) 
      trigger_ids
  with Not_found -> () 

(* 
 * Shuffle task from different nodes randomly to simulate network delay.
 *
 * It first buffer all triggers into a Hashtbl (sender_addr -> trigger queue),
 * then randmoly pop a trigger buffer queue and schedule it by calling
 * schedule_trigger. The order of trigger from the same sender address is
 * preserved. 
 * *)

let buffer_trigger s target address args sender_addr = 
  match address, target with 
  | VAddress address, VTarget trigger_id ->
      let inner_h = begin try Hashtbl.find s.shuffle_buffer address
                    with Not_found -> 
                      let h = Hashtbl.create 10 in
                      Hashtbl.add s.shuffle_buffer address h;
                      h
                    end
      in
      let q = begin try Hashtbl.find inner_h sender_addr
              with Not_found -> 
                let q' = Queue.create () in
                Hashtbl.add inner_h sender_addr q';
                q'
              end
      in
      Queue.push (trigger_id, args) q
  | _ -> error INVALID_TRIGGER_TARGET

(* if shuffling is on, we move messages from the shuffle buffer to the queues *)
let schedule_trigger_random s receive_address = 
  let per_node_buf = try Hashtbl.find s.shuffle_buffer receive_address
                     with Not_found -> error INVALID_SHUFFLE_BUFFER
  in
  (* set of available senders *)
  let senders_set = 
    Hashtbl.fold (fun sender_add _ acc -> sender_add::acc) per_node_buf []
  in
  let rec loop set len =
    if len <= 0 then ()
    else
      let i = Random.int len in
      let addr = at set i in
      let q = Hashtbl.find per_node_buf addr in
      if Queue.is_empty q then
        loop (list_remove addr set) (len - 1)
      else
        let trigger_id, args = Queue.take q in
        schedule_trigger s (VTarget trigger_id) (VAddress receive_address)
          args;
        loop set len
  in loop senders_set (List.length senders_set);
  Hashtbl.reset per_node_buf

(* Scheduler execution *)

let continue_processing s address =
  let has_messages = not @: Queue.is_empty @: get_global_queue s address in 
  let events_remain = 
    s.params.events_to_process < Int64.zero ||
    s.events_processed < s.params.events_to_process
  in
  has_messages && events_remain

(* compare the breakpoint filter to the actual arguments *)
let rec breakpoint_arg_test test_arg arg =
  let all l1 l2 = 
    List.for_all2 breakpoint_arg_test l1 l2 
  in
  match test_arg, arg with
  | VUnknown, _            -> true
  | VTuple l1, VTuple l2   -> all l1 l2
  | VOption o1, VOption o2 -> 
      begin match o1, o2 with
      | None, None         -> true
      | Some o1', Some o2' -> breakpoint_arg_test o1' o2'
      | _, _               -> false
      end
  | VSet l1, VSet l2       -> all l1 l2
  | VBag l1, VBag l2       -> all l1 l2
  | VList l1, VList l2     -> all l1 l2
  | VFunction _, _
  | _, VFunction _         -> error INVALID_BREAKPOINT
  | a, b when a = b        -> true
  | _                      -> false

let check_breakpoint s target_trig arg =
  let bp_type, bp' = 
    List.fold_left (fun (bp, acc) b -> 
      if b.trigger = target_trig && breakpoint_arg_test b.args arg then
        let b' = {b with counter = b.counter - 1} in
        if b'.counter <= 0 then 
          let t = match b.post_trigger with
            | true  -> Some PostBreakPoint 
            | false -> Some PreBreakPoint in
          t, acc
        else bp, b'::acc
      else bp, b::acc) 
    (None, []) 
    s.breakpoints
  in
  s.breakpoints <- bp';
  bp_type

let invoke_trigger s address (trigger_env, val_env) trigger_id arg =
  (* get the frozen function for the trigger and apply it to the env and args *)
  (List.assoc trigger_id trigger_env) val_env arg;
  (* log the state for this trigger *)
  LOG "Trigger %s@%s:\n%s" trigger_id (string_of_address address) (string_of_env val_env) 
    NAME "K3Runtime.TriggerState" LEVEL DEBUG;
  s.events_processed <- Int64.succ s.events_processed

(* process the events for a particular trigger queue *)
let process_trigger_queue s address env trigger_id max_to_process =
  let q = try get_trigger_input_queue s address trigger_id
          with Not_found -> error INVALID_TRIGGER_QUEUE
  in
  let rec loop num_left =
    if Queue.is_empty q || num_left <= 0 then NormalExec
    else 
      let args = Queue.peek q in
      match check_breakpoint s trigger_id args with
      | None     -> 
          invoke_trigger s address env trigger_id @: Queue.take q;
          loop (num_left - 1)
      | Some PreBreakPoint  -> BreakPoint
      | Some PostBreakPoint ->
          invoke_trigger s address env trigger_id @: Queue.take q;
          BreakPoint
  in
  let res = loop max_to_process in

  (* the block we were assigned wasn't enough for this trigger *)
  if not @: Queue.is_empty q then 
    schedule_task s address @: 
      BlockDispatch (trigger_id, dispatch_block_size s);
  res

let process_task s address prog_env =
  let q = get_global_queue s address in
  let pop_q () = ignore @: Queue.take q in
  try
    match Queue.peek q with
    | Background fn -> 
        fn prog_env;
        pop_q ();
        NormalExec

    | NamedDispatch (id, arg) ->
      if use_global_queueing s then 
        begin match check_breakpoint s id arg with
        | None     ->
            invoke_trigger s address prog_env id arg;
            pop_q ();
            NormalExec
        | Some PreBreakPoint  -> BreakPoint
        | Some PostBreakPoint -> 
            invoke_trigger s address prog_env id arg;
            pop_q ();
            BreakPoint
        end
      else NormalExec

    | BlockDispatch (id, max_to_process) ->
      pop_q ();
      if use_global_queueing s then NormalExec
      else process_trigger_queue s address prog_env id max_to_process

  with Queue.Empty -> error INVALID_GLOBAL_QUEUE

(* Scheduler toplevel methods *)

(* register the node and its triggers *)
let initialize_scheduler s address (trig_env,_) =
  if not @: is_node s address then register_node s address; 
  List.iter (fun (id, _) -> register_trigger s address id) trig_env

let node_has_work s address =
  if not @: is_node s address then false else 
  let node_queues = get_node_queues s address in
  let empty_global_q = Queue.is_empty @: fst node_queues in
  let empty_trigger_q = 
    Hashtbl.fold (fun _ q acc -> acc && Queue.is_empty q) (snd node_queues) true in
  let empty_shuffle_buffer = 
    if s.params.shuffle_tasks then 
      (Hashtbl.length @: Hashtbl.find s.shuffle_buffer address) = 0
    else true
  in
  not (empty_global_q && empty_trigger_q && empty_shuffle_buffer)

let network_has_work s = 
  Hashtbl.fold (fun addr _ acc -> acc || node_has_work s addr) s.node_queues false

let run_scheduler ?(slice = max_int) s address env =
  let rec loop i = 
    if i <= 0 || not @: continue_processing s address then NormalExec
    else 
      (* schedule shuffle trigger if the shuffle flag is on *)
      (if s.params.shuffle_tasks then schedule_trigger_random s address;
      match process_task s address env with
      | NormalExec -> loop (i-1)
      | BreakPoint -> BreakPoint)
  in loop slice

let use_shuffle_tasks s  = s.params.shuffle_tasks 
