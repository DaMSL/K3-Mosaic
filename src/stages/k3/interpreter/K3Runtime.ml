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

type task_t =
    Background of (program_env_t -> unit)
  | NamedDispatch of id_t * value_t 
  | BlockDispatch of id_t * int

type scheduler_spec = {
    mutable events_to_process : int64;
    mutable interleave_period : int;
    mutable shuffle_tasks     : bool;
  }

let default_interleave_period = 10
let default_events_to_process = Int64.minus_one

let dummy = ref 0 
let default_gc_period = 1.
let previous_gc_time = ref (Unix.time())

let default_params = {
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


type bp_t = PostBreakPoint of breakpoint_t 
 | PreBreakPoint of breakpoint_t

type status_t = NormalExec | BreakPoint of breakpoint_t

(* Three modes of queueing supported:
 * i. Global queueing, where all nodes and triggers are managed on a single
 *    queue.
 * ii. Per-node queueing, where all trigger messages are managed on a single
 *    per-node queue. In multithreading evaluation, this would suffer from contention
 *    on the queue.
 * iii. Per-trigger queue, with trigger dispatch block tasks are placed
 *     on the global queue to indicate work should be done on a trigger-specific
 *     queue
 *)

type node_queue_t =
    (* node,  (node    queue,   (trigger, trig queue of args) *)
    (address, (task_t K3Queue.t * (id_t, value_t K3Queue.t) Hashtbl.t)) Hashtbl.t

(* one global queue for more deterministic execution *)
type global_queue_t = (address * task_t) K3Queue.t

type queue_t = Global of global_queue_t
             | PerNode of node_queue_t
             | PerTrigger of node_queue_t

type queue_type = GlobalQ | PerNodeQ | PerTriggerQ

let string_of_queue_t = function
  | GlobalQ     -> "global queue"
  | PerNodeQ    -> "pernode queue"
  | PerTriggerQ -> "pertrigger queue"

 (* for causing intentional reordering within the simulated network (while
  * still maintaining TCP-like ordering from a single sender *)
type shuffle_buffer_t = 
    (* node * sender,   (trigger * args)) *)
    (address, (address, (id_t * value_t) K3Queue.t) Hashtbl.t) Hashtbl.t

type scheduler_state = {
  params : scheduler_spec;
  mutable events_processed : Int64.t;
  queue : queue_t;
  shuffle_buffer : shuffle_buffer_t;
  mutable breakpoints : breakpoint_t list;
 }

let use_shuffle_tasks s  = s.params.shuffle_tasks 

let init_scheduler_state ?(shuffle_tasks=false) 
                         ?(breakpoints=[])
                         ?(run_length=default_events_to_process) 
                         ?(queue_type=GlobalQ)
                         () = 
  (*Printf.printf "Creating %s with shuffle %s\n" *)
    (*(string_of_queue_t queue_type) (sob shuffle_tasks);*)
  {
    params = { default_params with shuffle_tasks = shuffle_tasks; 
               events_to_process = run_length };
    events_processed = Int64.zero;
    queue = (match queue_type with
     | GlobalQ     -> Global(K3Queue.create ())
     | PerNodeQ    -> PerNode(Hashtbl.create 10)
     | PerTriggerQ -> PerTrigger(Hashtbl.create 10));
    shuffle_buffer = Hashtbl.create 10;
    breakpoints = breakpoints;
  }

let add_breakpoint s bp = 
  s.breakpoints <- bp :: s.breakpoints

let use_global_queueing s = match s.queue with Global _ -> true | _ -> false
let dispatch_block_size s = s.params.interleave_period  

(* Node helpers *)
let is_node s address = match s.queue with
  | PerNode q | PerTrigger q -> Hashtbl.mem q address
  | Global q                 -> failwith "no node info in global queue"

let get_node_queues s address = match s.queue with
  | PerNode q | PerTrigger q -> Hashtbl.find q address
  | Global q                 -> failwith "no node queue in global queue"

let register_node s address = 
  if not @: is_node s address then match s.queue with
  | PerNode q
  | PerTrigger q -> Hashtbl.add q address (K3Queue.create (), Hashtbl.create 10)
  | Global q     -> failwith "no node data in global queue"
  else invalid_arg @: "duplicate node at "^string_of_address address

let unregister_node s address = match s.queue with
  | PerNode q | PerTrigger q -> Hashtbl.remove q address
  | Global q                 -> failwith "no node data in global queue"

(* Global queueing helpers *)
let get_global_queue s address = match s.queue with
  | PerNode q | PerTrigger q -> 
      (try fst @: Hashtbl.find q address
      with Not_found -> failwith @: "unknown node "^string_of_address address)
  | Global q     -> failwith "global data is immediately accessible"

(* Per trigger queueing helpers *)
let get_trigger_queues s address = 
  try snd @: get_node_queues s address
  with Not_found -> failwith @: "unknown node "^string_of_address address

let get_trigger_input_queue s address trigger_id =
  Hashtbl.find (get_trigger_queues s address) trigger_id

let register_trigger s address trigger_id =
  let trigger_queues = get_trigger_queues s address in
  if not @: Hashtbl.mem trigger_queues trigger_id then
    Hashtbl.add trigger_queues trigger_id @: K3Queue.create ()
  else invalid_arg @: "duplicate trigger registration for "^trigger_id

let unregister_trigger s address trigger_id force =
  try let q = get_trigger_input_queue s address trigger_id in
      if (K3Queue.is_empty q) || force then
        let trigger_queues = get_trigger_queues s address in
        Hashtbl.remove trigger_queues trigger_id
  with Not_found -> () 

(* Scheduling methods *)
let schedule_task s address task = match s.queue with
  | Global q -> K3Queue.push (address, task) q
  | PerNode q | PerTrigger q -> K3Queue.push task @: get_global_queue s address

let schedule_trigger s v_target v_address args = 
  match v_target, v_address with
  | VTarget trigger_id, VAddress address ->
    begin match s.queue with
    | Global _ | PerNode _ -> 
        schedule_task s address @: NamedDispatch (trigger_id, args)
    | PerTrigger _ ->
      let q = get_trigger_input_queue s address trigger_id in
      K3Queue.push args q;
      schedule_task s address @: 
        BlockDispatch (trigger_id, dispatch_block_size s)
    end
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
                let q' = K3Queue.create () in
                Hashtbl.add inner_h sender_addr q';
                q'
              end
      in
      K3Queue.push (trigger_id, args) q
  | _ -> error INVALID_TRIGGER_TARGET

(* if shuffling is on, we move messages from the shuffle buffer to the queues *)
let schedule_trigger_random s receive_address = 
  if Hashtbl.length s.shuffle_buffer = 0 then () else
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
      if K3Queue.is_empty q then
        loop (list_remove addr set) (len - 1)
      else
        let trigger_id, args = K3Queue.pop q in
        schedule_trigger s (VTarget trigger_id) (VAddress receive_address)
          args;
        loop set len
  in loop senders_set (List.length senders_set);
  Hashtbl.reset per_node_buf

(* Scheduler execution *)

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
        if b'.counter <= 0 then BreakPoint b, acc
        else bp, b'::acc
      else bp, b::acc) 
    (NormalExec, []) 
    s.breakpoints
  in
  s.breakpoints <- bp';
  bp_type

let invoke_trigger s address (trigger_env, val_env) trigger_id arg =
  (* add a level to the global queue *)
  (match s.queue with
  | Global q -> K3Queue.increase_level q
  | PerNode _ | PerTrigger _ ->
    let q = get_global_queue s address in
    K3Queue.increase_level q);
  (* if the current time between last GC is bigger than given interval,
   * then start GC. 
   * TODO the trigger id "max_acked_vid_send" is hardcode for the moment 
   * 1 sec period is too short to test on simple query *)
  (*
  if (List.mem_assoc "max_acked_vid_send" trigger_env) && 
    (Unix.time() -. !previous_gc_time) > default_gc_period 
  then 
    begin 
     (IdMap.find "max_acked_vid_send" trigger_env) val_env (VInt 1);
     previous_gc_time := Unix.time();
     LOG "GC start %f: \n" (Unix.time() -. !previous_gc_time) 
        NAME "K3Runtime.TriggerSTate" LEVEL DEBUG;
    end;
    *)
  (* get the frozen function for the trigger and apply it to the env and args *)
  (IdMap.find trigger_id trigger_env) val_env arg;
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
    if K3Queue.is_empty q || num_left <= 0 then NormalExec
    else 
      let args = K3Queue.peek q in
      let m_bp = check_breakpoint s trigger_id args in
      match m_bp with
      | NormalExec -> 
          invoke_trigger s address env trigger_id @: K3Queue.pop q;
          loop (num_left - 1)
      | BreakPoint bp when bp.post_trigger -> 
          invoke_trigger s address env trigger_id @: K3Queue.pop q;
          m_bp
      | BreakPoint bp -> m_bp
  in
  let res = loop max_to_process in

  (* the block we were assigned wasn't enough for this trigger *)
  if not @: K3Queue.is_empty q then 
    schedule_task s address @: 
      BlockDispatch (trigger_id, dispatch_block_size s);
  res

(* obtain the address of the next node on the global queue *)
let next_global_address s = match s.queue with
  | Global q -> 
      let addr, _ = K3Queue.peek q in
      addr
  | PerNode _ | PerTrigger _ -> failwith "non-global queue"

let process_task s address prog_env =
  let module K = K3Queue in
  try
    begin match s.queue with
    | Global q ->
        (* global mode ignores the address given *)
        begin match K.peek q with
        | _, Background fn -> 
            ignore @: K.pop q; fn prog_env; NormalExec
        | address', NamedDispatch (id, arg) ->
            let m_bp = check_breakpoint s id arg in
            begin match m_bp with
            | NormalExec ->
                ignore @: K.pop q;
                invoke_trigger s address' prog_env id arg; m_bp
            | BreakPoint bp when bp.post_trigger -> 
                ignore @: K.pop q;
                invoke_trigger s address' prog_env id arg; m_bp
            | BreakPoint bp -> m_bp
            end
        | _, BlockDispatch _ -> failwith "global queue doesn't handle block_dispatch"
        end

    | PerNode _ | PerTrigger _ -> 
      let q = get_global_queue s address in
        begin match s.queue, K.peek q with
        | _, Background fn -> 
            ignore @: K.pop q; fn prog_env; NormalExec
        | PerNode _, NamedDispatch (id, arg) ->
            let m_bp = check_breakpoint s id arg in
            begin match m_bp with
            | NormalExec ->
                ignore @: K.pop q;
                invoke_trigger s address prog_env id arg;
                m_bp
            | BreakPoint bp when bp.post_trigger -> 
                ignore @: K.pop q;
                invoke_trigger s address prog_env id arg;
                m_bp
            | BreakPoint bp -> m_bp
            end
        | PerTrigger _, NamedDispatch _ -> NormalExec
        | PerTrigger _, BlockDispatch (id, max_to_process) ->
          ignore @: K.pop q;
          process_trigger_queue s address prog_env id max_to_process
        | _, _ -> failwith "incorrect message in queue"
        end
      end
  with K3Queue.Empty -> error INVALID_GLOBAL_QUEUE

(* Scheduler toplevel methods *)

(* register the node and its triggers *)
let initialize_scheduler s address (trig_env,_) = match s.queue with
  | PerTrigger _ | PerNode _ -> 
    if not @: is_node s address then register_node s address; 
    IdMap.iter (fun id _ -> register_trigger s address id) trig_env
  | Global _ -> ()

let node_has_work s address =
  if not @: is_node s address then false else 
  let node_queues = get_node_queues s address in
  let empty_global_q = K3Queue.is_empty @: fst node_queues in
  let empty_shuffle_buffer = 
    if use_shuffle_tasks s then 
      try Hashtbl.length @: Hashtbl.find s.shuffle_buffer address = 0
      with Not_found -> false
    else true
  in
  not @: empty_global_q && empty_shuffle_buffer

let network_has_work s = match s.queue with
  | PerNode q | PerTrigger q -> Hashtbl.fold (fun addr _ acc ->
      acc || node_has_work s addr) q false
  | Global q -> not @: K3Queue.is_empty q

let continue_processing s address =
  let has_messages = match s.queue with
    | Global q -> network_has_work s
    | PerNode _ | PerTrigger _ -> node_has_work s address
  in
  let events_remain = 
    s.params.events_to_process < Int64.zero ||
    s.events_processed < s.params.events_to_process
  in
  has_messages && events_remain


(* address is ignored for global queue *)
let run_scheduler ?(slice = max_int) s address env =
  let rec loop i = 
    if i <= 0 || not @: continue_processing s address then NormalExec
    else 
      (* schedule shuffle trigger if the shuffle flag is on *)
      (if use_shuffle_tasks s then schedule_trigger_random s address;
      match process_task s address env with
      | NormalExec    -> loop (i-1)
      | BreakPoint bp -> BreakPoint bp)
  in loop slice

