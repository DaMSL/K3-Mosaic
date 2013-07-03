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

let runtime_errors = [
    INVALID_GLOBAL_QUEUE, (-1000, "invalid empty task queue");
    INVALID_TRIGGER_QUEUE, (-1001, "invalid dequeue on trigger");
    INVALID_TRIGGER_TARGET, (-1002, "invalid target for message send");
  ]
  
let error t =
  let (i, s) = try (List.assoc t runtime_errors)
               with Not_found -> (-1, "K3Runtime: unknown error code")
  in raise (RuntimeError(i, s))

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

type scheduler_spec = {
    mutable mode : queue_granularity_t;
    mutable events_to_process : int64;
    mutable interleave_period : int;
    mutable shuffle_tasks     : bool;
  }

let scheduler_params = {
    mode = PerTrigger;
    events_to_process = Int64.minus_one;
    interleave_period = 10;
    shuffle_tasks = false;
  }
    
let use_global_queueing () = scheduler_params.mode = Global
let dispatch_block_size () = scheduler_params.interleave_period  
let events_processed = ref Int64.zero

(* node address -> task queue * per trigger input queues *)
let node_queues = Hashtbl.create 10

(* 
 * receive_address -> (Hashtbl: (sender_address -> trigger_queue))
 * For test purpose, temp buffer to store messages from different
 * nodes, and then shuffer them to simulate network delay and messages
 * from different nodes are out of order
 * *)
let trigger_buffer = Hashtbl.create 10

(* Node helpers *)
let is_node address = Hashtbl.mem node_queues address

let get_node_queues address = Hashtbl.find node_queues address

let register_node address = 
  if not (Hashtbl.mem node_queues address) then
    Hashtbl.add node_queues address (Queue.create (), Hashtbl.create 10)
  else invalid_arg ("duplicate node at "^(string_of_address address))

let unregister_node address = Hashtbl.remove node_queues address

(* Global queueing helpers *)
let get_global_queue address = 
  try fst (get_node_queues address)
  with Not_found -> failwith ("unknown node "^(string_of_address address))

(* Per trigger queueing helpers *)
let get_trigger_queues address = 
  try snd (get_node_queues address)
  with Not_found -> failwith ("unknown node "^(string_of_address address))

let get_trigger_input_queue address trigger_id =
  Hashtbl.find (get_trigger_queues address) trigger_id

let register_trigger address trigger_id =
  let trigger_queues = get_trigger_queues address in
  if not (Hashtbl.mem trigger_queues trigger_id) then
    Hashtbl.add trigger_queues trigger_id (Queue.create ())
  else invalid_arg ("duplicate trigger registration for "^trigger_id)

let unregister_trigger address trigger_id force =
  try let q = get_trigger_input_queue address trigger_id in
      if (Queue.is_empty q) || force then
        let trigger_queues = get_trigger_queues address in
        Hashtbl.remove trigger_queues trigger_id
  with Not_found -> () 


(* Scheduling methods *)
let schedule_task address task = Queue.push task (get_global_queue address)

let schedule_trigger v_target v_address args = match v_target, v_address with
  | VTarget trigger_id, VAddress address ->
    if use_global_queueing()
    then schedule_task address (NamedDispatch (trigger_id, args))
    else
      let q = get_trigger_input_queue address trigger_id
      in Queue.push args q;
         schedule_task address (BlockDispatch (trigger_id, dispatch_block_size()))

  | _, _ -> error INVALID_TRIGGER_TARGET

let schedule_event source_bindings source_id source_address events =
  let schedule_fn trig_id = schedule_trigger (VTarget trig_id) (VAddress source_address) in 
  try
    let trigger_ids = List.map snd (List.filter (fun (x,y) -> x = source_id) source_bindings) in 
    List.iter (fun trigger_id -> List.iter (schedule_fn trigger_id) events) trigger_ids
  with Not_found -> () 

(* 
 * Shuffle task from different nodes randomly to simulate network delay.
 *
 * It first buffer all triggers into a Hashtbl (sender_addr -> trigger queue),
 * then randmoly pop a trigger buffer queue and schedule it by calling
 * schedule_trigger. The order of trigger from the same sender address is
 * preserverd. 
 * *)

let buffer_trigger v_target v_address args v_sender_addr = 
  let helper htbl = 
    try 
      let q = Hashtbl.find htbl v_sender_addr in
      Queue.push (v_target,v_address,args) q 
    with Not_found -> 
      let new_q = Queue.create () in
      Queue.push (v_target,v_address,args) new_q;
      Hashtbl.add htbl v_sender_addr new_q
  in
  match v_address, v_target with 
  | VAddress address, VTarget trigger_id ->
    ( 
     try
      let per_node_trigger_buf = Hashtbl.find trigger_buffer address in
      helper per_node_trigger_buf;
    with Not_found ->
      let new_htbl = Hashtbl.create 10 in
      helper new_htbl;
      Hashtbl.add trigger_buffer address new_htbl
    )
  | _ -> error INVALID_TRIGGER_TARGET

let rec remove_at n = function
      | [] -> []
      | h :: t -> if n = 0 then t else h :: remove_at (n-1) t

let schedule_trigger_random receive_address = 
  Random.self_init(); (*TO DO random onece*)
  let per_node_trigger_buf = Hashtbl.find trigger_buffer receive_address in

  let senders_lst_ref = 
    ref (Hashtbl.fold (fun sender_add _ lst -> sender_add::lst)
                      per_node_trigger_buf [] )
  in
  while ((List.length !senders_lst_ref) > 0 ) 
  do
    let i = Random.int (List.length !senders_lst_ref) in
    let addr = List.nth (!senders_lst_ref) i in
    let queue = Hashtbl.find per_node_trigger_buf addr in 
    if Queue.is_empty queue then 
      senders_lst_ref := remove_at i (!senders_lst_ref)
    else
      let trigger = Queue.take queue in 
      match trigger with (v_target,v_address,args) ->
        schedule_trigger v_target v_address args
  done;
  Hashtbl.reset per_node_trigger_buf


(* Scheduler execution *)

let continue_processing address =
  (* shedule buffered trigger if the shuffle flag is on*)
  if scheduler_params.shuffle_tasks && Hashtbl.mem trigger_buffer address
  then schedule_trigger_random address;
  
  let has_messages = not (Queue.is_empty (get_global_queue address)) in 
  let events_remain = 
    scheduler_params.events_to_process < Int64.zero ||
    !events_processed < scheduler_params.events_to_process
  in
  has_messages && events_remain

let invoke_trigger address (trigger_env, val_env) trigger_id arg =
  (List.assoc trigger_id trigger_env) val_env arg;
  (* log the state for this trigger *)
  LOG "Trigger %s@%s:\n%s" trigger_id (string_of_address address) (string_of_env val_env) 
    NAME "K3Runtime.TriggerState" LEVEL DEBUG;
  events_processed := Int64.succ !events_processed

let process_trigger_queue address env trigger_id max_to_process =
  let q = get_trigger_input_queue address trigger_id in
  let processed = ref max_to_process in
  while not (Queue.is_empty q) && !processed > 0 do
    try invoke_trigger address env trigger_id (Queue.take q);
        decr processed
    with Not_found -> error INVALID_TRIGGER_QUEUE
  done;
  if not (Queue.is_empty q) then 
    schedule_task address (BlockDispatch (trigger_id, dispatch_block_size()))

let process_task address env =
  try
    match Queue.take (get_global_queue address) with
    | Background fn -> fn env

    | NamedDispatch (id,arg) ->
      if use_global_queueing() then invoke_trigger address env id arg else ()

    | BlockDispatch (id, max_to_process) ->
      if use_global_queueing() then ()
      else
        process_trigger_queue address env id max_to_process

  with Queue.Empty -> error INVALID_GLOBAL_QUEUE

(* Scheduler toplevel methods *)
let configure_scheduler program_events = 
  scheduler_params.events_to_process <- program_events

(* register the node and its triggers *)
let initialize_scheduler address (trig_env,_) =
  if not @: is_node address then register_node address; 
  List.iter (fun (id, _) -> register_trigger address id) trig_env

let node_has_work address =
  if not(is_node address) then false else 
  let node_queues = get_node_queues address in
  let empty_global_q = Queue.is_empty (fst node_queues) in
  let empty_trigger_q = Hashtbl.fold (fun _ q acc -> acc && Queue.is_empty q) (snd node_queues) true in
  let empty_shuffle_buffer = 
    if scheduler_params.shuffle_tasks then 
      (( Hashtbl.length (Hashtbl.find trigger_buffer address)) = 0) 
    else true
  in
  not ( empty_global_q && empty_trigger_q && empty_shuffle_buffer)

let network_has_work () = 
  Hashtbl.fold (fun addr _ acc -> acc || node_has_work addr) node_queues false

 
let run_scheduler ?(slice = max_int) address env shuffle_tasks =
  let i = ref slice in
  scheduler_params.shuffle_tasks <- shuffle_tasks;
  while !i > 0 && continue_processing address
  do process_task address env; decr i done
