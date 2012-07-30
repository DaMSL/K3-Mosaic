(* A single-threaded scheduler and single-site peer simulator *)

open K3
open K3Values
open K3Util

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
  try fst (List.assoc t runtime_errors)
  with Not_found -> -1
  

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
  }

let scheduler_params = {
    mode = PerTrigger;
    events_to_process = Int64.minus_one;
    interleave_period = 10;
  }
    
let use_global_queueing () = scheduler_params.mode = Global
let dispatch_block_size () = scheduler_params.interleave_period  
let events_processed = ref Int64.zero

(* node address -> task queue * per trigger queues *)
let node_queues = Hashtbl.create 10

(* trigger id -> ref queue of tasks *)
let trigger_queues = Hashtbl.create 10

(* Node helpers *)

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

let get_trigger_queue address trigger_id =
  Hashtbl.find (get_trigger_queues address) trigger_id

let register_trigger address trigger_id =
  let trigger_queues = get_trigger_queues address in
  if not (Hashtbl.mem trigger_queues trigger_id) then
    Hashtbl.add trigger_queues trigger_id (Queue.create ())
  else invalid_arg ("duplicate trigger registration for "^trigger_id)

let unregister_trigger address trigger_id force =
  try let q = get_trigger_queue address trigger_id in
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
      let q = get_trigger_queue address trigger_id
      in Queue.push args q;
         schedule_task address (BlockDispatch (trigger_id, dispatch_block_size()))

  | _, _ -> raise (RuntimeError (error INVALID_TRIGGER_TARGET))

let schedule_event source_bindings source_id source_address events =
  try 
    let trigger_ids = List.map snd
      (List.filter (fun (x,y) -> x = source_id) source_bindings)
    in List.iter (fun trigger_id ->
          let schedule_fn =
            schedule_trigger (VTarget trigger_id) (VAddress source_address)
          in List.iter schedule_fn events
        ) trigger_ids
  with Not_found -> () 


(* Scheduler execution *)

let continue_processing address =
  not (Queue.is_empty (get_global_queue address))
  && !events_processed < scheduler_params.events_to_process

let invoke_trigger address (trigger_env, val_env) trigger_id arg =
  (List.assoc trigger_id trigger_env) val_env arg;
  events_processed := Int64.succ !events_processed

let process_trigger_queue address env trigger_id max_to_process =
  let q = get_trigger_queue address trigger_id in
  let processed = ref max_to_process in
  while not (Queue.is_empty q) && !processed > 0 do
    try invoke_trigger address env trigger_id (Queue.take q);
        decr processed
    with Not_found -> raise (RuntimeError (error INVALID_TRIGGER_QUEUE))
  done

let process_task address env =
  try
    match Queue.take (get_global_queue address) with
    | Background fn -> fn env

    | NamedDispatch (id,arg) ->
      if use_global_queueing() then invoke_trigger address env id arg else ()

    | BlockDispatch (id, max_to_process) ->
      if use_global_queueing() then ()
      else process_trigger_queue address env id max_to_process

  with Queue.Empty -> raise (RuntimeError (error INVALID_GLOBAL_QUEUE))

let run_scheduler address env =
  while continue_processing address do
    process_task address env
  done