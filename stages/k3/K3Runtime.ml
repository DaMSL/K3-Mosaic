(* A single-threaded scheduler and single-site peer simulator *)

open K3
open K3Values

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

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

(* task queue, which will not contain block dispatching when running in
 * global queueing mode. *)
let global_queue = Queue.create()

(* trigger id -> ref queue of tasks *)
let trigger_queues = Hashtbl.create 10

(* Per trigger queueing helpers *)
let get_trigger_queue trigger_id = Hashtbl.find trigger_queues trigger_id

let register_trigger trigger_id =
  if not (Hashtbl.mem trigger_queues trigger_id) then
    Hashtbl.add trigger_queues trigger_id (Queue.create ())
  else invalid_arg ("duplicate trigger registration for "^trigger_id)

let unregister_trigger trigger_id force =
  try let q = get_trigger_queue trigger_id in
      if (Queue.is_empty q) || force then
        Hashtbl.remove trigger_queues trigger_id
  with Not_found -> () 

(* Scheduling methods *)
let schedule_task task = Queue.push task global_queue

let schedule_local_trigger trigger_id args =
  if use_global_queueing()
  then schedule_task (NamedDispatch (trigger_id, args))
  else
    let q = get_trigger_queue trigger_id
    in Queue.push args q;
       schedule_task (BlockDispatch (trigger_id, dispatch_block_size()))

(* TODO: remote triggers *)
let schedule_trigger target addr args = match target with
  | VTarget trigger_id -> schedule_local_trigger trigger_id args
  | _ -> raise (RuntimeError (-1))

let schedule_event source_bindings source_id events =
  try 
    let trigger_ids = List.map snd
      (List.filter (fun (x,y) -> x = source_id) source_bindings)
    in List.iter (fun trig_id ->
        List.iter (schedule_local_trigger trig_id) events) trigger_ids
  with Not_found -> () 

(* Scheduler execution *)

let continue_processing () =
  not (Queue.is_empty global_queue)
  && !events_processed < scheduler_params.events_to_process

let invoke_trigger (trigger_env, val_env) trigger_id arg =
  (List.assoc trigger_id trigger_env) val_env arg;
  events_processed := Int64.succ !events_processed

let process_trigger_queue env trigger_id max_to_process =
  let q = get_trigger_queue trigger_id in
  let processed = ref max_to_process in
  while not (Queue.is_empty q) && !processed > 0 do
    try invoke_trigger env trigger_id (Queue.take q);
        decr processed
    with Not_found -> raise (RuntimeError (-1))
  done

let process_task env =
  try
    match Queue.take global_queue with
    | Background fn -> fn env

    | NamedDispatch (id,arg) ->
      if use_global_queueing() then invoke_trigger env id arg else ()

    | BlockDispatch (id, max_to_process) ->
      if use_global_queueing() then ()
      else process_trigger_queue env id max_to_process

  with Queue.Empty -> raise (RuntimeError (-1))

let run_scheduler env =
  while continue_processing () do
    process_task env
  done
