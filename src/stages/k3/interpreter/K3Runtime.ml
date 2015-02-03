(* A single-threaded scheduler and single-site peer simulator *)

open Util
open K3.AST
open K3Streams
open K3Values
open K3Values.Value
open K3Util
open K3Printing

module Q = K3Queue
let sp = Printf.sprintf

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

(* Error handling *)
type runtime_error_type =
    INVALID_GLOBAL_QUEUE
  | INVALID_TRIGGER_QUEUE
  | INVALID_TRIGGER_TARGET
  | INVALID_SHUFFLE_BUFFER of address
  | INVALID_BREAKPOINT
  | INVALID_ADDRESS

let runtime_error = function
  | INVALID_GLOBAL_QUEUE           -> -1000, "invalid empty task queue"
  | INVALID_TRIGGER_QUEUE          -> -1001, "invalid dequeue on trigger"
  | INVALID_TRIGGER_TARGET         -> -1002, "invalid target for message send"
  | INVALID_SHUFFLE_BUFFER address ->
      (-1003, "error in shuffle buffer: no rcv address "^string_of_address address)
  | INVALID_BREAKPOINT             -> -1004, "invalid breakpoint"
  | INVALID_ADDRESS                -> -1005, "invalid node address"

let error t =
  let i, s = runtime_error t in
  raise @@ RuntimeError(i, s)

(* Event queues and scheduling *)

type task_t = id_t * value_t

type scheduler_spec = {
    mutable events_to_process : int64;
    mutable interleave_period : int;
    mutable shuffle_tasks     : bool;
  }

let default_interleave_period = 10
let default_events_to_process = Int64.minus_one

let dummy = ref 0

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

(* Modes of queueing supported:
 * i. Global queueing, where all nodes and triggers are managed on a single
 *    queue.
 * ii. Per-node queueing, where all trigger messages are managed on a single
 *    per-node queue. In multithreading evaluation, this would suffer from contention
 *    on the queue.
 *)

type node_queue_t =
    (* node,  (node    queue,   *)
    (address, (task_t Q.t)) Hashtbl.t

(* one global queue for more deterministic execution *)
type global_queue_t = (address * task_t) Q.t

type queue_t = Global of global_queue_t

type queue_type = GlobalQ

let string_of_queue_t = function
  | GlobalQ     -> "global queue"

 (* for causing intentional reordering within the simulated network (while
  * still maintaining TCP-like ordering from a single sender *)
type shuffle_buffer_t =
    (* node * sender,   (trigger * args)) *)
    (address, (address, (id_t * value_t) Q.t) Hashtbl.t) Hashtbl.t

type scheduler_state = {
  params : scheduler_spec;
  mutable events_processed : int;
  queue : queue_t;
  (* time to sleep and remaining queue after sleep *)
  sleep_queue : (address, (int * task_t Q.t)) Hashtbl.t;
  sleep_t : int option;
 }

let use_shuffle_tasks s  = s.params.shuffle_tasks

let init_scheduler_state ?(shuffle_tasks=false)
                         ?(breakpoints=[])
                         ?(run_length=default_events_to_process)
                         ?(queue_type=GlobalQ)
                         () =
  {
    params = { default_params with events_to_process = run_length };
    events_processed = 0;
    queue = (match queue_type with
     | GlobalQ     -> Global(Q.create ()));
    sleep_queue = Hashtbl.create 10;
    sleep_t = None;
  }

let use_global_queueing s = match s.queue with Global _ -> true | _ -> false

(* Node helpers *)
let is_node s address = match s.queue with
  | Global q  -> failwith "no node info in global queue"

let get_node_queues s address = match s.queue with
  | Global q  -> failwith "no node queue in global queue"

let register_node s address =
  if not @@ is_node s address then match s.queue with
  | Global q  -> failwith "no node data in global queue"
  else invalid_arg @@ "duplicate node at "^string_of_address address

let unregister_node s address = match s.queue with
  | Global q  -> failwith "no node data in global queue"

(* Global queueing helpers *)
let get_global_queue s address = match s.queue with
  | Global q  -> failwith "global data is immediately accessible"

(* Scheduling methods *)
let schedule_task s address task = match s.queue with
  | Global q  -> Q.push (address, task) q

let schedule_trigger s v_target v_address args =
  match v_target, v_address with
  | VTarget trigger_id, VAddress address ->
      schedule_task s address @@ (trigger_id, args)
  | _ -> error INVALID_TRIGGER_TARGET

let schedule_event s source_bindings source_id source_address events =
  let schedule_fn trig_id =
    schedule_trigger s (VTarget trig_id) (VAddress source_address) in
  try
    let trigger_ids = snd_many @@
      List.filter (fun (x,_) -> x = source_id) source_bindings in
    List.iter
      (fun trigger_id -> List.iter (schedule_fn trigger_id) events)
      trigger_ids
  with Not_found -> ()

(* Scheduler execution *)

let invoke_trigger s address (trigger_env, val_env) trigger_id arg =
  (* add a level to the global queue *)
  (match s.queue with
  | Global q  -> Q.increase_level q);
  let trig = IdMap.find trigger_id trigger_env in
  (try (* re-raise exception with trig name *)
    trig address val_env arg
  with RuntimeError(id, msg) -> raise @@
    RuntimeError(id, Format.sprintf "In trigger %s: %s" trigger_id msg));
  (* log the state for this trigger *)
  let arg_s = string_of_value arg^"\n" in
  Log.log (sp "Trigger %s@%s\nargs = %s"
    trigger_id
    (string_of_address address)
    (arg_s^string_of_env val_env))
    ~name:"K3Runtime.TriggerState" `Debug;
  s.events_processed <- succ s.events_processed

(* obtain the address of the next node on the global queue *)
let next_global_address s = match s.queue with
  | Global q  -> fst @@ Q.peek q

let process_task s address prog_env =
  let addr, (id, arg) = try begin match s.queue with
      (* global mode ignores the address given *)
    | Global q  -> Q.pop q
    end
    with Q.Empty -> error INVALID_GLOBAL_QUEUE
  in
  (* check for sleeping node *)
  try
    let time, sleep_q = Hashtbl.find s.sleep_queue addr in
    Q.push (id, arg) sleep_q
  with Not_found ->
    (* just run the trigger *)
    invoke_trigger s addr prog_env id arg

(* Scheduler toplevel methods *)

(* register the node and its triggers *)
let initialize_scheduler s address (trig_env,_) = match s.queue with
  | Global _  -> ()

let node_has_work s address =
  try let node_queues = get_node_queues s address in
      not @@ Q.is_empty node_queues
  with Not_found -> error INVALID_ADDRESS

let network_has_work s = match s.queue with
  | Global q  -> not @@ Q.is_empty q

let continue_processing s address = match s.queue with
  | Global q  -> network_has_work s

(* address is ignored for global queue *)
let run_scheduler ?(slice = max_int) s address env =
  let rec loop i =
    if i <= 0 || not @@ continue_processing s address then ()
    else
      process_task s address env;
      loop @@ i-1
  in loop slice

