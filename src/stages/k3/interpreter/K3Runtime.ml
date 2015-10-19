(* A single-threaded scheduler and single-site peer simulator *)

open Util
open K3.AST
open K3Streams
open K3Values
open K3Values.Value
open K3Util
open K3Printing

module Q = K3Queue

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

(* Error handling *)
type runtime_error_type =
    INVALID_GLOBAL_QUEUE
  | INVALID_NODE_QUEUE
  | INVALID_TRIGGER_QUEUE
  | INVALID_TRIGGER_TARGET
  | INVALID_ADDRESS
  | SLEEP_UNSUPPORTED

let runtime_error = function
  | INVALID_GLOBAL_QUEUE           -> -1000, "invalid empty task queue"
  | INVALID_NODE_QUEUE             -> -1001, "invalid empty node queue"
  | INVALID_TRIGGER_QUEUE          -> -1002, "invalid dequeue on trigger"
  | INVALID_TRIGGER_TARGET         -> -1003, "invalid target for message send"
  | INVALID_ADDRESS                -> -1004, "invalid node address"
  | SLEEP_UNSUPPORTED              -> -1005, "sleep unsupported by global queue"

let error t =
  let i, s = runtime_error t in
  raise @@ RuntimeError(i, "", s)

(* Event queues and scheduling *)

(* Modes of queueing supported:
 * i. Global queueing, where all nodes and triggers are managed on a single
 *    queue.
 * ii. Per-node queueing, where all trigger messages are managed on a single
 *    per-node queue. In multithreading evaluation, this would suffer from contention
 *    on the queue.
 *)

module AddrSet = Set.Make(struct type t = address let compare = compare end)

type task_t   = id_t * value_t
type n_task_t = Task of task_t
              | Sleep of float (* wakeup time *)

type per_node_q = {
  h   : (address,  n_task_t Queue.t) Hashtbl.t;
  arr : (address * n_task_t Queue.t) array;
  mutable last : int; (* index of last peer executed *)
}

(* one global queue for more deterministic execution *)
type queue_t = Global  of (address * task_t) Q.t
             | PerNode of per_node_q

type queue_type = GlobalQ | PerNodeQ

type scheduler_state = {
  mutable events_processed : int;
  queue : queue_t;
  mutable msg_count : int;
  mutable active_peers : AddrSet.t;
  mutable active_peer_count : int;
}

let init_scheduler_state ?(queue_type=PerNodeQ)
                         ~peers =
  {
    events_processed = 0;
    msg_count = 0;
    active_peers = AddrSet.of_list @@ fst_many peers;
    active_peer_count = List.length peers;
    queue = begin match queue_type with
     | GlobalQ     -> Global(Q.create ())
     | PerNodeQ    ->
         let addrs = fst_many peers in
         let qs = List.map (fun addr -> addr, Queue.create ()) addrs in
         PerNode {
           h=hashtbl_of_list qs;
           arr=Array.of_list qs;
           last=0;
         } end;
  }

(* Scheduling at the lowest level *)
let schedule_task s address task =
  s.msg_count <- succ s.msg_count;
  match s.queue with
  | Global q  -> Q.push (address, task) q
  | PerNode pn ->
      let nodeq =
        try Hashtbl.find pn.h address
        with Not_found -> error INVALID_NODE_QUEUE
      in
      Queue.push (Task task) nodeq

(* make a node sleep (s) *)
let sleep s address t = match s.queue with
  | Global _ -> error SLEEP_UNSUPPORTED
  | PerNode pn ->
      let nodeq =
        try Hashtbl.find pn.h address
        with Not_found -> error INVALID_NODE_QUEUE
      in
      let q = Queue.create () in
      (* move all entries to another q *)
      Queue.transfer nodeq q;
      Queue.push (Sleep(Sys.time () +. t)) nodeq;
      (* move them back *)
      Queue.transfer q nodeq

(* halt a node *)
let halt s address =
  s.active_peers <- AddrSet.remove address s.active_peers;
  s.active_peer_count <- pred s.active_peer_count

let schedule_trigger s v_target v_address args =
  match v_target, v_address with
  | VTarget trigger_id, VAddress address ->
      schedule_task s address (trigger_id, args)
  | _ -> error INVALID_TRIGGER_TARGET

let schedule_event s src_bindings src_id src_address events =
  let schedule_fn trig_id =
    schedule_trigger s (VTarget trig_id) (VAddress src_address) in
  try
    let trigger_ids = snd_many @@
      List.filter (fun (x,_) -> x = src_id) src_bindings in
    List.iter (fun trigger_id -> List.iter (schedule_fn trigger_id) events)
      trigger_ids
  with Not_found -> ()

(* Scheduler execution *)

let invoke_trigger s address env trigger_id arg =
  (* add a level to the global queue *)
  begin match s.queue with
  | Global q  -> Q.increase_level q
  | _         -> ()
  end;
  (* reset the access env *)
  (env.accessed) := StrSet.empty;
  let trig = IdMap.find trigger_id env.triggers in
  begin try trig address env [arg]
    (* re-raise exception with trig name *)
  with RuntimeError(id, s, msg) -> raise @@
    RuntimeError(id, s, sp "In trigger %s: %s" trigger_id msg) end;
  (* log the state for this trigger *)
  Log.log (lazy (sp "\nTrigger %s@%s\nargs = %s\n%s" trigger_id (string_of_address address)
    (string_of_value arg) @@
    string_of_env ~skip_empty:false env)) ~name:"K3Runtime.TriggerState" `Debug;
  s.events_processed <- succ s.events_processed

let next_idx arr idx =
  let idx' = idx + 1 in
  if idx' >= Array.length arr then 0 else idx'

let decr_count s = s.msg_count <- pred s.msg_count

let process_task s prog_env_fn = match s.queue with
  | Global q  ->
      begin try
        decr_count s;
        let addr, (id, arg) = Q.pop q in
        invoke_trigger s addr (prog_env_fn addr) id arg
      with Q.Empty -> error INVALID_GLOBAL_QUEUE end

  | PerNode q ->
      (* round-robin *)
      let rec loop idx n =
        let cont () = loop (next_idx q.arr q.last) (n-1) in
        if n = 0 then q.last <- idx
        else
          let addr, nodeq = q.arr.(idx) in
          (* skip if q is empty or if we're inactive *)
          if Queue.is_empty nodeq || not (AddrSet.mem addr s.active_peers) then cont ()
          else begin match Queue.peek nodeq with
            | Sleep t ->
                if Sys.time () > t then begin
                  ignore @@ Queue.pop nodeq;
                  q.last <- idx
                end else cont ()
            | Task (id, arg) ->
                decr_count s;
                ignore @@ Queue.pop nodeq;
                (* save last executed *)
                q.last <- idx;
                invoke_trigger s addr (prog_env_fn addr) id arg
            end
      in
      loop (next_idx q.arr q.last) @@ Array.length q.arr

(* Scheduler toplevel methods *)

let network_has_work s = s.msg_count > 0 && s.active_peer_count > 0

(* address is ignored for global queue *)
let run_scheduler ?(slice = max_int) s get_env_fn =
  let rec loop i =
    if i >= slice || not (network_has_work s) then i
    else begin
      process_task s get_env_fn;
      loop @@ i+1
    end
  in
  loop 0

