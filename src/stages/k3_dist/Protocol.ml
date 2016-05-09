(* Basic protocol for init *)
open K3.AST
open K3Helpers
open Util
open K3Dist

module D = K3Dist
module C = GenCommon
module G = K3Global
module Std = K3StdLib
module P = ProgInfo
module GC = GarbageCollection
module TS = Timestamp

(**** protocol plan ****)
(* the master tells everyone who he is
 * everyone reports their role to the master
 * master fills in everyone's jobs, ack
 * master tells switches to start
 *)

(**** init code to fill out jobs map ****)

let ms_rcv_sw_init_ack_cnt = mk_counter "ms_rcv_sw_init_ack_cnt"
let ms_rcv_sw_init_ack_nm = "ms_rcv_sw_init_ack"
let ms_rcv_sw_init_ack c =
  mk_barrier ms_rcv_sw_init_ack_nm ~ctr:ms_rcv_sw_init_ack_cnt.id
    ~total:(mk_var D.num_switches.id)
    ~after:
      (mk_block [
        (* start timing *)
        mk_assign D.ms_start_time.id @@ mk_apply' "now_int" [mk_cunit];
        (* master initiates timestamp protocol *)
        TS.ms_init;
        (* master initiates gc protocol *)
        GC.ms_gc_init c; ])

let sw_rcv_init_nm = "sw_rcv_init"
let sw_rcv_init =
  mk_code_sink' sw_rcv_init_nm unit_arg [] @@
  mk_block [
    mk_assign D.sw_init.id mk_ctrue;
    (* functions to start switch role. dummy in interpreter *)
    mk_apply' "switchInit" [];
    mk_apply' "switchStart" [];
    (* ack to master *)
    C.mk_send_master ms_rcv_sw_init_ack_nm;
  ]

let ms_sys_init_barrier_cnt = mk_counter "ms_sys_init_barrier_cnt"

let ms_tell_sw_to_init = C.mk_send_all_switches sw_rcv_init_nm [mk_cunit]

(* master: barrier for system_ready_event. *)
let ms_sys_init_barrier_nm = "ms_"^D.sys_init^"_barrier"
let ms_sys_init_barrier =
  mk_barrier ms_sys_init_barrier_nm ~ctr:ms_sys_init_barrier_cnt.id
    ~total:(mk_var D.num_nodes.id) ~after:ms_tell_sw_to_init

(* pending sys_init_barrier_request *)
let nd_sys_init_barrier_req = mk_bool_ds "nd_sys_init_barrier_req"

let nd_sys_init_check_barrier =
  mk_if
    (* if a barrier is requested and stmt_cntrs are empty *)
    (mk_and
      (mk_var nd_sys_init_barrier_req.id) @@
      mk_eq (mk_var D.nd_stmt_cntr_size.id) @@ mk_cint 0)
    (mk_block [
      (* send and update flag *)
      C.mk_send_master ms_sys_init_barrier_nm;
      mk_assign nd_sys_init_barrier_req.id mk_cfalse])
    mk_cunit

let nd_sys_init_barrier_nm = "nd_sys_init_barrier"
let nd_sys_init_barrier =
  mk_code_sink' nd_sys_init_barrier_nm unit_arg [] @@
  mk_block [
    (* set flag *)
    mk_assign nd_sys_init_barrier_req.id mk_ctrue;
    (* check for barrier condition *)
    nd_sys_init_check_barrier;
  ]

(* switch: system ready event handling.
 * before the queues turn on, bypass them and send the fetch for sys_read_evt
 * from only the first switch.
 * should only be created when we have
 * system_ready_event code *)
let sw_sys_init_nm = "sw_"^D.sys_init
let sw_sys_init c =
  let ss = P.stmts_of_t c.p D.sys_init in
  mk_code_sink' sw_sys_init_nm unit_arg [] @@
  mk_let_block ["batch_id"] (mk_cint 1) @@
    [C.clear_poly_queues c] @
    (List.map (fun s ->
        mk_apply' (D.send_fetch_single_vid_name_of_t D.sys_init s) [sys_init_vid_k3])
      ss) @
    [
      C.send_poly_queues;

      (* send notifications *)
      C.mk_send_all_nodes nd_sys_init_barrier_nm [mk_cunit]
    ]

let ms_post_warmup_nm = "ms_post_warmup"
let ms_post_warmup c =
  mk_code_sink' ms_post_warmup_nm [] [] @@
  (* if we have a sys_ready event, we need to activate that first *)
    if c.sys_init then
      mk_send sw_sys_init_nm (mk_var TS.sw_next_switch_addr.id) []
    else (* else, tell switches to init *)
      ms_tell_sw_to_init

let ms_post_warmup_barrier_cnt = create_ds "ms_post_warmup_barrier_cnt" @@ mut t_int

let ms_post_warmup_barrier_nm = "ms_post_warmup_barrier"
let ms_post_warmup_barrier c =
  mk_barrier ms_post_warmup_barrier_nm ~ctr:ms_post_warmup_barrier_cnt.id
    ~total:(mk_cint @@ List.length @@ P.get_maps_with_keys c.p)
    ~after:(C.mk_send_me ms_post_warmup_nm)

(* warmup consists of reading from the files *)
let sw_warmup_nm = "sw_warmup"

let ms_rcv_jobs_ack_cnt = create_ds "ms_rcv_jobs_ack_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_jobs_ack_nm = "ms_rcv_jobs_ack"
let ms_rcv_jobs_ack c =
  mk_barrier ms_rcv_jobs_ack_nm ~ctr:ms_rcv_jobs_ack_cnt.id
    ~total:(mk_var D.num_peers.id)
    ~after:(mk_if (mk_var D.do_warmup.id)
             (mk_send sw_warmup_nm (mk_var TS.sw_next_switch_addr.id) []) @@
              C.mk_send_me ms_post_warmup_nm)

(* receive jobs, and calculate all dependent variables *)
let rcv_jobs_nm = "rcv_jobs"
let rcv_jobs c =
  mk_code_sink' rcv_jobs_nm ["jobs_in", immut D.jobs.t] [] @@
  mk_block [
    (* write the jobs table *)
    mk_assign D.jobs.id @@ mk_var "jobs_in";
    (* init the things that depend on jobs *)
    (* set timer addr *)
    delayed_init D.timer_addr;
    (* set nodes *)
    delayed_init D.nodes;
    delayed_init D.num_nodes;
    (* set switches *)
    delayed_init D.switches;
    delayed_init D.num_switches;
    (* if we're master *)
    mk_if (mk_eq (mk_var D.job.id) @@ mk_var D.job_master.id)
      (delayed_init GC.ms_num_gc_expected)
      mk_cunit;
    (* if we're a switch or master *)
    mk_if (mk_or (mk_eq (mk_var D.job.id) @@ mk_var D.job_switch.id) @@
                  mk_eq (mk_var D.job.id) @@ mk_var D.job_master.id)
      (* set next switch addr *)
      (delayed_init TS.sw_next_switch_addr)
      mk_cunit;
    (* if we're master *)
    mk_if_eq (mk_var D.job.id) (mk_var D.job_master.id)
      (delayed_init TS.ms_last_switch_addr) mk_cunit;
    (* add to node ring *)
    K3Ring.node_ring_init;
    (* init route's node bitmap *)
    unwrap_some K3Route.all_nodes_bitmap.d_init;
    (* pre-calculate all the route memoization tables *)
    mk_apply' K3Route.memo_init_all_nm [];
    mk_apply' K3Route.route_opt_init_all_nm [];
    (* reserve space in all the polyqueues *)
    C.reserve_poly_queue_code ~all:true c;
    (* ack the msg *)
    C.mk_send_master ms_rcv_jobs_ack_nm;
  ]

(* receive the role from everybody *)
let ms_rcv_job_cnt = create_ds "ms_rcv_job_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_job_nm = "ms_rcv_job"
let ms_rcv_job =
  mk_barrier ms_rcv_job_nm ~args:["addr", t_int; "job", t_int]
    (* insert into jobs *)
    ~pre:[mk_insert_at D.jobs.id (mk_var "addr") [mk_var "job"]]
    ~ctr:ms_rcv_job_cnt.id ~total:(mk_var D.num_peers.id)
    ~after:(C.mk_send_all_peers rcv_jobs_nm [mk_var D.jobs.id])

(* rcv the master's address and send him our role *)
let rcv_master_addr_nm = "rcv_master_addr"
let rcv_master_addr =
  let addr = "addr" in
  mk_code_sink' rcv_master_addr_nm [addr, t_addr] [] @@
  mk_block [
    (* assign the master addr *)
    mk_assign D.master_addr.id @@ mk_var addr;
    (* send our job to the master *)
    mk_send ms_rcv_job_nm (mk_var addr) [mk_var D.me_int.id; mk_var D.job.id]
  ]

(**** start point for master role ****)
(* master sends his address to all nodes *)
let ms_send_addr_self_nm = "ms_send_addr_self"
let ms_send_addr_self =
  mk_code_sink' ms_send_addr_self_nm unit_arg [] @@
    C.mk_send_all_peers rcv_master_addr_nm [G.me_var]


(**** shutdown protocol ****)

let nd_sent_done = create_ds "nd_sent_done" (mut t_bool) ~init:mk_cfalse
let sw_sent_done = create_ds "sw_sent_done" (mut t_bool) ~init:mk_cfalse

(**** protocol plan ****
 * Switches see sentinel and tell master
 * Master notifies nodes
 * Nodes check when their stmt_cntrs are empty and tell master
 * If all nodes are done, master calls time and ends, but waits
 * If a node sees more action, it calls undo on master
 *)

(* we need to flush the msg queue, so we only do shutdown ourselves *)
let shutdown_trig2_nm = "shutdown_trig2"
let shutdown_trig2 =
  mk_code_sink' shutdown_trig2_nm unit_arg [] @@
    mk_block [
      mk_if_eq (mk_var job.id) (mk_var job_node.id)
        (mk_print @@ mk_concat
           (mk_cstring "Number of batches stashed: ") @@
           mk_soi @@ mk_var nd_num_stashed.id)
        mk_cunit;
      mk_apply' "haltEngine" [mk_cunit]
    ]

let shutdown_trig_nm = "shutdown_trig"
let shutdown_trig =
  mk_code_sink' shutdown_trig_nm unit_arg [] @@
  mk_block [
    D.profile_funcs_stop;
    mk_send shutdown_trig2_nm G.me_var []
  ]

let ms_rcv_node_done_cnt = mk_counter "ms_rcv_node_done_cnt"

let ms_shutdown_nm = "ms_shutdown"
let ms_shutdown =
  mk_code_sink' ms_shutdown_nm unit_arg [] @@
    (* notify everyone to shut down *)
    C.mk_send_all_peers shutdown_trig_nm [mk_cunit]

(* master: receive notification that nodes are done with their work (stmt_cntrs empty) *)
let ms_rcv_node_done_nm = "ms_rcv_node_done"
let ms_rcv_node_done =
  mk_barrier ms_rcv_node_done_nm
    ~ctr:ms_rcv_node_done_cnt.id ~total:(mk_var D.num_nodes.id) ~after:
      (mk_block [
        (* update end time *)
        mk_assign D.ms_end_time.id @@ mk_apply' "now_int" [mk_cunit];
        mk_print @@ mk_concat
            (mk_cstring "Total time (ms): ") @@
             mk_soi @@
               mk_sub (mk_var D.ms_end_time.id) @@ mk_var D.ms_start_time.id;
        (* send ourselves a message to shutdown *)
        C.mk_send_me ms_shutdown_nm;
      ])

(* node: bool indicating received system done request that needs to be handled.
         bool is reset after handling *)
let nd_sys_done_req = mk_bool_ds "nd_sys_done_req"

(* switch: bool indicating received system done *)
let sw_all_done = mk_bool_ds "sw_all_done"

(* whether a node should notify the master that it's done *)
let nd_done_check_barrier =
  (* check stmt_cntrs for emptiness *)
  mk_if
    (mk_and
      (mk_var nd_sys_done_req.id) @@
      (* no stmt_cntrs *)
      mk_eq (mk_var D.nd_stmt_cntr_size.id) @@ mk_cint 0)
    (mk_block [
      (* notify master *)
      C.mk_send_master ms_rcv_node_done_nm;
      (* mark as done *)
      mk_assign nd_sys_done_req.id mk_cfalse])
    mk_cunit

let nd_rcv_done_nm = "nd_rcv_done"
let nd_rcv_done =
  mk_code_sink' nd_rcv_done_nm unit_arg [] @@
  mk_block [
    (* set done signal *)
    mk_assign nd_sys_done_req.id mk_ctrue;
    nd_done_check_barrier;
  ]

(* after all switched are done, stop sending vector clock *)
let sw_rcv_done_nm = "sw_rcv_done"
let sw_rcv_done =
  mk_code_sink' sw_rcv_done_nm unit_arg [] @@
    mk_assign sw_all_done.id mk_ctrue

(* Code for after deletion from stmt_cntrs *)
let nd_post_delete_stmt_cntr c =
  mk_block @@
    (* check for a sys done barrier *)
    [nd_done_check_barrier] @
    (* check for a sys init barrier *)
    (if c.sys_init then [nd_sys_init_check_barrier]
    else [])

let ms_rcv_switch_done_cnt = mk_counter "ms_rcv_switch_done_cnt"

let ms_rcv_switch_done_nm = "ms_rcv_switch_done"
let ms_rcv_switch_done =
  mk_barrier ms_rcv_switch_done_nm ~ctr:ms_rcv_switch_done_cnt.id
    ~total:(mk_var D.num_switches.id)
    ~after:(mk_block [
              C.mk_send_all_switches sw_rcv_done_nm [];
              (* if we're in debug_run, directly send done to master *)
              debug_run_test_var
                ~default:(C.mk_send_master ms_rcv_node_done_nm)
                debug_run_sw_send_all @@
                C.mk_send_all_nodes nd_rcv_done_nm []
            ])

(* check that the switch is done with its work *)
let sw_check_done ~check_size =
  mk_if
    (mk_and
      (mk_not @@ mk_var sw_sent_done.id) @@
      (if check_size then
        mk_and
          (mk_eq (mk_size @@ mk_var D.sw_event_queue.id) @@ mk_cint 0) else id_fn) @@
        mk_and
          (mk_eq (mk_var GC.sw_num_ack.id) @@ mk_var GC.sw_num_sent.id) @@
           mk_var D.sw_seen_sentinel.id)
    (mk_block [
      (* send *)
      C.mk_send_master ms_rcv_switch_done_nm;
      (* mark as sent *)
      mk_assign sw_sent_done.id mk_ctrue])
    mk_cunit

(* code for when switches see the sentinel *)
let sw_seen_sentinel ~check_size =
  mk_block [
    mk_assign D.sw_seen_sentinel.id mk_ctrue;
    sw_check_done ~check_size
  ]

(* global call for init of profiling *)
let init_profiling = create_ds "init_profiling" t_unit ~init:D.profile_funcs_start

let global_vars c =
  List.map decl_global @@
  (if c.sys_init then [
    ms_sys_init_barrier_cnt;
    nd_sys_init_barrier_req;
  ] else []) @
  [
    init_profiling;
    nd_sys_done_req;
    sw_all_done;
    nd_sent_done;
    sw_sent_done;
    ms_rcv_sw_init_ack_cnt;
    ms_rcv_jobs_ack_cnt;
    ms_rcv_job_cnt;
    ms_rcv_node_done_cnt;
    ms_rcv_switch_done_cnt;
    ms_post_warmup_barrier_cnt;
  ]

let triggers c =
  (if c.sys_init then [
    nd_sys_init_barrier;
    sw_sys_init c;
    ms_sys_init_barrier;
  ] else []) @ [
    ms_post_warmup c;
    ms_post_warmup_barrier c;
    ms_rcv_sw_init_ack c;
    sw_rcv_init;
    ms_rcv_jobs_ack c;
    rcv_jobs c;
    ms_rcv_job;
    rcv_master_addr;
    ms_send_addr_self;
    shutdown_trig2;
    shutdown_trig;
    ms_shutdown;
    ms_rcv_node_done;
    nd_rcv_done;
    sw_rcv_done;
    ms_rcv_switch_done;
  ]
