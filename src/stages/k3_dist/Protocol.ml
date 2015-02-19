(* Basic protocol for init *)
open K3.AST
open K3Helpers
open Util

module D = K3Dist
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

let ms_rcv_sw_init_ack_cnt = create_ds "ms_rcv_sw_init_ack_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_sw_init_ack_nm = "ms_rcv_sw_init_ack"
let ms_rcv_sw_init_ack c =
  mk_code_sink' ms_rcv_sw_init_ack_nm unit_arg [] @@
  mk_block [
    (* increment counter *)
    mk_incr ms_rcv_sw_init_ack_cnt.id;
    mk_if (mk_geq (mk_var ms_rcv_sw_init_ack_cnt.id) @@ mk_var D.num_switches.id)
      (mk_block [
        (* start timing *)
        mk_assign D.ms_start_time.id @@ mk_apply' "now_int" mk_cunit;
        (* master initiates timestamp protocol *)
        TS.ms_init;
        (* master initiates gc protocol *)
        GC.ms_gc_init c; ])
      mk_cunit;
  ]

let sw_rcv_init_nm = "sw_rcv_init"
let sw_rcv_init =
  mk_code_sink' sw_rcv_init_nm unit_arg [] @@
  mk_block [
    (* set to idle state if we're still in pre_init *)
    mk_if (mk_eq (mk_var D.sw_state.id) @@ mk_var D.sw_state_pre_init.id)
      (mk_assign D.sw_state.id @@ mk_var D.sw_state_idle.id) @@
      mk_cunit;
    (* start driver for all switches *)
    mk_send D.sw_driver_trig_nm G.me_var [mk_cunit];
    (* ack to master *)
    mk_send ms_rcv_sw_init_ack_nm (mk_var D.master_addr.id) [mk_cunit];
  ]

let ms_rcv_jobs_ack_cnt = create_ds "ms_rcv_jobs_ack_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_jobs_ack_nm = "ms_rcv_jobs_ack"
let ms_rcv_jobs_ack =
  mk_code_sink' ms_rcv_jobs_ack_nm unit_arg [] @@
  mk_block [
    mk_incr ms_rcv_jobs_ack_cnt.id;
    (* if we've receive all acks *)
    mk_if (mk_geq (mk_var ms_rcv_jobs_ack_cnt.id) @@ mk_var D.num_peers.id)
      (mk_block [
        (* tell switches to init *)
        mk_iter (mk_lambda' ["addr", t_addr] @@
            mk_send sw_rcv_init_nm (mk_var "addr") [mk_cunit]) @@
          mk_var D.switches.id]) @@
      mk_cunit
  ]

(* receive jobs, and calculate all dependent variables *)
let rcv_jobs_nm = "rcv_jobs"
let rcv_jobs =
  mk_code_sink' rcv_jobs_nm ["jobs_in", immut D.jobs.t] [] @@
  mk_block [
    (* write the jobs table *)
    mk_assign D.jobs.id @@ mk_var "jobs_in";
    (* init the things that depend on jobs *)
    (* if we're a switch *)
    mk_if (mk_leq (mk_var D.job.id) @@ mk_var D.job_switch.id)
      (* set next switch addr *)
      (delayed_init TS.sw_next_switch_addr)
      mk_cunit;
    (* set timer addr *)
    delayed_init D.timer_addr;
    (* set nodes *)
    delayed_init D.nodes;
    delayed_init D.num_nodes;
    (* set switches *)
    delayed_init D.switches;
    delayed_init D.num_switches;
    (* add to node ring *)
    K3Ring.ring_init;
    (* ack the msg *)
    mk_send ms_rcv_jobs_ack_nm (mk_var D.master_addr.id) [mk_cunit];
  ]

(* receive the role from everybody *)
let ms_rcv_job_cnt = create_ds "ms_rcv_job_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_job_nm = "ms_rcv_job"
let ms_rcv_job =
  let addr = hd @@ fst_many G.peers.e in
  mk_code_sink' ms_rcv_job_nm D.jobs.e [] @@
  mk_block [
    (* insert into jobs *)
    mk_insert D.jobs.id @@ ids_to_vars @@ fst_many D.jobs.e;
    (* increment counter *)
    mk_assign ms_rcv_job_cnt.id @@
      mk_add (mk_var ms_rcv_job_cnt.id) (mk_cint 1);
    (* if we've received all the answers *)
    mk_if (mk_geq (mk_var ms_rcv_job_cnt.id) @@ mk_var D.num_peers.id)
      (* send our jobs to everyone *)
      (mk_iter
        (mk_lambda' G.peers.e @@
          mk_send rcv_jobs_nm (mk_var addr) [mk_var D.jobs.id]) @@
        mk_var G.peers.id)
      mk_cunit
  ]

(* rcv the master's address and send him our role *)
let rcv_master_addr_nm = "rcv_master_addr"
let rcv_master_addr =
  let addr = "addr" in
  mk_code_sink' rcv_master_addr_nm [addr, t_addr] [] @@
  mk_block [
    (* assign the master addr *)
    mk_assign D.master_addr.id @@ mk_var addr;
    (* send our job to the master *)
    mk_send ms_rcv_job_nm (mk_var addr) [G.me_var; mk_var D.job.id]
  ]

(**** start point for master role ****)
(* master sends his address to all nodes *)
let ms_send_addr_self_nm = "ms_send_addr_self"
let ms_send_addr_self =
  let addr = hd @@ fst_many G.peers.e in
  mk_code_sink' ms_send_addr_self_nm unit_arg [] @@
  mk_iter (mk_lambda' G.peers.e @@
      mk_send rcv_master_addr_nm (mk_var addr) [G.me_var]) @@
    mk_var G.peers.id


(**** shutdown protocol ****)

(**** protocol plan ****
 * Switches see sentry and tell master
 * Master notifies nodes
 * Nodes check when their stmt_cntrs are empty and tell master
 * If all nodes are done, master calls time and ends, but waits
 * If a node sees more action, it calls undo on master
 *)

let shutdown_trig_nm = "shutdown_trig"
let shutdown_trig =
  mk_code_sink' shutdown_trig_nm unit_arg [] @@
  mk_apply' "haltEngine" mk_cunit

let ms_rcv_node_done_cnt =
  create_ds "ms_rcv_node_done_cnt" (mut t_int) ~init:(mk_cint 0)

let ms_shutdown_nm = "ms_shutdown"
let ms_shutdown =
  mk_code_sink' ms_shutdown_nm unit_arg [] @@
  (* double check that we're done *)
  mk_if (mk_geq (mk_var ms_rcv_node_done_cnt.id) @@ mk_var D.num_nodes.id)
    (* notify everyone to shut down *)
    (mk_iter (mk_lambda' ["addr", t_addr] @@
        mk_send shutdown_trig_nm (mk_var "addr") [mk_cunit]) @@
      mk_var G.peers.id)
    (* else, do nothing -- wait for shutdown count to be reached *)
    mk_cunit

(* master: receive notification that nodes' stmt_cntrs are empty *)
let ms_rcv_node_done_nm = "ms_rcv_node_done"
let ms_rcv_node_done =
  mk_code_sink' ms_rcv_node_done_nm ["done", t_bool] [] @@
  (* if done *)
  mk_if (mk_var "done")
    (mk_block [
      (* increment count *)
      mk_incr ms_rcv_node_done_cnt.id;
      (* if hit number *)
      mk_if (mk_geq (mk_var ms_rcv_node_done_cnt.id) @@ mk_var D.num_nodes.id)
        (mk_block [
          (* update end time *)
          mk_assign D.ms_end_time.id @@ mk_apply' "now_int" mk_cunit;
          (* sleep for a little to make sure *)
          mk_apply' "sleep" @@ mk_cint 5000;
          (* send ourselves a message to shutdown *)
          mk_send ms_shutdown_nm G.me_var [mk_cunit];
        ])
        mk_cunit;
    ]) @@
    (* else, a node is taking back its done state *)
    mk_decr ms_rcv_node_done_cnt.id

let nd_rcv_done_nm = "nd_rcv_done"
let nd_rcv_done =
  mk_code_sink' nd_rcv_done_nm unit_arg [] @@
  mk_block [
    (* set done state *)
    mk_assign D.nd_state.id @@ mk_var D.nd_state_done.id;
    (* check stmt_cntrs for emptiness, in case we won't see it elsewhere *)
    mk_is_empty (mk_var D.nd_stmt_cntrs.id)
      ~y:(mk_send ms_rcv_node_done_nm (mk_var D.master_addr.id) [mk_ctrue])
      ~n:mk_cunit;
  ]

(* code for when nodes insert into stmt_cntrs *)
let nd_insert_stmt_cntr insert_stmt =
  (* if stmt_cntrs are empty before insert *)
  mk_is_empty (mk_var D.nd_stmt_cntrs.id)
    ~y:(mk_block [
          (* do insert *)
          insert_stmt;
          (* if we're in done state *)
          mk_if (mk_eq (mk_var D.nd_state.id) @@ mk_var D.nd_state_done.id)
            (* send undo to master *)
            (mk_send ms_rcv_node_done_nm (mk_var D.master_addr.id) [mk_cfalse]; )
            mk_cunit])
    (* else, just insert *)
    ~n:insert_stmt

(* Code for after deletion from stmt_cntrs *)
let nd_delete_stmt_cntr =
  (* if we're in the done state *)
  mk_if (mk_eq (mk_var D.nd_state.id) @@ mk_var D.nd_state_done.id)
    (* if we're empty after delete *)
    (mk_is_empty (mk_var D.nd_stmt_cntrs.id)
      (* send to master *)
      ~y:(mk_send ms_rcv_node_done_nm (mk_var D.master_addr.id) [mk_ctrue])
      ~n:mk_cunit) @@
    mk_cunit

let ms_rcv_switch_done_cnt =
  create_ds "ms_rcv_switch_done_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_switch_done_nm = "ms_rcv_switch_done"
let ms_rcv_switch_done =
  mk_code_sink' ms_rcv_switch_done_nm unit_arg [] @@
  mk_block [
    (* increment the count *)
    mk_incr ms_rcv_switch_done_cnt.id;
    (* if we've received from all the switches *)
    mk_if (mk_geq (mk_var ms_rcv_switch_done_cnt.id) @@ mk_var D.num_switches.id)
      (mk_iter (mk_lambda' ["addr", t_addr] @@
          mk_send nd_rcv_done_nm (mk_var "addr") [mk_cunit]) @@
        mk_var D.nodes.id)
      mk_cunit;
  ]

(* code for when switches see the sentry *)
let sw_seen_sentry =
  mk_block [
    mk_assign D.sw_state.id @@ mk_var D.sw_state_done.id;
    mk_send ms_rcv_switch_done_nm (mk_var D.master_addr.id) [mk_cunit];
  ]

let global_vars = List.map decl_global [
  ms_rcv_sw_init_ack_cnt;
  ms_rcv_jobs_ack_cnt;
  ms_rcv_job_cnt;
  ms_rcv_node_done_cnt;
  ms_rcv_switch_done_cnt;
]

let triggers c = [
  ms_rcv_sw_init_ack c;
  sw_rcv_init;
  ms_rcv_jobs_ack;
  rcv_jobs;
  ms_rcv_job;
  rcv_master_addr;
  ms_send_addr_self;
  shutdown_trig;
  ms_shutdown;
  ms_rcv_node_done;
  nd_rcv_done;
  ms_rcv_switch_done;
]
