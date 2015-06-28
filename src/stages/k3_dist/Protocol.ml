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
    mk_if (mk_eq (mk_var ms_rcv_sw_init_ack_cnt.id) @@ mk_var D.num_switches.id)
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
    mk_assign D.sw_init.id mk_ctrue;
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
    mk_if (mk_eq (mk_var ms_rcv_jobs_ack_cnt.id) @@ mk_var D.num_peers.id)
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
    mk_if (mk_eq (mk_var ms_rcv_job_cnt.id) @@ mk_var D.num_peers.id)
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

let nd_sent_done = create_ds "nd_sent_done" (mut t_bool) ~init:mk_cfalse
let sw_sent_done = create_ds "sw_sent_done" (mut t_bool) ~init:mk_cfalse

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
  mk_if (mk_eq (mk_var ms_rcv_node_done_cnt.id) @@ mk_var D.num_nodes.id)
    (* notify everyone to shut down *)
    (mk_iter (mk_lambda' ["addr", t_addr] @@
        mk_send shutdown_trig_nm (mk_var "addr") [mk_cunit]) @@
      mk_var G.peers.id)
    (* else, do nothing -- wait for shutdown count to be reached *)
    mk_cunit

(* master: receive notification that nodes are done with their work (stmt_cntrs empty) *)
let ms_rcv_node_done_nm = "ms_rcv_node_done"
let ms_rcv_node_done =
  mk_code_sink' ms_rcv_node_done_nm ["done", t_bool] [] @@
  (* if done *)
  mk_if (mk_var "done")
    (mk_block [
      (* increment count *)
      mk_incr ms_rcv_node_done_cnt.id;
      (* if hit number *)
      mk_if (mk_eq (mk_var ms_rcv_node_done_cnt.id) @@ mk_var D.num_nodes.id)
        (mk_block [
          (* update end time *)
          mk_assign D.ms_end_time.id @@ mk_apply' "now_int" mk_cunit;
          (* send ourselves a message to shutdown *)
          mk_send ms_shutdown_nm G.me_var [mk_cunit];
        ])
        mk_cunit;
    ]) @@
    (* else, a node is taking back its done state *)
    mk_decr ms_rcv_node_done_cnt.id

(* whether a node should notify the master that it's done *)
let nd_notify_ms_done =
  (* check stmt_cntrs for emptiness *)
  mk_if
    (mk_and
      (* we haven't sent yet *)
      (mk_not @@ mk_var nd_sent_done.id) @@
      (* no stmt_cntrs *)
      mk_eq (mk_size_slow D.nd_stmt_cntrs) @@ mk_cint 0)
    (mk_block [
      (* notify master *)
      mk_send ms_rcv_node_done_nm (mk_var D.master_addr.id) [mk_ctrue];
      (* mark as done *)
      mk_assign nd_sent_done.id mk_ctrue]) @@
    mk_cunit

let nd_rcv_done_nm = "nd_rcv_done"
let nd_rcv_done =
  mk_code_sink' nd_rcv_done_nm unit_arg [] @@
  mk_block [
    (* set done state *)
    mk_assign D.nd_rcvd_sys_done.id mk_ctrue;
    nd_notify_ms_done
  ]

(* Code for after deletion from stmt_cntrs *)
let nd_delete_stmt_cntr =
  (* if we're received system done and we haven't already sent done *)
  mk_if (mk_var D.nd_rcvd_sys_done.id) nd_notify_ms_done mk_cunit

let ms_rcv_switch_done_cnt =
  create_ds "ms_rcv_switch_done_cnt" (mut t_int) ~init:(mk_cint 0)
let ms_rcv_switch_done_nm = "ms_rcv_switch_done"
let ms_rcv_switch_done =
  mk_code_sink' ms_rcv_switch_done_nm unit_arg [] @@
  mk_block [
    (* increment the count *)
    mk_incr ms_rcv_switch_done_cnt.id;
    (* if we've received from all the switches *)
    mk_if (mk_eq (mk_var ms_rcv_switch_done_cnt.id) @@ mk_var D.num_switches.id)
      (mk_iter (mk_lambda' ["addr", t_addr] @@
          mk_send nd_rcv_done_nm (mk_var "addr") [mk_cunit]) @@
        mk_var D.nodes.id)
      mk_cunit;
  ]

(* check that the switch is done with its work *)
let sw_check_done =
  mk_if
    (mk_and
      (mk_not @@ mk_var sw_sent_done.id) @@
      mk_and
        (mk_eq (mk_size_slow D.sw_trig_buf_idx) @@ mk_cint 0) @@
        mk_and
          (mk_eq (mk_var GC.sw_num_ack.id) @@ mk_var GC.sw_num_sent.id) @@
          mk_eq (mk_var D.sw_seen_sentry.id) mk_ctrue)
    (mk_block [
      (* send *)
      mk_send ms_rcv_switch_done_nm (mk_var D.master_addr.id) [mk_cunit];
      (* mark as sent *)
      mk_assign sw_sent_done.id mk_ctrue])
    mk_cunit

(* code for when switches see the sentry *)
let sw_seen_sentry =
  mk_block [
    mk_assign D.sw_seen_sentry.id mk_ctrue;
    sw_check_done
  ]

let global_vars = List.map decl_global [
  nd_sent_done;
  sw_sent_done;
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
