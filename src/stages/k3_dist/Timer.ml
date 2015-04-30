(* Timer peer code *)
(* The simplest way to implement timers *)
(* Works in resolution of 1 sec *)
open K3.AST
open K3Dist
open K3Helpers
open Util

module D = K3Dist
module G = K3Global
module Std = K3StdLib

(* list of trigs that can be notified *)
let timer_trigs c =
  (if c.enable_gc then [D.ms_send_gc_req_nm] else [])

(* obtain the timer id of a trigger in timer_trigs *)
let num_of_trig c trig =
  List.assoc trig @@ insert_index_snd @@ timer_trigs c

(* We really want to use an ordered map, but we currently don't have that *)
let tm_timer_list =
  let e = ["time", t_int; "trig_id", t_int; "addr", t_addr] in
  create_ds "tm_timer_list" (mut @@ wrap_tlist' @@ snd_many e) ~e

(* Check the time and dispatch any triggers *)
let tm_check_time_trig_nm = "tm_check_time"
let tm_check_time_trig c =
  (* dispatch code for triggers *)
  let dispatch_code =
    List.fold_right (fun (id, trig) acc ->
      mk_if (mk_eq (mk_var "trig_id") @@ mk_cint id)
        (mk_send trig (mk_var "addr") [mk_cunit])
        acc)
      (insert_index_fst @@ timer_trigs c)
      mk_cunit
  in
  mk_code_sink' tm_check_time_trig_nm unit_arg [] @@
    mk_case_ns (mk_peek' tm_timer_list.id) "timer"
      mk_cunit @@ (* if empty, stop *)
      mk_if
        (* if we've passed the timer time *)
        (mk_geq (mk_apply' "now_int" mk_cunit) @@ mk_fst @@ mk_var "timer")
        (mk_block [
          (* delete this entry *)
          mk_delete tm_timer_list.id [mk_var "timer"];
          (* activate the lambda and send ourselves a message *)
          mk_let (fst_many tm_timer_list.e) (mk_var "timer") dispatch_code;
          (* check for another expired timer *)
          mk_send tm_check_time_trig_nm G.me_var [mk_cunit];
        ]) @@
        (* else, wake ourselves in 1 second *)
        mk_block [
          mk_send tm_check_time_trig_nm G.me_var [mk_cunit];
          mk_apply' "sleep" @@ mk_cint 1000;
        ]

(* trig to insert a timer into the timer list. Send time from now *)
let tm_insert_timer_trig_nm = "tm_insert_timer"
let tm_insert_timer_trig =
  mk_code_sink' tm_insert_timer_trig_nm tm_timer_list.e [] @@
  mk_block [
    (* insert the new entry *)
    mk_insert tm_timer_list.id @@
      modify_e tm_timer_list.e
       ["time", mk_add (mk_var "time") @@ mk_apply' "now_int" mk_cunit];
    (* sort the list *)
    mk_assign tm_timer_list.id @@
      (* NOTE: if we had an ordered map, we wouldn't need this slow sort *)
      mk_sort
        (mk_assoc_lambda' (id_t_add "1" tm_timer_list.e) (id_t_add "2" tm_timer_list.e) @@
          mk_lt (mk_var "time1") @@ mk_var "time2") @@
        mk_var tm_timer_list.id;
    (* start to check the time *)
    mk_send tm_check_time_trig_nm G.me_var [mk_cunit];
  ]

let global_vars = [
  decl_global tm_timer_list;
]

let triggers c = [
  tm_insert_timer_trig;
  tm_check_time_trig c;
]

