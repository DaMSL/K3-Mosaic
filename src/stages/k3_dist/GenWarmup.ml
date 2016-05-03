open Util
open K3.AST
open K3Helpers
open K3Dist

module D = K3Dist
module C = GenCommon
module G = K3Global
module U = K3Util
module T = K3Typechecker
module R = K3Route
module P = ProgInfo
module K3R = K3Route
module K3S = K3Shuffle
module K3N = K3NewPrint
module Proto = Protocol

(*** warmup ***)

(* map paths for warmup *)
let sw_warmup_paths c =
  List.map (fun s -> create_ds s t_string) @@
  List.map (fun m -> P.map_name_of c.p m ^ "_warmup_path") @@
  P.get_map_list c.p

let sw_warmup_block_size = create_ds "sw_warmup_block_size" @@ t_int

(* receive warmup push *)
let nd_rcv_warmup_push c =
  let fn_nm = D.nd_rcv_warmup_push_nm in
  let m_tags =
    List.filter (fun t -> str_suffix "_warmup" t.tag) c.poly_tags in
  mk_global_fn fn_nm
    poly_args
    [t_int; t_int] @@
  mk_poly_skip_block fn_nm [
    mk_poly_iter'
      (mk_lambda3' p_tag p_idx p_off @@
        List.fold_left (fun acc_code ti ->
          let m_nm = str_drop_end (String.length "_warmup") ti.tag in
          let m_i_ts = P.map_ids_types_for c.p @@ P.map_id_of_name c.p m_nm in
          let k, v = list_split (-1) @@ ids_to_vars @@ fst_many m_i_ts in
          mk_if_eq (mk_var "tag") (mk_cint ti.itag)
            (mk_block [
                mk_poly_at_with' ti.tag @@
                mk_lambda' m_i_ts @@
                  mk_bind (mk_var m_nm) "d" @@
                mk_insert "d" [mk_cint 0; mk_tuple k; mk_tuple v] ;

                mk_poly_skip' ti.tag
            ])
            acc_code)
        (mk_error "unrecognized map name")
        m_tags)
  ]


let sw_warmup_push_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "warmup_push_bitmap" @@ wrap_tvector t_bool

(* loop trigger per map *)
let sw_warmup_loops c =
  let m_nm_ts = List.map (fun m -> m, P.map_name_of c.p m, P.map_ids_types_for c.p m) @@
    P.get_maps_with_keys c.p in
  List.map (fun (m, m_nm, id_ts) ->
    let fn_nm = m_nm^"_warmup_loop" in
    let k, v = list_split (-1) @@ fst_many @@ id_ts in
    mk_code_sink' fn_nm unit_arg [] @@
    mk_let ["batch_id"] (mk_cint 0) @@
    mk_block [
      C.clear_poly_queues c;
      mk_set_all sw_warmup_push_bitmap.id [mk_cfalse];
      mk_iter (mk_lambda' unknown_arg @@
        mk_if (mk_apply' "hasRead" [G.me_var; mk_cstring m_nm])
          (mk_let ["tuple"] (mk_apply' ("doRead"^m_nm) [G.me_var; mk_cstring m_nm]) @@
           mk_let k (mk_fst @@ mk_var "tuple") @@
           mk_let v (mk_snd @@ mk_var "tuple") @@
           R.route_lookup c m (mk_cint m :: (List.map mk_tup_just @@ ids_to_vars k)) (mk_cint 0) @@
             mk_iter_bitmap'
               (mk_block [
                   (* if we need to, send the warmup push header *)
                   mk_if (mk_not @@ mk_at' sw_warmup_push_bitmap.id @@ mk_var "ip")
                     (mk_block [
                         C.buffer_for_send nd_rcv_warmup_push_nm "ip" [];
                         mk_insert_at sw_warmup_push_bitmap.id (mk_var "ip") [mk_ctrue]
                     ])
                     mk_cunit;
                  C.buffer_for_send (m_nm^"_warmup") "ip" (ids_to_vars @@ k @ v)
               ])
               R.route_bitmap.id)
          mk_cunit
      ) @@
      mk_range TList (mk_cint 0) (mk_cint 1) @@ mk_var sw_warmup_block_size.id;
      C.send_poly_queues;
      mk_if (mk_apply' "hasRead" [G.me_var; mk_cstring m_nm])
        (mk_send_me fn_nm) @@
        (* if no more values, send to barrier *)
        mk_send_master Proto.ms_post_warmup_barrier_nm
    ])
  m_nm_ts

(* code for the switch to send warmup pushes *)
let sw_warmup c =
  let m_nm_ts = List.map (fun m -> m, P.map_name_of c.p m, P.map_types_for c.p m) @@
    P.get_maps_with_keys c.p in
  mk_code_sink' Proto.sw_warmup_nm [] [] @@
  mk_block @@
    List.flatten @@ List.map (fun (m, m_nm, ts) ->
        [mk_apply' "openFile" [G.me_var; mk_cstring m_nm; mk_var @@ m_nm^"_warmup_path";
                              mk_cstring "k3"; mk_cfalse; mk_cstring "r"];
         mk_send_me @@ m_nm^"_warmup_loop"])
      m_nm_ts

let global_vars c = List.map decl_global @@
  sw_warmup_paths c @
  [sw_warmup_block_size]
