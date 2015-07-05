(* module to generate expected sections for testing distributed code *)
open Util
open K3Helpers
open K3Dist
module P = ProgInfo
module D = K3Dist

let map_latest_val_code mt p map_id =
  let c = {D.default_config with p} in
  let map_ids_types_vid = P.map_ids_types_with_v_for p map_id in
  let map_ids_types = P.map_ids_types_for p map_id in
  let map_ids = fst_many map_ids_types in
  let map_types = snd_many map_ids_types in
  let set_type = wrap_t_calc' map_types in
  let map_ids_types_no_val = P.map_ids_types_no_val_for p map_id in
  let map_ids_no_val = fst_many map_ids_types_no_val in
  let max_vid, vid, acc, project = "max_vid", "vid", "acc", "project" in
  let inner_assoc =
    (* find the max vid *)
    mk_lambda2'
      [max_vid, t_vid; acc, set_type]
      map_ids_types_vid @@
      mk_if
        (* if the vid is greater than the max, we use only the new tuple *)
        (mk_gt (mk_var vid) @@ mk_var max_vid)
        (mk_tuple [mk_var vid; mk_singleton set_type @@ ids_to_vars map_ids]) @@
        (* else, if the vid is =, add to the set *)
        mk_if
          (mk_eq (mk_var vid) @@ mk_var max_vid)
          (mk_tuple [mk_var vid;
                      mk_combine
                      (mk_singleton set_type @@ ids_to_vars map_ids) @@
                      mk_var acc]) @@
          (* else, keep the same accumulators *)
          mk_tuple [mk_var max_vid; mk_var acc]
  in
  (* if the map is a singleton, we need to use just a fold. Otherwise, we need a
   * groupby. In either case, we need to project out what we don't need *)
  let code =
    let mapn = P.map_name_of p map_id in
    let mapn_deref = "map_d" in
    mk_bind (mk_var mapn) mapn_deref @@
    if null map_ids_no_val then
      mk_let
        ["_"; project]
        (mk_agg
          inner_assoc
          (mk_tuple [mk_var D.g_min_vid.id; mk_empty set_type]) @@
          D.calc_of_map_t c ~keep_vid:true map_id @@
            mk_var mapn_deref) @@
        mk_var project
    else
      mk_flatten @@ mk_map
        (mk_lambda'
          ["x", wrap_ttuple [wrap_ttuple (snd_many map_ids_types_no_val) ; wrap_ttuple [t_vid; set_type]]] @@
          mk_subscript 2 @@ mk_subscript 2 @@ mk_var "x") @@
        mk_gbagg
          (* group by the keys excluding the vid *)
          (mk_lambda' map_ids_types_vid @@
            mk_tuple @@ ids_to_vars map_ids_no_val)
          (* find the highest vid *)
          inner_assoc
          (mk_tuple [mk_var D.g_min_vid.id; mk_empty set_type]) @@
          D.calc_of_map_t c ~keep_vid:true map_id @@
            mk_var mapn_deref
  in code, mk_empty set_type

(* code for every map *)
let expected_code_all_maps mt p =
  let mapl = P.get_map_list p in
  list_map (fun m -> P.map_name_of p m, map_latest_val_code mt p m) mapl
