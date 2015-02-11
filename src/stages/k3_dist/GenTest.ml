(* module to generate expected sections for testing distributed code *)
open Util
open K3Helpers
open K3Dist
module P = ProgInfo
module D = K3Dist

let map_latest_val_code p map_id =
  let map_ids_types_vid = P.map_ids_types_with_v_for p map_id in
  let map_ids_types = P.map_ids_types_for p map_id in
  let map_ids = fst_many map_ids_types in
  let map_types = snd_many map_ids_types in
  let set_type = wrap_t_of_map @@ wrap_ttuple map_types in
  let map_ids_types_no_val = P.map_ids_types_no_val_for p map_id in
  let map_ids_no_val = fst_many map_ids_types_no_val in
  let inner_assoc =
    (* find the max vid *)
    mk_assoc_lambda
      (wrap_args ["_max_vid_", t_vid; "_acc_", set_type])
      (wrap_args map_ids_types_vid) @@
      mk_if
        (* if the vid is greater than the max, we use only the new tuple *)
        (mk_gt (mk_var "vid") @@ mk_var "_max_vid_")
        (mk_tuple [mk_var "vid"; mk_singleton set_type @@ ids_to_vars map_ids]) @@
        (* else, if the vid is =, add to the set *)
        mk_if
          (mk_eq (mk_var "vid") @@ mk_var "_max_vid_")
          (mk_tuple [mk_var "vid";
                      mk_combine
                      (mk_singleton set_type @@ ids_to_vars map_ids) @@
                      mk_var "_acc_"]) @@
          (* else, keep the same accumulators *)
          mk_tuple [mk_var "_max_vid_"; mk_var "_acc_"]
  in
  (* if the map is a singleton, we need to use just a fold. Otherwise, we need a
   * groupby. In either case, we need to project out what we don't need *)
  let code =
    let mapn = P.map_name_of p map_id in
    let mapn_deref = mapn^"_deref" in
    mk_bind (mk_var mapn) mapn_deref @@
    if null map_ids_no_val then
      mk_let
        ["_"; "_project_"]
        (mk_agg
          inner_assoc
          (mk_tuple [mk_var D.g_min_vid.id; mk_empty set_type]) @@
          mk_var mapn_deref) @@
        mk_var "_project_"
    else
      mk_flatten @@ mk_map
        (mk_assoc_lambda
          (wrap_args ["_", t_unit])
          (wrap_args ["_", t_unit; "_project_", set_type]) @@
          mk_var "_project_") @@
        mk_gbagg
          (* group by the keys excluding the vid *)
          (mk_lambda (wrap_args map_ids_types_vid) @@
            mk_tuple @@ ids_to_vars map_ids_no_val)
          (* find the highest vid *)
          inner_assoc
          (mk_tuple [mk_var D.g_min_vid.id; mk_empty set_type]) @@
          mk_var mapn_deref
  in code, mk_empty set_type

(* code for every map *)
let expected_code_all_maps p =
  let mapl = P.get_map_list p in
  list_map (fun m -> P.map_name_of p m, map_latest_val_code p m) mapl
