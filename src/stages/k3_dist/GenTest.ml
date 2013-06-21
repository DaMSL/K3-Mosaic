(* module to generate expected sections for testing distributed code *)
open Util
open K3Helpers
open K3Dist
module P = ProgInfo

let map_latest_val_code p map_id =
  let map_ids_types = P.map_ids_types_with_v_for p map_id in
  let map_ids_types_no_vid = P.map_ids_types_for p map_id in
  let map_ids_no_vid = fst_many map_ids_types_no_vid in
  let map_types_no_vid = snd_many map_ids_types_no_vid in
  let set_type = wrap_tset @: wrap_ttuple map_types_no_vid in
  let map_ids_types_no_v = P.map_ids_types_no_val_for p map_id in
  let map_ids_no_v = fst_many map_ids_types_no_v in
  (* project only the stuff we need *)
  mk_map
    (mk_assoc_lambda 
      (wrap_args ["_", t_unit])
      (wrap_args ["_", t_unit; "_project_", wrap_ttuple map_types_no_vid]) @:
      mk_var "_project_") @:
    mk_gbagg 
      (* group by the keys excluding the vid *)
      (mk_lambda (wrap_args map_ids_types) @:
        mk_tuple @: ids_to_vars map_ids_no_v)
      (* find the highest vid *)
      (mk_assoc_lambda
        (wrap_args ["_max_vid_", t_vid; "_acc_", wrap_ttuple map_types_no_vid])
        (wrap_args map_ids_types) @:
        (mk_if
          (* if the vid is greater than the max, we use only the new tuple *)
          (v_gt (mk_var "vid") (mk_var "_max_vid_"))
          (mk_tuple [mk_var "vid"; 
                    mk_singleton set_type @:
                      mk_tuple @: ids_to_vars map_ids_no_vid])
          (* else, if the vid is =, add to the set *)
          (mk_if
            (v_eq (mk_var "vid") (mk_var "_max_vid_"))
            (mk_tuple [mk_var "vid";
                        mk_combine
                        (mk_singleton set_type @:
                          mk_tuple @: ids_to_vars map_ids_no_vid)
                        (mk_var "_acc_")])
            (* else, keep the same accumulators *)
            (mk_tuple [mk_var "_max_vid_"; mk_var "_acc_"]))))
      (mk_tuple [min_vid_k3; mk_empty set_type]) @:
      mk_var @: P.map_name_of p map_id

(* code for every map *)
let expected_code_all_maps p =
  let mapl = P.get_map_list p in
  list_map (map_latest_val_code p) mapl
  
      

        

      

  



