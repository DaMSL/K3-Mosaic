(* Utility to generate partition maps using trace files *)
open Util

let error s = prerr_endline s; exit 1

type parameters = {
  mutable dist_file : string;
  mutable num_nodes : int;
  mutable factor : int;
  mutable debug : bool;
  mutable k3new : bool; (* k3new output *)
}

let cmd_line_params = {
  dist_file = "";
  num_nodes = 0;
  factor = 16; (* factor to multiply by to get finer partitions *)
  debug = false;
  k3new = false;
}

(* get all the maps from the distributed k3 file *)
let maps_and_dims file =
  let open K3Util in let open K3.AST in
  let prog = DriverHelpers.parse_k3_file file in
  match fst @@ global_of_program K3Dist.map_ids_id prog with
  | Global(_, _, Some e) ->
      let l = K3Helpers.list_of_k3_container e in
      let l = List.map (fun t -> begin match decompose_tuple t with
        | [x;y;z] -> x,y,z
        | _       -> failwith @@ "wrong format for "^K3Dist.map_ids_id
        end
      ) l
      in
      (* take only the names and dimensions *)
      List.map (fun (_,x,y) -> begin match tag_of_expr x, tag_of_expr y with
        | Const(CString(s)), Const(CInt(i)) -> s, i
        | _ -> failwith @@ "2: wrong format for "^K3Dist.map_ids_id
        end
      ) l
  | _ -> failwith @@ K3Dist.map_ids_id^" not found"

let print_maps_dims maps_dims =
  List.iter (fun (mapname, i) -> Printf.printf "%s:%d\n" mapname i) maps_dims

open K3Helpers

let inner_type = wrap_tlist @@ wrap_ttuple [t_int; t_int]
let pmap_types = wrap_tlist @@ wrap_ttuple [t_string; inner_type]

(* convert map_sizes structure to k3 expressions *)
let k3_of_maps_sizes maps_sizes =
  k3_container_of_list pmap_types @@
    List.map (fun (m, ss) ->
      mk_tuple [mk_cstring m;
        k3_container_of_list inner_type @@
          List.map (fun (dim, size) ->
            mk_tuple [mk_cint dim; mk_cint size]) ss]
  ) maps_sizes

(* add a declaration to the k3 ast *)
let k3_decl_of_k3_expr k3exp =
  K3Typechecker.type_bindings_of_program @@
    [mk_global_val_init "pmap_input" pmap_types k3exp]

let k3new_string_of_maps_sizes maps_sizes =
  let p, env, trig_env, _ = k3_decl_of_k3_expr @@ k3_of_maps_sizes maps_sizes in
  K3NewPrint.string_of_program p (env, trig_env)

let string_of_maps_sizes maps_sizes =
  let module B = Buffer in
  let buf = B.create 100 in
  B.add_string buf "[";
  B.add_string buf @@ String.concat ";" @@
    List.map (fun (mapname, l) ->
      let b = B.create 10 in
      Printf.bprintf b "%s,[" mapname;
      B.add_string b @@ String.concat ";" @@
        List.map (fun (dim, size) -> Printf.sprintf "%d,%d" dim size) l;
      B.add_string b "]";
      B.contents b
    ) maps_sizes;
  B.add_string buf "]";
  B.contents buf

(* naive implementation *)
(* incrementally increase dimensions until we reach the number of nodes in the
 * product of dimension sizes *)
let calc_part num_nodes dims =
  (* create a list of 1s *)
  let initial_list = list_map (fun i -> i, 1) @@ create_range 0 dims in
  let rec loop l prod =
    if prod >= num_nodes then l
    else
      (* keep increasing the dimension sizes until we meet our goal *)
      let prod', l' =
        mapfold (fun acc_prod (dim, size) ->
          if acc_prod >= num_nodes then acc_prod, (dim, size)
          else acc_prod * 2, (dim, size * 2) (* double the size of this dimension *)
        ) prod l
      in
      loop l' prod'
  in
  let l = loop initial_list 1 in
  (* remove partitions of 1 since they aren't really partitioning *)
  List.filter (fun (_, i) -> if i = 1 then false else true) l

(* first try - do an extremely naive implementation of partitioning *)
let calc_part_maps num_nodes maps_dims =
  filter_map (fun (mapname, dim) ->
    if dim <= 0 then None
    else Some(mapname, calc_part num_nodes dim)
  ) maps_dims

let param_specs =
  let c = cmd_line_params in Arg.align
  ["-n", Arg.Int  (fun i -> c.num_nodes <- i), "INTEGER Set number of nodes";
   "-d", Arg.Unit (fun _ -> c.debug <- true), "FLAG Set debug mode";
   "-f", Arg.Int  (fun f -> c.factor <- f), "INTEGER Set multiplicative factor";
   "--k3new", Arg.Unit (fun _ -> c.k3new <- true), "FLAG K3New output"]


let usage_msg = "partmap_tool [opts] k3_dist_file"^
     "\n---- Options ----"

let parse_cmd_line () =
  Arg.parse param_specs (fun f -> cmd_line_params.dist_file <- f) usage_msg

(* --- Start --- *)
let main () =
  parse_cmd_line ();
  if cmd_line_params.dist_file = "" then
    (Arg.usage param_specs usage_msg;
     error "\nNo input files specified")
  else if cmd_line_params.num_nodes <= 0 then
    (Arg.usage param_specs usage_msg;
     error "\nMust have number for nodes")
  else
    let p = cmd_line_params in
    let maps_dims = maps_and_dims p.dist_file in
    (* reduce by one for value *)
    let maps_dims = list_map (fun (a, i) -> a, i-1) maps_dims in
    (* print_maps_dims maps_dims; (* debug *) *)
    (* increase the amount of targets we need to hit, for finer grain
     * partitioning *)
    let maps_sizes = calc_part_maps (p.num_nodes * p.factor) maps_dims in
    let s = if p.k3new then
      k3new_string_of_maps_sizes maps_sizes
    else
      string_of_maps_sizes maps_sizes
    in
    print_endline s

let _ = if not !Sys.interactive then Printexc.print main ()
