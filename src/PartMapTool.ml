(* Utility to generate partition maps using trace files *)
open Util

let error s = prerr_endline s; exit 1

type parameters = {
  mutable trace_file : string;
  mutable num_nodes : int;
  mutable factor : int;
  mutable debug : bool;
}

let cmd_line_params = {
  trace_file = "";
  num_nodes = 0;
  factor = 16; (* factor to multiply by to get finer partitions *)
  debug = false;
}

(* get all the maps in the trace file *)
let maps_and_dims file =
  let _, maps, _ = FromTrace.parse_trace file in
  FromTrace.map_names_and_dims maps

let print_maps_dims maps_dims =
  List.iter (fun (mapname, i) -> Printf.printf "%s:%d\n" mapname i) maps_dims

let string_of_maps_sizes maps_sizes =
  let module B = Buffer in
  let buf = B.create 100 in
  B.add_string buf "[";
  B.add_string buf @: String.concat ";" @:
    List.map (fun (mapname, l) ->
      let b = B.create 10 in
      Printf.bprintf b "%s,[" mapname;
      B.add_string b @: String.concat ";" @:
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
  let initial_list = list_map (fun i -> i, 1) @: create_range 0 dims in
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

let param_specs = Arg.align
  ["-n", Arg.Int  (fun i -> cmd_line_params.num_nodes <- i), "Set number of nodes";
   "-d", Arg.Bool (fun b -> cmd_line_params.debug <- b), "Set debug mode";
   "-f", Arg.Int  (fun f -> cmd_line_params.factor <- f), "Set multiplicative factor"]


let usage_msg = "partmap_tool [opts] trace_file"^
     "\n---- Options ----"

let parse_cmd_line () =
  Arg.parse param_specs (fun f -> cmd_line_params.trace_file <- f) usage_msg

(* --- Start --- *)
let main () =
  parse_cmd_line ();
  if cmd_line_params.trace_file = "" then
    (Arg.usage param_specs usage_msg;
     error "\nNo input files specified")
  else if cmd_line_params.num_nodes <= 0 then
    (Arg.usage param_specs usage_msg;
     error "\nMust have number for nodes") 
  else
    let p = cmd_line_params in
    let maps_dims = maps_and_dims p.trace_file in
    (* reduce by one for value *)
    let maps_dims = list_map (fun (a, i) -> a, i-1) maps_dims in
    (* print_maps_dims maps_dims; (* debug *) *)
    (* increase the amount of targets we need to hit, for finer grain
     * partitioning *)
    let maps_sizes = calc_part_maps (p.num_nodes * p.factor) maps_dims in
    let s = string_of_maps_sizes maps_sizes in
    print_endline s

let _ = if not !Sys.interactive then Printexc.print main ()
