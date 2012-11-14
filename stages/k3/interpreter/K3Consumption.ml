(* Dealing with Sources and Consumption. *)
open K3.AST
open K3Util
open K3Values
open K3Typechecker
open K3Streams
open K3Runtime

exception ResourceError of id_t

type channel_impl_t =
  | In  of in_channel option
  | Out of out_channel option

type resource_impl_env_t = (id_t * channel_impl_t) list

(* Evaluation methods *)
let value_of_string t = match t with
  | TInt -> fun v -> VInt(int_of_string v)
  | TFloat -> fun v -> VFloat(float_of_string v)
  | _ -> fun v -> VString(v)

let pull_source i t s in_chan =
	let signature = 
		match t <| base_of %++ value_of |> (fun () -> raise (ResourceError i)) with
		| TTuple(ts) -> List.map base_of ts
		| _ -> raise (ResourceError i)
	in
  begin 
    print_endline ("Pulling from source "^i);
	  match s with
	  | (File _), CSV -> 
	    (try 
         let next_record = Str.split (Str.regexp ",") (input_line in_chan) in
	       Some (VTuple(List.map2 value_of_string signature next_record))
       with End_of_file -> None)
	  | _ -> raise (ResourceError i)
  end

(* Determines resources to be opened and closed between two instructions.
 * All file handles are reset between instructions.
 * Network handles remain open, thereby preserving their connections. *)
let resource_delta resource_env resource_impl_env d =
  let partition_net l = List.partition (is_net_handle resource_env)  l in 
  let opened_net, opened_files = partition_net (fst (List.split resource_impl_env)) in
  let net_resources, file_resources = partition_net (resources_of_dispatcher d) in
  let pass_net, open_net, close_net =
    List.map
      (fun id -> id, List.assoc id resource_impl_env)
      (ListAsSet.inter opened_net net_resources),
    ListAsSet.diff net_resources opened_net,
    ListAsSet.diff opened_net net_resources 
  in
  let open_files, close_files = file_resources, opened_files in
  pass_net, open_net, open_files, close_net, close_files

(* Initialize all resources used by a multiplexer given existing resources. *)
let initialize_resources resource_env resource_impl_env d =
  let open_channel_impl id =
    try match (handle_of_resource resource_env id) with
      | Some(source, _, File (filename), _) ->
        if source then [id, In(Some(open_in filename))]
        else [id, Out(Some(open_out filename))]
      
      | Some(source, _, Network addr, _) ->
        failwith "network handles not supported in the K3 interpreter"
      
      | _ -> []
    with Not_found -> []
  in
  let close_channel_impl id =
    try match List.assoc id resource_impl_env with
      | In(Some(c)) -> close_in c
      | Out(Some(c)) -> close_out c
      | _ -> ()
    with Not_found -> ()
  in
  let pass_net, open_net, open_files, close_net, close_files =
    resource_delta resource_env resource_impl_env d
  in
	  List.iter close_channel_impl (close_net@close_files);
	  let opened_resources = List.flatten (List.map open_channel_impl (open_net@open_files))
	  in pass_net@opened_resources

(* Accesses a resource from the given list of resource ids, returning a value
 * and a list of failed resources *)
let next_value resource_env resource_impl_env resource_ids =
  let random_element l = List.nth l (Random.int (List.length l)) in
  let channel_of_impl id = match List.assoc id resource_impl_env with
      | In(Some(c)) -> c
      | _ -> failwith "invalid channel for access"
  in
  let access_resource id = match handle_of_resource resource_env id with
    | Some (true, t, ct, cf) -> 
      (try Some(id, (pull_source id t (ct,cf) (channel_of_impl id)))
       with Not_found -> None) 
    | _ -> None
  in
  let track_failed_access result_none_f finished id = match access_resource id with
	  | None -> result_none_f (id::finished) 
	  | x -> finished, x
  in
  let rec randomize_access failed resource_ids = match resource_ids with
	  | [] -> failed, None
	  | [id] -> track_failed_access (fun f -> f,None) failed id
	  | _ -> 
	    let id = random_element resource_ids in
      track_failed_access
        (fun f -> randomize_access f (List.filter ((=) id) resource_ids)) failed id
  in randomize_access [] resource_ids

(* Run a pattern dispatcher for a single step *)
let rec run_dispatcher_step address d state_opt origin value =
  let module A = ResourceActions in
  let module F = ResourceFSM in 
    let next_access state_id = pre_entry_of_state (state_id, (List.assoc state_id d)) in
  let state = match state_opt with None -> (fst (List.hd d)) | Some (x) -> x in
  try
    let (id, (match_action, next)), (fail_action, fail) = List.assoc state d in
    match id = origin, match_action with
      | true, F.Output (A.Source(A.Dispatch(b), _)) ->
        schedule_event (List.map (fun t -> id, t) b) id address [value];  
        None, Some(next), next_access next

      (* TODO: egress pattern dispatching *)        
      | true, F.Output (A.Sink(A.Dispatch b)) -> 
        failwith "sink dispatching not yet supported"

      | _, F.Terminate -> Some(value), None, []
      | _, _ -> Some(value), Some(fail), next_access fail
  with Not_found -> failwith ("invalid state "^(string_of_int state))

(* Pattern-based dispatching. Given a resource specification and implementation
 * environment, and a dispatcher (i.e. an FSM), repeatedly steps through the
 * dispatcher. At each step the dispatcher indicates the resources to be accessed
 * next, and it is the responsibility of this executor to pull the next value 
 * from the list of resources *)
let run_dispatcher address resource_env resource_impl_env d =
  let init_value o v = ref o, ref v in
  let assign_value origin value o v = origin := o; value := v in
  let ri_env, rids =
    let x = initialize_resources resource_env resource_impl_env d 
    in x, List.map fst x
  in
  let finished_resources, resources_remain = ref [], ref true in
  let get_value value_f resources =
    match next_value resource_env resource_impl_env resources with
    | f, Some(o,Some(v)) ->
      finished_resources := ListAsSet.union !finished_resources f;
      resources_remain := ListAsSet.diff rids !finished_resources <> [];
      value_f o v
    | _, _ -> failwith "no value available from resources"
  in
  let state, (origin, value) =
    let s, rids = initial_resources_of_dispatcher d
    in ref(Some(s)), get_value init_value rids
  in
  while !state <> None && !resources_remain do 
    match run_dispatcher_step address d !state !origin !value with
    (* Retry value at next state *)
    | Some(v), Some(s), [] when v = !value -> state := Some(s)

    (* Terminal state *)
    | Some(v), None, [] -> state := None

    | rejected, next_state, next_access ->
        (* TODO: buffer rejected as desired *)
        state := next_state;
        get_value (assign_value origin value)
          (ListAsSet.diff next_access !finished_resources)
  done;
  ri_env
