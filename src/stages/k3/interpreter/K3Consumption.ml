(* Dealing with Sources and Consumption. *)
open Util
open K3.AST
open K3Util
open K3Values
open K3Values.Value
open K3Typechecker
open K3Streams
open K3Runtime
open K3Helpers

(* TODO: add more descriptive resource errors *)

exception ResourceError of id_t

type channel_impl_t =
  | In  of int ref * in_channel option (* count, channel *)
  | Out of out_channel option
  | InConst of expr_t list ref
  | InRand of int ref

type resource_impl_env_t = (id_t * channel_impl_t) list

(* Evaluation methods *)
let value_of_string t v = match t with
  | TInt    -> VInt(ios v)
  | TDate   -> VInt(int_of_sql_date v)
  | TFloat  -> VFloat(fos v)
  | TBool   -> VBool(bos v)
  | TString -> VString v
  | _       -> invalid_arg "Unknown value"

let r_comma = Str.regexp ","
let r_pipe  = Str.regexp "|"

(* pull (parse/generate) a single value out of a source *)
let pull_source id t res in_chan =
  let tuple_val, signature = match t.typ with
    | TUnit      -> false, [TUnit]
    | TBool      -> false, [TBool]
    | TInt       -> false, [TInt]
    | TDate      -> false, [TDate]
    | TFloat     -> false, [TFloat]
    | TString    -> false, [TString]
    | TTuple ts  -> true,  List.map unwrap_t ts
    | _          -> raise @@ ResourceError(id^": unhandled source type")
  in
  let sig_len = List.length signature in
  (*print_endline ("Pulling from source "^id);*)
  match res, in_chan with
  | Handle(t, File f, CSV), In(cnt, Some chan) ->
    begin try
      (* parse the lines *)
      let next_record = Str.split r_pipe (input_line chan) in
      if List.length next_record <> sig_len then
        raise @@ ResourceError(Printf.sprintf "%s: file %s, line %d, expected %d items but got %d" id f !cnt sig_len @@ List.length next_record);
      let fields = List.map2 value_of_string signature next_record in
      let r = if tuple_val then VTuple fields else List.hd fields in
      incr cnt; Some r
    with
      | Invalid_argument _ -> raise @@
          ResourceError(Printf.sprintf "%s: problem parsing file %s at line %d" id f !cnt)
      | End_of_file        -> None
    end
  | Stream(t, RandomStream _), InRand index ->
      if !index <= 0 then None
      else
        let rec random_val t = match t.typ with
          | TBool      -> VBool(Random.bool ())
          | TInt       -> VInt(Random.int max_int)
          | TDate      -> VInt(Random.int max_int)
          | TFloat     -> VFloat(Random.float max_float)
          | TTuple ts  -> VTuple(List.map random_val ts)
          | _          -> raise @@ ResourceError(id^": unhandled random type")
        in index := !index - 1;
        Some(random_val t)
    (* a constant stream *)
    | Stream(t, ConstStream _), InConst exp_l_ref ->
        begin match !exp_l_ref with
        | []    -> None
        | e::es ->
            exp_l_ref := es;
            let v = try K3Values.value_of_const_expr e
                    with Failure s ->
                      let err = Printf.sprintf "%s: we can't handle an expression of %s"
                        id  (K3Printing.flat_string_of_expr e) in
                      raise @@ ResourceError err
            in Some v
        end
    | _ -> raise @@ ResourceError (id^": not a proper source")

(* Determines resources to be opened and closed between two instructions.
 * All file handles are reset between instructions.
 * Network handles remain open, thereby preserving their connections. *)
(* resource_impl has our implementation ie. open stuff *)
let resource_delta resource_env resource_impl_env d =
  let partition_net l = List.partition (is_net_handle resource_env) l in
  (* ids of network and file resources *)
  let opened_net, opened_files = partition_net @@ List.map fst resource_impl_env in
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
(* @d: finite state machine *)
let initialize_resources resource_env resource_impl_env d =
  let open_channel_impl id =
    match handle_of_resource resource_env id with
      | Some(source, Handle (_, File (filename), _)) ->
        if source then [id, In(ref 0, Some(open_in filename))]
        else [id, Out(Some(open_out filename))]

      | Some(_, Stream(_, ConstStream e)) ->
          [id, InConst(ref @@ K3Helpers.list_of_k3_container e)]

      | Some(_, Stream(_, RandomStream i)) -> [id, InRand(ref i)]

      | Some(_, Handle(_, Network addr, _)) ->
        failwith "network handles not supported in the K3 interpreter"

      | _ -> []
  in
  let close_channel_impl id =
    try match List.assoc id resource_impl_env with
      | In(_, Some c) -> close_in c
      | Out(Some c) -> close_out c
      | _ -> ()
    with Not_found -> ()
  in
  let pass_net, open_net, open_files, close_net, close_files =
    resource_delta resource_env resource_impl_env d
  in
    (* close all resources that should be closed *)
  List.iter close_channel_impl (close_net@close_files);
  let opened_resources = List.flatten @@
    List.map open_channel_impl @@ open_net @ open_files in
  pass_net @ opened_resources

(* Accesses a resource from the given list of resource ids
 * @resource_env: ids and resources from source file
 * @resource_impl_env: open channels
 * returns: failed resource ids, (resource id, value)
 *)
let next_value resource_env resource_impl_env resource_ids =
  let random_element l = List.nth l @@ Random.int @@ List.length l in
  (* find the resource channel *)
  let channel_of_impl id =
    match List.assoc id resource_impl_env with
    | In _ | InConst _  | InRand _ as chan -> chan
    | _ -> failwith "invalid channel for access"
  in
  (* find the resource description *)
  let access_resource id = match handle_of_resource resource_env id with
    | Some (true, ((Handle (t, _, _) | Stream (t, _)) as x)) ->
       begin try pull_source id t x @@ channel_of_impl id
             with Not_found -> None
       end
    | _ -> None
  in
  (* wrapper to find resource that handles failed resources as well *)
  (* any failed resources are sent back to randomize_access *)
  let track_failed_access result_none_f finished id = match access_resource id with
    | None -> result_none_f @@ id::finished
    | x    -> finished, Some(id,x)
  in
  (* pick a random source to read from *)
  let rec randomize_access failed = function
    | []           -> failed, None
    | [id]         -> track_failed_access (fun f -> f, None) failed id
    | resource_ids ->
      let id = random_element resource_ids in
      track_failed_access
        (fun f -> randomize_access f @@ List.filter ((=) id) resource_ids) failed id
  in randomize_access [] resource_ids

(* Run a pattern dispatcher for a single step *)
let rec dispatch schedule_fn d state_opt origin value =
  let module A = ResourceActions in
  let module F = ResourceFSM in
  let next_access state_id =
    pre_entry_of_state (state_id, List.assoc state_id d) in
  (* if we're given a state, use that. Otherwise, go to first state *)
  let state = match state_opt with None -> fst (List.hd d) | Some x -> x in
  try
    let (id, (match_action, next)), (fail_action, fail) = List.assoc state d in
    match id = origin, match_action with
    | true, F.Output (A.Source(A.Dispatch b, _)) ->
      schedule_fn (List.map (fun t -> id, t) b) id [value];
      None, Some next, next_access next

    (* egress pattern dispatching *)
    | true, F.Output (A.Sink(A.Dispatch b)) ->
      failwith "sink dispatching not yet supported"

    | _, F.Terminate -> Some value, None, []
    | _, _ -> Some value, Some fail, next_access fail
  with Not_found -> failwith ("invalid state "^(string_of_int state))

(* type for allowing the dispatcher to maintain state between values *)
type dispatcher_t = {
  mutable ri_env : (string * channel_impl_t) list;
  mutable ri_env_ids : string list;
  mutable next_access_ids : string list;
  mutable state : K3Streams.ResourceFSM.state_id option;
  mutable done_res : id_t list; (* resources that are done *)
  mutable have_res : bool;
  mutable origin : id_t;
  mutable value : value_t;
}

let def_dispatcher =
  { ri_env = [];
    ri_env_ids = [];
    next_access_ids = [];
    state = None;
    done_res = [];
    have_res = true;
    origin = "";
    value = VUnknown;
  }

  (* check if we're still running *)
let is_running e = is_some e.state && e.have_res

(* create a dispatcher_t data structure and init the dispatcher *)
let init_dispatcher resource_env e fsm =
  let ri_env, ri_env_ids =
    let x = initialize_resources resource_env e.ri_env fsm in
    x, fst_many x
  in
  let state, next_access_ids = first some @@ initial_resources_of_dispatcher fsm in
  {def_dispatcher with ri_env; ri_env_ids; next_access_ids; state}

(* take a step in the dispatcher.
 * @e: dispatcher_t
 * return: whether we're done *)
let run_step schedule_fn e res_env (fsm:K3Streams.ResourceFSM.fsm_t) =
  let fin, access = next_value res_env e.ri_env (ListAsSet.diff e.next_access_ids e.done_res) in
  e.done_res <- ListAsSet.union e.done_res fin;
  e.have_res <- ListAsSet.diff e.ri_env_ids e.done_res <> [];
  begin match access with
  | Some(o, Some v) -> e.origin <- o; e.value <- v
  | _ -> ()
  end;
  let rec loop () =
    match dispatch schedule_fn fsm e.state e.origin e.value with
    (* A value failed sending, so try again at next state *)
    | Some v, Some s, [] when v = e.value ->
        e.state <- Some s;
        loop ()
    (* Terminal state *)
    | Some v, None, [] ->
        e.state <- None;
        false

    | _, state', access_ids' ->
        e.state <- state';
        e.next_access_ids <- access_ids';
        true
  in
  if is_running e then loop ()
  else false
