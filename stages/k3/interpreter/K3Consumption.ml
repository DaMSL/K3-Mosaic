(* Dealing with Sources and Consumption. *)
open K3
open K3Util
open K3Values
open K3Typechecker
open K3Streams

exception StreamError of id_t

(* Evaluation methods *)
let value_of_string t = match t with
  | TInt -> fun v -> VInt(int_of_string v)
  | TFloat -> fun v -> VFloat(float_of_string v)
  | _ -> fun v -> VString(v)

let pull_source i t s in_chan =
	let signature = 
		match t <| base_of %++ value_of |> (fun () -> raise (StreamError i)) with
		| TTuple(ts) -> List.map base_of ts
		| _ -> raise (StreamError i)
	in
  begin 
    print_endline ("Pulling from source "^i);
	  match s with
	  | (File _), CSV -> 
	    (try 
         let next_record = Str.split (Str.regexp ",") (input_line in_chan) in
	       Some (VTuple(List.map2 value_of_string signature next_record))
       with End_of_file -> None)
	  | _ -> raise (StreamError i)
  end


(* Go through all actions of an FSM, and initialize its channel implementations. *)
let initialize fsm =
  let init_file_impl filename i = match i with
    | In(None) -> In(Some(open_in filename))
    | Out(None) -> Out(Some(open_out filename))
    | _ -> i
  in
  let rec init_aux senv fsm = 
	  List.fold_left (fun (senv, nfsm) (i, (a,n,p)) ->
	    match a with
	      | AEndpoint(id, t, s, impl) ->
	        let nsenv, na = match s with
	          | File (filename), _ ->
	            (try senv, (List.assoc id senv)
	             with Not_found ->
	               let ne = AEndpoint(id, t, s, init_file_impl filename impl)
	               in ((id,ne)::senv), ne)
	
	          | _, _ -> raise (StreamError id) 
	        in nsenv, (nfsm@[i, (na,n,p)])

	      | _ -> senv, (nfsm@[i, (a,n,p)])
	    ) (senv, []) fsm
  in snd (init_aux [] fsm)


(* Run an FSM for a single event *)
let rec run fsm_env fsm state_opt =
  let run_endpoint id t s impl = match impl with
    | In(Some(c)) -> pull_source id t s c
    | _ -> raise (StreamError id)
  in
  let state = match state_opt with None -> (fst (List.hd fsm)) | Some (x) -> x in
  try
    let action, next, fail = List.assoc state fsm in
    match action with
      | AEndpoint (id, t, s, impl) ->
        let v = run_endpoint id t s impl in
        v, (if v = None then Some fail else Some next)

      | ADerived (id) -> 
        (try run fsm_env (List.assoc id fsm_env) None
         with Not_found -> failwith ("invalid derived state "^id)) 

      | AEOF -> None, None
  with Not_found -> failwith ("invalid state "^(string_of_int state))
