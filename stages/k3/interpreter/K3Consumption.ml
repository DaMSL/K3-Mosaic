(* Dealing with Sources and Consumption. *)
open K3
open K3Values
open K3Interpreter
open K3Typechecker

exception SourceError of id_t

let convert_type t =
    match t with
    | TInt -> fun v -> VInt(int_of_string v)
    | TFloat -> fun v -> VFloat(float_of_string v)
    | _ -> fun v -> VString(v)

let pull_source i t s =
    let signature = 
        match t <| base_of %++ value_of |> (fun () -> raise (SourceError i)) with
        | TTuple(ts) ->
            List.map base_of ts
        | _ -> raise (SourceError i)
    in
    match s with
    | CSV(channel) ->
        let next_record = Str.split (Str.regexp ",") (input_line channel) in
        Some (VTuple(List.map2 convert_type signature next_record))

(* Given a source environment, construct a function which can be polled for values. *)
let rec pull loop =
    match loop with

    | Source(i, t, s) -> pull_source i t s, None

    (* Pulling on a choice will pull each source in turn, and return the first
     * successful pull.
     *)
    | Choice(h :: t) -> (
        match pull h with
        | Some v, _ -> Some v, None
        | None, _ -> pull (Choice(t))
    )

    (* Pulling on a sequence will pull each source in turn, but will skip the
     * remaining sources if one fails.
     *)
    | Sequence(h :: t) -> (
        match pull h with

        | Some v, _ -> Some v, Some (Sequence t)
        | None, _ -> None, None
    )
    | Repeat(c, _) -> (
        match pull c with
        | Some v, _ -> Some v, Some c
        | None, _ -> None, None
    )
