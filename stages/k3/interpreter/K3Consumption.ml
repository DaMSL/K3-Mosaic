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
    | _ -> raise (SourceError i)

(* Go through a consumable tree, and open all generic file sources. *)
let rec open_file_sources loop =
    match loop with
    | Source(i, t, s) -> (
        match s with
        | FileSource(tag, filename) when tag = "csv" ->
            Source(i, t, (CSV(open_in filename)))
        | _ -> raise (SourceError i)
    )
    | Loop(i, c) -> Loop(i, open_file_sources c)
    | Choice(cs) -> Choice(List.map open_file_sources cs)
    | Sequence(cs) -> Sequence(List.map open_file_sources cs)
    | Optional(c) -> Optional(open_file_sources c)
    | Repeat(c, s) -> Repeat(open_file_sources c, s)

(* Given a source environment, construct a function which can be polled for values. *)
let rec pull loop =
    match loop with

    | Source(i, t, s) -> pull_source i t s, None
    | Loop(i, c) -> pull c

    (* Pulling on a choice will pull each source in turn, and return the first
     * successful pull.
     *)
    | Choice([]) -> None, None
    | Choice(h :: t) -> (
        match pull h with
        | Some v, _ -> Some v, None
        | None, _ -> pull (Choice(t))
    )

    (* Pulling on a sequence will pull each source in turn, but will skip the
     * remaining sources if one fails.
     *)
    | Sequence([]) -> None, None
    | Sequence(h :: t) -> (
        match pull h with

        | Some v, _ -> Some v, Some (Sequence t)
        | None, _ -> None, None
    )
    | Optional(c) -> fst (pull c), None
    | Repeat(c, _) -> (
        match pull c with
        | Some v, _ -> Some v, Some c
        | None, _ -> None, None
    )
