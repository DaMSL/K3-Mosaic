(* Dealing with Sources and Consumption. *)
open K3
open K3Values
open K3Interpreter

(* The types of sources we can have, along with the information to uniquely
 * identify them. Technically they should be hierarchical in terms of
 * capabilities, etc. but let's leave them be flat for now.
 *)
type source_t
    = CSV of string

type senv_t = (id_t * source_t) list

(* Given a source environment, construct a function which can be polled for values. *)
let construct_machine senv =
    let rec pull loop =
        match loop with

        | Source(i, t) -> pull_source (List.assoc i senv), None

        (* Pulling on a choice will pull each source in turn, and return the first
         * successful pull.
         *)
        | Choice(first, second) -> (
            match pull senv first with
            | Some v, _ -> Some v, None
            | None, _ -> pull (Choice(t))
        )

        (* Pulling on a sequence will pull each source in turn, but will skip the
         * remaining sources if one fails.
         *)
        | Sequence(h :: t) -> (
            match pull senv h with

            | Some v, None -> Some v, Some (Sequence t)

            | Some v, Some n -> Some v, Some (Sequence [n; t])
            | None, _ -> None, None
        )
        | Repeat(c, _) -> (
            match pull senv c with
            | Some v -> Some v, c
            | None -> None, None
        )
    in pull
