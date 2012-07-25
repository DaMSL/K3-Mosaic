(* Dealing with Sources and Consumption. *)
open K3
open K3Interpreter

(* The types of sources we can have, along with the information to uniquely
 * identify them. Technically they should be hierarchical in terms of
 * capabilities, etc. but let's leave them be flat for now.
 *)
type source_t
    = CSV of string
