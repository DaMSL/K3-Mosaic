(* An extended AST for K3's C++ backend *)
open Printing
open K3

(* Using mutually recursive modules allows us to extend the imperative
 * AST and simultaneously use its types as part of our extension *) 
module rec CPPImpl : Imperative.Export with module AST = Imperative.AST(CPPTarget)
 = struct module AST = Imperative.AST(CPPTarget) end 

and CPPTarget : Imperative.TargetLanguage = struct

open CPPImpl.AST
module U = ImperativeUtil.Util(CPPTarget)
open U
  
type ext_type_t =
  | TIterator of type_t
  | TPair     of type_t * type_t

type bmi_extractor_t =
  | BMIMember of type_t * id_t
  | BMIComposite of bmi_extractor_t list

type ext_type_decl_t =
    (* Type id * list of index tag type ids, and extractors *)
  | TBoostMultiIndex of id_t * (id_t * bmi_extractor_t) list 

type ext_collection_fn_t =
  | GetIndex  of id_t     (* index tag type id *)
  | BeginIterator
  | EndIterator
  | Copy
  | Clear

type ext_fn_t =
  | IteratorElement
  | IteratorIncrement
  | IteratorDecrement
  | PairFirst
  | PairSecond

type 'a ext_cmd_t = 
    (* Three place for loop, with init, test and advance expressions.
     * The loop body is an imperative child of this command. *) 
  | For of 'a expr_t option * 'a expr_t option * 'a expr_t option

    (* A do-while loop with a post-test predicate. The loop body is a child
     * of this imperative command. *)
  | DoWhile of 'a expr_t

let print_ext_type t =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match t with
  | TIterator t -> ptag "TIterator" [lazy (print_type t)]
  | TPair (l,r) -> ptag "TPair" [lazy (print_type l); lazy (print_type r)]

let print_ext_type_decl d =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  match d with
  | TBoostMultiIndex (id, indexes) ->
    (* TODO: indexes *)
    ptag "TBoostMultiIndex" [lazy (ps id)]

let print_ext_collection_fn f =
  let ptag ?(cut=CutHint) ?(ch=[]) t = pretty_tag_str CutHint "" t ch in
  match f with
  | GetIndex tag_id_t -> ptag "GetIndex" ~ch:[lazy (ps tag_id_t)] 
  | BeginIterator -> ptag "BeginIterator"
  | EndIterator -> ptag "EndIterator"
  | Copy -> ptag "Copy"
  | Clear -> ptag "Clear"

let print_ext_fn f = 
  let ptag ?(cut=CutHint) t = pretty_tag_str CutHint "" t [] in
  match f with
  | IteratorElement -> ptag "IteratorElement"
  | IteratorIncrement -> ptag "IteratorIncrement"
  | IteratorDecrement -> ptag "IteratorDecrement"
  | PairFirst -> ptag "PairFirst"
  | PairSecond -> ptag "PairSecond"

let print_ext_cmd string_of_meta c =
  let ptag ?(cut=CutHint) t ch = pretty_tag_str CutHint "" t ch in
  let lazy_expr_opt e_opt = lazy (match e_opt with 
    | None -> ptag "None" []
    | Some e -> print_expr string_of_meta e)
  in
  match c with
  | For (init_e_opt, test_e_opt, adv_e_opt) ->
      ptag "For" (List.map lazy_expr_opt [init_e_opt; test_e_opt; adv_e_opt])

  | DoWhile test_e -> ptag "DoWhile" [lazy (print_expr string_of_meta test_e)] 

end
