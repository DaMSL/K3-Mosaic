open K3.AST
open K3.Annotation
open K3Util
open K3Typechecker
open K3Streams
open Imperative
open ImperativeUtil

let k3_include = "k3.hpp"
let target_class_id, target_include = "k3_target", "k3_targets.hpp"
let runtime_class_id, runtime_include = "k3_runtime", "k3_runtime.hpp"
let options_class_id = "k3_options"

module Make = functor (Lang : TargetLanguage) ->
struct

(* Import AST and utilities *)
module ASTImport = struct module AST = Imperative.AST(Lang) end
open ASTImport.AST

module U = ImperativeUtil.Util(Lang)
open U

let mk_target_var_id id = "target_"^id

let mk_target_var mk_meta id =
  let meta t = t, mk_meta() in  
  let int_meta = meta int_t in
    (*U.mk_var int_meta (mk_target_var_id id)*)
    U.mk_fn int_meta
      (Member (Field (mk_target_var_id id)))
      [U.mk_var (meta (TNamed target_class_id)) "targets"]

let mk_trigger_dispatch_id id = "dispatch_"^id
let mk_trigger_dispatch_var_id id = "dispatch_obj_"^id
let mk_queue_trigger_internal_id id = "iqueue_"^id
let mk_queue_trigger_external_id id = "equeue_"^id

let runtime_type = TNamed runtime_class_id

let trig_common mk_meta id arg =
  let meta t = t, mk_meta() in  
  let event_id, event_var = let x = "event" in x, U.mk_var (meta TTop) x in
  let generic_arg = [event_id, TTop] in
  let trig_target_var = mk_target_var mk_meta id in
  let trig_arg_tuple = U.ib_type (TTuple (List.map snd (typed_vars_of_arg arg))) in 
  let trig_args = List.map (fun (aid,avt) -> aid, U.iv_type avt) (typed_vars_of_arg arg) in
  let trig_arg_vars = List.map (fun (id,t) -> U.mk_var (meta t) id) trig_args in
    event_id, event_var, generic_arg, trig_target_var, trig_arg_tuple, trig_args, trig_arg_vars

let generate_targets mk_meta protospec =
  let meta t = t, mk_meta() in
  let unit_meta, int_meta, string_meta = meta unit_t, meta int_t, meta string_t in
  let trig_specs = ListAsSet.no_duplicates (snd protospec) in 
  let target_decls, init_body_decls =
    List.split (List.map (fun (id, _, _) ->
        let target_val = mk_const int_meta (CInt (Hashtbl.hash id)) in
        let sym_val = mk_const string_meta (CString id) in
        let target =   
	        mk_var_decl (mk_target_var_id id) (ib_type TInt) (Some(Init(target_val))),
          unit_meta
        in
        let init =
          mk_expr unit_meta
            (mk_fn unit_meta (Named "add_symbol") [sym_val; target_val])
        in target, init
      ) trig_specs) 
  in
  let init_decl = [DFn("init", [], unit_t, init_body_decls), unit_meta]
  in DClass(target_class_id, Some("symbol_table"), target_decls@init_decl)

let generate_runtime mk_meta protospec =
  let meta t = t, mk_meta() in
  let unit_meta, int_meta = meta unit_t, meta int_t in
  let trig_specs = ListAsSet.no_duplicates (snd protospec) in 
  let imperative_of_dispatch_decls id arg =
    let event_id, event_var, generic_arg, trig_target_var,
        trig_arg_tuple, trig_args, trig_arg_vars = trig_common mk_meta id arg
    in
	  let dispatch_id = mk_trigger_dispatch_id id in
    let dispatch_t, dispatch_meta =  let t = TNamed dispatch_id in t, meta t in  
	  let dispatch_body =
	    let tuple_expr = mk_fn (meta trig_arg_tuple) (Cast trig_arg_tuple) [event_var] in
	    let dispatch_args = fst (List.fold_left (fun (acc,i) (_,t) ->
	        acc@[mk_fn (meta t) (Member (Position i)) [tuple_expr]], i+1)
	      ([],0) trig_args) 
	    in [mk_expr unit_meta (mk_fn unit_meta (Named id) dispatch_args)]
	  in
	  let dispatch_class =
	    let dispatch_fn = [DFn("dispatch", generic_arg, unit_t, dispatch_body), unit_meta]
	    in DClass(dispatch_id, Some("trigger_dispatch"), dispatch_fn) 
	  in
    let dispatch_var_decl, dispatch_var =
      let var_id = mk_trigger_dispatch_var_id id
      in (mk_var_decl var_id dispatch_t None), (mk_var dispatch_meta var_id)
    in
    dispatch_class, dispatch_var_decl, dispatch_var
  in 
  let runtime_decls, dispatch_decls_and_vars = 
    let x, y = List.split (List.map (fun (id, arg, _) ->
      let event_id, event_var, generic_arg, trig_target_var,
          trig_arg_tuple, trig_args, trig_arg_vars = trig_common mk_meta id arg
      in
      let dispatch_class, dispatch_var_decl, dispatch_var = imperative_of_dispatch_decls id arg in
      let internal_body =
        [mk_expr unit_meta
          (mk_fn unit_meta (Named "schedule")
            [trig_target_var; cast_to_top mk_meta (mk_tuple (meta trig_arg_tuple) trig_arg_vars)])] 
      in
      let external_body = 
        [mk_expr unit_meta (mk_fn unit_meta (Named "schedule") [trig_target_var; event_var])]
      in
      [dispatch_class, unit_meta;
       DFn(mk_queue_trigger_internal_id id, trig_args, unit_t, internal_body), unit_meta;
       DFn(mk_queue_trigger_external_id id, generic_arg, unit_t, external_body), unit_meta],
      (id, (dispatch_var_decl, dispatch_var)) 
      ) trig_specs)
    in List.flatten x, y
  in
  let runtime_init_decl =
    let body = List.flatten (List.map (fun (id, (dispatch_var_decl, dispatch_var)) ->
      let trig_target_var = mk_target_var mk_meta id in
        [mk_decl unit_meta dispatch_var_decl;
         call_proc mk_meta "register_dispatch" [trig_target_var; dispatch_var]]
      ) dispatch_decls_and_vars)
    in [DFn("init", [], unit_t, body), unit_meta] 
  in
    DClass(runtime_class_id, Some("runtime"), runtime_decls@runtime_init_decl)

end