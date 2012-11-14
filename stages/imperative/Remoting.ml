open K3.AST
open K3.Annotation
open K3Util
open K3Typechecker
open K3Streams
open Imperative
open ImperativeUtil
open Runtime

let protocol_spec_file = "k3_program.proto"
let protocol_class_id, protocol_include = "k3_protocol", "k3_protocol.hpp"

module Make = functor (Lang : TargetLanguage) ->
struct

(* Import AST and utilities *)
module ASTImport = struct module AST = Imperative.AST(Lang) end
open ASTImport.AST

module S = Runtime.Make(Lang)
module U = ImperativeUtil.Util(Lang)

open S
open U

let meta mk_meta t = t, mk_meta()

module type SerializerGenerator = sig
  type spec_t

	val merge_spec : spec_t -> spec_t -> spec_t
	val string_of_spec : spec_t -> string

  val serialized_type : type_t
  val serializer_type : string -> type_t -> type_t

	val generate_serializer :
    (unit -> annotation_t) -> id_t -> (type_t * annotation_t) decl_t option

	val generate_spec : string -> type_t -> spec_t

	val generate_serialize :
    (unit -> annotation_t) -> (type_t * annotation_t) expr_t -> type_t
    -> (type_t * annotation_t) cmd_t list * (type_t * annotation_t) expr_t
	  
	val generate_deserialize :
	  (unit -> annotation_t) -> (type_t * annotation_t) expr_t -> type_t
	  -> (type_t * annotation_t) cmd_t list * (type_t * annotation_t) expr_t
end

module type MessagingGenerator = sig
  val message_t : type_t

  val generate_message_type : (unit -> annotation_t) -> (type_t * annotation_t) decl_t

  val generate_serializer_include : protocol_spec -> string

  val generate_serializer_specs : protocol_spec -> string

  val generate_sender :
	  (unit -> annotation_t) -> id_t -> id_t -> protocol_spec
	  -> (type_t * annotation_t) decl_t option

	val generate_receiver :
	  (unit -> annotation_t) -> id_t -> id_t -> resource_env_t -> protocol_spec
	  -> (type_t * annotation_t) decl_t option
  
	val generate_send :
	  (unit -> annotation_t) -> (type_t * annotation_t) expr_t
	  -> constant_t -> address -> (type_t * annotation_t) expr_t -> type_t
	  -> (type_t * annotation_t) cmd_t list 
end

module Protobuf : SerializerGenerator = struct
  (* Field name, tag, spec field type, optional, default value *)
  type spec_type = (id_t * (int * string * bool * string option)) list
  type spec_type_env = (id_t * spec_type) list
  
  type spec_t = spec_type_env

  (* Helpers functions *)  
  let indent_lines l = let tab = "  " in List.map (fun s -> tab^s) l
  
  (* Serializer type helpers *)
  let msg_type_id type_sig = "pb_"^type_sig

  let serialized_type = ib_type TString
  let serializer_type s_sig s_t = TNamed (msg_type_id s_sig) 

  let generate_serializer mk_meta id = None
  
  (* Protocol specification file helpers *)
  let lines_of_spec_type spec_type = List.map (fun (id,(tag,t,o,v)) ->
      let tag_str = "= "^(string_of_int tag) in
      let opt_str = if o then "optional" else "" in
      let val_str = match v with None -> "" | Some(x) -> "[default = "^x^"]" in
      (String.concat " " [opt_str; t; id; tag_str; val_str])^";" 
    ) spec_type
  
  let string_of_spec_type ?(transform_fn=(fun x -> x)) st =
    String.concat "\n" (transform_fn (lines_of_spec_type st)) 
  
  let string_of_spec_type_decl (id,st) = 
    "message "^id^" {\n"^
      (string_of_spec_type ~transform_fn:indent_lines st)^"\n}"

  let generate_spec s t =
    let spec_error s = "invalid protocol type, "^s in
    let rec spec_of_k3_type field_id t =
      let error = spec_error "only base types and collections are supported" in
      let field_name id = "field"^(string_of_int id) in
      let type_id bt = "pb_"^(signature_of_type (TValue (canonical bt))) in
      let base_field ?(id=field_id) ?(default=None) type_id =
        [field_name id, (id, type_id, false, default)]
      in 
      match base_of t with
	    | TBool   -> [], base_field "bool"
      
      (* TODO: is there a better representation for a single byte? *)
	    | TByte   -> [], base_field "uint32"
	    
      | TInt    -> [], base_field "sint32"
	    | TFloat  -> [], base_field "double"
	    | TString -> [], base_field "bytes"
	    | TMaybe vt -> 
        let sub_decls, vt_fields = spec_of_k3_type 0 vt in
        let opt_fields = match vt_fields with
          | [id, (tag, t, false, v)] -> [id, (tag, t, true, v)]
          | _ -> failwith "invalid option value field"
        in
        let opt_type_id = type_id (TMaybe vt) in
        let new_decls = ListAsSet.union sub_decls [opt_type_id, opt_fields]
        in new_decls, base_field opt_type_id

	    | TTuple vt_fields ->         
	      let sub_decls, tuple_fields = 
	        let x,y = List.split (fst (List.fold_left (fun (acc,i) vt -> 
              acc@[spec_of_k3_type i vt], i+1
            ) ([],0) vt_fields))
          in List.flatten x, List.flatten y
	      in
	      let tuple_type_id = type_id (TTuple vt_fields) in 
        let new_decls = ListAsSet.union sub_decls [tuple_type_id, tuple_fields]
        in new_decls, base_field tuple_type_id
         
	    | TCollection (ct,vt) ->
        let ct_id, et_id = 0, 1 in  
	      let sub_decls, el_type = spec_of_k3_type et_id vt in
        let collection_type_id = type_id (TCollection (ct,vt)) in
        let collection_decl =
          let ct_val = match ct with TSet -> "TSet" | TBag -> "TBag" | TList -> "TList" in
          let ct_field = base_field ~id:ct_id ~default:(Some ct_val) "k3_collection" in
          [collection_type_id, ct_field@el_type]
        in 
        let new_decls = ListAsSet.union sub_decls collection_decl 
        in new_decls, base_field collection_type_id 
        
	    | TAddress -> [], base_field "k3_address"
	    | TTarget _ -> [], base_field "k3_target"
      
      | _ -> failwith error
    in
    let spec_of_type t = 
      let error = spec_error "only K3 native types are supported" in
      match t with 
      | TInternal it -> spec_of_k3_type 0 (vi_type t)

       (* TODO: TMap, TNamed? *)
      | _ -> failwith error 
    in 
      match spec_of_type t with
      | _, [] -> failwith "invalid IDL specification"
      
      | decls, ([_, (_, tid, _, _)] as msg) ->
        if List.mem_assoc tid decls then decls
        else if decls = [] then [msg_type_id s, msg] 
        else failwith (spec_error "invalid singleton message type")

      | _, _ -> failwith (spec_error "found multiple message fields")
 
  let merge_spec a b = ListAsSet.union a b

  let string_of_spec s =
    String.concat "\n" (List.map string_of_spec_type_decl s)

  let generate_serialize mk_meta arg_var arg_type =
    [], mk_fn (meta mk_meta string_t) (Member (Method "SerializeAsString")) [arg_var]
  
  let generate_deserialize mk_meta msg_var msg_type =
    let k3_t = match msg_type with TInternal t -> t | _ -> failwith "invalid K3 msg type" in
    let s_t = serializer_type (signature_of_type k3_t) k3_t in
    let s_m = meta mk_meta s_t in
    [], mk_fn s_m (Named "ParseFromString") [msg_var]  
end


module ZMQCloudless = functor (S: SerializerGenerator) ->
struct
  let message_t = TNamed "trigger_msg"

  let msg_target_id = "target"
  let msg_payload_id = "payload"

  let generate_message_type mk_meta =
    let m = meta mk_meta in
    let unit_meta, int_meta = m unit_t, m int_t in
    DClass("trigger_msg", None,
      [(U.mk_var_decl msg_target_id (U.ib_type TInt) None), unit_meta;
       (U.mk_var_decl msg_payload_id S.serialized_type None), unit_meta])

  let generate_serializer_include protospec = "k3_messages.h"

  let generate_serializer_specs protospec =
    let specs =
      List.map (fun (s,t) -> S.generate_spec s t) 
        (ListAsSet.no_duplicates
          (List.map (fun (s,_,_,t,_) -> s, i_type t) (fst protospec)))
    in
    if specs = [] then ""
    else 
      let x,y = List.hd specs, List.tl specs in
      S.string_of_spec (List.fold_left S.merge_spec x y)

  let generate_sender mk_meta class_id parent_class_id protospec =
    let m = meta mk_meta in
    let unit_meta, int_meta = m unit_t, m int_t in

	  let senders_by_type = List.fold_left (fun acc (msg_sig, target_id, target_t, arg_t, addr) ->
	      if List.mem_assoc (msg_sig, target_t, arg_t) acc then
	        let existing = List.assoc (msg_sig, target_t, arg_t) acc in
	        (List.remove_assoc (msg_sig, target_t, arg_t) acc)@
	          [(msg_sig, target_t, arg_t), existing@[target_id, addr]]
	      else acc@[(msg_sig, target_t, arg_t), [target_id, addr]]
	    ) [] (fst protospec)
	  in
	  let sender_decls =
	    List.map (fun ((msg_sig, target_t, arg_t), ids_and_addrs) ->
        let num_fields = 
          let id = fst (List.hd ids_and_addrs) in
          let args = List.filter (fun (x,_,_) -> id = x) (snd protospec) in 
          if List.length args > 0 then
            let _,arg,_ = List.hd args in List.length (vars_of_arg arg)
          else failwith ("invalid trigger id "^id)
        in
        let msg_t = i_type arg_t in
	      let args = ["target", i_type target_t; "address", ib_type TAddress; "payload", msg_t] in
        let msg_var = mk_var (m msg_t) "payload" in

        let set_fields_cmds s_obj_var fields =
          fst (List.fold_left (fun (acc,i) t -> 
              let field_expr = mk_fn (m (iv_type t)) (Member (Position i)) [msg_var] in
              let set_expr = call_method_proc
                mk_meta ("set_field"^(string_of_int i)) s_obj_var [field_expr] in  
              acc@[set_expr], i+1)
            ([], 0) fields)
        in
            
	      let body =
	        let s_obj_decl, s_obj_var =
	          let id = "serialized" in
	          mk_var_decl id (S.serializer_type msg_sig msg_t) None,
	          mk_var unit_meta id
	        in
	        let s_cmds, s_expr = S.generate_serialize mk_meta s_obj_var msg_t in
	        let send_fn =
	          mk_fn unit_meta (Named "send")
	            ((List.map (fun (id,t) -> mk_var (m t) id)
	               [List.nth args 0; List.nth args 1])@[s_expr])
	        in
	        let set_cmds =
	          let bt = base_of (value_of arg_t 
	            (fun _ -> failwith "invalid value type in serialization"))
	          in match num_fields, bt with
	            | x, TTuple fields when x > 1 -> set_fields_cmds s_obj_var fields
	            | _, _ -> [call_method_proc mk_meta "set_field0" s_obj_var [msg_var]]
	        in
	        s_cmds@[mk_decl unit_meta s_obj_decl]@set_cmds@[mk_expr unit_meta send_fn]
	      in
	      let fn = DFn("send_"^msg_sig, args, unit_t, body) in
	      fn, unit_meta
	      ) senders_by_type 
	  in
    if sender_decls = [] then None
    else
    let parent = if parent_class_id = "" then None else Some(parent_class_id)
    in Some(DClass(class_id, parent, sender_decls))
  
  let generate_receiver mk_meta class_id parent_class_id resource_env protospec =
    let m = meta mk_meta in
    let unit_meta, int_meta, serialized_meta =
      m unit_t, m int_t, m S.serialized_type in

    let trig_specs = ListAsSet.no_duplicates (snd protospec) in
    let scheduler_var = mk_var (m runtime_type) "scheduler" in

	  let recvrs_by_type =
	    List.fold_left (fun acc (id,arg,asig) ->
	      let t = tuple_type_of_arg arg in
	      if List.mem_assoc (asig,t) acc then
	        let existing = List.assoc (asig,t) acc in
	        (List.remove_assoc (asig,t) acc)@[(asig,t), existing@[id,arg]]
	      else acc@[(asig,t), [id,arg]] 
	    ) [] trig_specs 
	  in

    let mk_recvr_class_id id = "recv_dispatch_"^id in 
	  let recvr_decls =
	    List.flatten (List.map (fun ((asig,t), trigs) ->
          let event_id = "event" in 
	        let args, event_var = [event_id, message_t], mk_var (m message_t) event_id in
          let serialized_t = S.serializer_type asig (i_type t) in
	        List.flatten (List.map (fun (id,arg) ->
	          let dispatch_args serialized_var =
	            fst (List.fold_left (fun (acc,i) (_,vt) ->
                  let field_expr = invoke_method
                    (m (iv_type vt)) ("field"^(string_of_int i)) serialized_var []
                  in acc@[field_expr], i+1
	              ) ([],0) (typed_vars_of_arg arg))
	          in
            let body =
              let ds_cmds, ds_expr =
                S.generate_deserialize mk_meta
                  (mk_fn serialized_meta (Member (Field msg_payload_id)) [event_var]) (i_type t)
              in
		          let serialized_decl, serialized_var =
		            let id = "serialized" in
		            mk_var_decl id serialized_t (Some(Init(ds_expr))),
		            mk_var (m serialized_t) id
		          in
              let queue_cmd =
                call_method_proc mk_meta
                  (mk_queue_trigger_internal_id id)
                  scheduler_var (dispatch_args serialized_var)
              in
              ds_cmds@[mk_decl unit_meta serialized_decl; queue_cmd]
            in 
            let fn = DFn("recv", args, unit_t, body)
            in [DClass(mk_recvr_class_id id, 
                       Some("recv_dispatch"), [fn, unit_meta]), unit_meta]
	        ) trigs)
	      ) recvrs_by_type)
	  in
	  let recvr_init_decl =
	    let init_body = List.flatten (List.map (fun (_, trigs) ->
	        List.flatten (List.map (fun (id,arg) ->
	          let t = TNamed (mk_recvr_class_id id) in
	          let vid = "rdo_"^id in
	          [mk_decl unit_meta (mk_var_decl vid t None);
	           call_proc mk_meta "register_dispatch"
	            [mk_target_var mk_meta id; mk_var (m t) vid]]
	          ) trigs)
	      ) recvrs_by_type)
	    in
      if init_body = [] then []
      else [DFn("init", [], unit_t, init_body), unit_meta]
	  in
    let decls = recvr_decls@recvr_init_decl in
    if decls = [] then None
    else
    let parent = if parent_class_id = "" then None else Some(parent_class_id)
    in Some(DClass(class_id, parent, decls))
  
  let generate_send mk_meta sender_var target_const addr_const msg_var msg_type =
    let m = meta mk_meta in
    let unit_meta, int_meta, addr_meta = m unit_t, m int_t, m addr_t in
	  [mk_expr unit_meta
      (mk_fn unit_meta (Named "send")
        [mk_const int_meta target_const; mk_const addr_meta (CAddress addr_const); msg_var])]
end

module MGen : MessagingGenerator = ZMQCloudless(Protobuf)
module SGen : SerializerGenerator = Protobuf

end