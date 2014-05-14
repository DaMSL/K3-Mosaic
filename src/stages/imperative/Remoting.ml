open Tree
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

module R = Runtime.Make(Lang)
module U = ImperativeUtil.Util(Lang)

open R
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

	val serialize_expr :
    (unit -> annotation_t) -> (type_t * annotation_t) expr_t -> type_t
    -> (type_t * annotation_t) cmd_t list * (type_t * annotation_t) expr_t
	  
	val deserialize_expr :
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
  (* Field name, tag, spec field type, optional, repeated, default value *)
  type spec_type = (id_t * (int * string * bool * bool * string option)) list
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
  let lines_of_spec_type spec_type = List.map (fun (id,(tag,t,o,r,v)) ->
      let tag_str = "= "^(string_of_int tag) in
      let opt_str = if o then "optional" else "" in
      let rep_str = if r then "repeated" else "" in
      let val_str = match v with None -> "" | Some(x) -> "[default = "^x^"]" in
      (String.concat " " (List.filter ((<>) "") [opt_str; rep_str; t; id; tag_str; val_str]))^";" 
    ) spec_type
  
  let string_of_spec_type ?(transform_fn=(fun x -> x)) st =
    String.concat "\n" (transform_fn (lines_of_spec_type st)) 
  
  let string_of_spec_type_decl (id,st) = 
    "message "^id^" {\n"^
      (string_of_spec_type ~transform_fn:indent_lines st)^"\n}"

  let generate_spec s t =
    let spec_error s = "invalid protocol type, "^s in
    let rec spec_of_k3_type ?(repeated=false) field_id t =
      let error = spec_error "only base types and collections are supported" in
      let field_name id = "field"^(string_of_int id) in
      let type_id bt = msg_type_id (signature_of_type (TValue (canonical bt))) in
      let base_field ?(id=field_id) ?(default=None) type_id =
        [field_name id, (id, type_id, false, repeated, default)]
      in 
      match base_of t () with
	    | TBool   -> [], base_field "bool"
      
      (* TODO: is there a better representation for a single byte? *)
	    | TByte   -> [], base_field "uint32"
	    
      | TInt    -> [], base_field "sint32"
	    | TFloat  -> [], base_field "double"
	    | TString -> [], base_field "bytes"
	    | TMaybe vt -> 
        let sub_decls, vt_fields = spec_of_k3_type 0 vt in
        let opt_fields = match vt_fields with
          | [id, (tag, t, false, r, v)] -> [id, (tag, t, true, r, v)]
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
	      let sub_decls, el_type = spec_of_k3_type ~repeated:true et_id vt in
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
      
      | decls, ([_, (_, tid, _, _, _)] as msg) ->
        if List.mem_assoc tid decls then decls
        else if decls = [] then [msg_type_id s, msg] 
        else failwith (spec_error "invalid singleton message type")

      | _, _ -> failwith (spec_error "found multiple message fields")
 
  let merge_spec a b = ListAsSet.union a b

  let string_of_spec s =
    String.concat "\n" (List.map string_of_spec_type_decl s)

  (* Generates protobuf set/add method calls for deep K3 value serialization,
   * including nested collection values. *)
  (* TODO: handle external collection types, e.g., STL/BMI collections *)
  let rec serialize_value_cmds mk_meta src_expr src_vt dest_expr dest_id =
    let rcr = serialize_value_cmds mk_meta in
    let m = meta mk_meta in
    let unit_meta, int_meta, string_meta = m unit_t, m int_t, m string_t in
    let call_method = call_method_proc mk_meta in
    match base_of src_vt () with
    | TTuple fields ->
      fst (List.fold_left (fun (acc,i) t -> 
        let field_expr = mk_fn (m (iv_type t)) (Member (Position i)) [src_expr] in
        acc@(rcr field_expr t dest_expr ("field"^(string_of_int i))), i+1
      ) ([], 0) fields)

    | TCollection (c_t, e_t) ->
      let ct_str = match c_t with TSet -> "TSet" | TBag -> "TBag" | TList -> "TList" in
      let mk_imvd id vt init_expr_f = 
        let ks, kt = signature_of_type (TValue vt), iv_type vt in
        let n, t = id, serializer_type ks kt
        in n, m t, mk_var (m t) n, mk_var_decl n t (Some(Init (init_expr_f (m t))))
      in
      let loop_id, loop_sig, loop_t, loop_var =
        let n, t, it = "elem", TValue e_t, iv_type e_t
        in n, signature_of_type t, it, mk_var (m it) n
      in
      let c_obj_id, c_obj_meta, c_obj_var, c_obj_decl = 
        mk_imvd "s_coll" src_vt (fun m -> invoke_method m ("mutable_"^dest_id) dest_expr [])
      in
      let s_obj_id, s_obj_meta, s_obj_var, s_obj_decl = 
        mk_imvd "s_elem" e_t (fun m -> invoke_method m ("add_field1") c_obj_var [])
      in
      let loop_body = [mk_decl unit_meta s_obj_decl]@(rcr loop_var e_t s_obj_var "field0")
      in [mk_decl unit_meta c_obj_decl;
          call_method "set_field0" c_obj_var [mk_const string_meta (CString ct_str)];
          mk_for unit_meta loop_id loop_t src_expr loop_body]
    
    | _ -> [call_method ("set_"^dest_id) dest_expr [src_expr]]

  let serialize_expr mk_meta expr expr_t = 
    let m = meta mk_meta in
    let unit_meta = m unit_t in
    let expr_sig, expr_vt = signature_of_type (k3_type expr_t), vi_type expr_t in
    let s_obj_decl, s_obj_var =
      let s_id, s_t = "serialized", serializer_type expr_sig expr_t
      in mk_var_decl s_id s_t None, mk_var (m s_t) s_id
    in
    let s_cmds = serialize_value_cmds mk_meta expr expr_vt s_obj_var "field0" in
    let s_expr = mk_fn (m string_t) (Member (Method "SerializeAsString")) [s_obj_var]
    in [mk_decl unit_meta s_obj_decl]@s_cmds, s_expr

  (* Helper counter for unique deserialization symbols *)
  let deserialize_id = ref 0
  let get_deserialize_id () =
    let r = "coll"^(string_of_int !deserialize_id) in incr deserialize_id; r

  (* TODO: handle external collection types, e.g., STL/BMI collections *)
  let rec deserialize_value_cmds mk_meta src_expr src_id dest_vt =
    let rcr = deserialize_value_cmds mk_meta in  
    let m = meta mk_meta in
    let unit_meta, int_meta, string_meta = m unit_t, m int_t, m string_t in
    match base_of dest_vt () with
    | TTuple fields ->
      let field_exprs, field_cmds, _ = List.fold_left (fun (eacc,cmdacc,i) t -> 
          let e,cmds = rcr src_expr ("field"^(string_of_int i)) t
          in eacc@[e], cmdacc@cmds, i+1
        ) ([], [], 0) fields
      in mk_tuple (m (iv_type dest_vt)) field_exprs, field_cmds

    | TCollection (c_t, e_t) ->
      let mk_itv id vt = 
        let it = serializer_type (signature_of_type (TValue vt)) (iv_type vt)
        in id, it, mk_var (m it) id
      in
      let sc_id, sc_t, sc_var = mk_itv "s_coll" dest_vt in
      let se_id, se_t, se_var = mk_itv "s_elem" e_t in
      let c_decl, c_var = 
        let id, t = get_deserialize_id (), (iv_type dest_vt)
        in mk_var_decl id t None, mk_var (m t) id
      in
      let loop_src = 
        (* TODO: generalize from this C++ specific codegen *)
        let t = match se_t with
          | TNamed (x) -> TNamed ("RepeatedField<"^x^">")
          | _ -> failwith "invalid protobuf element type"
        in invoke_method (m t) "field1" (invoke_method (m sc_t) src_id src_expr []) [] in
      let loop_body = 
        let e, cmds = rcr se_var "field0" e_t in
        cmds@[mk_expr unit_meta (mk_fn unit_meta (Collection Insert) [c_var; e])]
      in
      c_var, [mk_decl unit_meta c_decl; mk_for unit_meta se_id se_t loop_src loop_body]

    | _ -> invoke_method (m (iv_type dest_vt)) src_id src_expr [], []

  let deserialize_expr mk_meta serialized_expr deserialize_t =
    let m = meta mk_meta in
    let unit_meta = m unit_t in
    let id = "serialized" in
    let k3_t = k3_type deserialize_t in
    let serialized_t, serialized_meta = 
      let s_t = serializer_type (signature_of_type k3_t) k3_t
      in s_t, m s_t
    in
    let serialized_decl, serialized_var =
      let s_init_expr = mk_fn serialized_meta (Named "ParseFromString") [serialized_expr]
      in mk_var_decl id serialized_t (Some(Init(s_init_expr))), mk_var serialized_meta id
    in
    let rc_expr, rc_cmds =
      deserialize_value_cmds
        mk_meta serialized_var "field0" (vi_type deserialize_t)
    in [mk_decl unit_meta serialized_decl]@rc_cmds, rc_expr
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
    (* Use both sender and receiver signatures to handle all message
     * types expected for triggers defined in the current program, and 
     * triggers used in other programs *)
    let proto_sigs_and_types =
      ListAsSet.no_duplicates 
      (List.map (fun (s,_,_,t,_) -> s, i_type t) (fst protospec))@
      (List.map (fun (id,arg,asig) -> asig, i_type (tuple_type_of_arg arg)) (snd protospec))
    in
    let specs = List.map (fun (s,t) -> S.generate_spec s t) proto_sigs_and_types in
    if specs = [] then "" else 
    let x,y = List.hd specs, List.tl specs in
    S.string_of_spec (List.fold_left S.merge_spec x y)

  (* TODO: send-bypass and serialization-bypass as appropriate for 
   * public/private triggers *)
  let generate_sender mk_meta class_id parent_class_id protospec =
    let m = meta mk_meta in
    let unit_meta, int_meta, string_meta = m unit_t, m int_t, m string_t in
	  let senders_by_type = List.fold_left (fun acc (msg_sig, target_id, target_t, arg_t, addr) ->
	      if List.mem_assoc (msg_sig, target_t, arg_t) acc then
	        let existing = List.assoc (msg_sig, target_t, arg_t) acc in
	        (List.remove_assoc (msg_sig, target_t, arg_t) acc)@
	          [(msg_sig, target_t, arg_t), existing@[target_id, addr]]
	      else acc@[(msg_sig, target_t, arg_t), [target_id, addr]]
	    ) [] (fst protospec)
	  in
	  let sender_decls =
      (* TODO: do we really need msg_sig, or can this be computed from expr/expr_type? *)
      List.map (fun ((msg_sig, target_t, arg_t), ids_and_addrs) ->
        let msg_t, msg_var = let t = i_type arg_t in t, mk_var (m t) "payload" in
        let args = ["target", i_type target_t; "address", ib_type TAddress; "payload", msg_t] in
        let body =
          let s_cmds, s_expr = S.serialize_expr mk_meta msg_var msg_t in
          let send_fn =
            mk_fn unit_meta (Named "send")
              ((List.map (fun (id,t) -> mk_var (m t) id)
                 [List.nth args 0; List.nth args 1])@[s_expr])
          in s_cmds@[mk_expr unit_meta send_fn]
        in 
        DFn("send_"^msg_sig, args, unit_t, body), unit_meta
      ) senders_by_type 
	  in
    if sender_decls = [] then None
    else
    let parent = if parent_class_id = "" then None else Some(TNamed (parent_class_id))
    in Some(DClass(class_id, parent, sender_decls))

  (* TODO: serialization-bypass as appropriate for native senders *)
  let generate_receiver mk_meta class_id parent_class_id resource_env protospec =
    let m = meta mk_meta in
    let unit_meta, int_meta, serialized_meta = m unit_t, m int_t, m S.serialized_type in
    let call_method = call_method_proc mk_meta in
    let trig_specs = ListAsSet.no_duplicates (snd protospec) in
    let scheduler_var = mk_var (m runtime_type) runtime_var_id in
	  let recvrs_by_type =
	    List.fold_left (fun acc (id,arg,asig) ->
	      let t = tuple_type_of_arg arg in
	      if not (List.mem_assoc (asig,t) acc) then acc@[(asig,t), [id,arg]] 
	      else let existing = List.assoc (asig,t) acc
             in (List.remove_assoc (asig,t) acc)@[(asig,t), existing@[id,arg]]
	    ) [] trig_specs 
	  in
    let mk_recvr_class_id id = "recv_dispatch_"^id in 
    let decompose_tuple_expr e = match tag_of_expr e with 
      | Tuple -> sub_tree e
      | _ -> failwith "invalid tuple deserialization"
    in
	  let recvr_decls = List.flatten (List.map (fun ((asig,t), trigs) ->
        let event_id = "event" in 
        let recv_args, event_var = [event_id, message_t], mk_var (m message_t) event_id in
        let arg_t, serialized_t = let x = i_type t in x, S.serializer_type asig x in
        let payload_expr = mk_fn serialized_meta (Member (Field msg_payload_id)) [event_var] in
        List.flatten (List.map (fun (id,arg) ->
          let decompose_f = if List.length (typed_vars_of_arg arg) > 1
                            then decompose_tuple_expr else (fun e -> [e]) in
          let body =
            let ds_cmds, ds_expr = S.deserialize_expr mk_meta payload_expr arg_t in
            let queue_cmds = [call_method (mk_queue_trigger_internal_id id)
                                scheduler_var (decompose_f ds_expr)]
            in ds_cmds@queue_cmds
          in 
          let members = [DFn("recv", recv_args, unit_t, body), unit_meta]
          in [DClass(mk_recvr_class_id id, Some(TNamed "recv_dispatch"), members), unit_meta]
        ) trigs)
      ) recvrs_by_type)
	  in
	  let recvr_init_decl =
	    let init_body = List.flatten (List.map (fun (_, trigs) ->
	        List.flatten (List.map (fun (id,arg) ->
	          let t, vid = TNamed (mk_recvr_class_id id), "rdo_"^id in
	          [mk_decl unit_meta (mk_var_decl vid t None);
	           call_proc mk_meta "register_dispatch" [mk_target_var mk_meta id; mk_var (m t) vid]]
	          ) trigs)
	      ) recvrs_by_type)
	    in
      if init_body = [] then []
      else [DFn("init", [], unit_t, init_body), unit_meta]
	  in
    let decls = recvr_decls@recvr_init_decl in
    if decls = [] then None
    else
    let parent = if parent_class_id = "" then None else Some(TNamed parent_class_id)
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
