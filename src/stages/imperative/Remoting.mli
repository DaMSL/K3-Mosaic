open K3.AST
open K3.Annotation
open K3Streams
open Imperative

val protocol_spec_file : string
val protocol_class_id : id_t
val protocol_include : string

(* Functorized imperative construction *)
module Make : functor (Lang : TargetLanguage) ->
sig
  module ASTImport : Imperative.Export with module AST = Imperative.AST(Lang)
  open ASTImport.AST

	module type SerializerGenerator = sig
    type spec_t

    (* Serialization spec helpers *)
    val merge_spec : spec_t -> spec_t -> spec_t
    val string_of_spec : spec_t -> string

    (* The representation type to which all values are serialized *)
    val serialized_type : type_t

    (* The IDL type used to manage serialization the given signature and type *)
    val serializer_type : string -> type_t -> type_t

    (* Generate a serializer class declaration if this is needed by the
     * serialization library. *)
	  val generate_serializer :
      (unit -> annotation_t) -> id_t -> (type_t * annotation_t) decl_t option

    (* Generates an internal structure for the serialization of the given type *)
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

    val generate_message_type :
      (unit -> annotation_t) -> (type_t * annotation_t) decl_t

    val generate_serializer_include : protocol_spec -> string

    (* TODO: protocol spec file generation and indicators for includes
     * and protocol spec file *)
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

  module MGen : MessagingGenerator
  module SGen : SerializerGenerator

end
