(* The K3 Programming Language *)

open Tree

(* Identifiers *)
type id_t = string

type container_type_t
    = TSet
    | TBag
    | TList

type base_type_t
    = TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TFloat
    | TString
    | TMaybe        of value_type_t
    | TTuple        of value_type_t list
    | TCollection   of container_type_t * value_type_t
    | TAddress
    | TTarget       of base_type_t

and mutable_type_t
    = TMutable      of base_type_t
    | TImmutable    of base_type_t

and value_type_t
    = TIsolated     of mutable_type_t
    | TContained    of mutable_type_t

and type_t
    = TFunction of value_type_t * value_type_t
    | TValue    of value_type_t

(* Arguments *)
type arg_t
    = AVar      of id_t * value_type_t
    | ATuple    of (id_t * value_type_t) list

(* Constants *)
type address = string * int (* IP * port *)

type constant_t
    = CUnit
    | CUnknown
    | CBool     of bool
    | CInt      of int
    | CFloat    of float
    | CString   of string
    | CAddress  of address
    | CTarget   of id_t
    | CNothing

(* Expressions *)
type expr_tag_t
    = Const of constant_t
    | Var   of id_t
    | Tuple

    | Just

    | Empty of value_type_t
    | Singleton of value_type_t
    | Combine

    | Range of container_type_t

    | Add
    | Mult
    | Neg

    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda of arg_t
    | Apply

    | Block
    | Iterate
    | IfThenElse

    | Map
    | FilterMap
    | Flatten
    | Aggregate
    | GroupByAggregate
    | Sort

    | Slice

    | Insert
    | Delete
    | Update

    | Peek

    | Assign
    | Deref

    | Send

(* Expression Tree *)
type 'a expr_t = ((int * expr_tag_t) * 'a) tree_t

type stop_behavior_t
    = UntilCurrent
    | UntilEmpty
    | UntilEOF

type stream_format_t = CSV | JSON

type stream_channel_t
    = File of stream_format_t * string
    | Network of stream_format_t * address

type stream_pattern_t =
    | Terminal      of id_t
    | Choice        of stream_pattern_t list
    | Sequence      of stream_pattern_t list
    | Optional      of stream_pattern_t
    | Repeat        of stream_pattern_t * stop_behavior_t

type stream_t
    = Source        of id_t * type_t * stream_channel_t
    | Sink          of id_t * type_t * stream_channel_t
    | Derived       of id_t * stream_pattern_t

(* TODO: produce, listen instructions *)
type instruction_t
    = Consume of id_t

type stream_statement_t =
    | Stream        of stream_t
    | Bind          of id_t * id_t
    | Instruction   of instruction_t

type stream_program_t = stream_statement_t list

(* Top-Level Declarations *)
type 'a declaration_t
    = Global        of id_t * type_t  * 'a expr_t option
    | Foreign       of id_t * type_t
    | Trigger       of id_t * arg_t * (id_t * value_type_t) list * 'a expr_t
    | Role          of id_t * stream_program_t
    | DefaultRole   of id_t

(* K3 Programs *)
type 'a program_t = 'a declaration_t list
