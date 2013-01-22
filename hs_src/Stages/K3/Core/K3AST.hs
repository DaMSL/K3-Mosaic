-- The K3 AST type signature
module Stages.K3.Core.K3AST where

import Util.Tree
import Stages.K3.Core.K3Annotations (K3Annotation)

type Id = String
type Address = (String, Int) -- IP, port

-- Start of AST signature

data ContainerType
    = TSet
    | TBag
    | TList
    deriving Show

data BaseType
    = TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TFloat
    | TString
    | TMaybe ValueType
    | TTuple [ValueType]
    | TCollection ContainerType ValueType
    | TAddress
    | TTarget BaseType
    deriving Show

data MutableType
    = TMutable BaseType Annotation
    | TImmutable BaseType Annotation
    deriving Show

data ValueType
    = TIsolated MutableType
    | TContained MutableType
    deriving Show

data Type
    = TFunction ValueType ValueType
    | TValue ValueType
    deriving Show

-- Arguments
data Arg
    = AIgnored
    | AVar Id ValueType
    | AMaybe Arg
    | ATuple [Arg]
    deriving Show

-- Constants
data Constant
    = CUnit
    | CUnknown
    | CBool Bool
    | CInt Int
    | CFloat Float
    | CString String
    | CAddress Address
    | CTarget Id       -- trigger name
    | CNothing
    deriving Show

-- Expressions
data ExprTag
    = Const Constant
    | Var Id
    | Tuple

    | Just

    | Empty ValueType
    | Singleton ValueType
    | Combine

    | Range ContainerType

    | Add
    | Mult
    | Neg

    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda Arg
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

    | Peek
    | Slice
    | Insert
    | Delete
    | Update

    | Assign
    | Deref

    | Send
    deriving Show

-- Expression Tree
type Expr = Tree ((Int, ExprTag), Annotation)

data StopBehavior
    = UntilCurrent
    | UntilEmpty
    | UntilEOF
    deriving Show

-- Flow program AST

-- The types of sources we can have, along with the information to uniquely
-- identify them.
data ChannelFormat = 
    CSV | JSON
    deriving Show

data ChannelType
    = File String
    | Network Address
    deriving Show

data ResourcePattern
    = Terminal Id
    | Choice [ResourcePattern]
    | Sequence [ResourcePattern]
    | Optional ResourcePattern
    | Repeat ResourcePattern StopBehavior
    deriving Show

-- TODO: produce, listen instructions
newtype Instruction = Consume Id
                      deriving Show

data FlowResource
  = Handle Type ChannelType ChannelFormat
  | Pattern ResourcePattern
  deriving Show

data FlowEndpoint
  = Resource Id FlowResource
  | Code Id Arg [(Id, ValueType, Annotation)] Expr
  deriving Show

data FlowStatement
  = Source FlowEndpoint
  | Sink FlowEndpoint
  | Bind Id Id
  | Instruction Instruction
  deriving Show

type FlowProgram = [(FlowStatement, Annotation)]

-- Top-Level Declarations
data Declaration
    = Global      Id Type (Maybe Expr)
    | Foreign     Id Type
    | Flow        FlowProgram
    | Role        Id FlowProgram
    | DefaultRole Id
    deriving Show

-- K3 Programs
type Program = [(Declaration, Annotation)]

type Annotation = [(K3Annotation Type)]

-- Testing
data CheckExpr = FileExpr String | InlineExpr Expr
                 deriving Show

type ExpressionTest = (Program, Expr, CheckExpr)

type ProgramTest = (Program, [(Id, CheckExpr)])

