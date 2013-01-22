-- The K3 AST type signature
module Stages.K3.Core.K3AST where

import Util.Tree

type Id = String
type Address = (String, Int) -- IP, port

-- Start of AST signature

data ContainerType
    = TSet
    | TBag
    | TList
    deriving Show

data BaseType a
    = TUnknown
    | TUnit
    | TBool
    | TByte
    | TInt
    | TFloat
    | TString
    | TMaybe (ValueType a)
    | TTuple [(ValueType a)]
    | TCollection ContainerType (ValueType a)
    | TAddress
    | TTarget (BaseType a)
    deriving Show

data MutableType a
    = TMutable (BaseType a) a
    | TImmutable (BaseType a) a
    deriving Show

data ValueType a
    = TIsolated (MutableType a)
    | TContained (MutableType a)
    deriving Show

data Type a
    = TFunction (ValueType a) (ValueType a)
    | TValue (ValueType a)
    deriving Show

-- Arguments
data Arg a
    = AIgnored
    | AVar Id (ValueType a)
    | AMaybe (Arg a)
    | ATuple [(Arg a)]
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
data ExprTag a
    = Const Constant
    | Var Id
    | Tuple

    | Just

    | Empty (ValueType a)
    | Singleton (ValueType a)
    | Combine

    | Range ContainerType

    | Add
    | Mult
    | Neg

    | Eq
    | Lt
    | Neq
    | Leq

    | Lambda (Arg a)
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
type Expr a = Tree ((Int, (ExprTag a)), a)

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

data FlowResource a
  = Handle (Type a) ChannelType ChannelFormat
  | Pattern ResourcePattern
  deriving Show

data FlowEndpoint a
  = Resource Id (FlowResource a)
  | Code Id (Arg a) [(Id, (ValueType a), a)] (Expr a)
  deriving Show

data FlowStatement a
  = Source (FlowEndpoint a)
  | Sink (FlowEndpoint a)
  | Bind Id Id
  | Instruction Instruction
  deriving Show

type FlowProgram a = [((FlowStatement a), a)]

-- Top-Level Declarations
data Declaration a
    = Global      Id (Type a) (Maybe (Expr a))
    | Foreign     Id (Type a)
    | Flow        (FlowProgram a)
    | Role        Id (FlowProgram a)
    | DefaultRole Id
    deriving Show

-- K3 Programs
type Program a = [((Declaration a), a)]


-- Testing
data CheckExpr a = FileExpr String | InlineExpr (Expr a)
                 deriving Show

type ExpressionTest a = ((Program a), (Expr a), (CheckExpr a))

type ProgramTest a = ((Program a), [(Id, (CheckExpr a))])

