module Stages.K3.Core.K3Annotations where

type Id = String

-- Unnamed tuple descriptor
type Positions = [Int]

data Dependency = Element | Positions Positions
                  deriving Show

data Rigidity = Constraint | Hint
                deriving Show
	
data DataAnnotation 
  = FunDep     Positions Dependency
  | MVFunDep   Positions Dependency
  | Unique     Positions
  | Ordered    Positions
  | Sequential
  | RandomAccess
  deriving Show

data ControlAnnotation
    = Effect   [Id] -- Variables ranged over by the effect
    | Parallel Int  -- Degree of parallelism
    deriving Show

data K3Annotation t
    = Data    Rigidity DataAnnotation
    | Control Rigidity ControlAnnotation
    | Type    t
    deriving Show

