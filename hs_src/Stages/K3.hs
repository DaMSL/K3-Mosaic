-- This is the K3 module to import for access to the AST
module Stages.K3 (module Stages.K3.Core.K3AST
  , module Stages.K3.Core.K3Annotations
  , module Stages.K3
) where

import Stages.K3.Core.K3AST 
import Stages.K3.Core.K3Annotations hiding (Id)

newtype Annotation = Annotation {getAnno::[K3Annotation (Type Annotation)]}

instance Show Annotation where
  show = show . getAnno

