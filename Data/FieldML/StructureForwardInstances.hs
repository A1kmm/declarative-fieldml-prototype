{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances, GADTs  #-}
module Data.FieldML.StructureForwardInstances where

import Data.Typeable
import Data.Data
import Data.FieldML.Structure

deriving instance Data t => Data (OrForward t ForwardPossible)
deriving instance Data (ELabel ForwardPossible)
deriving instance Data (NewDomain ForwardPossible)
deriving instance Data (CloneAnnotation ForwardPossible)
deriving instance Data (DomainExpression ForwardPossible)
deriving instance Data (DomainType ForwardPossible)
deriving instance Data (DomainHead ForwardPossible)
deriving instance Data (DomainClassRelation ForwardPossible)
deriving instance Data (Namespace ForwardPossible)
deriving instance Data (DomainClass ForwardPossible)
deriving instance Data (Instance ForwardPossible)
deriving instance Data (Expression ForwardPossible)
deriving instance Data (UnitExpr ForwardPossible)
deriving instance Data (Model ForwardPossible)
