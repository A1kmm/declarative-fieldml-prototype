{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances, GADTs  #-}
module Data.FieldML.StructureNoForwardInstances () where

import Data.Typeable
import Data.Data
import Data.FieldML.Structure

instance Data t => Data (OrForward t ()) where
  gfoldl k z (OFKnown a1) = (z OFKnown `k` a1)
  gunfold k z c = case constrIndex c of
    _ -> k (z OFKnown)
  toConstr _ = cOFKnown
  dataTypeOf _ = tOrForward
  dataCast2 f = gcast2 f
  
tOrForward :: Data.Data.DataType
tOrForward =
  mkDataType
    "Data.FieldML.Structure.OrForward"
    [cOFKnown]

cOFKnown :: Data.Data.Constr
cOFKnown = mkConstr tOrForward
             "OFKnown" [] Prefix

deriving instance Data (ELabel ())
deriving instance Data (NewDomain ())
deriving instance Data (CloneAnnotation ())
deriving instance Data (DomainExpression ())
deriving instance Data (DomainType ())
deriving instance Data (DomainHead ())
deriving instance Data (DomainClassRelation ())
deriving instance Data (Namespace ())
deriving instance Data (DomainClass ())
deriving instance Data (Instance ())
deriving instance Data (Expression ())
deriving instance Data (UnitExpr ())
deriving instance Data (Model ())
