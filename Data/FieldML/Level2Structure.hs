{-# LANGUAGE DeriveDataTypeable #-}
module Data.FieldML.Level2Structure
where

import qualified Data.FieldML.Level1Structure as L1
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.Data

type Identifier = BS.ByteString
type SrcSpan = L1.SrcSpan
type L2Kind = L1.L1Kind

newtype L2NamespaceID = L2NamespaceID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2DomainID = L2DomainID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ValueID = L2ValueID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2BaseUnitsID = L2BaseUnitsID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ClassID = L2ClassID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ScopedDomainID = L2ScopedDomainID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ScopedValueID = L2ScopedValueID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ScopedUnitID = L2ScopedUnitID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2DomainFunctionID = L2DomainFunctionID Int deriving (Eq, Ord, Data, Typeable, Show)
newtype L2ClassValueID = L2ClassValueID Int deriving (Eq, Ord, Data, Typeable, Show)

data L2Model =
  L2Model
  {
    l2ToplevelNamespace :: L2NamespaceID,
    l2AllNamespaces :: M.Map L2NamespaceID L2NamespaceContents,
    l2NextNamespace :: L2NamespaceID,
    l2AllDomains :: M.Map L2DomainID L2DomainContents,
    l2NextDomain :: L2DomainID,
    l2AllValues :: M.Map L2ValueID L2ValueContents,
    l2NextValue :: L2ValueID,
    l2AllAssertions :: [L2Expression],
    l2AllBaseUnits :: M.Map L2BaseUnitsID L2BaseUnitContents,
    l2NextBaseUnits :: L2BaseUnitsID,
    l2AllClasses :: M.Map L2ClassID L2ClassContents,
    l2NextClassID :: L2ClassID,
    l2AllInstances :: [L2InstanceContents],
    l2NextScopedDomainID :: L2ScopedDomainID,
    l2NextScopedValueID :: L2ScopedValueID,
    l2AllDomainFunctions :: M.Map L2DomainFunctionID L2DomainFunctionContents,
    l2NextDomainFunctionID :: L2DomainFunctionID,
    l2AllClassValues :: M.Map L2ClassValueID L2ClassValueContents,
    l2NextClassValueID :: L2ClassValueID
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data L2NamespaceContents =
  L2NamespaceContents
  {
    l2nsSrcSpan :: SrcSpan,
    l2nsNamespaces :: M.Map Identifier L2NamespaceID,
    l2nsDomains :: M.Map Identifier L2DomainType,
    l2nsNamedValues :: M.Map Identifier L2ValueID,
    l2nsClassValues :: M.Map Identifier L2ClassValueID,
    l2nsUnits :: M.Map Identifier L2UnitExpression,
    l2nsClasses :: M.Map Identifier L2ClassID,
    l2nsDomainFunctions :: M.Map Identifier L2DomainFunctionID,
    l2nsLabels :: M.Map Identifier L2Label,
    l2nsParent :: L2NamespaceID,
    l2nsNextLabel :: Int
  }
  deriving (Eq, Ord, Data, Typeable, Show)

data L2DomainContents =
  L2ClonelikeDomainContents {
      l2DomainSS :: SrcSpan,
      l2DomainCloneOf :: L2DomainType,
      l2DomainCloneTypeSpecific :: L2DomainCloneType
    } |
  L2BuiltinDomainContents {
    l2DomainSS :: SrcSpan,
    l2BuiltinDomainIdentifier :: BS.ByteString
    }
  deriving (Eq, Ord, Show, Data, Typeable)

data L2DomainCloneType = L2DomainClone |
                         L2DomainSubset { l2DomainSubsetUsing :: L2Expression } |
                         L2DomainConnect { l2DomainConnectUsing :: L2Expression }
                       deriving (Eq, Ord, Show, Data, Typeable)

data L2ValueContents = L2ValueContents {
    l2ValueSS :: SrcSpan,
    l2ValueType :: Maybe L2DomainType
  } deriving (Eq, Ord, Show, Data, Typeable)

data L2BaseUnitContents = L2BaseUnitContents SrcSpan deriving (Eq, Ord, Show, Data, Typeable)

data L2ClassContents = L2ClassContents {
  l2ClassSS :: SrcSpan,
  l2ClassParameters :: [(L2ScopedDomainID, L2Kind)],
  l2ClassDomainFunctions :: [L2DomainFunctionID],
  l2ClassValues :: [L2ClassValueID]
  }
                     deriving (Eq, Ord, Show, Data, Typeable)

data L2InstanceContents = L2InstanceContents {
  l2InstanceSS :: SrcSpan,
  l2InstanceOfClass :: L2ClassID,
  l2InstanceClassArguments :: [L2DomainType],
  l2InstanceDomainFunctions :: [(L2DomainFunctionID, [L2DomainType], L2DomainExpression)],
  l2InstanceValues :: [L2Expression]
  }
                     deriving (Eq, Ord, Show, Data, Typeable)

data L2Expression = L2ExApply { l2ExSS :: SrcSpan, l2ExOp :: L2Expression, l2ExArg :: L2Expression } |
                    L2ExReferenceLabel { l2ExSS :: SrcSpan, l2ExLabel :: L2Label } |
                    L2ExReferenceValue { l2ExSS :: SrcSpan, l2ExValueID :: L2ValueID } |
                    L2ExReferenceClassValue { l2ExSS :: SrcSpan, l2ExClassValue :: L2ClassValueID } |
                    L2ExBoundVariable { l2ExSS :: SrcSpan, l2ExBvar :: L2ScopedValueID } |
                    L2ExLiteralReal { l2ExSS :: SrcSpan,
                                      l2ExUnits :: L2UnitExpression, l2ExRealValue :: Double } |
                    L2ExMkProduct { l2ExSS :: SrcSpan, l2ExValues :: [(L2Label, L2Expression)] } |
                    L2ExMkUnion { l2ExSS :: SrcSpan, l2ExLabel :: L2Label, l2ExValue :: L2Expression } |
                    L2ExProject { l2ExSS :: SrcSpan, l2ExLabel :: L2Label } |
                    L2ExAppend { l2ExSS :: SrcSpan, l2ExLabel :: L2Label } |
                    L2ExLambda { l2ExSS :: SrcSpan, l2ExBvar :: L2ScopedValueID,
                                 l2ExValue :: L2Expression } |
                    L2ExCase { l2ExSS :: SrcSpan, l2ExExpr :: L2Expression,
                               l2ExValues :: [(L2Label, L2Expression)]} |
                    L2ExLet { l2ExSS :: SrcSpan, l2ExExpr :: L2Expression, l2ExClosure :: L2NamespaceContents } |
                    L2ExString { l2ExSS :: SrcSpan, l2ExStringValue :: BS.ByteString } |
                    L2ExSignature { l2ExSS :: SrcSpan, l2ExExpression :: L2Expression,
                                    l2ExSignature :: L2DomainExpression }
                      deriving (Eq, Ord, Show, Data, Typeable)

data L2UnitExpression = L2UnitExDimensionless { l2UnitExSS :: SrcSpan } |
                        L2UnitExRef { l2UnitExSS :: SrcSpan, l2UnitExRef :: L2BaseUnitsID } |
                        L2UnitExTimes { l2UnitExSS :: SrcSpan, l2UnitExUnits1 :: L2UnitExpression,
                                        l2UnitExUnits2 :: L2UnitExpression } |
                        L2UnitPow { l2UnitExSS :: SrcSpan, l2UnitExUnits :: L2UnitExpression,
                                    l2UnitExPower :: Double } |
                        L2UnitScalarMup { l2UnitExSS :: SrcSpan, l2UnitExScalar :: Double, 
                                          l2UnitExUnits :: L2UnitExpression } |
                        L2UnitScopedVar { l2UnitExSS :: SrcSpan, l2UnitExScoped :: L2ScopedUnitID }
                      deriving (Eq, Ord, Show, Data, Typeable)

data L2DomainType = L2DomainType { l2DomainTypeSS :: SrcSpan,
                                   l2DomainTypeUnitConstraints :: [(L2UnitExpression, L2UnitExpression)],
                                   l2DomainTypeDomainEqualities :: [(L2DomainExpression, L2DomainExpression)],
                                   l2DomainTypeDomainRelations :: [(L2ClassID, [L2DomainExpression])],
                                   l2DomainTypeExpression :: L2DomainExpression }
                    deriving (Eq, Ord, Show, Data, Typeable)

data L2DomainExpression = L2DomainExpressionProduct { l2DomainExpressionSS :: SrcSpan,
                                                      l2DomainExpressionLabels :: L2LabelledDomains } |
                          L2DomainExpressionDisjointUnion { l2DomainExpressionSS :: SrcSpan,
                                                            l2DomainExpressionLabels :: L2LabelledDomains } |
                          L2DomainExpressionFieldSignature {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainExpressionDomain :: L2DomainExpression,
                            l2DomainExpressionCodomain :: L2DomainExpression } |
                          L2DomainExpressionReal {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainExpressionUnits :: L2UnitExpression } |
                          L2DomainExpressionApply {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainExpressionDomain :: L2DomainExpression, 
                            l2DomainExpressionScopedVarName :: L2ScopedDomainID,
                            l2DomainExpressionValue :: L2DomainExpression } |
                          L2DomainFunctionEvaluate {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainExpressionFunction :: L2DomainFunctionID,
                            l2DomainExpressionArguments :: [L2DomainExpression]
                            } |
                          L2DomainVariableRef {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainVariable :: L2ScopedDomainID
                            } |
                          L2DomainReference {
                            l2DomainExpressionSS :: SrcSpan,
                            l2DomainExpressionRef :: L2DomainID }
                          deriving (Eq, Ord, Show, Data, Typeable)

data L2LabelledDomains = L2LabelledDomains [(L2Label, L2DomainExpression)]
                       deriving (Eq, Ord, Show, Data, Typeable)

data L2DomainFunctionContents = L2DomainFunctionContents SrcSpan L2DomainType deriving (Eq, Ord, Show, Data, Typeable)
data L2ClassValueContents = L2ClassValueContents SrcSpan L2DomainType deriving (Eq, Ord, Show, Data, Typeable)
data L2Label = L2Label { l2LabelEnsemble :: L2NamespaceID, l2LabelValue :: Integer } deriving (Eq, Ord, Show, Data, Typeable)
