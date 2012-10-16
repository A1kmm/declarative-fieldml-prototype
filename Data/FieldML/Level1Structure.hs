{-# LANGUAGE DeriveDataTypeable #-}
module Data.FieldML.Level1Structure
where

import qualified Data.ByteString.Lazy as LBS
import Data.Data

data SrcSpan = SrcSpan {
    srcFile :: String,
    srcStartRow :: Int,
    srcStartColumn :: Int,
    srcEndRow :: Int,
    srcEndColumn :: Int
  } deriving (Eq, Ord, Data, Typeable, Show)

-- Level 1 structure gives the raw structure of a model file, prior to any
-- attempt to resolve symbols whatsoever.

newtype L1NamespaceContents = L1NamespaceContents [L1NamespaceStatement] deriving (Eq, Ord, Data, Typeable, Show)
data L1NamespaceStatement =
  L1NSImport { l1nsSS :: SrcSpan,
               l1nsImportFrom :: (Maybe LBS.ByteString),
               l1nsImportPath :: L1RelOrAbsPath,
               l1nsImportWhat :: Maybe [L1Identifier],
               l1nsImportHiding :: Maybe [L1Identifier],
               l1nsImportAs :: Maybe L1Identifier
             } |
  L1NSNamespace { l1nsSS :: SrcSpan,
                  l1nsNamespaceName :: L1Identifier,
                  l1nsNamespaceContents :: L1NamespaceContents
                } |
  L1NSDomain { l1nsSS :: SrcSpan,
               l1nsDomainName :: L1Identifier,
               l1nsDomainParameters :: [L1ScopedID],
               l1nsDomainDefinition :: L1DomainDefinition,
               l1nsNamespaceContents :: L1NamespaceContents
             } |
  L1NSAssertion { l1nsSS :: SrcSpan,
                  l1nsExpression :: L1Expression
                } |
  L1NSNamedValue { l1nsSS :: SrcSpan,
                   l1nsValueName :: L1Identifier,
                   l1nsDomainType :: Maybe L1DomainType
                 } |
  L1NSClass { l1nsSS :: SrcSpan,
              l1nsClassName :: L1Identifier,
              l1nsClassParameters :: [(L1ScopedID, L1Kind)],
              l1nsClassDomainFunctions :: [(L1Identifier, Int)],
              l1nsClassValues :: [(L1Identifier, L1DomainType)]
            } |
  L1NSEnsemble { l1nsSS :: SrcSpan,
                 l1nsLabels :: [L1Identifier], 
                 l1nsAs :: Maybe L1Identifier } |
  L1NSUnit { l1nsSS :: SrcSpan,
             l1nsUnitNmae :: L1Identifier,
             l1nsUnitDefinition :: L1UnitDefinition } |
  L1NSInstance { l1nsSS :: SrcSpan,
                 l1nsInstanceOfClass :: L1RelOrAbsPath,
                 l1nsClassArguments :: [L1DomainType],
                 l1nsInstanceDomainFunctions :: [(L1Identifier, [L1DomainType], L1DomainExpression)],
                 l1nsInstanceValues :: [L1Expression]
               }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | One entry for each parameter, parameter is L1Kind [] if it doesn't itself have parameters.
data L1Kind = L1Kind [L1Kind] deriving (Eq, Ord, Data, Typeable, Show)

data L1DomainDefinition = L1CloneDomain { l1DomainDefSS :: SrcSpan, l1DomainDefType :: L1DomainType } |
                          L1SubsetDomain { l1DomainDefSS :: SrcSpan,
                                           l1DomainDefType :: L1DomainType,
                                           l1DomainDefUsing :: L1Expression } |
                          L1ConnectDomain { l1DomainDefSS :: SrcSpan,
                                            l1DomainDefType :: L1DomainType,
                                            l1DomainDefUsing :: L1Expression } |
                          L1DomainDefDomainType { l1DomainDefSS :: SrcSpan,
                                                  l1DomainDefType :: L1DomainType }
                          deriving (Eq, Ord, Show, Data, Typeable)

data L1DomainType = L1DomainType { l1DomainTypeSS :: SrcSpan, l1DomainTypeHead :: [L1DomainClassRelation],
                                   l1DomainTypeExpression ::L1DomainExpression }
                          deriving (Eq, Ord, Show, Data, Typeable)
data L1DomainClassRelation = L1DCRUnitConstraint { l1DCRExpr1 :: L1UnitExpression,
                                                   l1DCRExpr2 :: L1UnitExpression } |
                             L1DCREquality { l1DCRType1 :: L1DomainExpression,
                                             l1DCRType2 :: L1DomainExpression } |
                             L1DCRRelation { l1DCRClass :: L1RelOrAbsPath,
                                             l1DCRArguments :: [L1DomainExpression] }
                             deriving (Eq, Ord, Show, Data, Typeable)

data L1DomainExpression = L1DomainExpressionProduct { l1DomainExpressionSS :: SrcSpan,
                                                      l1DomainExpressionLabels :: L1LabelledDomains } |
                          L1DomainExpressionDisjointUnion { l1DomainExpressionSS :: SrcSpan,
                                                            l1DomainExpressionLabels :: L1LabelledDomains } |
                          L1DomainExpressionFieldSignature {
                            l1DomainExpressionSS :: SrcSpan,
                            l1DomainExpressionDomain :: L1DomainExpression,
                            l1DomainExpressionCodomain :: L1DomainExpression } |
                          L1DomainExpressionReal {
                            l1DomainExpressionSS :: SrcSpan,
                            l1DomainExpressionUnits :: L1UnitExpression } |
                          L1DomainExpressionApply {
                            l1DomainExpressionSS :: SrcSpan,
                            l1DomainExpressionDomain :: L1DomainExpression, 
                            l1DomainExpressionScopedVarName :: L1ScopedID,
                            l1DomainExpressionValue :: L1DomainExpression } |
                          L1DomainFunctionEvaluate {
                            l1DomainExpressionSS :: SrcSpan,
                            l1DomainExpressionFunction :: L1RelOrAbsPath,
                            l1DomainExpressionArguments :: [L1DomainExpression]
                            } |
                          L1DomainVariableRef {
                            l1DomainExpressionSS :: SrcSpan,
                            l1DomainVariable :: L1ScopedID
                                           } |
                          L1DomainReference {
                            l1DomainExpressionSS :: SrcSpan,
                            -- Note: The IntEnd case is a parsing convenience -
                            -- if it survives into the model, it is an error.
                            l1DomainExpressionRef :: L1RelOrAbsPathPossiblyIntEnd }
                          deriving (Eq, Ord, Show, Data, Typeable)

data L1LabelledDomains = L1LabelledDomains [(L1RelOrAbsPathPossiblyIntEnd, L1DomainExpression)]
                       deriving (Eq, Ord, Show, Data, Typeable)
data L1UnitExpression = L1UnitExDimensionless { l1UnitExSS :: SrcSpan } |
                        L1UnitExRef { l1UnitExSS :: SrcSpan, l1UnitExRef :: L1RelOrAbsPath } |
                        L1UnitExTimes { l1UnitExSS :: SrcSpan, l1UnitExUnits1 :: L1UnitExpression,
                                        l1UnitExUnits2 :: L1UnitExpression } |
                        L1UnitPow { l1UnitExSS :: SrcSpan, l1UnitExUnits :: L1UnitExpression,
                                    l1UnitExPower :: Double } |
                        L1UnitScalarMup { l1UnitExSS :: SrcSpan, l1UnitExScalar :: Double, 
                                          l1UnitExUnits :: L1UnitExpression } |
                        L1UnitScopedVar { l1UnitExSS :: SrcSpan, l1UnitExScoped :: L1ScopedID }
                      deriving (Eq, Ord, Show, Data, Typeable)

data L1Expression = L1ExApply { l1ExSS :: SrcSpan,
                                l1ExOp :: L1Expression,
                                l1ExArg :: L1Expression } |
                    L1ExReference { l1ExSS :: SrcSpan,
                                    l1ExIdentifier :: L1RelOrAbsPathPossiblyIntEnd } |
                    L1ExBoundVariable { l1ExSS :: SrcSpan,
                                        l1ExScoped :: L1ScopedID } |
                    L1ExLiteralReal { l1ExSS :: SrcSpan,
                                      l1ExUnits :: L1UnitExpression,
                                      l1ExRealValue :: Double } |
                    L1ExMkProduct { l1ExSS :: SrcSpan,
                                    l1ExValues :: [(L1RelOrAbsPathPossiblyIntEnd, L1Expression)] } |
                    L1ExMkUnion { l1ExSS :: SrcSpan,
                                  l1ExLabel :: L1RelOrAbsPathPossiblyIntEnd,
                                  l1ExValue :: L1Expression } |
                    L1ExProject { l1ExSS :: SrcSpan,
                                  l1ExLabel :: L1RelOrAbsPathPossiblyIntEnd } |
                    L1ExAppend { l1ExSS :: SrcSpan,
                                 l1ExLabel :: L1RelOrAbsPathPossiblyIntEnd } |
                    L1ExLambda { l1ExSS :: SrcSpan,
                                 l1ExBvar :: L1ScopedID,
                                 l1ExValue :: L1Expression } |
                    L1ExCase { l1ExSS :: SrcSpan,
                               l1ExExpr :: L1Expression,
                               l1ExValues :: [(L1RelOrAbsPathPossiblyIntEnd, L1Expression)] } |
                    L1ExLet { l1ExSS :: SrcSpan,
                               l1ExExpr :: L1Expression,
                               l1ExClosure :: L1NamespaceContents }
                  deriving (Eq, Ord, Show, Data, Typeable)

data L1UnitDefinition = L1UnitDefNewBase { l1UnitDefSS :: SrcSpan } |
                        L1UnitDefUnitExpr { l1UnitDefSS :: SrcSpan, l1UnitDefUnitExpr :: L1UnitExpression}
                        deriving (Eq, Ord, Data, Typeable, Show)

data L1RelOrAbsPath = L1RelOrAbsPath SrcSpan Bool L1RelPath deriving (Eq, Ord, Data, Typeable, Show)
data L1RelOrAbsPathPossiblyIntEnd = L1RelOrAbsPathNoInt { l1RelOrAbsPIESS :: SrcSpan, l1RelOrAbsPIEIsAbs :: Bool, 
                                                          l1RelOrAbsPIERelPath :: L1RelPath }
                                  | L1RelOrAbsPathInt { l1RelOrAbsPIESS :: SrcSpan, l1RelOrAbsPIEIsAbs :: Bool,
                                                        l1RelOrAbsPIERelPath :: L1RelPath, 
                                                        l1RelOrAbsPIEInt :: Int } deriving (Eq, Ord, Data, Typeable, Show)
data L1RelPath = L1RelPath { l1RelPathSS :: SrcSpan, l1RelPathIDs :: [L1Identifier] } deriving (Eq, Ord, Data, Typeable, Show)
data L1Identifier = L1Identifier { l1IdSS :: SrcSpan, l1IdBS :: LBS.ByteString } deriving (Eq, Ord, Data, Typeable, Show)
data L1ScopedID = L1ScopedID { l1ScopedIdSS :: SrcSpan, l1ScopedIdBS :: LBS.ByteString } deriving (Eq, Ord, Data, Typeable, Show)
