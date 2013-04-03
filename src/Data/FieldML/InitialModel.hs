{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.InitialModel (initialModel, blankNamespaceContents, nsBuiltinMain, nsSpecial, nsMain, biSrcSpan, nsNatural, nsInteger, reservedIDs)
where

import qualified Data.Map as M
import Data.FieldML.Level2Structure
import qualified Data.FieldML.Level1Structure as L1

biSrcSpan :: L1.SrcSpan
biSrcSpan = L1.SrcSpan "built-in" 0 0 0 0

-- We reserve IDs 0-99 of certain counters for builtin use.
reservedIDs = 100

nsSpecial = L2NamespaceID     0 -- ^ Pseudo-namespace ID for 'symbols' not reachable by the user.
nsBuiltinMain = L2NamespaceID 1 -- ^ Namespace for builtin symbols.
nsNatural = L2NamespaceID     2 -- ^ Namespace 'N', for natural numbers.
nsInteger = L2NamespaceID     3 -- ^ Namespace 'Z', for integers.
nsBoolean = L2NamespaceID     4 -- ^ Namespace 'Boolean', for booleans.
nsMain = L2NamespaceID        5 -- ^ The top user-defined namespace.

dNatural = L2DomainID 0
dInteger = L2DomainID 1
dBoolean = L2DomainID 2

vFalse           = L2ValueID  0
vTrue            = L2ValueID  1
vUndefined       = L2ValueID  2
-- Basic arithmetic on reals.
vRealPlus        = L2ValueID  3
vRealMinus       = L2ValueID  4
vRealTimes       = L2ValueID  5
vRealDivide      = L2ValueID  6
vRealPow         = L2ValueID  7
-- Basic arithmetic on ints.
vIntPlus         = L2ValueID  8
vIntMinus        = L2ValueID  9
vIntTimes        = L2ValueID 10
vIntDivide       = L2ValueID 11
vIntPow          = L2ValueID 12
vIntMod          = L2ValueID 13
-- Real value symbols.
vRealExpE        = L2ValueID 14
vRealPi          = L2ValueID 15
vRealEulerGamma  = L2ValueID 16
-- Real trigonometry.
vRealSin         = L2ValueID 17
vRealCos         = L2ValueID 18
vRealTan         = L2ValueID 19
vRealSinh        = L2ValueID 20
vRealCosh        = L2ValueID 21
vRealTanh        = L2ValueID 22
vRealASin        = L2ValueID 23
vRealACos        = L2ValueID 24
vRealATan        = L2ValueID 25
vRealASinh       = L2ValueID 26
vRealACosh       = L2ValueID 27
vRealATanh       = L2ValueID 28
-- Logic.
vAnd             = L2ValueID 30
vOr              = L2ValueID 31
vXor             = L2ValueID 32
vNot             = L2ValueID 33
-- Comparisons
vRealLess        = L2ValueID 34
vRealLessEqual   = L2ValueID 35
vIntLess         = L2ValueID 36
vIntLessEqual    = L2ValueID 37
-- Casts...
vUnsafeCastUnits = L2ValueID 40

cAllTypes   = L2ClassID 0
cIsClone = L2ClassID 1
cIsSubset = L2ClassID 2

cvEquals         = L2ClassValueID 0
cvAsClone        = L2ClassValueID 1
cvFromClone      = L2ClassValueID 2
cvUnsafeToSubset = L2ClassValueID 3
cvFromSubset     = L2ClassValueID 4

dfClonedFrom     = L2DomainFunctionID 0
dfSubsetFrom     = L2DomainFunctionID 1


sdidATParam = L2ScopedDomainID "a" 0
sdidAny = L2ScopedDomainID "a" 1

suidAny1 = L2ScopedUnitID "u" 0
suidAny2 = L2ScopedUnitID "v" 1


blankNamespaceContents :: SrcSpan -> L2NamespaceID -> L2NamespaceContents
blankNamespaceContents ss p =
  L2NamespaceContents {
    l2nsSrcSpan = ss, l2nsNamespaces = M.empty,
    l2nsDomains = M.empty, l2nsNamedValues = M.empty,
    l2nsClassValues = M.empty, l2nsUnits = M.empty,
    l2nsClasses = M.empty, l2nsDomainFunctions = M.empty,
    l2nsLabels = M.empty, l2nsParent = p, l2nsNextLabel = 0
    }

rdim :: L2DomainExpression
rdim = L2DomainExpressionReal biSrcSpan (L2UnitExDimensionless biSrcSpan)

initialModel = L2Model {
  l2ToplevelNamespace = nsMain,
  l2AllNamespaces = M.fromList [
    (nsBuiltinMain,
     (blankNamespaceContents biSrcSpan nsSpecial) {
       l2nsNamespaces = M.fromList [("Builtin", nsBuiltinMain),
                                    ("N", nsNatural),
                                    ("Z", nsInteger),
                                    ("Boolean", nsBoolean)],
       l2nsDomains = M.fromList [("N", L2DomainReference biSrcSpan dNatural),
                                 ("Z", L2DomainReference biSrcSpan dInteger),
                                 ("Boolean", L2DomainReference biSrcSpan dBoolean)],
       l2nsNamedValues = M.fromList [("true", vTrue), ("false", vFalse), ("undefined", vUndefined),
                                     ("realPlus", vRealPlus), ("realMinus", vRealMinus),
                                     ("realTimes", vRealTimes), ("realDivide", vRealDivide),
                                     ("realPow", vRealPow), ("intPlus", vIntPlus),
                                     ("intMinus", vIntMinus), ("intTimes", vIntTimes),
                                     ("intDivide", vIntDivide), ("intPow", vIntPow),
                                     ("intMod", vIntMod),
                                     ("realExpE", vRealExpE), ("realPi", vRealPi),
                                     ("realEulerGamma", vRealEulerGamma), ("sin", vRealSin),
                                     ("cos", vRealCos), ("tan", vRealTan), ("sinh", vRealSinh),
                                     ("cosh", vRealCosh), ("tanh", vRealTanh), ("asin", vRealASin),
                                     ("acos", vRealACos), ("atan", vRealATan), ("asinh", vRealASinh),
                                     ("acosh", vRealACosh), ("atanh", vRealATanh),
                                     ("&&", vAnd), ("||", vOr), ("xor", vXor), ("not", vNot),
                                     ("realLess", vRealLess), ("realLessEqual", vRealLessEqual),
                                     ("intLess", vIntLess), ("intLessEqual", vIntLessEqual),
                                     ("unsafeCastUnits", vUnsafeCastUnits)],
       l2nsClasses = M.fromList [("AllTypes", cAllTypes),
                                 ("IsClone", cIsClone),
                                 ("IsSubset", cIsSubset)],
       l2nsClassValues = M.fromList [("==", cvEquals)],
       l2nsLabels = M.fromList [("false", L2Label nsBoolean 0), ("true", L2Label nsBoolean 1)]
       }
    ),
    (nsNatural, blankNamespaceContents biSrcSpan nsBuiltinMain {-
          l2nsLabels = M.empty, -- Natural labels 0..inf are handled elsewhere.
          l2nsNextLabel = 0 -- Infinitely many labels.
          -}),
    (nsInteger, blankNamespaceContents biSrcSpan nsBuiltinMain {-
          l2nsLabels = M.empty, -- Integer labels are handled elsewhere.
          l2nsNextLabel = 0 -- Infinitely many labels.
        -}),
    (nsBoolean, (blankNamespaceContents biSrcSpan nsBuiltinMain) {
          l2nsNamespaces = M.empty, -- Should we have Boolean.true & Boolean.false namespaces?
          l2nsLabels = M.fromList [("false", L2Label nsBoolean 0), ("true", L2Label nsBoolean 1)],
          l2nsNextLabel = 2
        }),
    (nsMain, blankNamespaceContents biSrcSpan nsBuiltinMain)
                             ],
  l2NextNamespace = L2NamespaceID reservedIDs,
  l2AllDomains = M.fromList [(dNatural, L2BuiltinDomainContents biSrcSpan "naturals"),
                             (dInteger, L2BuiltinDomainContents biSrcSpan "integer"),
                             (dBoolean, L2BuiltinDomainContents biSrcSpan "boolean")],
  l2NextDomain = L2DomainID reservedIDs,
  l2AllValues = M.fromList [(vFalse, L2ValueContents biSrcSpan $ Just $ L2DomainReference biSrcSpan dBoolean),
                            (vTrue,  L2ValueContents biSrcSpan $ Just $ L2DomainReference biSrcSpan dBoolean),
                            (vUndefined, L2ValueContents biSrcSpan $ Just $ L2DomainVariableRef biSrcSpan sdidAny),
                            (vRealPlus, L2ValueContents biSrcSpan $ 
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim
                                                 (L2DomainExpressionFieldSignature biSrcSpan rdim rdim)
                            ),
                            (vRealMinus, L2ValueContents biSrcSpan $ Just $
                                           L2DomainExpressionFieldSignature biSrcSpan rdim
                                             (L2DomainExpressionFieldSignature biSrcSpan rdim rdim)
                            ),
                            (vRealTimes, L2ValueContents biSrcSpan $ Just $
                                           L2DomainExpressionFieldSignature biSrcSpan rdim
                                                 (L2DomainExpressionFieldSignature biSrcSpan rdim rdim)
                            ),
                            (vRealDivide, L2ValueContents biSrcSpan $
                                            Just $ L2DomainExpressionFieldSignature biSrcSpan rdim
                                                 (L2DomainExpressionFieldSignature biSrcSpan rdim rdim)
                            ),
                            (vRealPow, L2ValueContents biSrcSpan $
                                         Just $ L2DomainExpressionFieldSignature biSrcSpan rdim
                                                 (L2DomainExpressionFieldSignature biSrcSpan rdim rdim)
                            ),
                            (vIntPlus, L2ValueContents biSrcSpan $
                                         Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger)
                                                 (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dInteger))
                            ),
                            (vIntMinus, L2ValueContents biSrcSpan $
                                          Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger)
                                                 (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dInteger))
                            ),
                            (vIntTimes, L2ValueContents biSrcSpan $
                                          Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger)
                                                 (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dInteger))
                            ),
                            (vIntDivide, L2ValueContents biSrcSpan $
                                           Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger)
                                                 (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dInteger))
                            ),
                            (vIntPow, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger)
                                                 (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dInteger))
                            ),
                            (vRealExpE, L2ValueContents biSrcSpan $ Just $ rdim),
                            (vRealPi, L2ValueContents biSrcSpan $ Just $ rdim),
                            (vRealEulerGamma, L2ValueContents biSrcSpan $ Just $ rdim),
                            (vRealSin, L2ValueContents biSrcSpan $
                                       Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealCos, L2ValueContents biSrcSpan $
                                       Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealTan, L2ValueContents biSrcSpan $
                                       Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealSinh, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealCosh, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealTanh, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealASin, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealACos, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealATan, L2ValueContents biSrcSpan $
                                        Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealASinh, L2ValueContents biSrcSpan $
                                         Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealACosh, L2ValueContents biSrcSpan $
                                         Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vRealATanh, L2ValueContents biSrcSpan $
                                         Just $ L2DomainExpressionFieldSignature biSrcSpan rdim rdim),
                            (vAnd, L2ValueContents biSrcSpan $
                                   Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean)
                                   (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean) (L2DomainReference biSrcSpan dBoolean))),
                            (vOr, L2ValueContents biSrcSpan $
                                  Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean)
                                   (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean) (L2DomainReference biSrcSpan dBoolean))),
                            (vXor, L2ValueContents biSrcSpan $
                                   Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean)
                                   (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean) (L2DomainReference biSrcSpan dBoolean))),
                            (vNot, L2ValueContents biSrcSpan $
                                   Just $ L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dBoolean) (L2DomainReference biSrcSpan dBoolean)),
                            (vRealLess, L2ValueContents biSrcSpan $ Just $
                                        L2DomainExpressionFieldSignature biSrcSpan rdim (L2DomainExpressionFieldSignature biSrcSpan rdim (L2DomainReference biSrcSpan dBoolean))
                            ),
                            (vRealLessEqual, L2ValueContents biSrcSpan $ Just $
                                        L2DomainExpressionFieldSignature biSrcSpan rdim (L2DomainExpressionFieldSignature biSrcSpan rdim (L2DomainReference biSrcSpan dBoolean))
                            ),
                            (vIntLess, L2ValueContents biSrcSpan $ Just $
                                        L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dBoolean))
                            ),
                            (vIntLessEqual, L2ValueContents biSrcSpan $ Just $
                                        L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainExpressionFieldSignature biSrcSpan (L2DomainReference biSrcSpan dInteger) (L2DomainReference biSrcSpan dBoolean))
                            ),
                            (vUnsafeCastUnits, L2ValueContents biSrcSpan $ Just $
                                               (L2DomainExpressionLambda biSrcSpan [] [suidAny1, suidAny2] [] [] []
                                         (L2DomainExpressionFieldSignature biSrcSpan (L2DomainExpressionReal biSrcSpan (L2UnitScopedVar biSrcSpan suidAny1)) (L2DomainExpressionReal biSrcSpan (L2UnitScopedVar biSrcSpan suidAny2)))))
                           ],
  l2NextValue = L2ValueID reservedIDs,
  l2AllAssertions = [],
  l2AllBaseUnits = M.empty, -- Should SI units come built-in?
  l2NextBaseUnits = L2BaseUnitsID reservedIDs,
  l2AllClasses = M.fromList [(cAllTypes,
                              L2ClassContents biSrcSpan [(sdidATParam, L1.Kind [] [])] M.empty
                                                 (M.fromList [("==", cvEquals)])
                             ),
                             (cIsClone,
                              L2ClassContents biSrcSpan [(sdidAny, L1.Kind [] [])]
                                              (M.fromList [("ClonedFrom", dfClonedFrom)])
                                              (M.fromList [("asClone", cvAsClone),
                                                           ("fromClone", cvFromClone)])
                             ),
                             (cIsSubset,
                              L2ClassContents biSrcSpan [(sdidAny, L1.Kind [] [])]
                                              (M.fromList [("SubsetFrom", dfSubsetFrom)])
                                              (M.fromList [("unsafeToSubset", cvUnsafeToSubset),
                                                           ("fromClone", cvFromSubset)])
                             )],
  l2NextClassID = L2ClassID reservedIDs,
  l2AllInstances = [],
  l2NextScopedValueID = L2ScopedValueID reservedIDs,
  l2NextScopedUnitID = L2ScopedUnitID "!unnamed" reservedIDs,
  l2NextScopedDomainID = L2ScopedDomainID "!unnamed" reservedIDs,
  l2AllDomainFunctions = M.fromList [(dfClonedFrom, L2DomainFunctionContents biSrcSpan 1),
                                     (dfSubsetFrom, L2DomainFunctionContents biSrcSpan 1)],
  l2NextDomainFunctionID = L2DomainFunctionID reservedIDs,
  l2AllClassValues = M.fromList [(cvEquals, L2ClassValueContents biSrcSpan
                                              (L2DomainExpressionFieldSignature biSrcSpan
                                               (L2DomainVariableRef biSrcSpan sdidATParam)
                                               (L2DomainExpressionFieldSignature biSrcSpan
                                                (L2DomainVariableRef biSrcSpan sdidATParam)
                                                (L2DomainReference biSrcSpan dBoolean)
                                               ))),
                                 (cvAsClone, L2ClassValueContents biSrcSpan
                                              (L2DomainExpressionFieldSignature biSrcSpan
                                                (L2DomainFunctionEvaluate biSrcSpan dfClonedFrom
                                                 [L2DomainVariableRef biSrcSpan sdidAny])
                                                (L2DomainVariableRef biSrcSpan sdidAny))),
                                 (cvFromClone, L2ClassValueContents biSrcSpan
                                                (L2DomainExpressionFieldSignature biSrcSpan
                                                 (L2DomainVariableRef biSrcSpan sdidAny)
                                                 (L2DomainFunctionEvaluate biSrcSpan dfClonedFrom
                                                  [L2DomainVariableRef biSrcSpan sdidAny]))),
                                 (cvUnsafeToSubset,
                                  L2ClassValueContents biSrcSpan
                                    (L2DomainExpressionFieldSignature biSrcSpan
                                     (L2DomainFunctionEvaluate biSrcSpan dfSubsetFrom
                                      [L2DomainVariableRef biSrcSpan sdidAny])
                                     (L2DomainVariableRef biSrcSpan sdidAny))),
                                 (cvFromSubset, L2ClassValueContents biSrcSpan
                                                (L2DomainExpressionFieldSignature biSrcSpan
                                                 (L2DomainVariableRef biSrcSpan sdidAny)
                                                 (L2DomainFunctionEvaluate biSrcSpan dfSubsetFrom
                                                  [L2DomainVariableRef biSrcSpan sdidAny])))
                                 ],
  l2NextClassValueID = L2ClassValueID reservedIDs
  }
