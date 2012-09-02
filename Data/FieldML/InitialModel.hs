{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.InitialModel (initialModel)
where

import qualified Data.Map as M
  
biSrcSpan = SrcSpan "built-in" 0 0 0 0

-- We reserve IDs 0-99 of certain counters for builtin use.
reservedIDs = 100

nsSpecial = NamespaceID 0     -- ^ Pseudo-namespace ID for 'symbols' not reachable by the user.
nsBuiltinMain = NamespaceID 1 -- ^ Namespace for builtin symbols.
nsNatural = NamespaceID 2     -- ^ Namespace 'N', for natural numbers.
nsInteger = NamespaceID 3     -- ^ Namespace 'Z', for integers.
nsBoolean = NamespaceID 4     -- ^ Namespace 'Boolean', for booleans.

dNatural = DomainID 0
dInteger = DomainID 1
dBoolean = DomainID 2

vFalse = NamedValueID 0
vTrue = NamedValueID 1

cAddable = DomainClassID 0
cSubtractable = DomainClassID 1
cHasZero = DomainClassID 2
cHasMultiplicativeIdentity = DomainClassID 3
cMultiplicable = DomainClassID 4
cDivisable = DomainClassID 5
cTrig = DomainClassID 6
cHyperbolicTrig = DomainClassID 7
cPower = DomainClassID 8
cIsClone = DomainClassID 9
cIsSubset = DomainClassID 10
cIsConnect = DomainClassID 11

uMetre = UnitID 0
uSecond = UnitID 1
uAmpere = UnitID 2
uKilogram = UnitID 3
uCandela = UnitID 4
uMole = UnitID 5
uKelvin = UnitID 6

cdfResult = 0

cvAddablePlus = 0
cvSubtractableMinus = 0
cvHasZeroZero = 0
cvHasMultiplicativeIdentityMultiplicativeIdentity = 0
cvMultiplicableTimes = 0
cvDivisibleDivide = 0
cvTrigSin = 0
cvTrigCos = 1
cvTrigTan = 2
cvHTrigSinh = 0
cvHTrigCosh = 1
cvHTrigTanh = 2

initialModel = Model {
  nextNSID = NamespaceID reservedIDs,
  nextNewDomain = NewDomainID reservedIDs,
  nextNamedValue = NamedValueID reservedIDs,
  nextDomainClass = DomainClassID reservedIDs,
  nextScopedVariable = ScopedVariable reservedIDs,
  -- The first non-reserved namespace is the toplevel one...
  toplevelNamespace = NamespaceID reservedIDs,
  allNamespaces = M.fromList [
    (nsBuiltinMain,
     Namespace {
       nsSrcSpan = biSrcSpan,
       nsNamespaces = M.fromList [("Builtin", nsBuiltinMain),
                                  ("N", nsNatural),
                                  ("Z", nsInteger),
                                  ("Boolean", nsBoolean)],
       nsDomains = M.fromList [("N", dNatural),
                               ("Z", dInteger),
                               ("Boolean", dBoolean)],
       nsValues = M.fromList [("true", vTrue), ("false", vFalse)],
       nsClasses = M.fromList [("Addable", cAddable), ("Subtractable", cSubtractable),
                               ("HasZero", cHasZero), ("HasMultiplicativeIdentity", cHasMultiplicativeIdentity),
                               ("Multiplicable", cMultiplicable), ("Divisable", cDivisable),
                               ("Trig", cTrig), ("HyperbolicTrig", cHyperbolicTrig),
                               ("Power", cPower), ("IsClone", cIsClone), ("IsConnect", cIsConnect),
                               ("IsSubset", cIsSubset)
                              ],
       nsLabels = M.empty,
       nsUnits = M.fromList [("metre", UnitRef biSrcSpan uMetre),
                             ("second", UnitRef biSrcSpan uSecond),
                             ("kilogram", UnitRef biSrcSpan uKilogram),
                             ("ampere", UnitRef biSrcSpan uAmpere),
                             ("candela", UnitRef biSrcSpan uCandela),
                             ("kelvin", UnitRef biSrcSpan uKelvin),
                             ("mole", UnitRef biSrcSpan uMole),
                             ("hertz", UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-1)),
                             ("radian", UnitDimensionless),
                             ("steradian", UnitDimensionless),
                             ("newton", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram) (UnitRef biSrcSpan uMetre))
                                                            (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-1))),
                             ("pascal", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram)
                                                                                 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (-1)))
                                                            (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-1))),
                             ("joule", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram)
                                                                                 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (2)))
                                                            (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))),
                             ("watt", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram)
                                                                                 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (2)))
                                                            (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-3))),
                             ("coulomb", UnitTimes biSrcSpan (UnitRef biSrcSpan uSecond)
                                                             (UnitRef biSrcSpan uAmpere)),
                             ("volt", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram)
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2))
                                                          (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-3))
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (-1)))),
                             ("farad", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uKilogram) (-1))
                                                                                (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2))
                                                           (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-3))
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (2)))),
                             ("ohm", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uKilogram) (1))
                                                                                (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2))
                                                           (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-3))
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (-2)))),
                             ("siemens", UnitTimes biSrcSpan (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uKilogram) (-1))
                                                                                  (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (-2)))
                                                             (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (3))
                                                                                  (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (2)))),
                             ("weber", UnitTimes biSrcSpan ((UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uKilogram) (1))
                                                                                 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (2)))
                                                            (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (-1))))),
                             ("tesla", UnitTimes biSrcSpan (UnitRef biSrcSpan uKilogram)
                                                           (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))
                                                                                (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (-1)))),
                             ("henry", UnitTimes biSrcSpan ((UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uKilogram) (1))
                                                                                 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) (2)))
                                                            (UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))
                                                                               (UnitPow biSrcSpan (UnitRef biSrcSpan uAmpere) (-2))))),
                             ("lumen", UnitRef biSrcSpan uCandela),
                             ("lux", UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2) (UnitRef biSrcSpan uCandela)),
                             ("becquerel", UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-1)),
                             ("gray", UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2)
                                                          (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))),
                             ("sievert", UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 2)
                                                             (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-2))),
                             ("katal", UnitTimes biSrcSpan (UnitPow biSrcSpan (UnitRef biSrcSpan uSecond) (-1))
                                                           (UnitRef biSrcSpan uMole)),
                             ("litre", UnitScalarMup biSrcSpan 1E-3 (UnitPow biSrcSpan (UnitRef biSrcSpan uMetre) 3))
                            ],
       nsParent = nsSpecial, -- This is ignored by the system.
       nsNextLabel = 0
       }
    ),
    (nsNatural, Namespace {
          nsSrcSpan = biSrcSpan,
          nsNamespaces = M.empty,
          nsDomains = M.empty,
          nsValues = M.empty,
          nsClasses = M.empty,
          nsLabels = M.empty, -- Natural labels 0..inf are handled elsewhere.
          nsUnits = M.empty,
          nsParent = nsBuiltinMain,
          nsNextLabel = 0, -- Infinitely many labels.
        }),
    (nsInteger, Namespace {
          nsSrcSpan = biSrcSpan,
          nsNamespaces = M.empty,
          nsDomains = M.empty,
          nsValues = M.empty,
          nsClasses = M.empty,
          nsLabels = M.empty, -- Integer labels are handled elsewhere.
          nsUnits = M.empty,
          nsParent = nsBuiltinMain,
          nsNextLabel = 0 -- Infinitely many labels.
        }),
    (nsBoolean, Namespace {
          nsSrcSpan = biSrcSpan,
          nsNamespaces = M.empty, -- Should we have Boolean.true & Boolean.false namespaces?
          nsDomains = M.empty,
          nsValues = M.empty,
          nsClasses = M.empty,
          nsLabels = M.fromList [("false", ELabel nsBoolean 0), ("true", ELabel nsBoolean 1)],
          nsUnits = M.empty,
          nsParent = nsBuiltinMain,
          nsNextLabel = 2
        })
                             ],
  allNewDomains = M.fromList [(dNatural, BuiltinDomain), (dInteger, BuiltinDomain), (dBoolean, BuiltinDomain)],
  allDomainClasses = M.fromList [
    (cAddable,
     DomainClass { classSrcSpan = biSrcSpan,
                   classKind = simpleBiClass,
                   classDomainFunctions = M.fromList [("Result", cdfResult)],
                   classValues = M.fromList
                                 [("+", (cvAddablePlus, FieldSignature (DomainVariable (ScopedVariable 0))
                                                                       (FieldSignature (DomainVariable (ScopedVariable 1))
                                                                          (EvaluateDomainFunction cAddable [ScopedVariable 0, ScopedVariable 1])
                                                                       )
                                        ))]
                 }),
    (cSubtractable,
     DomainClass {
       classSrcSpan = biSrcSpan,
       classKind = simpleBiClass,
       classDomainFunctions = M.fromList [("Result", cdfResult)],
       classValues = M.fromList [("-", (cvSubtractableMinus,
                                        FieldSignature (DomainVariable (ScopedVariable 0))
                                          (FieldSignature (DomainVariable (ScopedVariable 1))
                                             (EvaluateDomainFunction cSubtractable [ScopedVariable 0, ScopedVariable 1])
                                          )
                                       ))]
                 }),
    (cHasZero,
     DomainClass { classSrcSpan = biSrcSpan,
                   classKind = simpleUniClass,
                   classDomainFunctions = M.empty,
                   classValues = M.fromList [("zero", (cvHasZeroZero, DomainVariable (ScopedVariable 0)))]}),
    (cHasMultiplicativeIdentity,
     DomainClass { classSrcSpan = biSrcSpan,
                   classKind = simpleUniClass,
                   classDomainFunctions = M.fromList [],
                   classValues = M.fromList [("multiplicativeIdentity", (cvHasMultiplicativeIdentityMultiplicativeIdentity,
                                                                         DomainVariable (ScopedVariable 0)))]}),
    (cMultiplicable,
     DomainClass {
       classSrcSpan = biSrcSpan,
       classKind = simpleBiClass,
       classDomainFunctions = M.fromList [("Result", cdfResult)],
       classValues = M.fromList [("*", (cvMultiplicableTimes,
                                        FieldSignature (DomainVariable (ScopedVariable 0))
                                          (FieldSignature (DomainVariable (ScopedVariable 1))
                                            (EvaluateDomainFunction cMultiplicable [ScopedVariable 0, ScopedVariable 1])
                                          )
                                       ))]
       }),
    (cDivisable,
     DomainClass {
       classSrcSpan = biSrcSpan,
       classKind = simpleBiClass,
       classDomainFunctions = M.fromList [("Result", cdfResult)],
       classValues = M.fromList [("/", (cvDivisibleDivide,
                                        FieldSignature (DomainVariable (ScopedVariable 0))
                                          (FieldSignature (DomainVariable (ScopedVariable 1))
                                            (EvaluateDomainFunction cDivisible [ScopedVariable 0, ScopedVariable 1])
                                          )
                                       ))]
       }),
    (cTrig,
     DomainClass {
       classSrcSpan = biSrcSpan,
       classKind = simpleUniClass,
       classDomainFunctions = ,
       classValues =
          }
    ),
    (cHyperbolicTrig, DomainClass { classSrcSpan = biSrcSpan, classKind = , classDomainFunctions = , classValues = }),
    (cPower, DomainClass { classSrcSpan = biSrcSpan, classKind = , classDomainFunctions = , classValues = }),
    (cIsClone, DomainClass { classSrcSpan = biSrcSpan, classKind = , classDomainFunctions = , classValues = }),
    (cIsSubset, DomainClass { classSrcSpan = biSrcSpan, classKind = , classDomainFunctions = , classValues = }),
    (cIsConnect, DomainClass { classSrcSpan = biSrcSpan, classKind = , classDomainFunctions = , classValues = })
    ]
  }

simpleUniClass = ClassKind [ClassKind []]
simpleBiClass = ClassKind [ClassKind [], ClassKind []]
