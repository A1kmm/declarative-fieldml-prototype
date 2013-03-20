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

nsSpecial = L2NamespaceID 0     -- ^ Pseudo-namespace ID for 'symbols' not reachable by the user.
nsBuiltinMain = L2NamespaceID 1 -- ^ Namespace for builtin symbols.
nsNatural = L2NamespaceID 2     -- ^ Namespace 'N', for natural numbers.
nsInteger = L2NamespaceID 3     -- ^ Namespace 'Z', for integers.
nsBoolean = L2NamespaceID 4     -- ^ Namespace 'Boolean', for booleans.
nsMain = L2NamespaceID 5        -- ^ The top user-defined namespace.

dNatural = L2DomainID 0
dInteger = L2DomainID 1
dBoolean = L2DomainID 2

vFalse = L2ValueID 0
vTrue = L2ValueID 1
vUndefined = L2ValueID 2

blankNamespaceContents :: SrcSpan -> L2NamespaceID -> L2NamespaceContents
blankNamespaceContents ss p =
  L2NamespaceContents {
    l2nsSrcSpan = ss, l2nsNamespaces = M.empty,
    l2nsDomains = M.empty, l2nsNamedValues = M.empty,
    l2nsClassValues = M.empty, l2nsUnits = M.empty,
    l2nsClasses = M.empty, l2nsDomainFunctions = M.empty,
    l2nsLabels = M.empty, l2nsParent = p, l2nsNextLabel = 0
    }

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
       l2nsNamedValues = M.fromList [("true", vTrue), ("false", vFalse), ("undefined", vUndefined)],
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
  l2AllValues = M.empty,
  l2NextValue = L2ValueID reservedIDs,
  l2AllAssertions = [],
  l2AllBaseUnits = M.empty, -- Should SI units come built-in?
  l2NextBaseUnits = L2BaseUnitsID reservedIDs,
  l2AllClasses = M.empty,
  l2NextClassID = L2ClassID reservedIDs,
  l2AllInstances = [],
  l2NextScopedValueID = L2ScopedValueID reservedIDs,
  l2NextScopedUnitID = L2ScopedUnitID "!unnamed" reservedIDs,
  l2NextScopedDomainID = L2ScopedDomainID "!unnamed" reservedIDs,
  l2AllDomainFunctions = M.empty,
  l2NextDomainFunctionID = L2DomainFunctionID reservedIDs,
  l2AllClassValues = M.empty,
  l2NextClassValueID = L2ClassValueID reservedIDs
  }
