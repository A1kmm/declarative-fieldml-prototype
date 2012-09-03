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
nsMain = NamespaceID 5        -- ^ The top user-defined namespace.

dNatural = DomainID 0
dInteger = DomainID 1
dBoolean = DomainID 2

vFalse = NamedValueID 0
vTrue = NamedValueID 1

blankNamespace ss p = Namespace { nsSrcSpan = ss, nsNamespaces = M.empty, nsDomains = M.empty,
                                  nsValues = M.empty, nsClasses = M.empty, nsLabels = M.empty,
                                  nsUnits = M.empty, nsParent = p, nsNextLabel = 0 }

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
       nsClasses = M.fromList [],
       nsLabels = M.empty,
       nsUnits = M.empty,
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
        }),
    (nsMain, blankNamespace biSrcSpan nsBuiltinMain)
                             ],
  allNewDomains = M.fromList [(dNatural, BuiltinDomain), (dInteger, BuiltinDomain), (dBoolean, BuiltinDomain)],
  allDomainClasses = M.empty,
  allNamedValues = M.empty,
  allUnits = M.empty,
  instancePool = M.empty,
  modelAssertions = []
  }
