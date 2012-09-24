{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
-- RankNTypes, ConstraintKinds, UndecidableInstances
module Data.FieldML.Structure
where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Typeable
import Data.Data

newtype NamespaceID = NamespaceID Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype NewDomainID = NewDomainID Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype NamedValueID = NamedValueID Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype DomainClassID = DomainClassID Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype ScopedVariable = ScopedVariable Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype UnitID = UnitID Integer deriving (Eq, Ord, Show, Typeable, Data)

data SrcSpan = SrcSpan {
    srcFile :: String,
    srcStartRow :: Int,
    srcStartColumn :: Int,
    srcEndRow :: Int,
    srcEndColumn :: Int
  } deriving (Eq, Ord, Data, Typeable)
instance Show SrcSpan where
  show (SrcSpan sf sr sc er ec)
    | sr==(-1) = sf
    | sr==er && sc == ec = sf ++ (':':(show sr ++ ':':(show sc)))
    | sr==er = sf ++ (':':(show sr ++ ':':(show sc))) ++ ('-':(show ec))
    | otherwise = sf ++ (':':(show sr ++ ':':(show sc))) ++ (" to " ++ (show er ++ ':':(show ec)))

-- | Used when a forward definition is possible.
data (Eq a, Ord a, Typeable a, Data a) => OrForward a = OFKnown a | OFForward NamespaceID SrcSpan BS.ByteString deriving (Eq, Ord, Typeable, Data)
-- | Used when a forward definition is not possible.
newtype (Eq a, Ord a, Typeable a, Data a) => NoForward a = NoForward a deriving (Eq, Ord, Typeable, Data)

{-
class (forall a . Eq (forward a), forall a . Ord (forward a), forall a . Typeable (forward a),
       forall a . Data (forward a)) => ForwardType forward where
  isForward :: forward a -> Bool
instance ForwardType OrForward where
  isForward (OFKnown _) = False
  isForward _ = True
instance ForwardType NoForward where isForward = const False
-}

data Model forward = Model {
  nextNSID :: NamespaceID,                      -- ^ The next NamespaceID to allocate.
  nextNewDomain :: NewDomainID,                 -- ^ The next NewDomainID to allocate.
  nextNamedValue :: NamedValueID,               -- ^ The next NamedValueID to allocate.
  nextDomainClass :: DomainClassID,             -- ^ The next DomainClassID to allocate.
  nextUnit :: UnitID,                           -- ^ The next UnitID to allocate.
  nextScopedVariable :: ScopedVariable,         -- ^ The next ScopedVariable to allocate.
  toplevelNamespace :: NamespaceID,             -- ^ The top-level namespace.
  allNamespaces :: M.Map NamespaceID (Namespace forward), -- ^ All namespaces in the model.
  allNewDomains :: M.Map NewDomainID (NewDomain forward), -- ^ All NewDomains in the model.
  allDomainClasses :: M.Map DomainClassID (DomainClass forward), -- ^ All domainclasses in the model.
  allNamedValues :: M.Map NamedValueID NamedValue, -- ^ All NamedValues in the model.
  allUnits :: M.Map UnitID Unit,                -- ^ All units in the model.
  instancePool :: M.Map (DomainClassID, [DomainType forward]) (Instance forward), -- ^ All domain instance definitions.
  modelAssertions :: [Expression forward],              -- ^ A list of assertions that hold in this model.
  modelForeignNamespaces :: M.Map (BS.ByteString, NamespaceID) NamespaceID, -- ^ Foreign => Local namespace map
  modelForeignDomains :: M.Map (BS.ByteString, NewDomainID) NewDomainID, -- ^ Foreign => Local domain map
  modelForeignDomainClasses :: M.Map (BS.ByteString, DomainClassID) DomainClassID, -- ^ Foreign => Local domain class map
  modelForeignValues :: M.Map (BS.ByteString, NamedValueID) NamedValueID, -- ^ Foreign => Local named value map
  modelForeignUnits :: M.Map (BS.ByteString, UnitID) UnitID -- ^ Foreign => Local units map
  }

{-
deriving instance Eq (Model OrForward)
deriving instance Eq (Model NoForward)
deriving instance Ord (Model OrForward)
deriving instance Ord (Model NoForward)
deriving instance Typeable (Model OrForward)
deriving instance Typeable (Model NoForward)
deriving instance Data (Model OrForward)
deriving instance Data (Model NoForward)
-}

deriving instance (Eq (forward a)) => Eq (Model forward)
deriving instance (Ord (forward a)) => Ord (Model forward)
deriving instance (Typeable (forward a)) => Typeable (Model forward)
deriving instance (Data (forward a)) => Data (Model forward)

-- , Ord, Data, Show, Typeable)

data ELabel forward = ELabel { labelEnsemble :: forward NamespaceID,
                               labelValue :: Integer
                             } deriving (Eq, Ord, Data, Show, Typeable)

data NewDomain forward = CloneDomain SrcSpan (CloneAnnotation forward)
                                     (DomainExpression forward) | -- ^ Clone another domain.
                         BuiltinDomain SrcSpan                    -- ^ A built-in domain.
                           deriving (Eq, Ord, Data, Show, Typeable)
domainSrcSpan :: NewDomain -> SrcSpan
domainSrcSpan (CloneDomain ss _ _ ) = ss
domainSrcSpan (BuiltinDomain ss) = ss

-- | Clone annotation containss information which isn't always strictly needed
--   interpret FieldML, but provides useful semantic information to some
--   applications.
data CloneAnnotation forward = NormalClone |             -- ^ A straight clone.
                       SubsetClone (Expression forward) |  -- ^ Clone, taking subset. Expression must be a field from the original domain onto boolean.
                       ConnectClone [Expression forward] -- ^ Clone, changing connectivity. Expressions must be a field from the original domain onto some other domain.
                       deriving (Eq, Ord, Data, Show, Typeable)

data DomainExpression forward =
  UseNewDomain NewDomainID | -- ^ Refer to a defined domain
  UseRealUnits (UnitExpr forward) | -- ^ Refer to a builtin Real with units domain.
  ApplyDomain (DomainExpression forward) (forward ScopedVariable) (DomainExpression forward) | -- ^ Apply one domain variable
  DomainVariable (forward ScopedVariable) | -- ^ Refer to a domain variable.
  ProductDomain (M.Map (ELabel forward) (DomainExpression forward)) | -- ^ Product domain.
  DisjointUnion (M.Map (ELabel forward) (DomainExpression forward)) | -- ^ Disjoint union.
  FieldSignature (DomainExpression forward) (DomainExpression forward) | -- ^ Field signature.
  -- | Completely evaluate a domainfunction. Negative values reference class arguments.
  EvaluateDomainFunction (forward DomainClassID) Int [DomainExpression forward]
  deriving (Eq, Ord, Data, Show, Typeable)

data DomainType forward = DomainType SrcSpan (DomainHead forward) (DomainExpression forward) deriving (Eq, Ord, Data, Show, Typeable)
data DomainHead forward = DomainHead [DomainClassRelation forward] deriving (Eq, Ord, Data, Show, Typeable)
data DomainClassRelation forward =
  DomainClassRelation (forward DomainClassID) [DomainExpression forward] |
  DomainClassEqual (DomainExpression forward) (DomainExpression forward) |
  DomainUnitConstraint (UnitExpr forward) -- ^ A units constraint on the domain.
  deriving (Eq, Ord, Data, Show, Typeable)

data Namespace forward = Namespace {
  nsSrcSpan :: SrcSpan,
  -- | All namespaces. Note that this includes entries for all domains and
  --   fields and classes, since they are also namespaces.
  nsNamespaces :: M.Map BS.ByteString (forward NamespaceID),
  -- | All domains.
  nsDomains :: M.Map BS.ByteString (DomainType forward),
  -- | All values.
  nsValues :: M.Map BS.ByteString (forward NamedValueID),
  -- | All classes.
  nsClasses :: M.Map BS.ByteString (forward DomainClassID),
  -- | All labels in the namespace.
  nsLabels :: M.Map BS.ByteString (ELabel forward),
  -- | The units in the namespace.
  nsUnits :: M.Map BS.ByteString (UnitExpr forward),
  -- | The parent of this namespace.
  nsParent :: NamespaceID,
  -- | The number of label IDs assigned in this namespace.
  nsNextLabel :: Int
                           }
               deriving (Eq, Ord, Data, Show, Typeable)

-- | One entry for each parameter, parameter is ClassKind [] if it doesn't itself have parameters.
data ClassKind = ClassKind [ClassKind] deriving (Eq, Ord, Data, Show, Typeable)

data DomainClass forward = DomainClass {
  classSrcSpan :: SrcSpan,
  -- | The number of parameters this class takes.
  classKind :: ClassKind,
  -- | The domain functions defined by instances of this class. The second
  --   argument is the number of arguments.
  classDomainFunctions :: M.Map BS.ByteString Int,
  -- | The values defined by instances of this class. ScopedVariables 0..(n-1) refer to the arguments.
  classValues :: M.Map BS.ByteString (Int, DomainExpression forward)
  } deriving (Eq, Ord, Data, Show, Typeable)

data Instance forward = Instance {
  instanceSrcSpan :: SrcSpan,
  -- | Instances with higher numerical precedence values override those with lower values.
  instancePrecedence :: Integer,
  -- | Any domain functions defined on this instance (must correspond to class domainfunctions).
  instanceDomainFunctions :: M.Map Int (DomainExpression forward),
  -- | Assertions involving any values in scope.
  instanceAssertions :: [Expression forward]
  } deriving (Eq, Ord, Data, Show, Typeable)

data NamedValue = NamedValue {
  namedValueSrcSpan :: SrcSpan,
  -- | Only relevant for field value instances when applied inline. Fields with higher numerical precedence values are applied first.
  namedValuePrecedence :: Integer
  } | FFINamedValue { binvModule :: BS.ByteString, -- ^ The module containing the FFI value.
                      binvBase :: BS.ByteString    -- ^ The base name of the FFI value.
                    } -- ^ A foreign named value.
                deriving (Eq, Ord, Data, Show, Typeable)

data Expression forward = Apply SrcSpan (Expression forward) (Expression forward) | -- ^ Apply an expression to a field expression
                          NamedValueRef SrcSpan (forward NamedValueID) | -- ^ Reference a named field.
                          LabelRef SrcSpan (ELabel forward)   | -- ^ Reference a label.
                          LiteralReal SrcSpan (UnitExpr forward) Double | -- ^ Reference a real value.
                          BoundVar SrcSpan (forward ScopedVariable) | -- ^ Reference a bound variable.
                          MkProduct SrcSpan (M.Map (ELabel forward) (Expression forward)) | -- ^ Construct a product.
                          MkUnion SrcSpan (ELabel forward) | -- ^ Make a field that constructs a union.
                          Project SrcSpan (ELabel forward) | -- ^ Make a field that deconstructs a union.
                          Append SrcSpan (ELabel forward)  | -- ^ Make a field that adds to a product.
                          Lambda SrcSpan (forward ScopedVariable) (Expression forward) | -- ^ Define a lambda field.
                          -- | Split on values of the first Expression. The final Expression specifies what to do if nothing matches.
                          Case SrcSpan (Expression forward) (M.Map (ELabel forward) (Expression forward)) |
                          -- | Undefined is used to specify that the value of an expression is not known. It may be used as a placeholder for unknown parameters or fields that need to be solved for.
                          Undefined SrcSpan
                        deriving (Eq, Ord, Data, Show, Typeable)

-- | Represents an expression on units.
data UnitExpr forward = 
  UnitDimensionless SrcSpan |             -- ^ Refer to dimensionless
  UnitRef SrcSpan (forward UnitID) |      -- ^ Refer to a unit defined elsewhere.
  UnitTimes SrcSpan (UnitExpr forward) (UnitExpr forward) | -- ^ Multiply two units. E.g. Times metre second = metre.second
  UnitPow SrcSpan (UnitExpr forward) Double | -- ^ Raise a unit to a power. e.g. Pow second -1 = second^-1
  UnitScalarMup SrcSpan Double (UnitExpr forward) | -- ^ Multiply a unit by a scalar. E.g. ScalarMup 1E-3 metre = millimetre
  UnitScopedVar SrcSpan (forward ScopedVariable) -- ^ Refer to a scoped variable.
  deriving (Eq, Ord, Data, Show, Typeable)

-- | Represents a unit that has been defined.
data Unit = NewBaseUnit { baseUnitSrcSpan :: SrcSpan }           -- ^ Defines a new builtin unit.
              deriving (Eq, Ord, Data, Show, Typeable)
