{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Data.FieldML.Level1ToLevel2 where

import qualified Data.FieldML.Level1Structure as L1
import qualified Data.FieldML.Level2Structure as L2
import Network.Curl
import Data.IORef
import System.FilePath
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.FieldML.Parser
import Data.FieldML.InitialModel
import Data.List
import Data.Typeable
import Data.Data
import Data.Default
import Data.Maybe
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as S
import qualified Data.Traversable as T

data LookupModel a = LookupModel { unlookupModel :: StateT (M.Map BS.ByteString L2.L2Model) (ErrorT String IO) a }

instance Monad LookupModel where
  return = LookupModel . return
  (LookupModel a) >>= f = LookupModel (a >>= liftM unlookupModel f)
  fail = LookupModel . lift . fail
instance Functor LookupModel where
  fmap = liftM
instance Applicative LookupModel where
  pure = return
  (<*>) = ap
instance MonadIO LookupModel where
  liftIO = LookupModel . lift . lift
instance MonadState LookupModel where
  type StateType LookupModel = M.Map BS.ByteString L2.L2Model
  get = LookupModel get
  put = LookupModel . put

runLookupModel :: LookupModel a -> ErrorT String IO a
runLookupModel x = evalStateT (unlookupModel x) M.empty

loadL2ModelFromURL :: [String] -> String -> ErrorT String IO (L2.L2Model)
loadL2ModelFromURL incl mpath =
  runLookupModel (loadL2ModelFromURL' incl mpath)

firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM (mh:t) = do
  h <- mh
  maybe (firstJustM t) (return . Just) h
firstJustM [] = return Nothing

loadL2ModelFromURL' :: [String] -> String -> LookupModel L2.L2Model
loadL2ModelFromURL' incl mpath = do
  r <-
    firstJustM $ flip map incl $ \tryIncl -> do
      (r, v) <- liftIO $ curlGetString_ (tryIncl ++ mpath) []
      if r == CurlOK
        then do
          nsc <- LookupModel . lift . ErrorT $ return (parseFieldML v)
          finalImpMap <- LookupModel . lift $ tryResolveAllImports incl nsc
          liftM Just $
            LookupModel . lift $ translateL1ToL2 mpath nsc finalImpMap
        else
          return Nothing
  case r of
    Just v -> return $ mtsL2Model v
    Nothing -> fail $ "Could not find model " ++ mpath ++ " anywhere in the include paths " ++ show mpath

tryResolveAllImports :: [String] -> L1.L1NamespaceContents -> ErrorT String IO (M.Map BS.ByteString L2.L2Model)
tryResolveAllImports incl nsc =
  flip execStateT M.empty $ do
    forM_ (nub . sort $ [v | L1.L1NSImport { L1.l1nsImportFrom = Just v} <- universeBi nsc]) $ \toImport -> do
      lm <- lift $ loadL2ModelFromURL incl (BSC.unpack toImport)
      modify $ M.insert toImport lm

data L1NSPath = L1NSPathStop | L1NSPathNamespace BS.ByteString L1NSPath | L1NSPathDomain BS.ByteString |
                L1NSPathClass BS.ByteString | L1NSPathValue BS.ByteString
                deriving (Eq, Ord, Show, Data, Typeable)

-- | Some L1 constructs can be translated to an equivalent canonical L1 that
--   doesn't use certain features. This function is run to simplify the L1
--   input slightly to make the L1 -> L2 translation slightly easier to write.
--   It makes the following transformations:
--     => import x as y => namespace y where import x
--     => import from x ... | x == own URI => import ...
handleL1SimpleSyntacticSugar :: BS.ByteString -> L1.L1NamespaceContents -> L1.L1NamespaceContents
handleL1SimpleSyntacticSugar self =
    transformBi (g . f)
  where
    f (imp@L1.L1NSImport { L1.l1nsSS = ss, L1.l1nsImportAs = Just impas}) =
      L1.L1NSNamespace { L1.l1nsSS = ss, L1.l1nsNamespaceName = impas,
                         L1.l1nsNamespaceContents = L1.L1NamespaceContents [imp{L1.l1nsImportAs = Nothing}]
                       }
    f x = x
    g (imp@L1.L1NSImport { L1.l1nsImportFrom = Just impfrom })
      | impfrom == self = imp { L1.l1nsImportFrom = Nothing }
    g x = x

translateL1ToL2 mpath l1ns impmap =
  flip runReaderT (handleL1SimpleSyntacticSugar (BSC.pack mpath) l1ns, mpath, impmap) $
    flip execStateT (def :: ModelTranslationState) $ do
      -- Create a tree of namespaces; this excludes namespaces that appear
      -- under let bindings, which get dealt with later as we process expressions.
      recursivelyCreateNamespaces (L1.SrcSpan mpath 0 0 0 0) l1ns [] nsBuiltinMain
      -- Change the tree into a DAG by processing imports.
      processNamespaceImports S.empty [] nsMain
      -- Process everything else now we have our starting names.
      recursivelyTranslateModel S.empty S.empty [(biSrcSpan, L1NSPathStop)]

type ForeignToLocal a = M.Map (L2.Identifier, a) a

data ModelTranslationState = ModelTranslationState {
    mtsL2Model :: L2.L2Model,
    mtsL2ToL1Map :: M.Map L2.L2NamespaceID L1.L1NamespaceContents,
    mtsForeignToLocalNS :: ForeignToLocal L2.L2NamespaceID,
    mtsForeignToLocalDomains ::
      ForeignToLocal L2.L2DomainID,
    mtsForeignToLocalValues ::
      ForeignToLocal L2.L2ValueID,
    mtsForeignToLocalBaseUnits ::
      ForeignToLocal L2.L2BaseUnitsID,
    mtsForeignToLocalClass ::
      ForeignToLocal L2.L2ClassID,
    mtsForeignToLocalScopedDomain ::
      ForeignToLocal L2.L2ScopedDomainID,
    mtsForeignToLocalScopedValue ::
      ForeignToLocal L2.L2ScopedValueID,
    mtsForeignToLocalScopedUnit ::
      ForeignToLocal L2.L2ScopedUnitID,
    mtsForeignToLocalDomainFunction ::
      ForeignToLocal L2.L2DomainFunctionID,
    mtsForeignToLocalClassValue ::
      ForeignToLocal L2.L2ClassValueID
  }

instance Default ModelTranslationState where
  def = ModelTranslationState {
    mtsL2Model = initialModel,
    mtsL2ToL1Map = M.empty,
    mtsForeignToLocalNS = M.empty,
    mtsForeignToLocalDomains = M.empty,
    mtsForeignToLocalValues = M.empty,
    mtsForeignToLocalBaseUnits = M.empty,
    mtsForeignToLocalClass = M.empty,
    mtsForeignToLocalScopedDomain = M.empty,
    mtsForeignToLocalScopedUnit = M.empty,
    mtsForeignToLocalScopedValue = M.empty,
    mtsForeignToLocalDomainFunction = M.empty,
    mtsForeignToLocalClassValue = M.empty
                              }

type ModelTranslation a = StateT ModelTranslationState
                          (ReaderT (L1.L1NamespaceContents, String, M.Map BS.ByteString L2.L2Model)
                                  (ErrorT String IO)) a
  
recursivelyCreateNamespaces ss nsc@(L1.L1NamespaceContents c) canonPath parentNS = do
  myNSID <- registerNewNamespace ss parentNS nsc
  -- If we are not at the nsMain level, our parent should link to us by name...
  when (not $ null canonPath) $
    modifyNamespaceContents parentNS $ \ns -> ns { L2.l2nsNamespaces = M.insert (head canonPath) myNSID (L2.l2nsNamespaces ns) }
  forM_ c $ \nsel ->
    case nsel of
      L1.L1NSNamespace { L1.l1nsSS = nsss, L1.l1nsNamespaceName = L1.L1Identifier _ nsname,
                         L1.l1nsNamespaceContents = newnsc } ->
        recursivelyCreateNamespaces nsss newnsc (nsname:canonPath) myNSID
      L1.L1NSDomain { L1.l1nsSS = nsss, L1.l1nsDomainName = L1.L1Identifier _ nsname,
                      L1.l1nsNamespaceContents = newnsc } ->
        recursivelyCreateNamespaces nsss newnsc (nsname:canonPath) myNSID
      _ -> return ()

registerNewNamespace :: L2.SrcSpan -> L2.L2NamespaceID -> L1.L1NamespaceContents -> ModelTranslation L2.L2NamespaceID
registerNewNamespace ss parent nsc = do
  nsID <- (L2.l2NextNamespace . mtsL2Model) <$> get
  modify $ \mts ->
    (
      mts { mtsL2Model = (mtsL2Model mts) {
                 L2.l2AllNamespaces = M.insert nsID (blankNamespaceContents ss parent) (L2.l2AllNamespaces (mtsL2Model mts)),
                 L2.l2NextNamespace = (\(L2.L2NamespaceID v) -> L2.L2NamespaceID (v + 1))
                                      (L2.l2NextNamespace (mtsL2Model mts))
               },
            mtsL2ToL1Map = M.insert nsID nsc (mtsL2ToL1Map mts)
          }
    )
  return nsID

modifyNamespaceContents :: L2.L2NamespaceID -> (L2.L2NamespaceContents -> L2.L2NamespaceContents) -> ModelTranslation ()
modifyNamespaceContents ns f =
  modify $ \mts -> mts { mtsL2Model = (mtsL2Model mts) {
                            L2.l2AllNamespaces = M.update (Just . f) ns (L2.l2AllNamespaces (mtsL2Model mts))
                            }
                       }

findOrCreateNamespace :: L2.SrcSpan -> L2.L2NamespaceID -> BS.ByteString -> L1.L1NamespaceContents -> ModelTranslation L2.L2NamespaceID
findOrCreateNamespace ss parentNS nsName nsc = do
  m <- mtsL2Model <$> get
  let Just pns = M.lookup parentNS (L2.l2AllNamespaces m)
  case M.lookup nsName (L2.l2nsNamespaces pns) of
    Nothing -> registerNewNamespace ss parentNS nsc
    Just nsid -> return nsid

-- | Recursively imports symbols into namespaces. Done keeps track of which namespaces have already
--   been processed. Stack keeps track of what namespaces we are working on now, for cycle detection.
--   ns is the namespace we are requesting be processed now.
--   This also imports everything (not just namespaces) required in this namespace from
--   other files, allowing all further processing to focus on the contents of this file.
processNamespaceImports :: S.Set L2.L2NamespaceID -> [L2.L2NamespaceID] -> L2.L2NamespaceID -> ModelTranslation ()
processNamespaceImports done stack ns
  | ns `S.member` done = return ()
  | ns `elem` stack = do
    let upstream = (dropWhile (/=ns) $ reverse (ns:stack))
    m <- mtsL2Model <$> get
    let upstreamSSs = mapMaybe (\ns -> L2.l2nsSrcSpan <$> (M.lookup ns (L2.l2AllNamespaces m))) upstream
    fail $ "Import loop detected within a module at " ++
                             (intercalate "\n  which imports " $
                                map show upstreamSSs)
  | otherwise = do
     ModelTranslationState { mtsL2Model = m, mtsL2ToL1Map = nsmap } <- get
     let Just (L1.L1NamespaceContents nsc) = M.lookup ns nsmap
     forM_ nsc $ \st -> case st of
       L1.L1NSImport { L1.l1nsSS = ss,
                       L1.l1nsImportFrom = Just from,
                       L1.l1nsImportPath = path,
                       L1.l1nsImportWhat = what,
                       L1.l1nsImportHiding = hiding } -> do
         -- Find the model we are importing from...
         (_, _, impMap) <- ask
         impMod <-
           maybe (fail $ "Reference to imported model " ++ (BSC.unpack from) ++ " at " ++
                  (show ss) ++ ", but that model does not appear to have been loaded successfully.")
                 return
                 (M.lookup from impMap)
         let L1.L1RelOrAbsPath pathSS isAbs rpath = path
         when (not isAbs) . fail $ "Import at " ++ (show pathSS) ++
           " uses both a from clause and a relative path, which is invalid."
         impNS <- findNamespaceInL2UsingL1Path nsMain impMod rpath
         symList <- importListFromWhatAndHiding impNS impMod what hiding
         mapM_ (recursivelyImportExternalSymbol impMod from impNS ns) symList
       _ -> undefined -- TODO

-- | Pulls in a symbol from another model, along with everything needed for that
--  symbol to be used.
recursivelyImportExternalSymbol :: L2.L2Model -> L2.Identifier -> L2.L2NamespaceID -> L2.L2NamespaceID -> L1.L1Identifier -> ModelTranslation ()
recursivelyImportExternalSymbol foreignMod foreignURL foreignNS localNS ident = do
  let foreignNSC = (L2.l2AllNamespaces foreignMod) ! foreignNS
      L1.L1Identifier identSS identName = ident
      tryLookupImportAndRegister :: (L2.L2NamespaceContents -> M.Map L2.Identifier d) ->
                                    (M.Map L2.Identifier d -> L2.L2NamespaceContents -> L2.L2NamespaceContents) ->
                                    (L2.L2Model -> BS.ByteString -> d ->
                                     ModelTranslation d) -> Maybe (ModelTranslation ())
      tryLookupImportAndRegister getMap setMap doImport = do
        d <- M.lookup identName (getMap foreignNSC)
        return $ do
          dl <- doImport foreignMod foreignURL d
          modifyNamespaceContents localNS $ \localNSC ->
            setMap (M.insert identName dl (getMap localNSC)) localNSC
  
  fromMaybe (fail $ "Internal error: unexpected unknown symbol " ++ (show identName) ++
                    " from " ++ (show identSS) ++ " in recursivelyImportExternalSymbol") $
    msum $ [
      tryLookupImportAndRegister L2.l2nsNamespaces (\x mod -> mod{L2.l2nsNamespaces=x}) recursivelyImportExternalNS,
      tryLookupImportAndRegister L2.l2nsDomains (\x mod -> mod{L2.l2nsDomains=x}) recursivelyImportExternalDomain,
      tryLookupImportAndRegister L2.l2nsNamedValues (\x mod -> mod{L2.l2nsNamedValues=x}) recursivelyImportExternalValue,
      tryLookupImportAndRegister L2.l2nsClassValues (\x mod -> mod{L2.l2nsClassValues=x})
                                 recursivelyImportExternalClassValue,
      tryLookupImportAndRegister L2.l2nsUnits (\x mod -> mod{L2.l2nsUnits=x}) recursivelyImportExternalUnits,
      tryLookupImportAndRegister L2.l2nsClasses (\x mod -> mod{L2.l2nsClasses=x}) recursivelyImportExternalClass,
      tryLookupImportAndRegister L2.l2nsDomainFunctions (\x mod -> mod{L2.l2nsDomainFunctions=x})
                                 recursivelyImportExternalDomainFunction,
      tryLookupImportAndRegister L2.l2nsLabels (\x mod -> mod{L2.l2nsLabels=x})
                                 recursivelyImportExternalLabel
      ]

-- | A utility function used for importing external importers that the checks the cache first, imports if it
--   if it isn't found, and saves the result in the cache.
cacheWrapExternalImport :: Ord d =>
  (ModelTranslationState -> ForeignToLocal d) ->
  (ForeignToLocal d -> ModelTranslationState -> ModelTranslationState) ->
  (L2.L2Model -> L2.Identifier -> d -> ModelTranslation d) ->  
  L2.L2Model -> L2.Identifier -> d -> ModelTranslation d
cacheWrapExternalImport getMap setMap f foreignMod foreignURL target = do
  st <- get
  case M.lookup (foreignURL, target) (getMap st) of
    Just localHit -> return localHit
    Nothing -> do
      r <- f foreignMod foreignURL target
      put (setMap (M.insert (foreignURL, target) r (getMap st)) st)
      return r

recursivelyImportExternalNS :: L2.L2Model -> L2.Identifier -> L2.L2NamespaceID -> ModelTranslation L2.L2NamespaceID
recursivelyImportExternalNS = cacheWrapExternalImport mtsForeignToLocalNS (\x m -> m {mtsForeignToLocalNS=x}) $
                              \foreignMod foreignURL targetNS -> do
  let foreignNSC = (L2.l2AllNamespaces foreignMod) ! targetNS
  newNSC <- L2.L2NamespaceContents (L2.l2nsSrcSpan foreignNSC) <$>
              T.mapM (recursivelyImportExternalNS foreignMod foreignURL) (L2.l2nsNamespaces foreignNSC) <*>
              T.mapM (recursivelyImportExternalDomain foreignMod foreignURL) (L2.l2nsDomains foreignNSC) <*>
              T.mapM (recursivelyImportExternalValue foreignMod foreignURL) (L2.l2nsNamedValues foreignNSC) <*>
              T.mapM (recursivelyImportExternalClassValue foreignMod foreignURL) (L2.l2nsClassValues foreignNSC) <*>
              T.mapM (recursivelyImportExternalUnits foreignMod foreignURL) (L2.l2nsUnits foreignNSC) <*>
              T.mapM (recursivelyImportExternalClass foreignMod foreignURL) (L2.l2nsClasses foreignNSC) <*>
              T.mapM (recursivelyImportExternalDomainFunction foreignMod foreignURL) (L2.l2nsDomainFunctions foreignNSC) <*>
              T.mapM (recursivelyImportExternalLabel foreignMod foreignURL) (L2.l2nsLabels foreignNSC) <*>
              recursivelyImportExternalNS foreignMod foreignURL (L2.l2nsParent foreignNSC) <*>
              (pure (L2.l2nsNextLabel foreignNSC))
  newNSID <- (L2.l2NextNamespace . mtsL2Model) <$> get
  modify $ \mts -> mts {
      mtsL2Model = (mtsL2Model mts) {
         L2.l2NextNamespace = (\(L2.L2NamespaceID nid) -> L2.L2NamespaceID (nid + 1)) newNSID,
         L2.l2AllNamespaces = M.insert newNSID
                                       newNSC (L2.l2AllNamespaces . mtsL2Model $ mts)
                                    }
    }
  return newNSID
  
recursivelyImportExternalDomain :: L2.L2Model -> L2.Identifier -> L2.L2DomainType -> ModelTranslation L2.L2DomainType
recursivelyImportExternalDomain foreignMod foreignURL targetDomain =
  L2.L2DomainType (L2.l2DomainTypeSS targetDomain)
                  <$> (mapM (\(e1,e2) ->
                              (,) <$> 
                              (recursivelyImportExternalUnitExpression foreignMod foreignURL e1) <*> 
                              (recursivelyImportExternalUnitExpression foreignMod foreignURL e2))
                       (L2.l2DomainTypeUnitConstraints targetDomain))
                  <*> (mapM (\(e1,e2) ->
                              (,) <$> 
                              (recursivelyImportExternalDomainExpression foreignMod foreignURL e1) <*> 
                              (recursivelyImportExternalDomainExpression foreignMod foreignURL e2))
                       (L2.l2DomainTypeDomainEqualities targetDomain))
                  <*> (mapM (\(c,el) ->
                              (,) <$> 
                              (recursivelyImportExternalClass foreignMod foreignURL c) <*>
                              (mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) el))
                       (L2.l2DomainTypeDomainRelations targetDomain))
                  <*> (recursivelyImportExternalDomainExpression foreignMod foreignURL $
                       L2.l2DomainTypeExpression targetDomain)

recursivelyImportExternalUnitExpression :: L2.L2Model -> L2.Identifier -> L2.L2UnitExpression -> ModelTranslation L2.L2UnitExpression
recursivelyImportExternalUnitExpression foreignMod foreignURL targetEx =
  case targetEx of
    ex@L2.L2UnitExDimensionless{} -> return ex
    L2.L2UnitExRef ss ref -> L2.L2UnitExRef ss <$> (recursivelyImportExternalBaseUnit foreignMod foreignURL ref)
    L2.L2UnitExTimes ss ex1 ex2 -> L2.L2UnitExTimes ss <$> (recursivelyImportExternalUnitExpression foreignMod foreignURL ex1)
                                                      <*> (recursivelyImportExternalUnitExpression foreignMod foreignURL ex2)
    L2.L2UnitPow ss ex1 power -> L2.L2UnitPow ss <$> (recursivelyImportExternalUnitExpression foreignMod foreignURL ex1)
                                                 <*> (pure power)
    L2.L2UnitScalarMup ss scal ex -> L2.L2UnitScalarMup ss scal <$> (recursivelyImportExternalUnitExpression foreignMod foreignURL ex)
    L2.L2UnitScopedVar ss su -> L2.L2UnitScopedVar ss <$> (recursivelyImportExternalScopedUnit foreignMod foreignURL su)

recursivelyImportExternalScopedUnit :: L2.L2Model -> L2.Identifier -> L2.L2ScopedUnitID -> ModelTranslation L2.L2ScopedUnitID
recursivelyImportExternalScopedUnit = cacheWrapExternalImport mtsForeignToLocalScopedUnit (\x m -> m {mtsForeignToLocalScopedUnit = x}) $
                                      \foreignMod foreignURL targetSUID -> do
  newSUID <- (L2.l2NextScopedUnitID . mtsL2Model) <$> get
  modify $ \mts -> mts { mtsL2Model = (mtsL2Model mts) {
                            L2.l2NextScopedUnitID = (\(L2.L2ScopedUnitID i) -> L2.L2ScopedUnitID (i + 1)) newSUID
                                                       }}
  return newSUID

recursivelyImportExternalBaseUnit :: L2.L2Model -> L2.Identifier -> L2.L2BaseUnitsID -> ModelTranslation L2.L2BaseUnitsID
recursivelyImportExternalBaseUnit = cacheWrapExternalImport mtsForeignToLocalBaseUnits (\x m -> m{mtsForeignToLocalBaseUnits=x}) $
                                    \foreignMod foreignURL bu -> 
  undefined -- TODO

recursivelyImportExternalDomainExpression :: L2.L2Model -> L2.Identifier -> L2.L2DomainExpression -> ModelTranslation L2.L2DomainExpression
recursivelyImportExternalDomainExpression foreignMod foreignURL targetEx = undefined -- TODO

recursivelyImportExternalValue :: L2.L2Model -> L2.Identifier -> L2.L2ValueID -> ModelTranslation L2.L2ValueID
recursivelyImportExternalValue foreignMod foreignURL targetValue = undefined -- TODO

recursivelyImportExternalClassValue :: L2.L2Model -> L2.Identifier -> L2.L2ClassValueID -> ModelTranslation L2.L2ClassValueID
recursivelyImportExternalClassValue foreignMod foreignURL targetValue = undefined -- TODO

recursivelyImportExternalUnits :: L2.L2Model -> L2.Identifier -> L2.L2UnitExpression -> ModelTranslation L2.L2UnitExpression
recursivelyImportExternalUnits foreignMod foreignURL targetUnits = undefined -- TODO

recursivelyImportExternalClass :: L2.L2Model -> L2.Identifier -> L2.L2ClassID -> ModelTranslation L2.L2ClassID
recursivelyImportExternalClass foreignMod foreignURL targetClass = undefined -- TODO

recursivelyImportExternalDomainFunction :: L2.L2Model -> L2.Identifier -> L2.L2DomainFunctionID -> ModelTranslation L2.L2DomainFunctionID
recursivelyImportExternalDomainFunction foreignMod foreignURL targetDF = undefined -- TODO

recursivelyImportExternalLabel :: L2.L2Model -> L2.Identifier -> L2.L2Label -> ModelTranslation L2.L2Label
recursivelyImportExternalLabel foreignMod foreignURL targetLabel = undefined -- TODO

-- | Takes an optional list of what to import to import, an optional list of what to hide, and a target namespace and model,
--   and builds a list of symbols to import. Fails with an error if there is a symbol in the what or hiding list which
--   doesn't actually exist.
importListFromWhatAndHiding ::
  Monad m => L2.L2NamespaceID -> L2.L2Model -> Maybe [L1.L1Identifier] -> Maybe [L1.L1Identifier] -> m [L1.L1Identifier]
importListFromWhatAndHiding nsID m what hiding = do
  let impList = allSymbolNames nsID m
      impSet = S.fromList impList
      whatSet = S.fromList (fromMaybe [] what)
      hidingSet = S.fromList (fromMaybe [] hiding)
      missingSyms = (whatSet `S.union` hidingSet) `S.difference` impSet
  when (not (S.null missingSyms)) . fail $
    "Import statement mentions the following symbols which don't exist: " ++
    (intercalate ", " $
     map (\(L1.L1Identifier ss n) ->
           (BSC.unpack n) ++ " at " ++ (show ss)) $ S.toList missingSyms)
  let finalSet = case (what, hiding) of
        (Nothing, Nothing) -> impSet
        (Just _, Nothing) -> whatSet
        (Nothing, Just _) -> impSet `S.difference` hidingSet
        (Just _, Just _) -> whatSet `S.difference` hidingSet
  return . S.toList $ finalSet

-- | Finds all symbols in a particular namespace, of all types.
allSymbolNames :: L2.L2NamespaceID -> L2.L2Model -> [L1.L1Identifier]
allSymbolNames nsID m =
  let
    Just nsc = M.lookup nsID (L2.l2AllNamespaces m)
  in
   map (L1.L1Identifier (L2.l2nsSrcSpan nsc)) $
     concatMap (\f -> f nsc)
       [M.keys . L2.l2nsNamespaces,
        M.keys . L2.l2nsDomains,
        M.keys . L2.l2nsNamedValues,
        M.keys . L2.l2nsClassValues,
        M.keys . L2.l2nsUnits,
        M.keys . L2.l2nsClasses,
        M.keys . L2.l2nsDomainFunctions,
        M.keys . L2.l2nsLabels
       ]

-- | Finds a namespace in a Level 2 model, treating a particular namespace as root and applying a level 1 namespace path. Fails with an error if the
--   namespace cannot be found. Does not look up the tree from the starting namespace for other matches if it is not found at the root.
findNamespaceInL2UsingL1Path :: Monad m => L2.L2NamespaceID -> L2.L2Model -> L1.L1RelPath -> m L2.L2NamespaceID
findNamespaceInL2UsingL1Path ns0 _ (L1.L1RelPath _ []) = return ns0
findNamespaceInL2UsingL1Path ns0 l2mod (L1.L1RelPath _ ((L1.L1Identifier ss l1id):l1ids)) = do
  let Just nsc0 = M.lookup ns0 (L2.l2AllNamespaces l2mod)
  case M.lookup l1id (L2.l2nsNamespaces nsc0) of
    Nothing ->
      fail $ "Attempt to import namespace " ++ (BSC.unpack l1id) ++ " which doesn't exist, at " ++ show ss
    Just ns1 ->
      findNamespaceInL2UsingL1Path ns1 l2mod (L1.L1RelPath ss l1ids)

recursivelyTranslateModel :: S.Set L1NSPath -> S.Set L1NSPath -> [(L2.SrcSpan, L1NSPath)] -> ModelTranslation ()
recursivelyTranslateModel deferred done queue =
  if null queue
    then return ()
    else
      do
        let firstEntry = head queue
        r <- tryTranslatePartNow firstEntry done
        case r of
          Nothing ->
            recursivelyTranslateModel (S.delete (snd firstEntry) deferred)
                                      (S.insert (snd firstEntry) done)
                                      (tail queue)
          Just deps ->
            forM_ deps $ \dep ->
              when (S.member dep deferred) $
                fail ("The model contains an illegal reference cycle - need to process " ++
                      show dep ++ " to process " ++ show firstEntry ++ ", and vice versa.")
              recursivelyTranslateModel (S.insert (snd firstEntry) deferred) done
                ((map (\x -> (fst firstEntry, x)) deps) ++ queue)

-- | Finds a statement in a model. context describes where the references come from, and are
--   used for error reporting and cycle detection. The L1RelPath is an absolute path from the
--   beginning of the model to the targeted statement.
--   Produces 'Nothing' if the reference turns out to be in an import.
findRelevantStatement :: [L2.SrcSpan] -> L1.L1RelPath -> ModelTranslation (Maybe L1.L1NamespaceStatement)
findRelevantStatement context ref = undefined

tryTranslatePartNow :: (L2.SrcSpan, L1NSPath) -> S.Set L1NSPath -> ModelTranslation (Maybe [L1NSPath])
tryTranslatePartNow p done = do
  (l1ns, _, _) <- ask
  tryTranslatePartNow' 0 p done nsMain l1ns id False

tryTranslatePartNow' :: Int -> (L1.SrcSpan, L1NSPath) -> S.Set L1NSPath -> L2.L2NamespaceID -> L1.L1NamespaceContents ->
                        (L1NSPath -> L1NSPath) ->
                        Bool ->
                        ModelTranslation (Maybe [L1NSPath])
tryTranslatePartNow' impDepth (ssFrom, nsp@(L1NSPathNamespace nsName nsPath)) done thisNSID (L1.L1NamespaceContents l1ns) pToHere dom =
  let nextLevel = [(ss, nc, False) | L1.L1NSNamespace { L1.l1nsSS = ss, L1.l1nsNamespaceName = L1.L1Identifier _ nn,
                                                        L1.l1nsNamespaceContents = nc } <- l1ns,
                                     nn == nsName] ++
                  [(ss, nc, True) | L1.L1NSDomain { L1.l1nsSS = ss, L1.l1nsDomainName = L1.L1Identifier _ nn,
                                                    L1.l1nsNamespaceContents = nc} <- l1ns,
                                    nn == nsName ]
  in
    case nextLevel of
      ((ss, downContents, dom):_) -> do
        newNSID <- findOrCreateNamespace ss thisNSID nsName downContents
        tryTranslatePartNow' impDepth (ssFrom, nsPath) done newNSID downContents (pToHere . L1NSPathNamespace nsName) dom
      [] -> do
        imp <- tryFindImportOf l1ns nsName
        case imp of
          Nothing -> fail $ "Cannot find namespace " ++ (BSC.unpack nsName) ++
                            " in namespace " ++ (show $ pToHere L1NSPathStop) ++
                            " referenced from " ++ (show ssFrom)
          {- unneeded
          Just (_, Just _, L1.L1RelOrAbsPath ss isAbs rpath) -> do
            -- Namespaces in external imports either exist already or they never will...
            when (not isAbs) $ fail $ "Import at " ++ (show ss) ++
              " combines a relative URL path with a model URL, which is invalid. " ++
              "Did you mean to put a / in front of the path to import?"
            exists <- doesImportedPartExist url (rpathToNSPathFn rpath nsPath)
            when (isJust exists) . fail $
              "Reference to " ++ (show $ pToHere nsp) ++ " at " ++
              (show ssFrom) ++ " is invalid because " ++ (BS.unpack . fst . fromJust $ exists) ++
              " does not exist in " ++ (show . snd . fromJust $ exists)
            return Nothing -}
          Just (_, Nothing, L1.L1RelOrAbsPath ss isAbs rpath) -> do
            apath <- rpathToNSPathFn ss nsp isAbs rpath
            if apath `S.member` done
              then return Nothing
              else do
                (l1nsTop, _, _) <- ask
                when (impDepth > 255) . fail $
                  "Number of redirections following imports exceeds 255; this usually means that there is a cycle of imports, at " ++ (show ss) ++
                  " processing reference from " ++ (show ssFrom)
                tryTranslatePartNow' (impDepth + 1) (ssFrom, apath) done nsMain l1nsTop id False

tryTranslatePartNow' impDepth (ssFrom, L1NSPathStop) done thisNSID l1ns pToHere dom = do
  tryTranslatePartNow' impDepth (ssFrom, L1NSPathStop) done thisNSID l1ns pToHere dom

tryFindImportOf l1ns nsName = undefined

rpathToNSPathFn :: L1.SrcSpan -> L1NSPath -> Bool -> L1.L1RelPath -> ModelTranslation L1NSPath
rpathToNSPathFn ss _ True rpath = -- Absolute path...
  rpathToNSPathFn ss L1NSPathStop False rpath

rpathToNSPathFn ss L1NSPathStop False (L1.L1RelPath rpss []) = return L1NSPathStop
rpathToNSPathFn ss L1NSPathStop False (L1.L1RelPath rpss (rphead:rpids)) = do
  ModelTranslationState { mtsL2Model = m } <- get
  undefined
