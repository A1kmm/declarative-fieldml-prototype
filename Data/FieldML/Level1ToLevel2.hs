{-# LANGUAGE DeriveDataTypeable, TypeFamilies, PatternGuards #-}
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
  -- Try to find the model first...
  maybe (fail $ "Could not find model " ++ mpath ++ " anywhere in the include paths " ++ show incl) return =<< (
    firstJustM $ flip map incl $ \tryIncl -> do
      let fullPath = tryIncl ++ mpath
      alreadyLoaded <- M.lookup (BSC.pack fullPath) <$> get
      case alreadyLoaded of
        Just m -> return $ Just m
        Nothing -> do
          (r, v) <- liftIO $ curlGetString_ (tryIncl ++ mpath) []
          if r == CurlOK
            then do
              nsc <- LookupModel . lift . ErrorT $ return (parseFieldML v)
              finalImpMap <- tryResolveAllImports incl nsc
              mod <- LookupModel . lift $ translateL1ToL2 mpath nsc finalImpMap
              modify (M.insert (BSC.pack fullPath) mod)
              return $ Just mod
            else
              return Nothing)

tryResolveAllImports :: [String] -> L1.L1NamespaceContents -> LookupModel (M.Map BS.ByteString L2.L2Model)
tryResolveAllImports incl nsc = do
  forM_ (nub . sort $ [v | L1.L1NSImport { L1.l1nsImportFrom = Just v} <- universeBi nsc]) $ \toImport -> do
    lm <- loadL2ModelFromURL' incl (BSC.unpack toImport)
    modify $ M.insert toImport lm
  get

data L1NSPath = L1NSPathStop | L1NSPathNamespace BS.ByteString L1NSPath | L1NSPathDomain BS.ByteString |
                L1NSPathClass BS.ByteString | L1NSPathValue BS.ByteString
                deriving (Eq, Ord, Show, Data, Typeable)

-- | Some L1 constructs can be translated to an equivalent canonical L1 that
--   doesn't use certain features. This function is run to simplify the L1
--   input slightly to make the L1 -> L2 translation slightly easier to write.
--   It makes the following transformations:
--     => import x as y => namespace y where import x
--     => import from x ... | x == own URI => import ...
--     => ensemble {...} as x => namespace x where ensemble {...}
handleL1SimpleSyntacticSugar :: BS.ByteString -> L1.L1NamespaceContents -> L1.L1NamespaceContents
handleL1SimpleSyntacticSugar self =
    transformBi (g . f)
  where
    f (imp@L1.L1NSImport { L1.l1nsSS = ss, L1.l1nsImportAs = Just impas}) =
      L1.L1NSNamespace { L1.l1nsSS = ss, L1.l1nsNamespaceName = impas,
                         L1.l1nsNamespaceContents = L1.L1NamespaceContents [imp{L1.l1nsImportAs = Nothing}]
                       }
    f (ens@(L1.L1NSEnsemble{ L1.l1nsSS = ss, L1.l1nsAs = Just x})) =
      L1.L1NSNamespace { L1.l1nsSS = ss, L1.l1nsNamespaceName = x,
                         L1.l1nsNamespaceContents = L1.L1NamespaceContents [
                           ens { L1.l1nsAs = Nothing }
                           ]}
    f x = x
    g (imp@L1.L1NSImport { L1.l1nsImportFrom = Just impfrom })
      | impfrom == self = imp { L1.l1nsImportFrom = Nothing }
    g x = x

translateL1ToL2 :: String -> L1.L1NamespaceContents -> M.Map BS.ByteString L2.L2Model -> ErrorT String IO L2.L2Model
translateL1ToL2 mpath l1ns impmap =
  flip runReaderT (handleL1SimpleSyntacticSugar (BSC.pack mpath) l1ns, mpath, impmap) $
    flip evalStateT (def :: ModelTranslationState) $ do
      -- Create a tree of namespaces; this excludes namespaces that appear
      -- under let bindings, which get dealt with later as we process expressions.
      recursivelyCreateNamespaces (L1.SrcSpan mpath 0 0 0 0) l1ns [] nsBuiltinMain
      -- Change the tree into a DAG by processing imports.
      processNamespaceImports S.empty [] nsMain
      -- Process everything else now we have our starting names.
      recursivelyTranslateModel nsMain S.empty S.empty [(biSrcSpan, L1NSPathStop)]
      getL2Model

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
    mtsForeignToLocalScopedUnit = M.empty,
    mtsForeignToLocalScopedValue = M.empty,
    mtsForeignToLocalDomainFunction = M.empty,
    mtsForeignToLocalClassValue = M.empty
                              }

data ScopeInformation = ScopeInformation {
  siValueIDMap  :: M.Map L1.L1ScopedID L2.L2ScopedValueID,
  siUnitIDMap   :: M.Map L1.L1ScopedID L2.L2ScopedUnitID
  }

instance Default ScopeInformation where
  def = ScopeInformation M.empty M.empty

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

getL2Model :: ModelTranslation L2.L2Model
getL2Model = mtsL2Model <$> get

modifyL2Model :: (L2.L2Model -> L2.L2Model) -> ModelTranslation ()
modifyL2Model f = modify $ \mts -> mts { mtsL2Model = f (mtsL2Model mts) }

registerNewNamespace :: L2.SrcSpan -> L2.L2NamespaceID -> L1.L1NamespaceContents -> ModelTranslation L2.L2NamespaceID
registerNewNamespace ss parent nsc = do
  nsID <- L2.l2NextNamespace <$> getL2Model
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
  modifyL2Model $ \mod ->
    mod {
      L2.l2AllNamespaces = M.update (Just . f) ns (L2.l2AllNamespaces mod)
      }

getNamespaceContents :: L2.L2NamespaceID -> ModelTranslation L2.L2NamespaceContents
getNamespaceContents nsid = (fromJust . M.lookup nsid . L2.l2AllNamespaces) <$> getL2Model

findNamespace :: L2.L2NamespaceID -> BS.ByteString -> ModelTranslation (Maybe L2.L2NamespaceID)
findNamespace parentNS nsName = do
  m <- mtsL2Model <$> get
  let Just pns = M.lookup parentNS (L2.l2AllNamespaces m)
  return $ M.lookup nsName (L2.l2nsNamespaces pns)

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
-- For now this only does cross-module imports; this is probably going to end up
-- being the cleanest design, so we can get rid of the cycle detection since there
-- is already cycle detection for modules.
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
       _ -> return ()

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
      tryLookupImportAndRegister L2.l2nsUnits (\x mod -> mod{L2.l2nsUnits=x}) recursivelyImportExternalUnitExpression,
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

recursivelyImportExternalNSContents :: L2.L2Model -> L2.Identifier -> L2.L2NamespaceContents -> ModelTranslation L2.L2NamespaceContents
recursivelyImportExternalNSContents foreignMod foreignURL foreignNSC =
  L2.L2NamespaceContents (L2.l2nsSrcSpan foreignNSC) <$>
    T.mapM (recursivelyImportExternalNS foreignMod foreignURL) (L2.l2nsNamespaces foreignNSC) <*>
    T.mapM (recursivelyImportExternalDomain foreignMod foreignURL) (L2.l2nsDomains foreignNSC) <*>
    T.mapM (recursivelyImportExternalValue foreignMod foreignURL) (L2.l2nsNamedValues foreignNSC) <*>
    T.mapM (recursivelyImportExternalClassValue foreignMod foreignURL) (L2.l2nsClassValues foreignNSC) <*>
    T.mapM (recursivelyImportExternalUnitExpression foreignMod foreignURL) (L2.l2nsUnits foreignNSC) <*>
    T.mapM (recursivelyImportExternalClass foreignMod foreignURL) (L2.l2nsClasses foreignNSC) <*>
    T.mapM (recursivelyImportExternalDomainFunction foreignMod foreignURL) (L2.l2nsDomainFunctions foreignNSC) <*>
    T.mapM (recursivelyImportExternalLabel foreignMod foreignURL) (L2.l2nsLabels foreignNSC) <*>
    recursivelyImportExternalNS foreignMod foreignURL (L2.l2nsParent foreignNSC) <*>
    (pure (L2.l2nsNextLabel foreignNSC))

recursivelyImportExternalNS :: L2.L2Model -> L2.Identifier -> L2.L2NamespaceID -> ModelTranslation L2.L2NamespaceID
recursivelyImportExternalNS =
  cacheWrapExternalImport mtsForeignToLocalNS (\x m -> m {mtsForeignToLocalNS=x}) $
    \foreignMod foreignURL targetNS ->
    if ((\(L2.L2NamespaceID n) -> n) targetNS) < reservedIDs && targetNS /= nsMain
       -- Reserved IDs are a special case: We match on them numerically, and
       -- they are globally unique, so just re-use the same numerical ID. We exclude
       -- nsMain, however, because that is actually 
    then return targetNS
    else
      do
        let foreignNSC = (L2.l2AllNamespaces foreignMod) ! targetNS
        newNSC <- recursivelyImportExternalNSContents foreignMod foreignURL foreignNSC
        newNSID <- L2.l2NextNamespace <$> getL2Model
        modifyL2Model $ \mod -> mod {
          L2.l2NextNamespace = (\(L2.L2NamespaceID nid) -> L2.L2NamespaceID (nid + 1)) newNSID,
          L2.l2AllNamespaces = M.insert newNSID
                               newNSC (L2.l2AllNamespaces mod)
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
  newSUID <- L2.l2NextScopedUnitID <$> getL2Model
  modifyL2Model $ \mod -> mod {
    L2.l2NextScopedUnitID = (\(L2.L2ScopedUnitID i) -> L2.L2ScopedUnitID (i + 1)) newSUID
    }
  return newSUID

recursivelyImportExternalScopedValue :: L2.L2Model -> L2.Identifier -> L2.L2ScopedValueID -> ModelTranslation L2.L2ScopedValueID
recursivelyImportExternalScopedValue = cacheWrapExternalImport mtsForeignToLocalScopedValue (\x m -> m {mtsForeignToLocalScopedValue = x}) $
                                      \foreignMod foreignURL targetSVID -> do
  newSVID <- L2.l2NextScopedValueID <$> getL2Model
  modifyL2Model $ \mod -> mod {
    L2.l2NextScopedValueID = (\(L2.L2ScopedValueID i) -> L2.L2ScopedValueID (i + 1)) newSVID
    }
  return newSVID

recursivelyImportExternalBaseUnit :: L2.L2Model -> L2.Identifier -> L2.L2BaseUnitsID -> ModelTranslation L2.L2BaseUnitsID
recursivelyImportExternalBaseUnit = cacheWrapExternalImport mtsForeignToLocalBaseUnits (\x m -> m{mtsForeignToLocalBaseUnits=x}) $
                                    \foreignMod foreignURL bu -> do
  newBUID <- L2.l2NextBaseUnits <$> getL2Model
  modifyL2Model $ \mod -> mod {
    L2.l2NextBaseUnits = (\(L2.L2BaseUnitsID i) -> L2.L2BaseUnitsID (i + 1)) newBUID,
    -- Note that L2BaseUnitContents only contains a SrcSpan so needs no translation...
    L2.l2AllBaseUnits = M.insert newBUID (fromJust . M.lookup bu . L2.l2AllBaseUnits $ foreignMod)
                                 (L2.l2AllBaseUnits mod)
    }
  return newBUID

recursivelyImportExternalDomainExpression :: L2.L2Model -> L2.Identifier -> L2.L2DomainExpression -> ModelTranslation L2.L2DomainExpression
recursivelyImportExternalDomainExpression foreignMod foreignURL targetEx =
  case targetEx of
    L2.L2DomainExpressionProduct ss l ->
      L2.L2DomainExpressionProduct ss <$>
        recursivelyImportExternalLabelledDomains foreignMod foreignURL l
    L2.L2DomainExpressionDisjointUnion ss l ->
      L2.L2DomainExpressionDisjointUnion ss <$>
        recursivelyImportExternalLabelledDomains foreignMod foreignURL l
    L2.L2DomainExpressionFieldSignature ss dd dcd ->
      L2.L2DomainExpressionFieldSignature ss
        <$> recursivelyImportExternalDomainExpression foreignMod foreignURL dd
        <*> recursivelyImportExternalDomainExpression foreignMod foreignURL dcd
    L2.L2DomainExpressionReal ss u ->
      L2.L2DomainExpressionReal ss
        <$> recursivelyImportExternalUnitExpression foreignMod foreignURL u
    L2.L2DomainExpressionApply ss dom sv val ->
      L2.L2DomainExpressionApply ss
        <$> recursivelyImportExternalDomainExpression foreignMod foreignURL dom
        <*> pure sv
        <*> recursivelyImportExternalDomainExpression foreignMod foreignURL val
    L2.L2DomainFunctionEvaluate ss dfid args ->
      L2.L2DomainFunctionEvaluate ss
        <$> recursivelyImportExternalDomainFunction foreignMod foreignURL dfid
        <*> mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) args
    L2.L2DomainVariableRef ss sdid ->
      return $ L2.L2DomainVariableRef ss sdid

recursivelyImportExternalLabelledDomains :: L2.L2Model -> L2.Identifier -> L2.L2LabelledDomains -> ModelTranslation L2.L2LabelledDomains
recursivelyImportExternalLabelledDomains foreignMod foreignURL (L2.L2LabelledDomains ldl) =
  L2.L2LabelledDomains <$>
    mapM (\(lab, ex) ->
           (,) <$> recursivelyImportExternalLabel foreignMod foreignURL lab
               <*> recursivelyImportExternalDomainExpression foreignMod foreignURL ex) ldl

recursivelyImportExternalExpression :: L2.L2Model -> L2.Identifier -> L2.L2Expression -> ModelTranslation L2.L2Expression
recursivelyImportExternalExpression foreignMod foreignURL targetEx =
  case targetEx of
    L2.L2ExApply ss op arg ->
      L2.L2ExApply ss
        <$> recursivelyImportExternalExpression foreignMod foreignURL op
        <*> recursivelyImportExternalExpression foreignMod foreignURL arg
    L2.L2ExReferenceLabel ss l ->
      L2.L2ExReferenceLabel ss
        <$> recursivelyImportExternalLabel foreignMod foreignURL l
    L2.L2ExReferenceValue ss valueID ->
      L2.L2ExReferenceValue ss
        <$> recursivelyImportExternalValue foreignMod foreignURL valueID
    L2.L2ExReferenceClassValue ss cval ->
      L2.L2ExReferenceClassValue ss
        <$> recursivelyImportExternalClassValue foreignMod foreignURL cval
    L2.L2ExBoundVariable ss svid ->
      L2.L2ExBoundVariable ss
        <$> recursivelyImportExternalScopedValue foreignMod foreignURL svid
    L2.L2ExLiteralReal ss uex rv ->
      L2.L2ExLiteralReal ss
        <$> recursivelyImportExternalUnitExpression foreignMod foreignURL uex
        <*> (return rv)
    L2.L2ExMkProduct ss vals ->
      L2.L2ExMkProduct ss
        <$> mapM (\(lab, ex) ->
                   (,)
                     <$> recursivelyImportExternalLabel foreignMod foreignURL lab
                     <*> recursivelyImportExternalExpression foreignMod foreignURL ex) vals
    L2.L2ExMkUnion ss l ex ->
      L2.L2ExMkUnion ss <$> recursivelyImportExternalLabel foreignMod foreignURL l
                        <*> recursivelyImportExternalExpression foreignMod foreignURL ex
    L2.L2ExProject ss l ->
      L2.L2ExProject ss <$> recursivelyImportExternalLabel foreignMod foreignURL l
    L2.L2ExAppend ss l ->
      L2.L2ExAppend ss <$> recursivelyImportExternalLabel foreignMod foreignURL l
    L2.L2ExLambda ss bv val ->
      L2.L2ExLambda ss <$> recursivelyImportExternalScopedValue foreignMod foreignURL bv
                       <*> recursivelyImportExternalExpression foreignMod foreignURL val
    L2.L2ExCase ss expr values ->
      L2.L2ExCase ss <$> recursivelyImportExternalExpression foreignMod foreignURL expr
                     <*> mapM (\(l, ex) ->
                                (,)
                                  <$> recursivelyImportExternalLabel foreignMod foreignURL l
                                  <*> recursivelyImportExternalExpression foreignMod foreignURL ex
                              ) values
    L2.L2ExLet ss expr closureNS closureExprs ->
      L2.L2ExLet ss <$> recursivelyImportExternalExpression foreignMod foreignURL expr
                    <*> recursivelyImportExternalNS foreignMod foreignURL closureNS
                    <*> mapM (recursivelyImportExternalExpression foreignMod foreignURL) closureExprs
    L2.L2ExString ss bs -> pure $ L2.L2ExString ss bs
    L2.L2ExSignature ss ex sig ->
      L2.L2ExSignature ss
        <$> recursivelyImportExternalExpression foreignMod foreignURL ex
        <*> recursivelyImportExternalDomainType foreignMod foreignURL sig

recursivelyImportExternalLabel :: L2.L2Model -> L2.Identifier -> L2.L2Label -> ModelTranslation L2.L2Label
recursivelyImportExternalLabel foreignMod foreignURL (L2.L2Label ens val) =
  L2.L2Label <$> recursivelyImportExternalNS foreignMod foreignURL ens
             <*> pure val

recursivelyImportExternalValue :: L2.L2Model -> L2.Identifier -> L2.L2ValueID -> ModelTranslation L2.L2ValueID
recursivelyImportExternalValue =
  cacheWrapExternalImport mtsForeignToLocalValues (\x m -> m{mtsForeignToLocalValues=x}) $
    \foreignMod foreignURL foreignValueID -> do
      let foreignValueContents = (fromJust . M.lookup foreignValueID . L2.l2AllValues $ foreignMod)
      newValueID <- L2.l2NextValue <$> getL2Model
      localValueContents <- L2.L2ValueContents (L2.l2ValueSS foreignValueContents) <$>
                              maybe (return Nothing)
                                (\fvc -> Just <$>
                                         recursivelyImportExternalDomainType foreignMod foreignURL fvc)
                                (L2.l2ValueType foreignValueContents)
      modifyL2Model $ \mod -> mod {
        L2.l2NextValue = (\(L2.L2ValueID i) -> L2.L2ValueID (i + 1)) newValueID,
        L2.l2AllValues = M.insert newValueID localValueContents
                              (L2.l2AllValues mod)
        }
      return newValueID

recursivelyImportExternalDomainType :: L2.L2Model -> L2.Identifier -> L2.L2DomainType -> ModelTranslation L2.L2DomainType
recursivelyImportExternalDomainType foreignMod foreignURL (L2.L2DomainType ss u deq dreln ex) =
  L2.L2DomainType ss
    <$> mapM (\(u1, u2) -> (,) <$> recursivelyImportExternalUnitExpression foreignMod foreignURL u1
                               <*> recursivelyImportExternalUnitExpression foreignMod foreignURL u2) u
    <*> mapM (\(d1, d2) -> (,)<$> recursivelyImportExternalDomainExpression foreignMod foreignURL d1
                               <*> recursivelyImportExternalDomainExpression foreignMod foreignURL d2) deq
    <*> mapM (\(cid, dexs) -> (,) <$> recursivelyImportExternalClass foreignMod foreignURL cid
                                  <*> mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) dexs) dreln
    <*> recursivelyImportExternalDomainExpression foreignMod foreignURL ex

recursivelyImportExternalClassValue :: L2.L2Model -> L2.Identifier -> L2.L2ClassValueID -> ModelTranslation L2.L2ClassValueID
recursivelyImportExternalClassValue =
  cacheWrapExternalImport mtsForeignToLocalClassValue (\x m -> m{mtsForeignToLocalClassValue=x}) $
    \foreignMod foreignURL foreignValueID -> do
      let Just (L2.L2ClassValueContents ss dt) = M.lookup foreignValueID . L2.l2AllClassValues $ foreignMod
      localValueContents <- L2.L2ClassValueContents ss <$>
                              (recursivelyImportExternalDomainType foreignMod foreignURL dt)
      newValueID <- L2.l2NextClassValueID <$> getL2Model
      modifyL2Model $ \mod -> mod {
        L2.l2NextClassValueID = (\(L2.L2ClassValueID i) -> L2.L2ClassValueID (i + 1)) newValueID,
        L2.l2AllClassValues = M.insert newValueID localValueContents
                                (L2.l2AllClassValues mod)
        }
      return newValueID

recursivelyImportExternalClass :: L2.L2Model -> L2.Identifier -> L2.L2ClassID -> ModelTranslation L2.L2ClassID
recursivelyImportExternalClass =
  cacheWrapExternalImport mtsForeignToLocalClass (\x m -> m{mtsForeignToLocalClass=x}) $
    \foreignMod foreignURL foreignClassID -> do
      newClassID <- L2.l2NextClassID <$> getL2Model
      let Just (L2.L2ClassContents ss p df vals) = M.lookup foreignClassID . L2.l2AllClasses $ foreignMod
      localClassContents <- L2.L2ClassContents ss p
                              <$> T.mapM (\dfid ->
                                           recursivelyImportExternalDomainFunction foreignMod
                                                                                   foreignURL dfid) df
                              <*> T.mapM (\valueId ->
                                           recursivelyImportExternalClassValue foreignMod foreignURL valueId) vals
      modifyL2Model $ \mod -> mod {
        L2.l2NextClassID = (\(L2.L2ClassID i) -> L2.L2ClassID (i + 1)) newClassID,
        L2.l2AllClasses = M.insert newClassID localClassContents
                            (L2.l2AllClasses mod)
        }
      return newClassID
      
recursivelyImportExternalDomainFunction :: L2.L2Model -> L2.Identifier -> L2.L2DomainFunctionID -> ModelTranslation L2.L2DomainFunctionID
recursivelyImportExternalDomainFunction =
  cacheWrapExternalImport mtsForeignToLocalDomainFunction (\x m -> m{mtsForeignToLocalDomainFunction=x}) $
   \foreignMod foreignURL foreignDF -> do
     newDFID <- L2.l2NextDomainFunctionID <$> getL2Model
     -- No need for any translation...
     let Just localDFContents = M.lookup foreignDF (L2.l2AllDomainFunctions foreignMod)
     modifyL2Model $ \mod -> mod {
       L2.l2NextDomainFunctionID = (\(L2.L2DomainFunctionID i) -> L2.L2DomainFunctionID (i + 1)) newDFID,
       L2.l2AllDomainFunctions = M.insert newDFID localDFContents
                                   (L2.l2AllDomainFunctions mod)
       }
     return newDFID

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

-- | Describes how to add to an existing L2Model by translating a list of parts
-- referred to by L1NSPath (in queue - along with the SrcSpan that triggered the
-- need to translate). deferred contains a set of paths that depend on
-- these paths being translated first - if any of them are a dependency of this list
-- of needed parts, it means the model has an invalid cyclic reference. done contains
-- a set of the model parts that have already been translated.
recursivelyTranslateModel :: L2.L2NamespaceID -> S.Set L1NSPath -> S.Set L1NSPath -> [(L2.SrcSpan, L1NSPath)] -> ModelTranslation ()
recursivelyTranslateModel thisNSID deferred done queue =
  if null queue
    then return ()
    else
      do
        let firstEntry = head queue
        r <- tryTranslatePartNow thisNSID firstEntry done
        case r of
          Nothing ->
            recursivelyTranslateModel thisNSID (S.delete (snd firstEntry) deferred)
                                               (S.insert (snd firstEntry) done)
                                               (tail queue)
          Just deps ->
            forM_ deps $ \dep ->
              when (S.member dep deferred) $
                fail ("The model contains an illegal reference cycle - need to process " ++
                      show dep ++ " to process " ++ show firstEntry ++ ", and vice versa.")
              recursivelyTranslateModel thisNSID (S.insert (snd firstEntry) deferred) done
                ((map (\x -> (fst firstEntry, x)) deps) ++ queue)

-- | Attempts to translate the model part referred to by snd p from the Level 1 structure
-- into the Level 2 structure. done is the set of all paths that were already translated.
-- fst p gives a SrcSpan that referenced this model part.
tryTranslatePartNow :: L2.L2NamespaceID -> (L1.SrcSpan, L1NSPath) -> S.Set L1NSPath -> ModelTranslation (Maybe [L1NSPath])
tryTranslatePartNow thisNSID p done = do
  (l1ns, _, _) <- ask
  tryTranslatePartNow' p done thisNSID l1ns id Nothing

tryTranslatePartNow' :: (L1.SrcSpan, L1NSPath) -> S.Set L1NSPath -> L2.L2NamespaceID -> L1.L1NamespaceContents ->
                        (L1NSPath -> L1NSPath) ->
                        Maybe ([L1.L1ScopedID], L1.L1DomainDefinition) ->
                        ModelTranslation (Maybe [L1NSPath])
tryTranslatePartNow' (ssFrom, nsp@(L1NSPathNamespace nsName nsPath)) done thisNSID (L1.L1NamespaceContents l1ns) pToHere domainDetails =
  let nextLevel = [(ss, nc, Nothing) | L1.L1NSNamespace { L1.l1nsSS = ss, L1.l1nsNamespaceName = L1.L1Identifier _ nn,
                                                          L1.l1nsNamespaceContents = nc } <- l1ns,
                                     nn == nsName] ++
                  [(ss, nc, Just (dp, dd)) | L1.L1NSDomain { L1.l1nsSS = ss, L1.l1nsDomainName = L1.L1Identifier _ nn,
                                                             L1.l1nsDomainParameters = dp, L1.l1nsDomainDefinition = dd,
                                                             L1.l1nsNamespaceContents = nc } <- l1ns,
                                    nn == nsName ]
  in
    case nextLevel of
      ((ss, downContents, isDomain):_) -> do
        newNSID <- findOrCreateNamespace ss thisNSID nsName downContents
        tryTranslatePartNow' (ssFrom, nsPath) done newNSID downContents (pToHere . L1NSPathNamespace nsName) domainDetails
      [] -> do
        -- Note: We don't check imports because paths are supposed to have been
        -- already translated to not include any imports.
        fail $ "Cannot find namespace " ++ (BSC.unpack nsName) ++
                            " in namespace " ++ (show $ pToHere L1NSPathStop) ++
                            " referenced from " ++ (show ssFrom)

tryTranslatePartNow' (ssFrom, L1NSPathStop) done thisNSID nsc pToHere (Just (domainParameters, domainDefinition)) = do
  thisNSContents <- getNamespaceContents thisNSID
  let parentNS = L2.l2nsParent thisNSContents
  parentContents <- getNamespaceContents parentNS
  let nameInParent = fst . fromJust . find ((==thisNSID) . snd) . M.toList . L2.l2nsNamespaces $ parentContents
  (case M.lookup nameInParent (L2.l2nsDomains parentContents) of
    Just dd -> return Nothing
    Nothing -> do
      ttdd <- tryTranslateDomainDefinition ssFrom done thisNSID pToHere domainParameters domainDefinition
      case ttdd of
        Left deps -> return $ Just deps
        Right domType -> do
          ModelTranslationState {mtsL2Model = mod} <- get
          let thisNSContents' = (L2.l2AllNamespaces mod)!thisNSID
          let parentContents' = (L2.l2AllNamespaces mod)!parentNS
          modifyL2Model $ \mod -> mod {
            L2.l2AllNamespaces =
               M.insert parentNS (parentContents' {
                                     L2.l2nsDomains = M.insert nameInParent domType (L2.l2nsDomains parentContents')
                                     }) (L2.l2AllNamespaces mod)
            }
          return Nothing
    ) `mplus`
      -- Now do the part that is common to domains and namespaces.
      tryTranslatePartNow' (ssFrom, L1NSPathStop) done thisNSID nsc pToHere Nothing

tryTranslatePartNow' (ssFrom, L1NSPathStop) done thisNSID (L1.L1NamespaceContents nsc) pToHere Nothing = do
  -- Translate a namespace...
  -- Firstly build a list of dependencies...
  deps <- concatMapM
          (\nss -> case nss of
              L1.L1NSImport {
                L1.l1nsImportFrom = Nothing,
                L1.l1nsImportPath = p,
                L1.l1nsImportWhat = what,
                L1.l1nsImportHiding = hiding,
                L1.l1nsImportAs = impId } -> do
                  maybeToList <$> (ensureNoLocalImports =<<
                   arpathToNSPathFn (pToHere L1NSPathStop) p)
              L1.L1NSNamespace { L1.l1nsNamespaceName = L1.L1Identifier _ ident } -> do
                maybeToList <$> (ensureNoLocalImports
                          (pToHere (L1NSPathNamespace ident L1NSPathStop)))
              L1.L1NSDomain { L1.l1nsDomainName = L1.L1Identifier _ ident } -> do
                maybeToList <$> (ensureNoLocalImports
                          (pToHere (L1NSPathDomain ident)))
              L1.L1NSAssertion { L1.l1nsExpression = ex } ->
                getExpressionDependencies pToHere ex
              L1.L1NSNamedValue { L1.l1nsDomainType = Just dt } ->
                getDomainTypeDependencies pToHere dt
              L1.L1NSClass { L1.l1nsClassName = n, L1.l1nsClassValues = cv } ->
                concatMapM (getDomainTypeDependencies pToHere . snd) cv
              L1.L1NSUnit { L1.l1nsUnitDefinition = ud } ->
                getUnitDefinitionDependencies pToHere ud
              L1.L1NSInstance { L1.l1nsInstanceOfClass = c,
                                L1.l1nsClassArguments = dts,
                                L1.l1nsInstanceDomainFunctions = dfs,
                                L1.l1nsInstanceValues = exs } -> do
                classp <-
                  ensureNoLocalImports =<< arpathToNSPathFn (pToHere L1NSPathStop) c
                dtsp <- concatMapM (getDomainTypeDependencies pToHere) dts
                dfsp <- concatMapM (\(_, dts, dex) ->
                            (++) <$> concatMapM (getDomainTypeDependencies pToHere) dts
                                 <*> getDomainExpressionDependencies pToHere dex
                          ) dfs
                expd <- concatMapM (getExpressionDependencies pToHere) exs
                return $ (maybeToList classp) ++ dtsp ++ dfsp ++ expd
              _ -> return []
          ) nsc
  -- Filter dependencies to remove those that are done...
  let remainingDeps = filter (not . flip S.member done) deps
  case remainingDeps of
    (_:_) -> return $ Just remainingDeps
    [] -> do
      r <- translateNSContents def thisNSID done nsc
      modifyNamespaceContents thisNSID (const r)
      return Nothing

-- Everything else is loaded by simply processing the parent namespace.
translatePartNow' (ssFrom, _) done thisNSID nsc pToHere domainInfo
  | parentNS `S.member` done = return Nothing
  | otherwise = return . Just $ [parentNS]
  where
    parentNS = pToHere (L1NSPathStop)

translateNSContents :: ScopeInformation -> L2.L2NamespaceID -> S.Set L1NSPath -> [L1.L1NamespaceStatement] -> ModelTranslation L2.L2NamespaceContents
translateNSContents scope thisNSID done nsc = do
  l2nsc <- getNamespaceContents thisNSID
  flip (flip foldM l2nsc) nsc $ \l2nsc nss ->
    case nss of
      -- Note: We assume all remaining imports are local due to
      -- processNamespaceImports, and have no 'as' clause because
      -- of handleL1SimpleSyntacticSugar...
      L1.L1NSImport {
        L1.l1nsSS = ss,
        L1.l1nsImportPath = rapath, 
        L1.l1nsImportWhat = what,
        L1.l1nsImportHiding = hidingWhat
                    } -> do
        nsf <- findNamespaceByRAPath thisNSID rapath
        case nsf of
          Nothing ->
            fail $ showString "Import at " . shows ss .
                   showString " refers to a namespace " .
                   shows rapath $ " that doesn't exist."
          Just impFrom -> do
            allSyms <- (S.fromList . allSymbolNames impFrom) <$> getL2Model
            impContents <- getNamespaceContents impFrom
            let whatSet = S.fromList <$> what
            let hidingSet = S.fromList <$> hidingWhat
            let includedSyms =
                  maybe allSyms (S.intersection allSyms) whatSet
            let notExcludedSyms =
                  maybe includedSyms (S.difference includedSyms) hidingSet
            let errorIncludedSyms =
                  (maybe S.empty (flip S.difference allSyms) whatSet) `S.union`
                  (maybe S.empty (flip S.difference allSyms) hidingSet)
            when (not . S.null $ errorIncludedSyms) $ do
                fail $ showString "Import at " . shows ss .
                       showString " mentions namespace member(s) that don't exist: " $
                       intercalate ", " $
                       map (\(L1.L1Identifier idss bs) ->
                             BSC.unpack bs ++ " at " ++ (show idss)) $
                         S.toList errorIncludedSyms
            -- TODO check for name conflicts.
            flip (flip foldM l2nsc) (S.toList notExcludedSyms) $ \l2nsc (L1.L1Identifier ss sym) -> do
                let tryLookupImportAndAdd :: (L2.L2NamespaceContents -> M.Map L2.Identifier a) ->
                                             (M.Map L2.Identifier a -> L2.L2NamespaceContents -> L2.L2NamespaceContents) ->
                                             L2.L2NamespaceContents ->
                                             Maybe L2.L2NamespaceContents
                    tryLookupImportAndAdd f g l2nsc =
                      (\toImp -> g (M.insert sym toImp (f l2nsc)) l2nsc) <$> (M.lookup sym (f impContents))

                return . fromJust . msum $ [
                      (tryLookupImportAndAdd L2.l2nsNamespaces (\m x -> x { L2.l2nsNamespaces = m }) l2nsc) >>=
                      -- All domains are also namespaces, so import the domain
                      -- alongside the namespace rather than as a separate rule...                      
                      (\v -> maybe (Just v) Just $
                             tryLookupImportAndAdd L2.l2nsNamespaces (\m x -> x { L2.l2nsNamespaces = m }) v),
                      tryLookupImportAndAdd L2.l2nsNamedValues (\m x -> x { L2.l2nsNamedValues =  m}) l2nsc,
                      tryLookupImportAndAdd L2.l2nsClassValues (\m x -> x { L2.l2nsClassValues =  m}) l2nsc,
                      tryLookupImportAndAdd L2.l2nsUnits (\m x -> x { L2.l2nsUnits =  m}) l2nsc,
                      tryLookupImportAndAdd L2.l2nsClasses (\m x -> x { L2.l2nsClasses =  m}) l2nsc,
                      tryLookupImportAndAdd L2.l2nsDomainFunctions (\m x -> x { L2.l2nsDomainFunctions =  m}) l2nsc,
                      tryLookupImportAndAdd L2.l2nsLabels (\m x -> x { L2.l2nsLabels =  m}) l2nsc
                      ]
      L1.L1NSNamespace {} ->
        -- TODO - check for name conflicts?
        return l2nsc -- The required work should have already been done.
      L1.L1NSDomain {
        L1.l1nsSS = ss,
        L1.l1nsDomainName = L1.L1Identifier idss ident,
        L1.l1nsDomainParameters = params,
        L1.l1nsDomainDefinition = dd } -> do
        Just domNSID <- findNamespace thisNSID ident
        domStatement <- translateDomainDefinition scope ss domNSID done params dd
        -- TODO - check for name conflicts
        return $ l2nsc {
          L2.l2nsDomains = M.insert ident domStatement (L2.l2nsDomains l2nsc)
          }
      L1.L1NSAssertion { L1.l1nsSS = ss, L1.l1nsExpression = ex } -> do
        l2ex <- translateExpression scope ss thisNSID done ex
        modifyL2Model $ \mod -> mod { L2.l2AllAssertions = l2ex:(L2.l2AllAssertions mod) }
        return l2nsc
      L1.L1NSNamedValue { L1.l1nsSS = ss, L1.l1nsValueName = L1.L1Identifier { L1.l1IdBS = n },
                          L1.l1nsDomainType = mt } -> do
        l2t <- maybe (return Nothing) (\t -> Just <$> translateDomainType scope ss thisNSID done t) mt
        valueID <- L2.l2NextValue <$> getL2Model
        modifyL2Model $ \mod -> mod {
          L2.l2NextValue = L2.L2ValueID ((\(L2.L2ValueID n) -> n + 1) valueID),
          L2.l2AllValues = M.insert valueID (L2.L2ValueContents ss l2t) (L2.l2AllValues mod)
          }
        -- TODO - check for name conflicts.
        return $ l2nsc {
          L2.l2nsNamedValues = M.insert n valueID (L2.l2nsNamedValues l2nsc)
          }
      L1.L1NSClass { L1.l1nsSS = ss, L1.l1nsClassName = L1.L1Identifier _ n, L1.l1nsClassParameters = params,
                     L1.l1nsClassDomainFunctions = df, L1.l1nsClassValues = v } -> do
        (_, scope', lcp) <-
          flip (flip foldM (S.empty, scope, [])) params $
            \(seenIDs, scope, lcp) (domID@(L1.L1ScopedID domIDSS domIDName), kind) -> do
              when (domIDName `S.member` seenIDs) $
                fail $ "Scoped domain identifier " ++ (BSC.unpack domIDName) ++
                       " appears multiple times in the same class signature, at " ++
                       (show domIDSS)
              return $ (S.insert domIDName seenIDs, scope, (domIDName, kind):lcp)
        (_, ldfnames) <-
          flip (flip foldM (S.empty, [])) df $
            \(seenIDs, ldfnames) (L1.L1Identifier dfss dfname, nargs) -> do
              when (dfname `S.member` seenIDs) $
                fail $ "Domain function identifier " ++ (BSC.unpack dfname) ++
                       " appears multiple times in the same class signature, at " ++
                       (show dfss)
              dfID <- L2.l2NextDomainFunctionID <$> getL2Model
              modifyL2Model $ \mod -> mod {
                  L2.l2AllDomainFunctions = M.insert dfID (L2.L2DomainFunctionContents dfss nargs)
                                                          (L2.l2AllDomainFunctions mod),
                  L2.l2NextDomainFunctionID = (\(L2.L2DomainFunctionID n) -> L2.L2DomainFunctionID (n + 1)) dfID
                }
              return (S.insert dfname seenIDs, (dfname, dfID):ldfnames)
        (_, lcvnames) <-
          flip (flip foldM (S.empty, [])) v $
            \(seenIDs, lcvnames) (L1.L1Identifier cvss cvname, cvtype) -> do
              when (cvname `S.member` seenIDs) $
                fail $ "Class value identifier " ++ (BSC.unpack cvname) ++
                       " appears multiple times in the same class signature, at " ++
                       (show cvss)
              cvID <- L2.l2NextClassValueID <$> getL2Model
              l2cvtype <- translateDomainType scope' ss thisNSID done cvtype
              modifyL2Model $ \mod -> mod {
                  L2.l2AllClassValues = M.insert cvID (L2.L2ClassValueContents cvss l2cvtype)
                                                      (L2.l2AllClassValues mod),
                  L2.l2NextClassValueID = (\(L2.L2ClassValueID n) -> L2.L2ClassValueID (n + 1)) cvID
                }
              return (S.insert cvname seenIDs, (cvname, cvID):lcvnames)
        let l2cc = L2.L2ClassContents { L2.l2ClassSS = ss,
                                        L2.l2ClassParameters = reverse lcp,
                                        L2.l2ClassDomainFunctions = M.fromList ldfnames,
                                        L2.l2ClassValues = M.fromList lcvnames
                                      }
        cid <- L2.l2NextClassID <$> getL2Model
        -- Register the class globally...
        modifyL2Model $ \mod -> mod { L2.l2AllClasses = M.insert cid l2cc (L2.l2AllClasses mod), 
                                      L2.l2NextClassID = (\(L2.L2ClassID n) -> L2.L2ClassID (n + 1)) cid }
        -- TODO - check for name conflicts (class, domain function, class value)
        -- let confDf = ((S.fromList (map fst ldfnames)) `S.intersect`
        --               (S.fromList . M.toList . L2.l2nsClassValues $ l2nsc))
        
        return $ l2nsc {
          L2.l2nsClassValues = (L2.l2nsClassValues l2nsc) `M.union` (M.fromList lcvnames),
          L2.l2nsDomainFunctions = (L2.l2nsDomainFunctions l2nsc) `M.union` (M.fromList ldfnames),
          L2.l2nsClasses = M.insert n cid (L2.l2nsClasses l2nsc)
                       }

      L1.L1NSEnsemble { L1.l1nsLabels = l } -> do
        (_, idLabels, nextLabel') <-
          flip (flip foldM (S.empty, [], L2.l2nsNextLabel l2nsc)) l $
            \(seenIDs, idLabels, nextLabel) (L1.L1Identifier idss n) -> do
              when (n `S.member` seenIDs) . fail $
                "Duplicate label " ++ (BSC.unpack n) ++ " in ensemble at " ++ (show idss)
              return $ (S.insert n seenIDs, (n, L2.L2Label thisNSID (fromIntegral nextLabel)):idLabels, nextLabel + 1)
        -- TODO check for name conflicts
        return $ l2nsc {
            L2.l2nsNextLabel = nextLabel',
            L2.l2nsLabels = (L2.l2nsLabels l2nsc) `M.union` (M.fromList idLabels)
          }
      L1.L1NSUnit { L1.l1nsSS = ss, L1.l1nsUnitName = L1.L1Identifier uss n, L1.l1nsUnitDefinition = d } -> do
        l2uDef <- translateUnitDefinition scope ss thisNSID done d
        -- TODO check for name conflicts
        return $ l2nsc {
            L2.l2nsUnits = M.insert n l2uDef (L2.l2nsUnits l2nsc)
          }
      L1.L1NSInstance { L1.l1nsSS = ss, L1.l1nsInstanceOfClass = cpath,
                        L1.l1nsClassArguments = args, L1.l1nsInstanceDomainFunctions = dfs,
                        L1.l1nsInstanceValues = vals } -> do
        -- Find the class...
        (nspath, className) <- maybe (fail $ "Attempt to use an empty path as a class name, at " ++ show ss)
                                     return $ trySplitRAPathOnLast cpath
        mnsid <- findNamespaceByRAPath thisNSID nspath
        case mnsid of
          Nothing -> fail $ "Cannot resolve relative path " ++ (show cpath) ++ " to class for instance at " ++
                            (show ss)
          Just classNamespace -> do
            cnsc <- getNamespaceContents classNamespace
            classID <-
                  maybe (fail $ "Cannot find class " ++ (BSC.unpack . L1.l1IdBS $ className) ++
                                " named in instance at " ++ (show . L1.l1IdSS $ className))
                        return
                        (M.lookup (L1.l1IdBS className) (L2.l2nsClasses cnsc))
            Just class' <- (M.lookup classID . L2.l2AllClasses) <$> getL2Model
            l2args <- mapM (translateDomainType scope ss thisNSID done) args
            l2dfs <- mapM (\(L1.L1Identifier ifss instfuncident, dts, dex) ->
                            (,,) <$>
                              (case M.lookup instfuncident (L2.l2ClassDomainFunctions class') of
                                  Nothing -> fail $ "Instance refers to domain function " ++
                                                    (BSC.unpack instfuncident) ++ " at " ++
                                                    (show ifss) ++ ", but that domain function " ++
                                                    "could not be found in the class."
                                  Just ifid -> return ifid
                              ) <*>
                              (mapM (translateDomainType scope ss thisNSID done) dts) <*>
                              translateDomainExpression scope ss thisNSID done dex
                          ) dfs
            l2exprs <- mapM (translateExpression scope ss thisNSID done) vals
            modifyL2Model $ \mod ->
              mod { L2.l2AllInstances = 
                       (L2.L2InstanceContents { L2.l2InstanceSS = ss,
                                                L2.l2InstanceOfClass = classID, 
                                                L2.l2InstanceClassArguments = l2args,
                                                L2.l2InstanceDomainFunctions = l2dfs,
                                                L2.l2InstanceValues = l2exprs }):
                       (L2.l2AllInstances mod) }
            return l2nsc
  
trySplitRAPathOnLast :: L1.L1RelOrAbsPath -> Maybe (L1.L1RelOrAbsPath, L1.L1Identifier)
trySplitRAPathOnLast (L1.L1RelOrAbsPath ss ra p) =
   maybe Nothing (\(x, y) -> Just (L1.L1RelOrAbsPath ss ra x, y))
         (trySplitRPathOnLast p)

trySplitRPathOnLast (L1.L1RelPath rpss []) = Nothing
trySplitRPathOnLast (L1.L1RelPath rpss l) = Just (L1.L1RelPath rpss (init l), last l)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f l = catMaybes `liftM` sequence (map f l)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat `liftM` sequence (map f l)

allocDomainID :: ModelTranslation L2.L2DomainID
allocDomainID = do
  newID <- L2.l2NextDomain <$> getL2Model
  modifyL2Model $ \mod -> mod { L2.l2NextDomain = (\(L2.L2DomainID n) -> L2.L2DomainID (n + 1)) newID }
  return newID

tryTranslateDomainDefinition :: L1.SrcSpan -> S.Set L1NSPath -> L2.L2NamespaceID -> (L1NSPath -> L1NSPath)
                                -> [L1.L1ScopedID] -> L1.L1DomainDefinition -> ModelTranslation (Either [L1NSPath] L2.L2DomainType)
tryTranslateDomainDefinition ssFrom done nsid pToHere domainVars dd = do
  deps <- getDomainDefinitionDependencies pToHere dd
  let missing = S.toList ((S.fromList deps) `S.difference` done)
  if null missing
    then Right <$> translateDomainDefinition def ssFrom nsid done [] dd
    else return . Left $ missing

foldOverNSScopesM :: (s -> L2.L2NamespaceID -> ModelTranslation s) -> s -> L2.L2NamespaceID -> ModelTranslation s
foldOverNSScopesM f s0 nsid = do
  s1 <- f s0 nsid
  nsc <- (fromJust . M.lookup nsid . L2.l2AllNamespaces) <$> getL2Model
  foldOverNSScopesM f s1 (L2.l2nsParent nsc)

findScopedSymbolByRAPath :: L1.SrcSpan -> L2.L2NamespaceID -> L1.L1RelOrAbsPath ->
                            (L2.L2NamespaceContents -> L1.L1Identifier -> Maybe a) -> ModelTranslation a
findScopedSymbolByRAPath ss startNS ra tryGet = do
  (nsra@(L1.L1RelOrAbsPath _ isabs rp), symName) <- maybe (fail $ "Inappropriate use of an empty path, at " ++ show ss)
                             return $ trySplitRAPathOnLast ra
  -- Recursively find the namespace...
  nsid <- maybe (fail $ "Cannot find namespace " ++ show nsra ++ " mentioned at " ++ show ss) return =<<
          findNamespaceByRAPath startNS nsra
  let foldStrategy =
        if (not isabs && null (L1.l1RelPathIDs rp))
           then flip (flip foldOverNSScopesM Nothing) nsid
           else (\x -> x Nothing nsid)
  r <- foldStrategy $ \s nsid' -> (mplus s) <$> do
    nsc <- (fromJust . M.lookup nsid' . L2.l2AllNamespaces) <$> getL2Model
    return $ tryGet nsc symName
  maybe (fail $ "Symbol " ++ (BSC.unpack . L1.l1IdBS $ symName) ++
                " not found in namespace at " ++ (show . L1.l1IdSS $ symName)) return r

translateLabel :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1RelOrAbsPathPossiblyIntEnd -> ModelTranslation L2.L2Expression
translateLabel scope _ nsid done (L1.L1RelOrAbsPathInt pss ra rp v) = do
  nsid' <- maybe (fail $ "Cannot find namespace " ++ (show ra) ++ " mentioned at " ++ show pss)
                return =<< findNamespaceByRAPath nsid (L1.L1RelOrAbsPath pss ra rp)
  when (nsid' /= nsInteger && nsid' /= nsNatural) . fail $
    "Literal integer syntax can only be used to refer to labels in the \
    \built in Integer and Natural domains(e.g. Natural:1), so usage at " ++
    (show pss) ++ " is invalid."
  when (nsid' == nsNatural && v < 0) . fail $
    "Attempt to reference a negative natural number, at " ++ (show pss)
  return . L2.L2ExReferenceLabel pss $ L2.L2Label nsid' (fromIntegral v)

translateLabel scope _ nsid done (L1.L1RelOrAbsPathNoInt pss isabs rp) = do
  let ra = L1.L1RelOrAbsPath pss isabs rp
  findScopedSymbolByRAPath pss nsid ra $ \nsc labelName ->
    case M.lookup (L1.l1IdBS labelName) (L2.l2nsLabels nsc) of
      Just l -> Just $ L2.L2ExReferenceLabel pss l
      Nothing ->
        case M.lookup (L1.l1IdBS labelName) (L2.l2nsNamedValues nsc) of
          Just nv -> Just $ L2.L2ExReferenceValue pss nv
          Nothing -> case M.lookup (L1.l1IdBS labelName) (L2.l2nsClassValues nsc) of
            Just cv -> Just $ L2.L2ExReferenceClassValue pss cv
            Nothing -> Nothing

translateExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1Expression -> ModelTranslation L2.L2Expression
translateExpression scope _ nsid done (L1.L1ExApply ss op arg) =
  L2.L2ExApply ss <$> translateExpression scope ss nsid done op
                  <*> translateExpression scope ss nsid done arg

translateExpression scope _ nsid done (L1.L1ExReference ss l) =
  translateLabel scope ss nsid done l

translateExpression scope _ _ done (L1.L1ExBoundVariable ss scopedID@(L1.L1ScopedID idss _)) =
  L2.L2ExBoundVariable ss <$>
    maybe (fail $ "Reference to unknown local variable at " ++ show idss)
          return (M.lookup scopedID (siValueIDMap scope))

translateExpression scope _ nsid done (L1.L1ExLiteralReal ss units rv) =
  L2.L2ExLiteralReal ss
    <$> translateUnitExpression scope ss nsid done units
    <*> (pure rv)

translateExpression scope _ nsid done (L1.L1ExMkProduct ss values) =
  L2.L2ExMkProduct ss
    <$> mapM (\(l, ex) ->
               (,) <$> (do
                           lret <- translateLabel scope ss nsid done l
                           case lret of
                             L2.L2ExReferenceLabel _ lab -> return lab
                             _ -> fail $
                                  "Label in product must actually be a label, not "
                                  ++ "a named value, at " ++ (show ss)
                       )
                   <*> translateExpression scope ss nsid done ex
             ) values

translateExpression scope _ nsid done (L1.L1ExMkUnion ss label value) =
  L2.L2ExMkUnion ss
    <$> (do
            lret <- translateLabel scope ss nsid done label
            case lret of
              L2.L2ExReferenceLabel _ lab -> return lab
              _ -> fail $
                   "Label in union must actually be a label, not "
                   ++ "a named value. at " ++ (show ss)
        )
    <*> translateExpression scope ss nsid done value

translateExpression scope _ nsid done (L1.L1ExProject ss label) =
  L2.L2ExProject ss
    <$> (do
            lret <- translateLabel scope ss nsid done label
            case lret of
              L2.L2ExReferenceLabel _ lab -> return lab
              _ -> fail $
                   "Label in 'project' invocation must actually be a label, not "
                   ++ "a named value, at " ++ (show ss)
        )

translateExpression scope _ nsid done (L1.L1ExAppend ss label) =
  L2.L2ExAppend ss
    <$> (do
            lret <- translateLabel scope ss nsid done label
            case lret of
              L2.L2ExReferenceLabel _ lab -> return lab
              _ -> fail $
                   "Label in 'append' invocation must actually be a label, not "
                   ++ "a named value, at " ++ (show ss)
        )

translateExpression scope _ nsid done (L1.L1ExLambda ss bvar value) = do
  newBVar <- L2.l2NextScopedValueID <$> getL2Model
  modifyL2Model $ \mod -> mod {
    L2.l2NextScopedValueID = (\(L2.L2ScopedValueID n) ->
                               L2.L2ScopedValueID (n + 1)) newBVar }
  L2.L2ExLambda ss newBVar <$>
      translateExpression (scope { siValueIDMap = M.insert bvar newBVar (siValueIDMap scope)})
                           ss nsid done value

translateExpression scope _ nsid done (L1.L1ExCase ss expr values) =
  L2.L2ExCase ss
    <$> translateExpression scope ss nsid done expr
    <*> mapM (\(l, cex) ->
               (,)
                 <$> (do
                         lret <- translateLabel scope ss nsid done l
                         case lret of
                           L2.L2ExReferenceLabel _ lab -> return lab
                           _ -> fail $
                                "Label in 'append' invocation must actually be a label, not "
                                ++ "a named value, at " ++ (show ss)
                     )
                 <*> translateExpression scope ss nsid done cex) values

translateExpression scope _ nsid done (L1.L1ExLet ss expr closure) = do
  letNS <- registerNewNamespace ss nsid closure
  (_, nsAsserts) <-
    isolateAssertions
      (recursivelyTranslateModel letNS S.empty done [(ss, L1NSPathStop)])
  L2.L2ExLet ss
    <$> translateExpression scope ss letNS done expr
    <*> (pure letNS)
    <*> (pure nsAsserts)

translateExpression scope _ nsid done (L1.L1ExString ss sv) =
  return $ L2.L2ExString ss sv

translateExpression scope _ nsid done (L1.L1ExSignature ss ex sig) = do
  L2.L2ExSignature ss <$> (translateExpression scope ss nsid done ex)
                      <*> (translateDomainType scope ss nsid done sig)

translateUnitExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1UnitExpression -> ModelTranslation L2.L2UnitExpression
translateUnitExpression scope _ nsid _ (L1.L1UnitExDimensionless ss) =
  pure $ L2.L2UnitExDimensionless ss
translateUnitExpression scope _ nsid _ (L1.L1UnitExRef ss refpath) =
  findScopedSymbolByRAPath ss nsid refpath $ \nsc unitName -> M.lookup (L1.l1IdBS unitName) (L2.l2nsUnits nsc)
translateUnitExpression scope _ nsid done (L1.L1UnitExTimes ss expr1 expr2) =
  L2.L2UnitExTimes ss <$> translateUnitExpression scope ss nsid done expr1
                      <*> translateUnitExpression scope ss nsid done expr2
translateUnitExpression scope _ nsid done (L1.L1UnitPow ss expr1 powerOf) =
  L2.L2UnitPow ss <$> (translateUnitExpression scope ss nsid done expr1)
                  <*> (pure powerOf)
translateUnitExpression scope _ nsid done (L1.L1UnitScalarMup ss scalv expr1) =
  L2.L2UnitScalarMup ss scalv <$> (translateUnitExpression scope ss nsid done expr1)
translateUnitExpression scope _ nsid done (L1.L1UnitScopedVar ss scopedv) =
  L2.L2UnitScopedVar ss <$>
    maybe (fail $ "Reference to unknown scoped units name " ++ (BSC.unpack . L1.l1ScopedIdBS $ scopedv) ++ " at " ++ (show ss)) return (M.lookup scopedv (siUnitIDMap scope))

translateUnitDefinition :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1UnitDefinition -> ModelTranslation L2.L2UnitExpression
translateUnitDefinition scope _ nsid _ (L1.L1UnitDefNewBase ss) = do
  buID <- L2.l2NextBaseUnits <$> getL2Model
  modifyL2Model $ \mod ->
    mod { L2.l2NextBaseUnits = (\(L2.L2BaseUnitsID n) -> L2.L2BaseUnitsID (n + 1)) buID,
          L2.l2AllBaseUnits = M.insert buID (L2.L2BaseUnitContents ss) (L2.l2AllBaseUnits mod) }
  return $ L2.L2UnitExRef ss buID
translateUnitDefinition scope _ nsid done (L1.L1UnitDefUnitExpr ss expr) =
  translateUnitExpression scope ss nsid done expr

translateDomainExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1DomainExpression -> ModelTranslation L2.L2DomainExpression
translateDomainExpression scope _ nsid done (L1.L1DomainExpressionProduct ss labels) =
  L2.L2DomainExpressionProduct ss <$>  translateLabelledDomains scope ss nsid done labels
translateDomainExpression scope _ nsid done (L1.L1DomainExpressionDisjointUnion ss labels) =
  L2.L2DomainExpressionDisjointUnion ss <$>  translateLabelledDomains scope ss nsid done labels
translateDomainExpression scope _ nsid done (L1.L1DomainExpressionFieldSignature ss dom cod) =
  L2.L2DomainExpressionFieldSignature ss <$> translateDomainExpression scope ss nsid done dom
                                         <*> translateDomainExpression scope ss nsid done cod
translateDomainExpression scope _ nsid done (L1.L1DomainExpressionReal ss units) =
  L2.L2DomainExpressionReal ss <$> translateUnitExpression scope ss nsid done units
translateDomainExpression scope _ nsid done (L1.L1DomainExpressionApply ss dom scopedVar value) =
  L2.L2DomainExpressionApply ss
    <$> translateDomainExpression scope ss nsid done dom
    <*> pure (L1.l1ScopedIdBS scopedVar)
    <*> translateDomainExpression scope ss nsid done value
translateDomainExpression scope _ nsid _ (L1.L1DomainFunctionEvaluate ss func args) = undefined -- TODO
translateDomainExpression scope _ nsid _ (L1.L1DomainVariableRef ss scopedID) = undefined -- TODO
translateDomainExpression scope _ nsid _ (L1.L1DomainReference _ (rapi@L1.L1RelOrAbsPathInt{})) = fail undefined -- TODO
translateDomainExpression scope _ nsid _ (L1.L1DomainReference _ (L1.L1RelOrAbsPathNoInt ss ra rp)) = undefined -- TODO

translateLabelledDomains :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID ->
                            S.Set L1NSPath ->
                            L1.L1LabelledDomains -> ModelTranslation L2.L2LabelledDomains
translateLabelledDomains = undefined -- TODO


translateDomainDefinition :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID ->
                             S.Set L1NSPath ->
                             [L1.L1ScopedID] -> L1.L1DomainDefinition -> ModelTranslation L2.L2DomainType
translateDomainDefinition = undefined -- TODO

translateDomainType :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> S.Set L1NSPath -> L1.L1DomainType -> ModelTranslation L2.L2DomainType
translateDomainType = undefined -- TODO

-- | Runs a model translation such that any new assertions that are added do not
--   end up in the global assertion list, but instead becomes part of the result.
isolateAssertions :: ModelTranslation a -> ModelTranslation (a, [L2.L2Expression])
isolateAssertions f = do
  origAssertions <- L2.l2AllAssertions <$> getL2Model
  modifyL2Model $ \mod -> mod { L2.l2AllAssertions = [] }
  r <- f
  newAssertions <- L2.l2AllAssertions <$> getL2Model
  modifyL2Model $ \mod -> mod { L2.l2AllAssertions = origAssertions }
  return (r, newAssertions)

-- | Recursively change a path to the equivalent path that does not reference
--   any local imports. Produces Nothing if a symbol eventually refers to a
--   non-local import; leaves symbols which don't have any imports unchanged.
--   Fails with an error if it finds an import that refers to something that
--   doesn't exist.
ensureNoLocalImports :: L1NSPath -> ModelTranslation (Maybe L1NSPath)
ensureNoLocalImports p = undefined -- TODO

getDomainDefinitionDependencies :: (L1NSPath -> L1NSPath) -> L1.L1DomainDefinition -> ModelTranslation [L1NSPath]
getDomainDefinitionDependencies pToHere (L1.L1CloneDomain _ dt) =
  getDomainTypeDependencies pToHere dt
getDomainDefinitionDependencies pToHere (L1.L1SubsetDomain _ dt susing) =
  (++) <$> getExpressionDependencies pToHere susing
       <*> getDomainTypeDependencies pToHere dt
getDomainDefinitionDependencies pToHere (L1.L1ConnectDomain _ dt cusing) =
  (++) <$> getExpressionDependencies pToHere cusing
       <*> getDomainTypeDependencies pToHere dt
getDomainDefinitionDependencies pToHere (L1.L1DomainDefDomainType _ dt) =
  getDomainTypeDependencies pToHere dt

getExpressionDependencies :: (L1NSPath -> L1NSPath) -> L1.L1Expression -> ModelTranslation [L1NSPath]
getExpressionDependencies pToHere ex = undefined -- TODO

getDomainTypeDependencies :: (L1NSPath -> L1NSPath) -> L1.L1DomainType -> ModelTranslation [L1NSPath]
getDomainTypeDependencies pToHere dt = undefined -- TODO

getUnitDefinitionDependencies :: (L1NSPath -> L1NSPath) -> L1.L1UnitDefinition -> ModelTranslation [L1NSPath]
getUnitDefinitionDependencies pToHere ud = undefined -- TODO

getDomainExpressionDependencies :: (L1NSPath -> L1NSPath) -> L1.L1DomainExpression -> ModelTranslation [L1NSPath]
getDomainExpressionDependencies pToHere dex = undefined -- TODO

-- | Finds a Level 2 namespace using a Level 1 RelOrAbsPath
findNamespaceByRAPath :: L2.L2NamespaceID -> L1.L1RelOrAbsPath -> ModelTranslation (Maybe L2.L2NamespaceID)
findNamespaceByRAPath thisNSID rapath = foldOverNSScopesM undefined undefined thisNSID -- TODO

arpathToNSPathFn :: L1NSPath -> L1.L1RelOrAbsPath -> ModelTranslation L1NSPath
arpathToNSPathFn p (L1.L1RelOrAbsPath ss abs rp) = rpathToNSPathFn ss p abs rp

rpathToNSPathFn :: L1.SrcSpan -> L1NSPath -> Bool -> L1.L1RelPath -> ModelTranslation L1NSPath
rpathToNSPathFn ss _ True rpath = -- Absolute path...
  rpathToNSPathFn ss L1NSPathStop False rpath

rpathToNSPathFn ss L1NSPathStop False (L1.L1RelPath rpss []) = return L1NSPathStop
rpathToNSPathFn ss L1NSPathStop False (L1.L1RelPath rpss (rphead:rpids)) = do
  ModelTranslationState { mtsL2Model = m } <- get
  undefined -- TODO
