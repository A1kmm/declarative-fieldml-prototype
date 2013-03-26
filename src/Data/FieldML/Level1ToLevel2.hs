{-# LANGUAGE DeriveDataTypeable, TypeFamilies, PatternGuards, OverloadedStrings #-}
module Data.FieldML.Level1ToLevel2 (loadL2ModelFromURL) where

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
import qualified Data.Foldable as T
import Data.Monoid
import qualified Debug.Trace

-- | Loads an L2 model, and all dependencies, or returns an error.
loadL2ModelFromURL :: [String] -> String -> ErrorT String IO (L2.L2Model)
loadL2ModelFromURL incl mpath =
  runLookupModel (loadL2ModelFromURL' incl mpath)

-- | The monad in which the overall model load occurs; it stores a
--   a list of model URLs already loaded (with their cached L2 models)
--   and allows for model loading errors to occur. It also allows
--   lifted IO monad expressions.
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

-- | Runs a LookupModel in the ErrorT String IO monad.
runLookupModel :: LookupModel a -> ErrorT String IO a
runLookupModel x = evalStateT (unlookupModel x) M.empty

-- | Tries to load a particular model from the cache or by loading it from
--   the filesystem (looking in the specified paths) and parsing it. The
--   parse may recursively load further models.
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
              nsc <- LookupModel . lift . ErrorT $ return (parseFieldML mpath v)
              finalImpMap <- tryResolveAllImports incl nsc
              mod <- LookupModel . lift $ translateL1ToL2 mpath nsc finalImpMap
              modify (M.insert (BSC.pack fullPath) mod)
              return $ Just mod
            else
              return Nothing)

-- | Attempts to load all imports mentioned in a L1 model.
tryResolveAllImports :: [String] -> L1.L1NamespaceContents -> LookupModel (M.Map BS.ByteString L2.L2Model)
tryResolveAllImports incl nsc = do
  forM_ (nub . sort $ [v | L1.L1NSImport { L1.l1nsImportFrom = Just v} <- universeBi nsc]) $ \toImport -> do
    lm <- loadL2ModelFromURL' incl (BSC.unpack toImport)
    modify $ M.insert toImport lm
  get

-- | Attempts to translate a parsed L1 model into L2. This function depends
--   on all imports being loaded already.
translateL1ToL2 :: String -> L1.L1NamespaceContents -> M.Map BS.ByteString L2.L2Model -> ErrorT String IO L2.L2Model
translateL1ToL2 mpath l1ns impmap =
  flip runReaderT (handleL1SimpleSyntacticSugar (BSC.pack mpath) l1ns, mpath, impmap) $
    flip evalStateT ((def :: ModelTranslationState) { mtsL2ToL1Map = M.singleton nsMain l1ns} ) $ do
      -- Create a skeleton model, where all symbols except those that are
      -- imported exist, but do not yet have their proper definitions set up
      -- yet.
      (l1ns, _, _) <- ask
      buildSkeletonModel (L1.SrcSpan mpath 0 0 0 0) l1ns nsMain
      -- Load all external imports into the model.
      processExternalImports nsMain
      -- Resolve all internal imports in the model.
      processInternalImports nsMain
      -- Fill out the model skeleton with the actual functions.
      recursivelyTranslateModel nsMain
      -- Repair references to temporary IDs for aliases...
      fixAliasReferences nsMain
      getL2Model

newtype DesugarTmp = DesugarTmp Int
desugarTmpName :: State DesugarTmp BS.ByteString
desugarTmpName = do
  DesugarTmp sugId <- get
  put (DesugarTmp (sugId + 1))
  return $ BSC.pack ("_desugar_" ++ show sugId)

-- | Some L1 constructs can be translated to an equivalent canonical L1 that
--   doesn't use certain features. This function is run to simplify the L1
--   input slightly to make the L1 -> L2 translation slightly easier to write.
--   It makes the following transformations:
--     => import x as y => namespace y where import x
--     => import from x ... | x == own URI => import ...
--     => ensemble {...} as x => namespace x where ensemble {...}
handleL1SimpleSyntacticSugar :: BS.ByteString -> L1.L1NamespaceContents -> L1.L1NamespaceContents
handleL1SimpleSyntacticSugar self =
    transformBi (g . f) . (\v -> evalState (transformBiM (
                                               desugarComplexLambda .
                                               desugarL1Patterns .
                                               desugarFCase) v) (DesugarTmp 0))
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

desugarFCase :: L1.L1Expression -> State DesugarTmp L1.L1Expression
desugarFCase (L1ExFCase ss values) = do
  tmpScopedID <- L1.L1ScopedID ss <$> desugarTmpName
  L1ExLambda ss (L1.L1PatternBind ss tmpScopedID) (L1ExCase ss (L1ExBoundVariable ss tmpScopedID) values)
desugarFCase x = return x

desugarComplexLambda :: L1.L1Expression -> State DesugarTmp L1.L1Expression
desugarComplexLambda simpleLambda@(L1.L1ExLambda _ (L1.L1PatternBind _ _) _) = return simpleLambda
desugarComplexLambda (L1.L1ExLambda ss complexPattern value) =
  return $
    L1.L1ExFCase ss [(complexPattern, value)]
desugarComplexLambda x = return x

pathGlobal :: BS.ByteString -> L1.L1RelOrAbsPathPossiblyIntEnd
pathGlobal g = L1.L1RelOrAbsPathNoInt ss True (L1RelPath ss [L1.L1Identifier ss g])

makeErrorExpression :: BS.ByteString -> L1.L1Expression
makeErrorExpression v = (L1.L1ExApply ss
                         (L1.L1ExReference ss (pathGlobal "error"))
                         (L1.L1ExString ss v)
                        )

desugarL1Patterns :: L1.L1Expression -> State DesugarTmp L1.L1Expression
desugarL1Patterns ex@(L1.L1ExCase ss expr values) =
    -- We sequentially test each value for a match, and if they match, we then go ahead and try to extract the contents.
  let
    addPatternToCase (pat, ifEx) otherwiseEx =
      L1.L1ExCase ss
        <$> (testPatternUsing ss expr pat)
        <*> (
              (\x y -> [x, y])
               <$> ((,) (L1.L1PatternAs ss (pathGlobal "true") L1.L1PatternIgnore) <$> patternToExtractLambdas expr pat ifEx)
               <*> (pure (L1.L1ExLambda ss svar (L1.L1PatternAs ss (pathGlobal "false") L1.L1PatternIgnore, otherwiseEx)))
            )
    foldr addPatternToCase (makeErrorExpression $ "Nothing matched pattern at " <> (BSC.pack . show $ ss))  values
  return ()
desugarL1Patterns ex = ex

testPatternUsing :: L1.L1Expression -> L1.L1Pattern -> State DesugarTmp L1.L1Expression
testPatternUsing _ (L1.L1PatternIgnore ss) = L1.L1ExReference ss (pathGlobal "true")
testPatternUsing _ (L1.L1PatternBind ss _) = L1.L1ExReference ss (pathGlobal "true")
testPatternUsing testEx (L1.L1PatternAs ss label pattern) = do
  lambdaScoped <- L1.L1ScopedID ss <$> desugarTmpName
  subPatternTest <- testPatternUsing (L1.L1ExBoundVariable ss lambdaScoped) pattern
  return $ 
    L1.L1ExCase ss testEx [(L1.L1PatternAs ss label L1.L1PatternIgnore,
                            L1.L1ExLambda ss (L1.L1PatternBind ss lambdaScoped) subPatternTest),
                           (L1.L1PatternIgnore ss, L1.L1ExReference ss (pathGlobal "false"))]
testPatternUsing testEx (L1.L1PatternProduct ss []) =
  return $ (L1.L1ExReference ss (pathGlobal "true"))
testPatternUsing testEx (L1.L1PatternProduct ss args) =
  let
    testProductPart (label, pat) = testPatternUsing (L1.L1ExApply (L1.L1ExProject ss label) testEx) pat
  in
   foldM (\(exOther, prodThis) -> L1.L1ExApply (L1.L1ExApply ss (L1.L1ExReference ss (pathGlobal "&&")) exOther)
                                               <$> testProductPart prodThis
         ) <$> (testProductPart (last args)) <*> (pure . init $ args)

patternToExtractLambdas :: L1.L1Expression -> L1.L1Pattern -> L1.L1Expression -> State DesugarTmp L1.L1Expression
patternToExtractLambdas testEx (L1.L1PatternIgnore ss) ifEx = do
  lambdaScoped <- L1.L1ScopedID ss <$> desugarTmpName
  return $ L1.L1ExLambda ss (L1.L1PatternBind ss lambdaScoped) ifEx
patternToExtractLambdas testEx (L1.L1PatternBind ss svar) ifEx = return $ L1.L1ExLambda ss (L1.L1PatternBind ss svar) ifEx
patternToExtractLambdas testEx (L1.L1PatternAs ss label pattern) ifEx = do
  lambdaScoped <- L1.L1ScopedID ss <$> desugarTmpName
  subLambda <- patternToExtractLambdas testEx pattern ifEx
  return $ 
    L1.L1ExLambda ss (L1.L1PatternBind ss subLambda) $ subLambda
    (L1.L1ExCase ss (L1.L1ExBoundVariable ss lambdaScoped)
                        [(L1.L1PatternAs ss label (L1.L1PatternIgnore ss), ifEx)])

-- | The ModelTranslation monad carries all state and reader information needed to
--   translate a L1 model into a L2 model.
type ModelTranslation a = StateT ModelTranslationState
                          (ReaderT (L1.L1NamespaceContents, String, M.Map BS.ByteString L2.L2Model)
                                  (ErrorT String IO)) a
-- | ModelTranslationState carries all information needed to describe the current state
--   of the model translation from L1 -> L2 in progress.
data ModelTranslationState = ModelTranslationState {
    mtsL2Model :: L2.L2Model,                                      -- ^ The L2 model, so far.
    -- | Maps L2 namespace IDs to L1 contents.
    -- Note: This may be unnecessary in the two pass system.
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
    mtsForeignToLocalScopedDomain ::
      ForeignToLocal L2.L2ScopedDomainID,
    mtsForeignToLocalDomainFunction ::
      ForeignToLocal L2.L2DomainFunctionID,
    mtsForeignToLocalClassValue ::
      ForeignToLocal L2.L2ClassValueID,
    -- | Temporary IDs are used during translation for aliases, before we are
    --   ready to actually process the alias. Since they are used in types
    --   that normally carry permanent IDs, they are negative to distinguish
    --   them.
    mtsNextTempId :: Int,
    -- | Maps temporary IDs to a domain type.
    mtsTempIDDomainType :: M.Map L2.L2DomainID L2.L2DomainExpression,
    -- | Maps temporary IDs to a unit expression.
    mtsTempIDUnitEx :: M.Map L2.L2BaseUnitsID L2.L2UnitExpression
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
    mtsForeignToLocalClassValue = M.empty,
    mtsForeignToLocalScopedDomain = M.empty,
    mtsTempIDDomainType = M.empty,
    mtsTempIDUnitEx = M.empty,
    mtsNextTempId = -1
                              }

-- | The type of a map from a foreign URL (model identifier) and ID to a local
--   identifier. Used to keep track of whether a foreign symbol has already
--   been imported.
type ForeignToLocal a = M.Map (L2.Identifier, a) a

-- | Scope information is carried down expression trees, but not across to
--   different branches (unlike the other state).
data ScopeInformation = ScopeInformation {
  siValueIDMap  :: M.Map L1.L1ScopedID L2.L2ScopedValueID,
  siUnitIDMap   :: M.Map L1.L1ScopedID L2.L2ScopedUnitID,
  siDomainIDMap :: M.Map L1.L1ScopedID L2.L2ScopedDomainID
  }
instance Default ScopeInformation where
  def = ScopeInformation M.empty M.empty M.empty

buildSkeletonModel ss nsc@(L1.L1NamespaceContents c) myNSID = do
  let dtRef ss x = L2.L2DomainReference ss x
  let dummyDT ss = dtRef ss (L2.L2DomainID 0)
  let dummyCVC ss = L2.L2ClassValueContents ss (dummyDT ss)

  -- TODO - check for name conflicts.
  forM_ c $ \nsel ->
    case nsel of
      L1.L1NSNamespace { L1.l1nsSS = nsss, L1.l1nsNamespaceName = L1.L1Identifier _ nsname,
                         L1.l1nsNamespaceContents = newnsc } -> do
        newNSID <- registerNewNamespace nsss myNSID newnsc
        newLabel <- L2.l2nsNextLabel <$> getNamespaceContents myNSID
        Debug.Trace.trace ("Adding Label#" ++ show newLabel ++ " into " ++ show myNSID ++ " for " ++ (show nsname)) (return ())
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
          L2.l2nsNamespaces = M.insert nsname newNSID (L2.l2nsNamespaces l2nsc),
          L2.l2nsLabels = M.insert nsname (L2.L2Label myNSID (fromIntegral newLabel)) (L2.l2nsLabels l2nsc),
          L2.l2nsNextLabel = newLabel + 1
             }
        tmpId <- mtsNextTempId <$> get
        modify $ \mts -> mts { mtsNextTempId = tmpId - 1 }
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
          L2.l2nsDomains = M.insert nsname (dtRef nsss $ L2.L2DomainID tmpId) (L2.l2nsDomains l2nsc)
          }

        buildSkeletonModel nsss newnsc newNSID
      L1.L1NSDomain { L1.l1nsSS = nsss, L1.l1nsDomainName = L1.L1Identifier _ nsname,
                      L1.l1nsNamespaceContents = newnsc, 
                      L1.l1nsDomainDefinition = dd } -> do
        newNSID <- registerNewNamespace nsss myNSID newnsc
        newLabel <- L2.l2nsNextLabel <$> getNamespaceContents myNSID
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
          L2.l2nsNamespaces = M.insert nsname newNSID (L2.l2nsNamespaces l2nsc),
          L2.l2nsLabels = M.insert nsname (L2.L2Label myNSID (fromIntegral newLabel)) (L2.l2nsLabels l2nsc),
          L2.l2nsNextLabel = newLabel + 1
             }
        buildSkeletonModel nsss newnsc newNSID
        case dd of
          L1.L1DomainDefDomainType ddss _ -> do
            -- It is an alias, but we aren't ready to process the alias yet,
            -- so we allocate a temporary ID...
            tmpId <- mtsNextTempId <$> get
            modify $ \mts -> mts { mtsNextTempId = tmpId - 1 }
            modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
              L2.l2nsDomains = M.insert nsname (dtRef ddss $ L2.L2DomainID tmpId) (L2.l2nsDomains l2nsc)
              }
          _ -> do
            -- It's a clonelike domain, so allocate an actual domain ID...
            newDomainID <- L2.l2NextDomain <$> getL2Model
            modifyL2Model $ \mod -> mod {
              L2.l2NextDomain = (\(L2.L2DomainID i) -> L2.L2DomainID (i + 1)) newDomainID,
              L2.l2AllDomains = M.insert newDomainID (L2.L2ClonelikeDomainContents nsss (dummyDT nsss) L2.L2DomainClone)
                                         (L2.l2AllDomains mod)
              }
            modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
              L2.l2nsDomains = M.insert nsname (dtRef nsss newDomainID) (L2.l2nsDomains l2nsc)
              }
      L1.L1NSNamedValue { L1.l1nsSS = nsss, L1.l1nsValueName = L1.L1Identifier _ nvname } -> do
        newValueID <- L2.l2NextValue <$> getL2Model
        modifyL2Model $ \mod -> mod {
          L2.l2NextValue = (\(L2.L2ValueID i) -> L2.L2ValueID (i + 1)) newValueID,
          L2.l2AllValues = M.insert newValueID (L2.L2ValueContents nsss Nothing)
                             (L2.l2AllValues mod)
          }
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
          L2.l2nsNamedValues = M.insert nvname newValueID (L2.l2nsNamedValues l2nsc)
          }
      L1.L1NSClass { L1.l1nsSS = nsss, L1.l1nsClassName = L1.L1Identifier _ clname, L1.l1nsClassDomainFunctions = dfs, 
                     L1.l1nsClassValues = cvs } -> do
        l2dfs <- forM dfs $ \(L1.L1Identifier dfss dfname, n) -> do
          newDFID <- L2.l2NextDomainFunctionID <$> getL2Model
          modifyL2Model $ \mod -> mod {
            L2.l2NextDomainFunctionID =
               (\(L2.L2DomainFunctionID i) -> L2.L2DomainFunctionID (i + 1)) newDFID,
            L2.l2AllDomainFunctions =
              M.insert newDFID (L2.L2DomainFunctionContents dfss n) (L2.l2AllDomainFunctions mod)
                                      }
          return (dfname, newDFID)
        l2cvs <- forM cvs $ \(L1.L1Identifier cvss cvname, _) -> do
          newCVID <- L2.l2NextClassValueID <$> getL2Model
          modifyL2Model $ \mod -> mod {
            L2.l2NextClassValueID =
               (\(L2.L2ClassValueID i) -> L2.L2ClassValueID (i + 1)) newCVID,
            L2.l2AllClassValues =
              M.insert newCVID (dummyCVC cvss) (L2.l2AllClassValues mod)
                                      }
          return (cvname, newCVID)
        newClassID <- L2.l2NextClassID <$> getL2Model
        modifyL2Model $ \mod -> mod {
          L2.l2NextClassID = (\(L2.L2ClassID i) -> L2.L2ClassID (i + 1)) newClassID,
          L2.l2AllClasses = M.insert newClassID (L2.L2ClassContents nsss [] (M.fromList l2dfs) (M.fromList l2cvs))
                                     (L2.l2AllClasses mod)
          }
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
          L2.l2nsClasses = M.insert clname newClassID (L2.l2nsClasses l2nsc),
          L2.l2nsDomainFunctions = foldl' (\s (dfname, dfid) -> M.insert dfname dfid s) (L2.l2nsDomainFunctions l2nsc) l2dfs,
          L2.l2nsClassValues = foldl' (\s (cvname, cvid) -> M.insert cvname cvid s) (L2.l2nsClassValues l2nsc) l2cvs
          }
      L1.L1NSEnsemble { L1.l1nsLabels = labs } ->
        forM_ labs $ \(L1.L1Identifier labss lab) -> do
          modifyNamespaceContents myNSID $ \l2nsc ->
            let
              newLabel = L2.l2nsNextLabel l2nsc
            in
             l2nsc { L2.l2nsNextLabel = newLabel + 1,
                     L2.l2nsLabels = M.insert lab (L2.L2Label myNSID (fromIntegral newLabel)) (L2.l2nsLabels l2nsc) }
      L1.L1NSUnit { L1.l1nsSS = nsss, L1.l1nsUnitName = L1.L1Identifier _ uname } -> do
        tmpId <- mtsNextTempId <$> get
        modify $ \mts -> mts { mtsNextTempId = tmpId - 1 }
        modifyNamespaceContents myNSID $ \l2nsc -> l2nsc {
              L2.l2nsUnits = M.insert uname (L2.L2UnitExRef nsss (L2.L2BaseUnitsID tmpId))
                                            (L2.l2nsUnits l2nsc)
              }
      _ -> return ()

-- | Recursively imports symbols into namespaces from other models (leaving local
--   imports alone).
--   ns is the namespace we are requesting be processed now.
--   This imports everything (not just namespaces) required in this namespace from
--   other files, allowing all further processing to focus on the contents of this file.
processExternalImports :: L2.L2NamespaceID -> ModelTranslation ()
processExternalImports ns = do
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
    L1.L1NSNamespace { L1.l1nsNamespaceName = L1.L1Identifier _ nsid, L1.l1nsNamespaceContents = nsc} -> do
      Just childNSID <- findExactNamespace ns nsid
      processExternalImports childNSID
    L1.L1NSDomain { L1.l1nsDomainName = L1.L1Identifier _ nsid, L1.l1nsNamespaceContents = nsc} -> do
      Just childNSID <- findExactNamespace ns nsid
      processExternalImports childNSID
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
      tryLookupImportAndRegister L2.l2nsDomains (\x mod -> mod{L2.l2nsDomains=x}) recursivelyImportExternalDomainExpression,
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
    T.mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) (L2.l2nsDomains foreignNSC) <*>
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
recursivelyImportExternalScopedUnit m ident (targetSUID@(L2.L2ScopedUnitID suname _)) =
  (setScopedUnitName suname) <$>
  (cacheWrapExternalImport mtsForeignToLocalScopedUnit (\x m -> m {mtsForeignToLocalScopedUnit = x}) $
     \foreignMod foreignURL targetSUID -> do
       newSUID <- L2.l2NextScopedUnitID <$> getL2Model
       modifyL2Model $ \mod -> mod {
         L2.l2NextScopedUnitID = (\(L2.L2ScopedUnitID _ i) -> L2.L2ScopedUnitID "unnamed!" (i + 1)) newSUID
         }
       return newSUID
  ) m ident targetSUID

recursivelyImportExternalScopedDomain :: L2.L2Model -> L2.Identifier -> L2.L2ScopedDomainID -> ModelTranslation L2.L2ScopedDomainID
recursivelyImportExternalScopedDomain m ident (targetSUID@(L2.L2ScopedDomainID suname _)) =
  (setScopedDomainName suname) <$>
  (cacheWrapExternalImport mtsForeignToLocalScopedDomain (\x m -> m {mtsForeignToLocalScopedDomain = x}) $
     \foreignMod foreignURL targetSUID -> do
       newSUID <- L2.l2NextScopedDomainID <$> getL2Model
       modifyL2Model $ \mod -> mod {
         L2.l2NextScopedDomainID = (\(L2.L2ScopedDomainID _ i) -> L2.L2ScopedDomainID "unnamed!" (i + 1)) newSUID
         }
       return newSUID
  ) m ident targetSUID

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
    L2.L2DomainExpressionApply ss domArgs unitArgs val ->
      L2.L2DomainExpressionApply ss
        <$> mapM (\(sdid, domExpr) ->
                   (,) sdid
                       <$> recursivelyImportExternalDomainExpression foreignMod foreignURL domExpr)
                 domArgs
        <*> mapM (\(sdid, unitExpr) ->
                   (,) sdid
                       <$> recursivelyImportExternalUnitExpression foreignMod foreignURL unitExpr)
                 unitArgs
        <*> recursivelyImportExternalDomainExpression foreignMod foreignURL val
    L2.L2DomainFunctionEvaluate ss dfid args ->
      L2.L2DomainFunctionEvaluate ss
        <$> recursivelyImportExternalDomainFunction foreignMod foreignURL dfid
        <*> mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) args
    L2.L2DomainVariableRef ss sdid ->
      L2.L2DomainVariableRef ss <$> (recursivelyImportExternalScopedDomain foreignMod foreignURL sdid)
    L2.L2DomainExpressionLambda ss scopedDoms scopedUnits uConstraints dEqs dRels dex ->
      L2.L2DomainExpressionLambda ss
        <$> mapM (\(d, k) ->
                   (,)
                     <$> recursivelyImportExternalScopedDomain foreignMod foreignURL d
                     <*> pure k) scopedDoms
        <*> mapM (recursivelyImportExternalScopedUnit foreignMod foreignURL) scopedUnits
        <*> mapM (\(uex1, uex2) -> (,)
                    <$> recursivelyImportExternalUnitExpression foreignMod foreignURL uex1
                    <*> recursivelyImportExternalUnitExpression foreignMod foreignURL uex2) uConstraints
        <*> mapM (\(dex1, dex2) -> (,)
                    <$> recursivelyImportExternalDomainExpression foreignMod foreignURL dex1
                    <*> recursivelyImportExternalDomainExpression foreignMod foreignURL dex2) dEqs
        <*> mapM (\(cls, dexs) -> (,)
                      <$> recursivelyImportExternalClass foreignMod foreignURL cls
                      <*> mapM (recursivelyImportExternalDomainExpression foreignMod foreignURL) dexs
                 ) dRels
        <*> recursivelyImportExternalDomainExpression foreignMod foreignURL dex

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
        <*> recursivelyImportExternalDomainExpression foreignMod foreignURL sig

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
                                         recursivelyImportExternalDomainExpression foreignMod foreignURL fvc)
                                (L2.l2ValueType foreignValueContents)
      modifyL2Model $ \mod -> mod {
        L2.l2NextValue = (\(L2.L2ValueID i) -> L2.L2ValueID (i + 1)) newValueID,
        L2.l2AllValues = M.insert newValueID localValueContents
                              (L2.l2AllValues mod)
        }
      return newValueID

recursivelyImportExternalClassValue :: L2.L2Model -> L2.Identifier -> L2.L2ClassValueID -> ModelTranslation L2.L2ClassValueID
recursivelyImportExternalClassValue =
  cacheWrapExternalImport mtsForeignToLocalClassValue (\x m -> m{mtsForeignToLocalClassValue=x}) $
    \foreignMod foreignURL foreignValueID -> do
      let Just (L2.L2ClassValueContents ss dt) = M.lookup foreignValueID . L2.l2AllClassValues $ foreignMod
      localValueContents <- L2.L2ClassValueContents ss <$>
                              (recursivelyImportExternalDomainExpression foreignMod foreignURL dt)
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

-- | Process internal imports until either all imports are resolved, or an
--   error such as a cycle or missing identifier is found.
processInternalImports :: L2.L2NamespaceID -> ModelTranslation ()
processInternalImports startFrom = do
  allNS <- S.insert startFrom <$> findNamespaceDescendents startFrom
  ModelTranslationState { mtsL2ToL1Map = nsmap } <- get
  -- Make a list of namespaces that actually have local imports to process...
  let
      isFinished nsid = do
            Just (L1.L1NamespaceContents l1nsc) <- M.lookup nsid . mtsL2ToL1Map <$> get
            return $ not (any isLocalImport l1nsc)
      isLocalImport (L1.L1NSImport { L1.l1nsImportFrom = Nothing }) = True
      isLocalImport _ = False        
  (pending, finished) <- (\(a, b) -> (S.fromList a, S.fromList b)) <$> partitionM isFinished allNS
  let
      processInternalImports'' pending finished =
        if S.null pending
          then return ()
          else do
            let chosen = head (S.elems pending)
            (pending', finished') <-
              processInternalImports' [chosen] (S.delete chosen pending) finished
            processInternalImports'' pending' finished'

      processInternalImports' :: [L2.L2NamespaceID] -> S.Set L2.L2NamespaceID -> S.Set L2.L2NamespaceID ->
                                 ModelTranslation (S.Set L2.L2NamespaceID, S.Set L2.L2NamespaceID)
      processInternalImports' stack@(curNSID:_) pending finished = do
        let (Just (L1.L1NamespaceContents l1nsc)) = M.lookup curNSID nsmap
        flip (flip foldM (pending, finished)) l1nsc $ \(pending, finished) st -> case st of
          L1.L1NSImport { L1.l1nsSS = ss,
                          L1.l1nsImportFrom = Nothing,
                          L1.l1nsImportPath = path,
                          L1.l1nsImportWhat = what,
                          L1.l1nsImportHiding = hiding } -> do
            childNSID <- findScopedSymbolByRAPath ss curNSID path
                           (\nsc ident -> M.lookup (L1.l1IdBS ident) (L2.l2nsNamespaces nsc))
            when (childNSID `elem` stack) $ do
              paths <- intercalate ", which is imported by " <$> mapM (nsidToFriendlyName nsMain)
                         ((childNSID:(takeWhile (/= childNSID) stack)) ++ [childNSID])
              fail $ "Illegal import cycle found involving namespace " ++
                     paths ++ ", at " ++ (show ss)
            (pending', finished') <-
              if (childNSID `S.member` finished)
                then return (pending, finished)
                else processInternalImports' (childNSID:stack) (S.delete childNSID pending) finished
            l2model <- getL2Model
            childNSC <- getNamespaceContents childNSID
            importSyms <- importListFromWhatAndHiding childNSID l2model what hiding
            forM_ importSyms $ \(L1.L1Identifier _ importSym) -> do
              -- TODO - check importSym doesn't already exist in curNSID.
              let
                importSomething :: (L2.L2NamespaceContents -> M.Map L2.Identifier a) ->
                                   (L2.L2NamespaceContents -> M.Map L2.Identifier a -> L2.L2NamespaceContents) ->
                                   ModelTranslation ()
                importSomething getter setter =
                    case M.lookup importSym (getter childNSC) of
                      Nothing -> return ()
                      Just impSomething ->
                        modifyNamespaceContents curNSID (\nsc -> setter nsc (M.insert importSym impSomething (getter nsc)))
              importSomething L2.l2nsNamespaces (\nsc x -> nsc { L2.l2nsNamespaces = x })
              importSomething L2.l2nsDomains (\nsc x -> nsc { L2.l2nsDomains = x })
              importSomething L2.l2nsNamedValues (\nsc x -> nsc { L2.l2nsNamedValues = x })
              importSomething L2.l2nsClassValues (\nsc x -> nsc { L2.l2nsClassValues = x })
              importSomething L2.l2nsUnits (\nsc x -> nsc { L2.l2nsUnits = x })
              importSomething L2.l2nsClasses (\nsc x -> nsc { L2.l2nsClasses = x })
              importSomething L2.l2nsDomainFunctions (\nsc x -> nsc { L2.l2nsDomainFunctions = x })
              importSomething L2.l2nsLabels (\nsc x -> nsc { L2.l2nsLabels = x })
              
            return (pending', S.insert curNSID finished')
  processInternalImports'' pending finished

-- | Recursively translates a model, starting from the specified namespace.
recursivelyTranslateModel :: L2.L2NamespaceID -> ModelTranslation ()
recursivelyTranslateModel thisNSID = do
  Just (L1.L1NamespaceContents l1nsc) <- (M.lookup thisNSID . mtsL2ToL1Map) <$> get
  translateNSContents def thisNSID l1nsc
  
-- | Updates the contents of a namespace with the actual definitions, replacing
--   the temporary 'skeleton' definitions.
translateNSContents :: ScopeInformation -> L2.L2NamespaceID -> [L1.L1NamespaceStatement] -> ModelTranslation ()
translateNSContents scope thisNSID nsc =
  forM_ nsc $ \nss ->
    case nss of
      -- Note: We assume all remaining imports are local due to
      -- processNamespaceImports, and have no 'as' clause because
      -- of handleL1SimpleSyntacticSugar...
      L1.L1NSImport {} ->
        -- Imports are already done.
        return ()
      L1.L1NSNamespace { L1.l1nsNamespaceName = nsNameL1@(L1.L1Identifier idss nsName) } -> do
        l2nsc <- getNamespaceContents thisNSID
        Just childNSID <- M.lookup nsName . L2.l2nsNamespaces <$> getNamespaceContents thisNSID
        Just (L1.L1NamespaceContents childNSC) <- (M.lookup childNSID . mtsL2ToL1Map) <$> get
        translateNSContents scope childNSID childNSC
        Just (L2.L2DomainReference _ tmpId) <-
              M.lookup nsName . L2.l2nsDomains <$> getNamespaceContents thisNSID
        labs <- (L2.L2DomainExpressionDisjointUnion idss . L2.L2LabelledDomains .
                   (\n -> [(L2.L2Label childNSID (fromIntegral i), L2.L2DomainExpressionProduct idss (L2.L2LabelledDomains [])) |
                        i <- [0..(L2.l2nsNextLabel n)]])) <$>
                  getNamespaceContents childNSID
        modify $ \mts -> mts { mtsTempIDDomainType = M.insert tmpId labs
                                                              (mtsTempIDDomainType mts) }
        return ()
      L1.L1NSDomain { L1.l1nsSS = ss,
                      L1.l1nsDomainName = L1.L1Identifier idss ident,
                      L1.l1nsDomainDefinition = dd } -> do
        Just domNSID <- findExactNamespace thisNSID ident
        domStatement <- translateDomainDefinition scope ss domNSID dd
        Just (L2.L2DomainReference _ tmpId) <-
              M.lookup ident . L2.l2nsDomains <$> getNamespaceContents thisNSID
        modify $ \mts -> mts { mtsTempIDDomainType = M.insert tmpId domStatement (mtsTempIDDomainType mts) }
        modifyNamespaceContents thisNSID $ \l2nsc -> l2nsc {
          L2.l2nsDomains = M.insert ident domStatement (L2.l2nsDomains l2nsc)
          }
      L1.L1NSAssertion { L1.l1nsSS = ss, L1.l1nsExpression = ex } -> do
        l2ex <- translateExpression scope ss thisNSID ex
        modifyL2Model $ \mod -> mod { L2.l2AllAssertions = l2ex:(L2.l2AllAssertions mod) }
      L1.L1NSNamedValue { L1.l1nsSS = ss, L1.l1nsValueName = L1.L1Identifier { L1.l1IdBS = n },
                          L1.l1nsDomainType = mt } -> do
        l2t <- maybe (return Nothing) (\t -> Just <$> translateDomainExpression scope ss thisNSID t) mt
        Just valueID <- M.lookup n . L2.l2nsNamedValues <$> getNamespaceContents thisNSID
        modifyL2Model $ \mod -> mod {
          L2.l2AllValues = M.insert valueID (L2.L2ValueContents ss l2t) (L2.l2AllValues mod)
          }
      L1.L1NSClass { L1.l1nsSS = ss, L1.l1nsClassName = L1.L1Identifier _ n,
                     L1.l1nsClassDomainFunctions = df, L1.l1nsClassValues = cvs } -> do
        -- The only thing that needs to change from the skeleton is the ClassValueContents...
        Just classID <- M.lookup n . L2.l2nsClasses <$> getNamespaceContents thisNSID
        Just classContents <- (M.lookup classID . L2.l2AllClasses) <$> getL2Model
        forM_ cvs $ \(L1.L1Identifier cvss cvName, cvtype) -> do
          let Just cvID = M.lookup cvName (L2.l2ClassValues classContents)
          l2cvtype <- translateDomainExpression scope ss thisNSID cvtype
          modifyL2Model $ \mod -> mod {
            L2.l2AllClassValues = M.insert cvID (L2.L2ClassValueContents cvss l2cvtype)
                                                (L2.l2AllClassValues mod)
            }
      L1.L1NSEnsemble {} -> return ()
      L1.L1NSUnit { L1.l1nsSS = ss, L1.l1nsUnitName = L1.L1Identifier uss n, L1.l1nsUnitDefinition = d } -> do
        l2uDef <- translateUnitDefinition scope ss thisNSID d
        Just (L2.L2UnitExRef _ tmpId) <- M.lookup n . L2.l2nsUnits <$> getNamespaceContents thisNSID
        modify $ \mts -> mts { mtsTempIDUnitEx = M.insert tmpId l2uDef (mtsTempIDUnitEx mts) }
        modifyNamespaceContents thisNSID $ \l2nsc -> l2nsc {
            L2.l2nsUnits = M.insert n l2uDef (L2.l2nsUnits l2nsc)
          }
      L1.L1NSInstance { L1.l1nsSS = ss, L1.l1nsInstanceOfClass = cpath,
                        L1.l1nsClassArguments = args, L1.l1nsInstanceDomainFunctions = dfs,
                        L1.l1nsInstanceValues = vals } -> do
        -- Find the class...
        classID <-
          (findScopedSymbolByRAPath ss thisNSID cpath $ \nsc className ->
            M.lookup (L1.l1IdBS className) (L2.l2nsClasses nsc))
        Just class' <- (M.lookup classID . L2.l2AllClasses) <$> getL2Model
        l2args <- mapM (translateDomainExpression scope ss thisNSID) args
        l2dfs <- mapM (\(L1.L1Identifier ifss instfuncident, dts, dex) ->
                            (,,) <$>
                              (case M.lookup instfuncident (L2.l2ClassDomainFunctions class') of
                                  Nothing -> fail $ "Instance refers to domain function " ++
                                                    (BSC.unpack instfuncident) ++ " at " ++
                                                    (show ifss) ++ ", but that domain function " ++
                                                    "could not be found in the class."
                                  Just ifid -> return ifid
                              ) <*>
                              (mapM (translateDomainExpression scope ss thisNSID) dts) <*>
                              translateDomainExpression scope ss thisNSID dex
                          ) dfs
        l2exprs <- mapM (translateExpression scope ss thisNSID) vals
        modifyL2Model $ \mod ->
          mod { L2.l2AllInstances = 
                       (L2.L2InstanceContents { L2.l2InstanceSS = ss,
                                                L2.l2InstanceOfClass = classID, 
                                                L2.l2InstanceClassArguments = l2args,
                                                L2.l2InstanceDomainFunctions = l2dfs,
                                                L2.l2InstanceValues = l2exprs }):
                       (L2.l2AllInstances mod) }
        return ()
  
-- | Translates an L1Label to an L2Expression.
translateLabelEx :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1RelOrAbsPathPossiblyIntEnd -> ModelTranslation L2.L2Expression
translateLabelEx scope _ nsid (L1.L1RelOrAbsPathInt pss ra rp v) = do
  nsid' <- findNamespaceByRAPath pss nsid (L1.L1RelOrAbsPath pss ra rp)
  when (nsid' /= nsInteger && nsid' /= nsNatural) . fail $
    "Literal integer syntax can only be used to refer to labels in the \
    \built in Integer and Natural domains(e.g. Natural:1), so usage at " ++
    (show pss) ++ " is invalid."
  when (nsid' == nsNatural && v < 0) . fail $
    "Attempt to reference a negative natural number, at " ++ (show pss)
  return . L2.L2ExReferenceLabel pss $ L2.L2Label nsid' (fromIntegral v)

translateLabelEx scope _ nsid (L1.L1RelOrAbsPathNoInt pss isabs rp) = do
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

-- | Translates an L1Label to an L2Label, giving an appropriate error if the
--   label is something other than an ensemble label.
translateLabelOnly context scope ss nsid p = do
  r <- translateLabelEx scope ss nsid p
  case r of
    L2.L2ExReferenceLabel _ l -> return l
    _ -> fail $ "Expected a label, not a value, in " ++ context ++ " at " ++ show (L1.l1RelOrAbsPIESS p)

-- | Translates an L1Expression to a L2Expression.
translateExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1Expression -> ModelTranslation L2.L2Expression
translateExpression scope _ nsid (L1.L1ExApply ss op arg) =
  L2.L2ExApply ss <$> translateExpression scope ss nsid op
                  <*> translateExpression scope ss nsid arg

translateExpression scope _ nsid (L1.L1ExReference ss l) =
  translateLabelEx scope ss nsid l

translateExpression scope _ _ (L1.L1ExBoundVariable ss scopedID@(L1.L1ScopedID idss _)) =
  L2.L2ExBoundVariable ss <$>
    maybe (fail $ "Reference to unknown local variable at " ++ show idss)
          return (M.lookup scopedID (siValueIDMap scope))

translateExpression scope _ nsid (L1.L1ExLiteralReal ss units rv) =
  L2.L2ExLiteralReal ss
    <$> translateUnitExpression scope ss nsid units
    <*> (pure rv)

translateExpression scope _ nsid (L1.L1ExMkProduct ss values) =
  L2.L2ExMkProduct ss
    <$> mapM (\(l, ex) ->
               (,) <$> translateLabelOnly "product label" scope ss nsid l
                   <*> translateExpression scope ss nsid ex
             ) values

translateExpression scope _ nsid (L1.L1ExMkUnion ss label value) =
  L2.L2ExMkUnion ss
    <$> translateLabelOnly "union label" scope ss nsid label
    <*> translateExpression scope ss nsid value

translateExpression scope _ nsid (L1.L1ExProject ss label) =
  L2.L2ExProject ss
    <$> translateLabelOnly "projection label" scope ss nsid label

translateExpression scope _ nsid (L1.L1ExAppend ss label) =
  L2.L2ExAppend ss
    <$> translateLabelOnly "append label" scope ss nsid label

translateExpression scope _ nsid (L1.L1ExLambda ss (L1.L1PatternBind _ bvar) value) = do
  newBVar <- L2.l2NextScopedValueID <$> getL2Model
  modifyL2Model $ \mod -> mod {
    L2.l2NextScopedValueID = (\(L2.L2ScopedValueID n) ->
                               L2.L2ScopedValueID (n + 1)) newBVar }
  L2.L2ExLambda ss newBVar <$>
      translateExpression (scope { siValueIDMap = M.insert bvar newBVar (siValueIDMap scope)})
                           ss nsid value

translateExpression scope _ nsid (L1.L1ExCase ss expr values) =
  L2.L2ExCase ss
    <$> translateExpression scope ss nsid expr
    <*> mapM (\(L1.L1PatternAs _ l (L1.L1PatternIgnore _), cex) ->
               (,)
                 <$> translateLabelOnly "case label" scope ss nsid l
                 <*> translateExpression scope ss nsid cex) values

translateExpression scope _ nsid (L1.L1ExLet ss expr closure) = do
  letNS <- registerNewNamespace ss nsid closure
  let L1.L1NamespaceContents closureList = closure
  buildSkeletonModel ss closure letNS
  (_, nsAsserts) <- isolateAssertions (translateNSContents scope letNS closureList)
  L2.L2ExLet ss
    <$> translateExpression scope ss letNS expr
    <*> (pure letNS)
    <*> (pure nsAsserts)

translateExpression scope _ nsid (L1.L1ExString ss sv) =
  return $ L2.L2ExString ss sv

translateExpression scope _ nsid (L1.L1ExSignature ss ex sig) = do
  L2.L2ExSignature ss <$> (translateExpression scope ss nsid ex)
                      <*> (translateDomainExpression scope ss nsid sig)

-- | Translates an L1UnitExpression to a L2UnitExpression.
translateUnitExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1UnitExpression -> ModelTranslation L2.L2UnitExpression
translateUnitExpression scope _ nsid (L1.L1UnitExDimensionless ss) =
  pure $ L2.L2UnitExDimensionless ss
translateUnitExpression scope _ nsid (L1.L1UnitExRef ss refpath) =
  findScopedSymbolByRAPath ss nsid refpath $ \nsc unitName -> M.lookup (L1.l1IdBS unitName) (L2.l2nsUnits nsc)
translateUnitExpression scope _ nsid (L1.L1UnitExTimes ss expr1 expr2) =
  L2.L2UnitExTimes ss <$> translateUnitExpression scope ss nsid expr1
                      <*> translateUnitExpression scope ss nsid expr2
translateUnitExpression scope _ nsid (L1.L1UnitPow ss expr1 powerOf) =
  L2.L2UnitPow ss <$> (translateUnitExpression scope ss nsid expr1)
                  <*> (pure powerOf)
translateUnitExpression scope _ nsid (L1.L1UnitScalarMup ss scalv expr1) =
  L2.L2UnitScalarMup ss scalv <$> (translateUnitExpression scope ss nsid expr1)
translateUnitExpression scope _ nsid (L1.L1UnitScopedVar ss scopedv) =
  L2.L2UnitScopedVar ss <$>
    maybe (fail $ "Reference to unknown scoped units name " ++ (BSC.unpack . L1.l1ScopedIdBS $ scopedv) ++ " at " ++ (show ss)) return (M.lookup scopedv (siUnitIDMap scope))

-- | Translates an L1UnitDefinition to an L2UnitDefinition
translateUnitDefinition :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1UnitDefinition -> ModelTranslation L2.L2UnitExpression
translateUnitDefinition scope _ nsid (L1.L1UnitDefNewBase ss) = do
  buID <- L2.l2NextBaseUnits <$> getL2Model
  modifyL2Model $ \mod ->
    mod { L2.l2NextBaseUnits = (\(L2.L2BaseUnitsID n) -> L2.L2BaseUnitsID (n + 1)) buID,
          L2.l2AllBaseUnits = M.insert buID (L2.L2BaseUnitContents ss) (L2.l2AllBaseUnits mod) }
  return $ L2.L2UnitExRef ss buID
translateUnitDefinition scope _ nsid (L1.L1UnitDefUnitExpr ss expr) =
  translateUnitExpression scope ss nsid expr

-- | Translates an L1DomainExpression to an L2DomainExpression
translateDomainExpression :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1DomainExpression -> ModelTranslation L2.L2DomainExpression
translateDomainExpression scope _ nsid (L1.L1DomainExpressionProduct ss labels) =
  L2.L2DomainExpressionProduct ss <$>  translateLabelledDomains scope ss nsid labels
translateDomainExpression scope _ nsid (L1.L1DomainExpressionDisjointUnion ss labels) =
  L2.L2DomainExpressionDisjointUnion ss <$>  translateLabelledDomains scope ss nsid labels
translateDomainExpression scope _ nsid (L1.L1DomainExpressionFieldSignature ss dom cod) =
  L2.L2DomainExpressionFieldSignature ss <$> translateDomainExpression scope ss nsid dom
                                         <*> translateDomainExpression scope ss nsid cod
translateDomainExpression scope _ nsid (L1.L1DomainExpressionReal ss units) =
  L2.L2DomainExpressionReal ss <$> translateUnitExpression scope ss nsid units
translateDomainExpression scope _ nsid (L1.L1DomainExpressionApply ss args value) = do
  let processOneArg (domArgs, unitArgs) (L1.L1ScopedID _ sdName, Left dex) = do
        domArg <- ((,) (L2.L2ScopedDomainID sdName 0)) <$>
                    translateDomainExpression scope ss nsid dex
        return (domArg : domArgs, unitArgs)
      processOneArg (domArgs, unitArgs) (L1.L1ScopedID _ sdName, Right uex) = do
        unitArg <- (,) (L2.L2ScopedUnitID sdName 0)  <$> translateUnitExpression scope ss nsid uex
        return (domArgs, unitArg : unitArgs)
  (domArgs, unitArg : unitArgs) <- foldM processOneArg ([], []) args
  L2.L2DomainExpressionApply ss domArgs unitArgs <$> translateDomainExpression scope ss nsid value
translateDomainExpression scope _ nsid (L1.L1DomainFunctionEvaluate ss func args) =
  L2.L2DomainFunctionEvaluate ss
    <$> findScopedSymbolByRAPath ss nsid func
          (\nsc dfName ->
              M.lookup (L1.l1IdBS dfName) (L2.l2nsDomainFunctions nsc)
          )
    <*> mapM (translateDomainExpression scope ss nsid) args
translateDomainExpression scope _ nsid (L1.L1DomainVariableRef ss sdName) =
  case (M.lookup sdName . siDomainIDMap) scope of
    Nothing ->
      fail ("Cannot find scoped domain variable " ++ BSC.unpack (L1.l1ScopedIdBS sdName) ++ " referenced at " ++
            show ss)
    Just scopedDomain ->
      return $ L2.L2DomainVariableRef ss scopedDomain

translateDomainExpression scope _ nsid (L1.L1DomainReference ss (rapi@L1.L1RelOrAbsPathInt{})) =
  fail $ "Attempt to use a label ending in an integer as a domain name, which is invalid, at " ++ (show ss)
  
translateDomainExpression scope _ nsid (L1.L1DomainReference ss (L1.L1RelOrAbsPathNoInt _ isabs rp)) = do
  domainType <-
    findScopedSymbolByRAPath ss nsid (L1.L1RelOrAbsPath ss isabs rp) $
      \nsc domainName ->
         M.lookup (L1.l1IdBS domainName) (L2.l2nsDomains nsc)
  return domainType

translateDomainExpression scope _ nsid (L1.L1DomainExpressionLambda ss dh dexpr) = do
  let processDomainHeadMember (scope, sd, su) (L1.L1DHMScopedDomain name kind) = do
        sdid <- setScopedDomainName (L1.l1ScopedIdBS name) . L2.l2NextScopedDomainID <$> getL2Model
        modifyL2Model $ \mod -> mod {
          L2.l2NextScopedDomainID = (\(L2.L2ScopedDomainID _ v) -> L2.L2ScopedDomainID "!unknown" (v + 1)) sdid
        }
        return (scope { siDomainIDMap = M.insert name sdid (siDomainIDMap scope) }, (sdid, kind) : sd, su)
      processDomainHeadMember (scope, sd, su) (L1.L1DHMScopedUnit name) = do
        suid <- setScopedUnitName (L1.l1ScopedIdBS name) . L2.l2NextScopedUnitID <$> getL2Model
        modifyL2Model $ \mod -> mod {
            L2.l2NextScopedUnitID = (\(L2.L2ScopedUnitID _ v) -> L2.L2ScopedUnitID "!unknown" (v + 1)) suid
         }
        return (scope { siUnitIDMap = M.insert name suid (siUnitIDMap scope) }, sd, suid : su)
      processDomainHeadMember x _ = return x
  (scope', scopedDomains, scopedUnits) <-
        foldM processDomainHeadMember (scope, [], []) dh
  let processDomainHeadMember' (uc, de, dr) (L1.L1DHMUnitConstraint uex1 uex2) = do
        uex1' <- translateUnitExpression scope' ss nsid uex1
        uex2' <- translateUnitExpression scope' ss nsid uex2
        return ((uex1', uex2'):uc, de, dr)
      processDomainHeadMember' (uc, de, dr) (L1.L1DHMEquality dex1 dex2) = do
        dex1' <- translateDomainExpression scope' ss nsid dex1
        dex2' <- translateDomainExpression scope' ss nsid dex2
        return (uc, (dex1', dex2'):de, dr)
      processDomainHeadMember' (uc, de, dr) (L1.L1DHMRelation cpath args) = do
        args' <- mapM (translateDomainExpression scope' ss nsid) args
        classID <-
          (findScopedSymbolByRAPath ss nsid cpath $ \nsc className ->
            M.lookup (L1.l1IdBS className) (L2.l2nsClasses nsc))
        return (uc, de, (classID, args'):dr)
      processDomainHeadMember' x _ = return x
  (uConstrs, dEqs, dRels) <-
    foldM processDomainHeadMember' ([], [], []) dh
  dexpr2 <- translateDomainExpression scope' ss nsid dexpr
  return $ L2.L2DomainExpressionLambda ss scopedDomains scopedUnits uConstrs dEqs dRels dexpr2

-- | Translates an L1LabeleldDomains into an L2LabelledDomains.
translateLabelledDomains :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID ->
                            L1.L1LabelledDomains -> ModelTranslation L2.L2LabelledDomains
translateLabelledDomains scope ss nsid (L1.L1LabelledDomains ls) =
  L2.L2LabelledDomains
    <$> mapM (\(l, ex) ->
                (,) <$> translateLabelOnly "labelled domains" scope ss nsid l
                    <*> translateDomainExpression scope ss nsid ex
             ) ls

-- | Produces a L2DomainType linking to a new L2ClonelikeDomainContents.
makeClonelikeDomain :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID ->
                       L1.L1DomainExpression -> L2.L2DomainCloneType ->
                       ModelTranslation L2.L2DomainExpression
makeClonelikeDomain scope ss nsid dt ct = do
  domID <- L2.l2NextDomain <$> getL2Model
  cldc <- L2.L2ClonelikeDomainContents ss
              <$> translateDomainExpression scope ss nsid dt
              <*> (pure ct)
  modifyL2Model $ \mod -> mod {
      L2.l2NextDomain = (\(L2.L2DomainID n) -> L2.L2DomainID (n + 1)) domID,
      L2.l2AllDomains = M.insert domID cldc (L2.l2AllDomains mod)
    }
  return $ L2.L2DomainReference ss domID

-- | Translates an L1DomainDefinition to an L2DomainType, registering new domains
--   as required for the definition.
translateDomainDefinition :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID ->
                             L1.L1DomainDefinition -> ModelTranslation L2.L2DomainExpression
translateDomainDefinition scope _ nsid
  (L1.L1CloneDomain ss dt) =
    makeClonelikeDomain scope ss nsid dt L2.L2DomainClone
translateDomainDefinition scope _ nsid
  (L1.L1SubsetDomain ss dt ex) =
    makeClonelikeDomain scope ss nsid dt
      =<< (L2.L2DomainSubset <$> (translateExpression scope ss nsid ex))
translateDomainDefinition scope _ nsid
  (L1.L1ConnectDomain ss dt ex) =
    makeClonelikeDomain scope ss nsid dt
      =<< (L2.L2DomainConnect <$> (translateExpression scope ss nsid ex))
translateDomainDefinition scope _ nsid
  (L1.L1DomainDefDomainType ss dt) =
    translateDomainExpression scope ss nsid dt

{-
-- | Translates an L1DomainType to an L2DomainType.
translateDomainType :: ScopeInformation -> L1.SrcSpan -> L2.L2NamespaceID -> L1.L1DomainExpression -> ModelTranslation L2.L2DomainExpression
translateDomainType scope _ nsid (L1.L1DomainType ss dcrs expr) = do
  (unitConstraints, domainEqualities, domainRelations) <-
    foldM (\(unitConstraints, domainEqualities, domainRelations) dcr ->
            case dcr of
              L1.L1DCRUnitConstraint expr1 expr2 ->
                (,,)
                  <$> ((:unitConstraints) <$>
                       ((,) <$> translateUnitExpression scope ss nsid expr1
                            <*> translateUnitExpression scope ss nsid expr2))
                  <*> pure domainEqualities
                  <*> pure domainRelations
              L1.L1DCREquality expr1 expr2 ->
                (,,) unitConstraints
                  <$> ((:domainEqualities) <$>
                       ((,) <$> translateDomainExpression scope ss nsid expr1
                            <*> translateDomainExpression scope ss nsid expr2))
                  <*> pure domainRelations
              L1.L1DCRRelation cls args ->
                (,,) unitConstraints domainEqualities
                  <$> ((:domainRelations) <$>
                       ((,) <$> findScopedSymbolByRAPath ss nsid cls
                                   (\nsc className -> M.lookup (L1.l1IdBS className)
                                                          (L2.l2nsClasses nsc))
                            <*> mapM (translateDomainExpression scope ss nsid)
                                     args
                       )
                      )
          ) ([], [], []) dcrs
  L2.L2DomainType ss unitConstraints domainEqualities domainRelations
    <$> translateDomainExpression scope ss nsid expr
-}

-- | Attempts to globally replace all temporary aliases for types with their
--   true values, or fails with an error for the user if a cycle is found.
fixAliasReferences :: L2.L2NamespaceID -> ModelTranslation ()
fixAliasReferences nsid = do
  initialMaps@(tmpdts, tmpunitexs) <- (\mts -> (mtsTempIDDomainType mts, mtsTempIDUnitEx mts)) <$> get
  let tmpList = map Left (M.keys tmpdts) ++ map Right (M.keys tmpunitexs)
  ((finaldts, finalunitexs), _) <- flip (flip foldM (initialMaps, S.empty)) tmpList $
    \(currentMaps, done) toFix ->
      tryResolveOneMapEntry [] currentMaps done toFix
  modifyL2Model $ \mod -> 
    transformBi (replaceTempDomainEx finaldts) .
    transformBi (replaceTempUnitEx finalunitexs) $ mod

-- | Applies a substitution map to domain type.
replaceTempDomainEx :: M.Map L2.L2DomainID L2.L2DomainExpression -> L2.L2DomainExpression -> L2.L2DomainExpression
replaceTempDomainEx mSubst (L2.L2DomainReference ss tmpId)
  | Just domainType <- M.lookup tmpId mSubst = domainType
replaceDomainEx _ x = x

-- | Replaces a temporary unit ID reference with the expression it resolves to.
replaceTempUnitEx :: M.Map L2.L2BaseUnitsID L2.L2UnitExpression -> L2.L2UnitExpression -> L2.L2UnitExpression
replaceTempUnitEx m (L2.L2UnitExRef ss buid)
  | Just uex <- M.lookup buid m = uex
replaceTempUnitEx _ x = x

-- | Attempts to change an entry in temporary maps into its final form, making
--   all prerequisite entries into their final form in the process.
tryResolveOneMapEntry :: [Either L2.L2DomainID L2.L2BaseUnitsID] ->
                         (M.Map L2.L2DomainID L2.L2DomainExpression, M.Map L2.L2BaseUnitsID L2.L2UnitExpression) ->
                         S.Set (Either L2.L2DomainID L2.L2BaseUnitsID) ->
                         Either L2.L2DomainID L2.L2BaseUnitsID ->
                         ModelTranslation ((M.Map L2.L2DomainID L2.L2DomainExpression,
                                            M.Map L2.L2BaseUnitsID L2.L2UnitExpression),
                                           S.Set (Either L2.L2DomainID L2.L2BaseUnitsID))
tryResolveOneMapEntry stack currentMaps@(dm,um) done toFix
  | toFix `S.member` done = return (currentMaps, done)
  | toFix `elem` stack = do
    let describeMapEntry (Left dt) = "domain type at " ++ (show $ L2.l2DomainExpressionSS (dm!dt))
        describeMapEntry (Right uex) = "units expression at " ++ (show $ L2.l2UnitExSS (um!uex))
    let paths = intercalate ", which is referenced by "
                  (map describeMapEntry
                  ((toFix:(takeWhile (/= toFix) stack)) ++ [toFix]))
    fail $ "Aliases form a cycle, which is invalid: " ++ paths
  | otherwise =
    let
      initialContents = either (Left . flip M.lookup dm) (Right . flip M.lookup um) toFix
      allDeps = [Left domId | L2.L2DomainReference { L2.l2DomainExpressionRef = domId } <-
                    universeBi initialContents, domId `M.member` dm] ++
                [Right buid | L2.L2UnitExRef { L2.l2UnitExRef = buid } <-
                    universeBi initialContents, buid `M.member` um]
    in do
     (finalMaps@(dm, um), done') <-
       foldM (\(currentMaps, done) dep -> tryResolveOneMapEntry (toFix:stack) currentMaps done dep)
             (currentMaps, done) allDeps
     let
       fixup :: Data a => a -> a
       fixup =
           transformBi (replaceTempDomainEx dm) . transformBi (replaceTempUnitEx um)
     return (either (\l -> (M.update (Just . fixup) l dm, um))
                    (\r -> (dm, M.update (Just . fixup) r um)) toFix,
             S.insert toFix done)

-- | Maps a namespace ID to a 'friendly name' that can be displayed to the user.
--   The context namespace is used so that when there are several options the
--   most appropriate choice can be shown.
nsidToFriendlyName :: L2.L2NamespaceID -> L2.L2NamespaceID -> ModelTranslation String
nsidToFriendlyName context target
  | context == target = return "main namespace"
  | otherwise = do
    n <- BSC.unpack <$>
           (fromMaybe "anonymous namespace" <$> (nsidToFriendlyName' context target))
    nsc <- getNamespaceContents target
    return $ n ++ (" at " ++ show (L2.l2nsSrcSpan nsc))

nsidToFriendlyName' context target = do
  allNS <- (M.toList . L2.l2nsNamespaces) <$> getNamespaceContents context
  foldM (\m (nsname, nsid) -> (liftM (mplus m)) (if nsid == target
                                           then return (Just nsname)
                                           else (liftM $ ((nsname <> "::") <>)) <$>
                                                  nsidToFriendlyName' nsid target))
        Nothing allNS

-- Utility functions for working with L2 models...
-- | Fetch the current L2 model out of the state.
getL2Model :: ModelTranslation L2.L2Model
getL2Model = mtsL2Model <$> get

-- | Modify the L2 model in the state.
modifyL2Model :: (L2.L2Model -> L2.L2Model) -> ModelTranslation ()
modifyL2Model f = modify $ \mts -> mts { mtsL2Model = f (mtsL2Model mts) }

-- | Fetch the contents of a namespace using the namespace ID.
getNamespaceContents :: L2.L2NamespaceID -> ModelTranslation L2.L2NamespaceContents
getNamespaceContents nsid = (fromJust . M.lookup nsid . L2.l2AllNamespaces) <$> getL2Model

-- | Modify the contents of a namespace using namespace ID.
modifyNamespaceContents :: L2.L2NamespaceID -> (L2.L2NamespaceContents -> L2.L2NamespaceContents) -> ModelTranslation ()
modifyNamespaceContents ns f =
  modifyL2Model $ \mod ->
    mod {
      L2.l2AllNamespaces = M.update (Just . f) ns (L2.l2AllNamespaces mod)
      }

-- | Unconditionally register a new namespace.
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

-- | Finds a particular namespace by name in a particular namespace. Note:
--   This does not do scope resolution, i.e. does not search for the
--   namespace in parent namespaces, and so should only be used when you know
--   exactly where the namespace should be.
findExactNamespace :: L2.L2NamespaceID -> BS.ByteString -> ModelTranslation (Maybe L2.L2NamespaceID)
findExactNamespace parentNS nsName = do
  m <- mtsL2Model <$> get
  let Just pns = M.lookup parentNS (L2.l2AllNamespaces m)
  return $ M.lookup nsName (L2.l2nsNamespaces pns)

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

allocDomainID :: ModelTranslation L2.L2DomainID
allocDomainID = do
  newID <- L2.l2NextDomain <$> getL2Model
  modifyL2Model $ \mod -> mod { L2.l2NextDomain = (\(L2.L2DomainID n) -> L2.L2DomainID (n + 1)) newID }
  return newID

-- | Finds a symbol, using the scoped resolution rules and a custom function to
--   check each namespace for a symbol of the appropriate type. Fails with an error
--   appropriate for the end user if the symbol cannot be found.
findScopedSymbolByRAPath :: L1.SrcSpan -> L2.L2NamespaceID -> L1.L1RelOrAbsPath ->
                            (L2.L2NamespaceContents -> L1.L1Identifier -> Maybe a) -> ModelTranslation a
findScopedSymbolByRAPath ss startNS ra tryGet = do
  (nsra@(L1.L1RelOrAbsPath _ isabs rp), symName) <- maybe (fail $ "Inappropriate use of an empty path, at " ++ show ss)
                             return $ trySplitRAPathOnLast ra
  -- Recursively find the namespace...
  nsid <- findNamespaceByRAPath ss startNS nsra
  let foldStrategy =
        if (not isabs && null (L1.l1RelPathIDs rp))
           then flip (flip foldOverNSScopesM Nothing) nsid
           else (\x -> x Nothing nsid)
  r <- foldStrategy $ \s nsid' -> (mplus s) <$> do
    nsc <- (fromJust . M.lookup nsid' . L2.l2AllNamespaces) <$> getL2Model
    return $ tryGet nsc symName
  maybe (fail $ "Symbol " ++ (BSC.unpack . L1.l1IdBS $ symName) ++
                " not found in namespace at " ++ (show . L1.l1IdSS $ symName)) return r

trySplitRAPathOnLast :: L1.L1RelOrAbsPath -> Maybe (L1.L1RelOrAbsPath, L1.L1Identifier)
trySplitRAPathOnLast (L1.L1RelOrAbsPath ss ra p) =
   maybe Nothing (\(x, y) -> Just (L1.L1RelOrAbsPath ss ra x, y))
         (trySplitRPathOnLast p)

trySplitRPathOnLast (L1.L1RelPath rpss []) = Nothing
trySplitRPathOnLast (L1.L1RelPath rpss l) = Just (L1.L1RelPath rpss (init l), last l)

-- | Performs a fold over all namespaces where a symbol might be found.
foldOverNSScopesM :: (s -> L2.L2NamespaceID -> ModelTranslation s) -> s -> L2.L2NamespaceID -> ModelTranslation s
foldOverNSScopesM f s0 nsid
  | nsid == nsSpecial = return s0
  | otherwise = do
    s1 <- f s0 nsid
    nsc <- (fromJust . M.lookup nsid . L2.l2AllNamespaces) <$> getL2Model
    foldOverNSScopesM f s1 (L2.l2nsParent nsc)

-- | Finds a Level 2 namespace using a Level 1 RelOrAbsPath
findNamespaceByRAPath :: L1.SrcSpan -> L2.L2NamespaceID -> L1.L1RelOrAbsPath -> ModelTranslation L2.L2NamespaceID
findNamespaceByRAPath _ thisNSID (L1.L1RelOrAbsPath _ _ (L1.L1RelPath _ [])) = return thisNSID
findNamespaceByRAPath ss thisNSID rapath = findScopedSymbolByRAPath ss thisNSID rapath $ \nsc ident ->
  M.lookup (L1.l1IdBS ident) (L2.l2nsNamespaces nsc)

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

setScopedUnitName :: BS.ByteString -> L2.L2ScopedUnitID -> L2.L2ScopedUnitID
setScopedUnitName bs (L2.L2ScopedUnitID _ suid) = L2.L2ScopedUnitID bs suid

setScopedDomainName :: BS.ByteString -> L2.L2ScopedDomainID -> L2.L2ScopedDomainID
setScopedDomainName bs (L2.L2ScopedDomainID _ suid) = L2.L2ScopedDomainID bs suid

-- | Find all distinct namespace IDs that are children of a given namespace.
--   Note that namespaces that are children by virtue of an import are excluded.
findNamespaceDescendents :: L2.L2NamespaceID -> ModelTranslation (S.Set L2.L2NamespaceID)
findNamespaceDescendents nsid = do
  nsc <- getNamespaceContents nsid
  trueChildren <- 
    filterM (\childNS -> ((==nsid) . L2.l2nsParent) <$> getNamespaceContents childNS) $
      M.elems (L2.l2nsNamespaces nsc)
  descs1 <- S.unions <$> mapM findNamespaceDescendents trueChildren
  return $ descs1 `S.union` (S.fromList trueChildren)

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

-- Utility functions that are needed in this file, but don't use any special data structures from here.

-- | Finds the first 'Just' value return from a list of monadic expressions.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM (mh:t) = do
  h <- mh
  maybe (firstJustM t) (return . Just) h
firstJustM [] = return Nothing

-- | Like mapMaybe, but runs in a monad.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f l = catMaybes `liftM` sequence (map f l)

-- | Like concatMap, but runs in a monad.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat `liftM` sequence (map f l)

partitionM :: (T.Foldable t, Monad f) => (a -> f Bool) -> t a -> f ([a], [a])
partitionM which cont = T.foldrM (\c (a, b) -> do
                                     cwhich <- which c
                                     return (if cwhich then (a, (c:b)) else ((c:a), b))
                                 ) ([], []) cont
