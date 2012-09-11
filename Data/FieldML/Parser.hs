{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.Parser where

import Data.FieldML.Structure
import qualified Data.ByteString.Char8 as BS
import Text.Parsec.ByteString
import System.FilePath
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Text.Parsec hiding ((<|>))
import Control.Applicative

data LookupModel a = LookupModel { unlookupModel :: ReaderT (IORef (M.Map BS.ByteString Model)) (ErrorT String IO) a }

instance Monad (LookupModel a) where
  return = LookupModel . return
  (LookupModel a) >>= f = LookupModel (a >>= liftM unlookupModel f)
  fail = LookupModel . lift . fail

data ParserState = ParserState {
    psSearchPaths :: [String],
    psModel :: Model,
    psCurrentNamespace :: NamespaceID,
    psIndent :: Int
  }

type ModelParser a = ParsecT BS.ByteString ParserState LookupModel a

justOrFail failWith Nothing = fail failWith
justOrFail _ (Just x) = return x

lookupModel :: BS.ByteString -> ModelParser Model
lookupModel modName = do
  cache <- (lift . LookupModel) (lift . lift . readIORef =<< ask)
  case M.lookup modName cache of
    Just mod -> return mod
    Nothing -> do
      paths <- psSearchPaths <$> getState
      justOrFail ("Cannot find requested model " ++ BS.unpack modName) =<<
        (liftM msum (seq (forM paths $ \path -> do
          let mpath = path </> ((BS.unpack modName) ++ ".fieldml")
          (r, v) <- lift . LookupModel . lift . lift $ curlGetString_ mpath []
          if r == CurlOK
            then do
              st <- getState
              pr <- runParserT modelParser (st { psModel = initialModel, psIndent = 0 }) mpath v
              case pr of
                Left err -> fail (show err ++ "\n")
                Right m -> do
                  (lift . LookupModel) $ ask >>= lift . lift . flip modifyIORef (M.insert modName m)
                  return (Just m)
            else
              return Nothing
                         )))

modelParser :: ModelParser Model
modelParser = do
  namespaceContentsParser
  psModel <$> getState

namespaceContentsParser = many $
  importStatement <|> namespaceBlock

namespaceBlock = do
  nsName <- (token "namespace" *> identifier <* token "where")
  parseNewNamespace nsName
  return ()

withNewNamespace name sp f = do
  ensureIdentifierUnique name
  st <- getState
  let oldModel = psModel st
  let oldNSID = psCurrentNamespace st
  let newNSID = nextNSID oldModel
  withCurrentNamespace $ \ns -> ns { nsNamespaces = M.insert name newNSID (nsNamespaces ns) }
  let newModel = oldModel {
          nextNSID = (\NamespaceID n -> NamespaceID (n + 1)) newNSID,
          allNamespaces = M.insert newNSID (blankNamespace sp oldNSID) (allNamespaces oldModel)
        }
  putState $ st { psCurrentNamespace = newNSID, psModel = newModel }
  f
  st' <- getState
  putState $ st' { psCurrentNamespace = oldNSID }
  return newNSID

parseNewNamespace name = do
  sp <- mkSrcPoint
  withNewNamespace $ do
    blockBegin
    namespaceContentsParser
    blockEnd
    sp' <- finishSrcSpan sp
    withCurrentNamespace $ \ns -> ns { nsSrcSpan = sp' }

importStatement =
    doImport =<<
        ((,,,,) <$> (token "import" *> blockBegin *>
                     (optionMaybe (token "from" *> urlNoHash)))
                <*> pathSpec
                <*> optionMaybe (tokenSep *> char '(' *>
                                 (identifier `sepBy` (char ',')) <*
                                 tokenSep <* char ')')
                <*> optionMaybe (token "hiding" *>
                                 (tokenSep *> char '(' *>
                                  (identifier `sepBy` (char ',')) <*
                                  tokenSep <* char ')'))
                <*> optionMaybe (token "as" *> identifier)
        ) <* blockEnd

doImport (fromWhere, startWhere, whichSymbols, hidingWhichSymbols, Just asNamespace) = do
  ensureIdentifierUnique asNamespace
  sp <- mkSrcPoint
  withNewNamespace asNamespace sp $ do
    doImport (fromWhere, startWhere, whichSymbols, hidingWhichSymbols, Nothing)

doImport (mu@(Just modelURL), startWhere, whichSymbols, hidingWhichSymbols, _) = do
  m <- lookupModel modelURL
  let startWhere' = if not null startWhere && head startWhere == "::" then tail startWhere else startWhere
  importFromModel mu m (toplevelNamespace m) startWhere' whichSymbols hidingWhichSymbols
  
doImport (_, startWhere, whichSymbols, hidingWhichSymbols, _) = do
  st <- getState
  let (ns, path) = if not null startWhere && head startWhere == "::" then
                     (toplevelNamespace . psModel $ st, tail startWhere)
                   else
                     (psCurrentNamespace ns, tail startWhere)
  importFromModel modelURL (psModel st) (toplevelNamespace . psModel $ st) startWhere whichSymbols hidingWhichSymbols

untilM p m v0
  | p v0 = do
    v <- m v0
    untilM p m v
  | otherwise = v0

partitionM f v = partitionM' f v ([], [])
partitionM' _ [] r = return r
partitionM' f (h:t) (a, b) = do
  r <- f h
  let n = if r then (h:a, b) else (a, h:b)
  partitionM' f t n

importFromModel modelURL m ns (nsname:startWhere) whichSymbols hidingWhichSymbols = do
  case tryFindNamespaceScoped m ns nsname of
    Nothing -> fail $ "Namespace at " ++ (show . nsSrcSpan $ (allNamespaces ns)!ns) ++ " has no child namespace " ++
                      (BS.unpack nsname)
    Just ns' ->
      importFromModel modelURL m ns' startWhere whichSymbols hidingWhichSymbols

importFromModel modelURL m nsID [] whichSymbols hidingWhichSymbols = do
  let ns = (allNamespaces m) ! nsID
  let allSyms = concatMap (\f -> f ns) [keys . nsNamespaces, keys . nsDomains,
                                        keys . nsValues, keys . nsClasses, keys . nsLabels,
                                        keys . nsUnits]
  mapM_ (importSymbolFromModel modelURL m nsID) $
    case whichSymbols of
      Just l -> l
      Nothing -> case hidingWhichSymbols of
        Just l -> S.toList ((S.fromList allSyms) `S.difference` (S.fromList l))
        Nothing -> allSyms

  -- Find expressions involving at least one known named value. This is an iterative
  -- process, because expressions can bring in new named values.
  flip (untilM snd) (modelAssertions m, True) $ \remainingExpr -> do
    let isKnownNamedValueRef (NamedValueRef _ nvid) = do
          (srcurl, _, srcnvid) <- traceToSource modelForeignValues modelURL m nvid
          (isJust . M.lookup (srcurl, srcnvid)) <$> ((modelForeignValues . psModel) <$> getState)
        isKnownNamedValueRef _ = return False
    (newexpr, remaining') <- partitionM (\expr -> any id <$> mapM isKnownNamedValueRef (universe expr)) remainingExpr
    newExprLocal <- ensureStructureLocal modelURL m newexpr
    modifyState $ \st ->
      st { psModel = (psModel st) { modelAssertions = newExprLocal ++ (modelAssertions (psModel st)) } }
    return (remaining', not (null newexpr))

  -- Find instances which reference a class and domains we have already seen...
  forM_ (M.toList . instancePool m) $ \entry@((dcid, dtl), inst) -> do
    (srcurl, _, srcdcid) <- traceToSource modelForeignDomainClasses modelURL m dcid
    curmod <- psModel <$> getState
    when ((srcurl, srcdcid) `M.member` (modelForeignDomainClasses curmod)) $
      when (all id <$> (forM [d | UseNewDomain d <- universeBi dtl] $ \d -> do
                           (srcurl, _, srcndid) <- traceToSource modelForeignDomains modelURL m d
                           return ((srcurl, srcndid) `M.member` (modelForeignDomains curmod)))) $ do
        (localHead, localInstance) <- ensureStructureLocal modelURL m entry
        modifyState $ \st ->
          st { psModel = (psModel st) { instancePool = M.insert localHead localInstance (instancePool (psModel st)) } }

importSymbolFromModel modelURL m ns sym =
  case
    (importOneNamespace modelURL m sym <$> tryFindNamespaceScoped m ns sym) `mplus`
    (importOneDomain modelURL m sym <$> tryFindDomainScoped m ns sym) `mplus`
    (importOneValue modelURL m sym <$> tryFindValueScoped m ns sym) `mplus`
    (importOneClass modelURL m sym <$> tryFindClassScoped m ns sym) `mplus`
    (importOneLabel modelURL m sym <$> tryFindLabelScoped m ns sym) `mplus`
    (importOneUnit modelURL m sym <$> tryFindUnitScoped m ns sym)
  of
    Nothing -> fail $ "Attempt to import " ++ (BS.unpack sym) ++ " from " ++ (BS.unpack $ fromMaybe "self" modelURL) ++
                       " but the symbol could not be found"
    Just v -> v

tryFindNamespaceScoped = tryFindSomethingScoped nsNamespaces
tryFindDomainScoped = tryFindSomethingScoped nsDomains
tryFindValueScoped = tryFindSomethingScoped nsValues
tryFindClassScoped = tryFindSomethingScoped nsClasses
tryFindLabelScoped = tryFindSomethingScoped nsLabels
tryFindUnitScoped = tryFindSomethingScoped nsUnits

tryFindSomethingScoped f m ns sym =
  let
    nsData = (allNamespaces m) ! ns
  in
   case M.lookup sym (f nsData) of
     Just something -> return something
     Nothing -> if (nsParent nsData == nsSystem) then
                  Nothing
                else
                  tryFindSomethingScoped f m (nsParent nsData) sym

traceToSource f url model fid =
  case lookup fid (flip (M.toList (f model))) of
    Nothing -> return (url, model, fid)
    Just (url', fid') -> do
      model' <- lookupModel url'
      return (url', model', fid')

ensureStructureLocalM foreignURL mForeign s =
  transformBiM (ensureNamespaceLocal foreignURL mForeign) =<<
  transformBiM (ensureDomainLocal foreignURL mForeign) =<<
  transformBiM (ensureDomainClassLocal foreignURL mForeign) =<<
  transformBiM (ensureNamedValueLocal foreignURL mForeign) =<<
  transformBiM (ensureUnitsLocal foreignURL mForeign) s

ensureSomethingLocal modelForeignSomething allSomething nextID incrementNextID setAllSomething setForeignSomething foreignURL' mForeign' foreignID' = do
  (foreignURL, mForeign, foreignID) <- traceToSource modelForeignSomething foreignURL' mForeign' foreignID'
  st <- getState
  let mLocal = psModel st
  case foreignURL of
    Nothing -> return foreignID
    Just v -> case M.lookup foreignID (modelForeignSomething mLocal) of
      Just lid -> return lid
      Nothing -> do
        let nsData = (allSomething mForeign) ! foreignID
        let localID = nextID mLocal
        putState (st { psModel = incrementNextID mLocal })
        nsNewData <- ensureStructureLocalM foreignURL mForeign nsData
        st' <- getState
        putState (st' { psModel = ((psModel st') `setAllSomething` (M.insert localID  nsNewData $ allSomething (psModel st')))
                                    `setForeignSomething` (M.insert (foreignURL, foreignID) localID $
                                                           modelForeignSomething (psModel st')) } )
        return localID

ensureNamespaceLocal = 
  ensureSomethingLocal modelForeignNamespaces allNamespaces nextNSID
    (\m v -> m { nextNSID = (\(NamespaceID n) -> NamespaceID (n+1)) (nextNSID m) })
    (\m v -> m { allNamepsaces = v })
    (\m v -> m { modelForeignNamepsaces = v })

ensureDomainLocal = 
  ensureSomethingLocal modelForeignDomains allNewDomains nextNewDomain
    (\m v -> m { nextNewDomain = (\(NewDomainID n) -> NewDomainID (n+1)) (nextNewDomain m) })
    (\m v -> m { allNewDomains = v })
    (\m v -> m { modelForeignDomains = v })

ensureDomainClassLocal = 
  ensureSomethingLocal modelForeignDomainClasses allDomainClasses nextDomainClass
    (\m v -> m { nextDomainClass = (\(DomainClassID n) -> DomainClassID (n+1)) (nextDomainClass m) })
    (\m v -> m { allDomainClasses = v })
    (\m v -> m { modelForeignDomainClasses = v })

ensureNamedValueLocal =
  ensureSomethingLocal modelForeignValues allNamedValues nextNamedValue
    (\m v -> m { nextNamedValue = (\(NamedValueID n) -> NamedValueID (n+1)) (nextNamedValue m) })
    (\m v -> m { allNamedValues = v })
    (\m v -> m { modelForeignValues = v })

ensureUnitsLocal =
  ensureSomethingLocal modelForeignUnits allUnits nextUnit
    (\m v -> m { nextUnit = (\(UnitID n) -> UnitID (n+1)) (nextUnit m) })
    (\m v -> m { allUnits = v })
    (\m v -> m { modelForeignUnits = v })

importOneNamespace foreignURL mForeign sym foreignID = do
  localID <- ensureNamespaceLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsNamespaces = M.insert sym localID (nsNamespaces ns)
    }

importOneDomain foreignURL mForeign sym foreignID = do
  localID <- ensureDomainLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsDomains = M.insert sym localID (nsDomains ns)
    }

importOneValue foreignURL mForeign sym foreignID = do
  localID <- ensureNamedValueLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsValues = M.insert sym localID (nsValues ns)
    }

importOneClass foreignURL mForeign sym foreignID = do
  localID <- ensureDomainClassLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsClasses = M.insert sym localID (nsClasses ns)
    }

importOneLabel foreignURL mForeign sym (ELabel foreignID val) = do
  localID <- ensureLabelLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsLabels = M.insert sym (ELabel localID val) (nsLabels ns)
    }

importOneUnit foreignURL mForeign sym foreignID = do
  localID <- ensureUnitsLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsUnits = M.insert sym localID (nsUnits ns)
    }

withCurrentNamespace f = do
  st <- getState
  let curModel = psModel st
  putState $ st { psModel = curModel { allNamespaces =
    M.insert (psCurrentNamespace st) (f ((allNamespaces curModel)!(psCurrentNamespace st))) (allNamespaces curModel)
                                     } }

getIndent = (head . psIndent) <$> getState

commentLine = char '#' >> many (noneOf "\r\n") >> (oneOf "\r\n" <|> eof)

getColumn = sourceColumn <$> getPosition

blockBegin = do
  oldind <- getIndent
  many (commentLine <|> oneOf "\r\n ")
  l <- getColumn
  when (oldind >= l) . fail $ "Expected more than " ++ (show oldind) ++ " spaces at start of block."
  modifyState (\s -> s { psIndent = l:(psIndent s) })
  tokenSep

tokenSep = try $ do
  many (commentLine <|> oneOf "\r\n ")
  l <- getColumn
  ind <- getIndent
  when (l < ind) (fail $ "Expected at least " ++ (show l) ++ " spaces within block.")

blockEnd = try $ do
  i <- getIndent
  many (commentLine <|> oneOf " \r\n")
  c <- getColumn
  when (c >= i) . fail $ "Expected line with indent of less than " ++ (show i) ++ " at end of block."
  modifyState (\s -> s { psIndent = tail (psIndent s) })
identChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
endToken = lookAhead (noneOf identChars <|> eof)
token n = try (tokenSep *> string n *> endToken *> pure ())
urlNoHash = tokenSep *> (BS.pack <$> many (noneOf " \t\r\n#"))
identifier = BS.pack <$> many (oneOf identChars)

mkSrcPoint = do
  SourcePos sn l c <- getPosition
  return $ SrcSpan sn l c l c

finishSrcSpan sp = do
  let SrcSpan _ sr sc _ _ = sp
  SourcePos _ er ec <- getPosition
  return $ SrcSpan sn sr sc er ec

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left v) = Left (f v)
mapLeft _ (Right v) = Right v

loadModel :: [String] -> String -> ErrorT String IO Model
loadModel incl mpath = do
  (r, v) <- lift $ curlGetString_ mpath []
  if r == CurlOK then do
      ErrorT . mapLeft show $ lift $ runParserT modelParser (ParserState incl initialModel nsMain (repeat (-1))) mpath (v :: BS.ByteString)
    else
      fail $ "Can't load initial model " ++ mpath ++ ": " ++ (show r)
