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
  -- Find expressions involving at least one known named value. This can be a recursive
  -- process, because expressions can bring in other expressions.
  flip (untilM snd) (modelAssertions m, True) $ \remainingExpr -> do
    let isKnownNamedValueRef (NamedValueRef _ nvid) = do
          (srcurl, _, srcnvid) <- traceToSource allNamedValues modelURL m nvid
          (isJust . M.lookup (srcurl, srcnvid)) <$> ((modelForeignValues . psModel) <$> getState)
        isKnownNamedValueRef _ = return False
    (newexpr, remaining') <- partitionM (\expr -> any <$> mapM isKnownNamedValueRef (universe expr)) remainingExpr
    
    return (remaining', not (null newexpr))
  -- Find instances with a head and classes we have already seen...
  instancePool m

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

importSymbolFromModel modelURL m ns sym =
  case
    (importOneNamespace modelURL m <$> tryFindNamespaceScoped m ns sym) `mplus`
    (importOneDomain modelURL m <$> tryFindDomainScoped m ns sym) `mplus`
    (importOneValue modelURL m <$> tryFindValueScoped m ns sym) `mplus`
    (importOneClass modelURL m <$> tryFindClassScoped m ns sym) `mplus`
    (importOneLabel modelURL m <$> tryFindLabelScoped m ns sym) `mplus`
    (importOneUnit modelURL m <$> tryFindUnitScoped m ns sym)
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

ensureNamespaceLocal foreignURL' mForeign' foreignID' = do
  (foreignURL, mForeign, foreignID) <- traceToSource modelForeignNamespaces foreignURL' mForeign' foreignID'
  st <- getState
  -- The namespace may be imported again from the foreign namespace, so trace it back to the source.
  let mLocal = psModel st
  case foreignURL of
    Nothing -> return foreignID
    Just v -> case M.lookup foreignID (modelForeignNamespaces mLocal) of
      Just lid -> return lid
      Nothing -> do
        let nsData = (allNamespace mForeign) ! foreignID
        let localID = nextNSID mLocal
        putState (st { psModel = mLocal { nextNSID = (\NamespaceID n -> NamespaceID (n + 1)) localID } })
        nsNewData <- ensureStructureLocalM foreignURL mForeign nsData
        st' <- getState
        putState (st' { psModel = (psModel st') { allNamespaces = M.insert localID  nsNewData $ allNamespaces (psModel st'),
                                                  modelForeignNamespaces = M.insert (foreignURL, foreignID) localID $
                                                                             modelForeignNamespaces (psModel st')} })

importOneNamespace foreignURL mForeign foreignID = do
  -- Step 1: ensure nsID is valid here...
  mLocal' <- psModel <$> getState
  (mLocal, localID) <- case foreignURL of
        Nothing -> return (mLocal, foreignID)
        Just v -> case M.lookup foreignID (modelForeignNamespaces mLocal) of
          Just lid -> return (mLocal, lid)
          Nothing -> do
            
  M.lookup (m ) nsID

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
