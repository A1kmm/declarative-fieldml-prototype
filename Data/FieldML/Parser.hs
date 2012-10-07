{-# LANGUAGE OverloadedStrings,ScopedTypeVariables,PatternGuards,GADTs #-}
module Data.FieldML.Parser where

import Data.FieldML.Structure
import Data.FieldML.StructureForwardInstances
import Data.FieldML.StructureNoForwardInstances
import Data.FieldML.InitialModel
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Text.Parsec.ByteString
import Text.Parsec.Pos (SourcePos)
import System.FilePath
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Text.Parsec hiding ((<|>),  many)
import Control.Applicative
import Data.IORef
import Network.Curl
import Data.Maybe
import Data.Generics.Uniplate.Data
import Data.Monoid
import Data.Data hiding (Infix, Prefix)
import Data.List
import Data.Char
import Text.Parsec.Expr

-- Note: Models are cached in ForwardPossible form, but should not actually contain any
-- forward references. The reason for using ForwardPossible form is to allow them to be
-- easily transferred into the ForwardPossible model being built.
data LookupModel a = LookupModel { unlookupModel :: ReaderT (IORef (M.Map BS.ByteString (Model ForwardPossible))) (ErrorT String IO) a }

instance Monad LookupModel where
  return = LookupModel . return
  (LookupModel a) >>= f = LookupModel (a >>= liftM unlookupModel f)
  fail = LookupModel . lift . fail

data ParserState = ParserState {
    psSearchPaths :: [String],
    psModel :: Model ForwardPossible,
    psCurrentNamespace :: NamespaceID,
    psIndent :: [Int],
    psScopedVars :: M.Map BS.ByteString ScopedVariable
  }

type ModelParser a = ParsecT BS.ByteString ParserState LookupModel a

loadModel :: [String] -> String -> ErrorT String IO (Model ())
loadModel incl mpath = do
  (r, v) <- lift $ curlGetString_ mpath []
  if r == CurlOK
    then do
      mlist <- lift (newIORef M.empty)
      mf <- flip runReaderT mlist $
              unlookupModel $
                (either (fail.show) return =<<
                   runParserT modelParser (ParserState incl initialModel nsMain (repeat 1) M.empty)
                   mpath (v :: BS.ByteString))
      ErrorT . return $ resolveForwardDefinitions mf
    else
      fail $ "Can't load initial model " ++ mpath ++ ": " ++ (show r)

lookupModel :: BS.ByteString -> ModelParser (Model ForwardPossible)
lookupModel modName = do
  cache <- (lift . LookupModel) (lift . lift . readIORef =<< ask)
  case M.lookup modName cache of
    Just mod -> return mod
    Nothing -> do
      paths <- psSearchPaths <$> getState
      justOrFail ("Cannot find requested model " ++ BS.unpack modName) =<<
        (liftM msum (forM paths $ \path -> do
          let mpath = path </> ((BS.unpack modName) ++ ".fieldml")
          (r, v) <- lift . LookupModel . lift . lift $ curlGetString_ mpath []
          if r == CurlOK
            then do
              st <- getState
              pr <- lift $ runParserT modelParser (st { psCurrentNamespace = nsMain, psModel = initialModel,
                                                        psIndent = repeat 1, psScopedVars = M.empty }) mpath v
              case pr of
                Left err -> prettyFail (show err ++ "\n")
                Right m -> do
                  (lift . LookupModel) $ ask >>= lift . lift . flip modifyIORef (M.insert modName m)
                  return (Just m)
            else
              return Nothing
                         ))

modelParser :: ModelParser (Model forward)
modelParser = do
  namespaceContentsParser
  psModel <$> getState

namespaceContentsParser :: ModelParser ()
namespaceContentsParser = (many $ withVarScope $
  importStatement <|> namespaceBlock <|> domainBlock) >> (return ())

namespaceBlock :: ModelParser ()
namespaceBlock = do
  nsName <- (ftoken "namespace" *> identifier <* ftoken "where")
  parseNewNamespace nsName

domainBlock :: ModelParser ()
domainBlock = do
  sp <- mkSrcPoint
  ftoken "domain"
  blockBegin
  ident <- (identifier <* tokenSep <* char '=')
  withNewNamespace ident sp $ do
    dtype <- (do
                (cloneAnnot, domType) <-
                  ((ftoken "clone" *> ((,) NormalClone <$> domainType)) <|>
                   (do
                       ftoken "subset"
                       dexpr <- domainType
                       ftoken "using"
                       sexpr <- anyExpression
                       return ((SubsetClone sexpr), dexpr)
                   ) <|>
                   (do
                       ftoken "connect"
                       dexpr <- domainType
                       ftoken "using"
                       sexpr <- sepBy anyExpression (try (tokenSep *> char ','))
                       return ((ConnectClone sexpr), dexpr)
                   ))
                sp' <- finishSrcSpan sp
                st <- getState
                withModel $ \m ->
                  m { allNewDomains = M.insert (nextNewDomain m)
                                        (CloneDomain sp' cloneAnnot domType)
                                        (allNewDomains m),
                      nextNewDomain = (\(NewDomainID id) -> NewDomainID (id + 1))
                                      (nextNewDomain m)
                    }
                return $ (DomainType sp' (DomainHead []) . UseNewDomain . OFKnown . nextNewDomain . psModel) st
             ) <|> domainType
    withCurrentNamespace $
      \ns -> ns { nsDomains = M.insert ident dtype (nsDomains ns) }
    ((ftoken "where" *> namespaceContentsParser) <|> pure ())
  blockEnd

domainType = do
  sp <- mkSrcPoint
  dh <- (try (domainHead <* tokenSep <* "=>") <|> pure (DomainHead []))
  tokenSep
  de <- domainExpression
  sp' <- finishSrcSpan sp
  return $ DomainType sp' dh de

domainHead = (char '(' *> (DomainHead <$> sepBy domainClassRelation (char ',')) <* char ')') <|>
             ((DomainHead . (:[])) <$> domainClassRelation)

domainClassRelation = (DomainUnitConstraint <$> (ftoken "unit" *> unitExpression)
                                            <*> (tokenSep *> char '~' *> tokenSep *> unitExpression)) <|>
                      (try $ DomainClassEqual <$>
                         domainType <*> ((tokenSep *> char '~' *> tokenSep) *> domainType)) <|>
                      (DomainClassRelation <$> identifiedClass <*> sepBy domainType tokenSep)

domainExpression =
  (char '{' *> (ProductDomain <$> (labelMap (char ','))) <* char '}') <|>
  (try $ char '(' *> (DisjointUnion <$> mFailUnless "A disjoint union needs >1 domain" (not . M.null) (labelMap (char '|'))) <* char ')') <|>
  (FieldSignature <$> (try (domainExpression <* tokenSep <* string "<-" <* tokenSep)) <*> domainExpression) <|>
  (char '(' *> domainExpression <* char ')') <|>
  (UseRealUnits <$> (try (ftoken "R" *> tokenSep *> char '[') *> tokenSep *> unitExpression <* tokenSep <* char ']')) <|>
  ((try $ ApplyDomain <$> domainExpression <*> ((tokenSep *> refScopedVar) <* tokenSep <* char '=')) <*>
   (tokenSep *> domainExpression)
  ) <|>
  (DomainVariable <$> refScopedVar) <|>
  (UseNewDomain <$> identifiedDomain)

someInteger :: Integral a => ModelParser a
someInteger = (fst . foldl' (\(v,m) d -> (v + m * ((fromIntegral . digitToInt) d),
                                          m * 10)) (0,1)) <$> (many1 digit)
someSignedInteger :: Integral a => ModelParser a
someSignedInteger = (char '-' *> (negate <$> someInteger)) <|> someInteger

someDouble = do
  iPart <- someSignedInteger <|> pure 0
  fPart <- (char '.' *>
             ((fst . foldl' (\(v,m) d -> (v + m * ((fromIntegral . digitToInt) d),
                                          m / 10)) (0,0.1)) <$> (many1 digit))
           ) <|> pure 0
  ePart <- ((char 'E' <|> char 'e') *> someInteger) <|> pure 0
  return $ (iPart + fPart) * (10**ePart)

unitExpression =
  buildExpressionParser [
        [Postfix (try (tokenSep >> "**" >> tokenSep) >> mkSrcPoint >>= \ss -> someDouble >>= \pow -> return (flip (UnitPow ss) pow))],
        [Infix (try (tokenSep >> "*" >> tokenSep) >> mkSrcPoint >>= \ss -> return (UnitTimes ss)) AssocLeft,
         Prefix (someDouble >>= \scalar -> mkSrcPoint >>= \ss -> (char '*' *> return (UnitScalarMup ss scalar)))]
      ] (do
            ss <- mkSrcPoint
            (char '(' *> unitExpression <* char ')') <|>
             (ftoken "dimensionless" *> (pure $ UnitDimensionless ss)) <|>
             (UnitRef ss <$> identifiedUnit) <|>
             (UnitScopedVar ss <$> refScopedVar)
        )

anyExpression =
  buildExpressionParser [
      [Infix (mkSrcPoint >>= \ss -> return $ Apply ss) AssocLeft]
    ] ((mkSrcPoint >>= \ss -> try (ftoken "N" >> "::" >> tokenSep) >>
                              someInteger >>= \i -> return (LabelRef ss (OFKnown (ELabel nsNatural i)))) <|>
       (mkSrcPoint >>= \ss -> try (ftoken "I" >> "::" >> tokenSep) >>
                              someSignedInteger >>= \i -> return (LabelRef ss (OFKnown (ELabel nsInteger i)))) <|>
       (do
           ss <- mkSrcPoint
           rUnits <- try (ftoken "R" *> maybeUnits <* "::" <* tokenSep)
           dval <- someDouble
           return $ LiteralReal ss rUnits dval
       ) <|>
       (BoundVar <$> mkSrcPoint <*> refScopedVar) <|>
       (do
           ss <- mkSrcPoint
           try (tokenSep *> char '{' *> tokenSep)
           m <- M.fromList <$> (sepBy ((,) <$> identifiedLabel <*> (tokenSep *> char ':' *> anyExpression)) (char ','))
           tokenSep *> char '}'
           ss' <- finishSrcSpan ss
           return $ MkProduct ss' m
       ) <|>
       (ftoken "as" *> (MkUnion <$> mkSrcPoint <*> identifiedLabel)) <|>
       (ftoken "lookup" *> (Project <$> mkSrcPoint <*> identifiedLabel)) <|>
       (ftoken "append" *> (Append <$> mkSrcPoint <*> identifiedLabel)) <|>
       (char '\\' *> (Lambda <$> mkSrcPoint <*> refScopedVar <*> (tokenSep *> string "->" *> anyExpression))) <|>
       (Case <$> (ftoken "case" *> mkSrcPoint) <*> anyExpression <*>
        (ftoken "of" *> blockBegin *>
         (M.fromList <$> many ((,) <$> identifiedLabel <*> (string "->" *> tokenSep *>
                                                            anyExpression))) <* blockEnd)) <|>
       (NamedValueRef <$> mkSrcPoint <*> identifiedValue)
      )

maybeUnits =
  (char '[' *> unitExpression <* char ']') <|> (UnitDimensionless <$> mkSrcPoint)

refScopedVar = do
  ident <- try (tokenSep *> char '_') *> justIdentifier
  st <- getState
  case M.lookup ident (psScopedVars st) of
    Just sv -> return sv
    Nothing -> do
      let nsv = nextScopedVariable (psModel st)
      putState $ st { psModel = (psModel st) { nextScopedVariable = (\(ScopedVariable svid) -> ScopedVariable (svid + 1)) nsv },
                      psScopedVars = M.insert ident nsv (psScopedVars st) }
      return nsv

labelMap sep =
  M.fromList <$>
    (sepBy (((,) <$> (identifiedLabel <* tokenSep <* char ':') <*> domainExpression) <|>
            (identifiedDomainAndLabel)) sep)

withNewNamespace name sp f = do
  ensureIdentifierUnique name
  st' <- getState
  let oldModel' = psModel st'
  let newNSID = nextNSID oldModel'
  withCurrentNamespace $ \ns -> ns { nsNamespaces = M.insert name (OFKnown newNSID) (nsNamespaces ns) }
  st <- getState
  let oldNSID = psCurrentNamespace st
  let oldModel = psModel st
  let newModel = oldModel {
          nextNSID = (\(NamespaceID n) -> NamespaceID (n + 1)) newNSID,
          allNamespaces = M.insert newNSID (blankNamespace sp oldNSID) (allNamespaces oldModel)
        }
  putState $ st { psCurrentNamespace = newNSID, psModel = newModel }
  r <- f
  st' <- getState
  putState $ st' { psCurrentNamespace = oldNSID }
  return r

parseNewNamespace :: BS.ByteString -> ModelParser ()
parseNewNamespace name = do
  sp <- mkSrcPoint
  withNewNamespace name sp $ do
    blockBegin
    namespaceContentsParser
    blockEnd
    sp' <- finishSrcSpan sp
    withCurrentNamespace $ \ns -> ns { nsSrcSpan = sp' }

unitsSrcSpan (UnitDimensionless ss) = ss
unitsSrcSpan (UnitRef ss _) = ss
unitsSrcSpan (UnitTimes ss _ _) = ss
unitsSrcSpan (UnitPow ss _ _) = ss
unitsSrcSpan (UnitScalarMup ss _ _) = ss
unitsSrcSpan (UnitScopedVar ss _) = ss

anyNamedValueSrcSpan (NamedValue ss _) = ss
anyNamedValueSrcSpan (FFINamedValue {}) = biSrcSpan

knownOr :: a -> (t -> a) -> OrForward t ForwardPossible -> a
knownOr _ k (OFKnown v) = k v
knownOr v _ (OFForward {}) = v

forwardSrcSpan = SrcSpan "forward-defined" 0 0 0 0

ensureIdentifierUnique :: BS.ByteString -> ModelParser ()
ensureIdentifierUnique name = do
  ns <- getCurrentNamespace
  st <- getState
  let curModel = psModel st
  let r = (M.lookup name (nsNamespaces ns) >>= \ident ->
            return ("namespace", knownOr forwardSrcSpan (\ident' -> nsSrcSpan $ (allNamespaces curModel)!ident') ident)) `mplus`
          (M.lookup name (nsDomains ns) >>= \(DomainType ss _ _) ->
            return ("domain", ss)) `mplus`
          (M.lookup name (nsValues ns) >>= \ident ->
            return ("value", knownOr forwardSrcSpan (\ident' -> anyNamedValueSrcSpan $ (allNamedValues curModel)!ident') ident)) `mplus`
          (M.lookup name (nsClasses ns) >>= \ident ->
            return ("class", knownOr forwardSrcSpan (\ident' -> classSrcSpan $ (allDomainClasses curModel)!ident') ident)) `mplus`
          (M.lookup name (nsLabels ns) >>= \ident ->
            return ("label", nsSrcSpan $ (allNamespaces curModel)!(labelEnsemble ident))) `mplus`
          (M.lookup name (nsUnits ns) >>= \u ->
            return ("unit", unitsSrcSpan u))
  case r of
    Nothing -> return ()
    Just (what, sp) -> prettyFail $ "Attempt to redefine symbol " <> (BS.unpack name) <>
                       " which has already been defined at line " <>
                       (show sp) <> " as a " <> what

importStatement :: ModelParser ()
importStatement =
    doImport =<<
        (((,,,,) <$> (ftoken "import" *> blockBegin *>
                      (optionMaybe (ftoken "from" *> urlNoHash)))
                 <*> pathSpec
                 <*> optionMaybe (tokenSep *> char '(' *>
                                  (identifier `sepBy` (char ',')) <*
                                  tokenSep <* char ')')
                 <*> optionMaybe (ftoken "hiding" *>
                                  (tokenSep *> char '(' *>
                                   (identifier `sepBy` (char ',')) <*
                                   tokenSep <* char ')'))
                 <*> optionMaybe (ftoken "as" *> identifier)
         ) <* blockEnd)

doImport :: (Maybe BS.ByteString, [BS.ByteString], Maybe [BS.ByteString], Maybe [BS.ByteString], Maybe BS.ByteString) -> ModelParser ()
doImport (fromWhere, startWhere, whichSymbols, hidingWhichSymbols, Just asNamespace) = do
  ensureIdentifierUnique asNamespace
  sp <- mkSrcPoint
  withNewNamespace asNamespace sp $ do
    doImport (fromWhere, startWhere, whichSymbols, hidingWhichSymbols, Nothing)

doImport (mu@(Just modelURL), startWhere, whichSymbols, hidingWhichSymbols, _) = do
  m <- lookupModel modelURL
  st <- getState
  let startWhere' = if not (null startWhere) && head startWhere == "::" then tail startWhere else startWhere
  importFromModel mu m (toplevelNamespace . psModel $ st) startWhere' whichSymbols hidingWhichSymbols

doImport (_, startWhere, whichSymbols, hidingWhichSymbols, _) = do
  st <- getState
  let (ns, path) = if not (null startWhere) && head startWhere == "::" then
                     (toplevelNamespace . psModel $ st, tail startWhere)
                   else
                     (psCurrentNamespace st, tail startWhere)
  importFromModel Nothing (psModel st) (toplevelNamespace . psModel $ st) startWhere whichSymbols hidingWhichSymbols

untilM p v0 m
  | p v0 = do
    v <- m v0
    untilM p v m
  | otherwise = return v0

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f v = partitionM' f v ([], [])
partitionM' _ [] r = return r
partitionM' f (h:t) (a, b) = do
  r <- f h
  let n = if r then (h:a, b) else (a, h:b)
  partitionM' f t n

importFromModel modelURL fromModel ns (nsname:startWhere) whichSymbols hidingWhichSymbols = do
  intoModel <- psModel <$> getState
  case tryFindNamespaceScoped fromModel ns nsname of
    Nothing -> prettyFail $ "Namespace at " ++ (show . nsSrcSpan $ (allNamespaces fromModel)!ns) ++ " has no child namespace " ++
               (BS.unpack nsname)
    Just ns' ->
      importFromModel modelURL fromModel ns' startWhere whichSymbols hidingWhichSymbols

importFromModel modelURL fromModel nsID [] whichSymbols hidingWhichSymbols = do
  intoModel <- psModel <$> getState
  let intoNS = (allNamespaces intoModel) ! nsID
  let allSyms = concatMap (\f -> f intoNS) [M.keys . nsNamespaces, M.keys . nsDomains,
                                            M.keys . nsValues, M.keys . nsClasses, M.keys . nsLabels,
                                            M.keys . nsUnits]
  mapM_ (importSymbolFromModel modelURL fromModel nsID) $
    case whichSymbols of
      Just l -> l
      Nothing -> case hidingWhichSymbols of
        Just l -> S.toList ((S.fromList allSyms) `S.difference` (S.fromList l))
        Nothing -> allSyms

  -- Find expressions involving at least one known named value. This is an iterative
  -- process, because expressions can bring in new named values.
  untilM snd (modelAssertions fromModel, True) $ \(remainingExpr, _) -> do
    let isKnownNamedValueRef (NamedValueRef _ nvid) = do
          (msrcurl, _, srcnvid) <- traceToSource modelForeignValues modelURL fromModel nvid
          case msrcurl of
            Nothing -> return True
            Just srcurl -> 
              (isJust . M.lookup (srcurl, srcnvid)) <$> ((modelForeignValues . psModel) <$> getState)
        isKnownNamedValueRef _ = return False
    (newexpr, remaining') <-
      (partitionM :: (a -> ModelParser Bool) -> [a] -> (ModelParser ([a], [a]))) ((\expr -> ((any id) :: [Bool] -> Bool) <$> ((mapM isKnownNamedValueRef ((universeBi expr) :: [Expression])) :: ModelParser [Bool])) :: (Expression -> ModelParser Bool)) (remainingExpr :: [Expression])
      
    newExprLocal <- ensureStructureLocalM modelURL fromModel newexpr
    modifyState $ \st ->
      st { psModel = (psModel st) { modelAssertions = newExprLocal ++ (modelAssertions (psModel st)) } }
    return (remaining', not (null newexpr))

  -- Find instances which reference a class and domains we have already seen...
  forM_ (M.toList . instancePool $ fromModel) $ \entry@((dcid, dtl), inst) -> do
    (msrcurl, _, srcdcid) <- traceToSource modelForeignDomainClasses modelURL fromModel dcid
    case msrcurl of
      Nothing -> return ()
      Just srcurl -> do
        curmod <- psModel <$> getState
        when ((srcurl, srcdcid) `M.member` (modelForeignDomainClasses curmod)) $ do
          areAllUsed <- all id <$> (forM [d | UseNewDomain d <- universeBi dtl] $ \d -> do
                                       (Just srcurl, _, srcndid) <- traceToSource modelForeignDomains modelURL fromModel d
                                       return ((srcurl, srcndid) `M.member` (modelForeignDomains curmod)))
          when areAllUsed $ do
            (localHead, localInstance) <- ensureStructureLocalM modelURL fromModel entry
            modifyState $ \st ->
              st { psModel = (psModel st) { instancePool = M.insert localHead localInstance (instancePool (psModel st)) } }

importSymbolFromModel :: Maybe BS.ByteString -> Model forward -> NamespaceID -> BS.ByteString -> ModelParser ()
importSymbolFromModel modelURL m ns sym =
  case
    (importOneNamespace modelURL m sym <$> tryFindNamespaceScoped m ns sym) `mplus`
    (importOneDomain modelURL m sym <$> tryFindDomainScoped m ns sym) `mplus`
    (importOneValue modelURL m sym <$> tryFindValueScoped m ns sym) `mplus`
    (importOneClass modelURL m sym <$> tryFindClassScoped m ns sym) `mplus`
    (importOneLabel modelURL m sym <$> tryFindLabelScoped m ns sym) `mplus`
    (importOneUnit modelURL m sym <$> tryFindUnitScoped m ns sym)
  of
    Nothing -> prettyFail $ "Attempt to import " ++ (BS.unpack sym) ++ " from " ++ (BS.unpack $ fromMaybe "self" modelURL) ++
                       " but the symbol could not be found: " ++ (show m)
    Just v -> v >> return ()

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
     Nothing -> if (nsParent nsData == nsSpecial)
                  then Nothing
                  else tryFindSomethingScoped f m (nsParent nsData) sym

identifiedSomething whenNull = do
  ss <- mkSrcPoint
  fullPath <- pathSpec
  ss' <- finishSrcSpan ss
  st <- getState
  
  when (null fullPath) $ fail "Expected a path"
  let (startNS, relPath) = if head fullPath == "::"
                             then (toplevelNamespace . psModel $ st, tail fullPath)
                             else (psCurrentNamespace st, fullPath)
  when (null relPath) whenNull
  return $ OFForward startNS ss' True relPath

identifiedNamespace :: ModelParser (OrForward NamespaceID ForwardPossible)
identifiedNamespace = identifiedSomething return
identifiedDomain :: ModelParser (OrForward NewDomainID ForwardPossible)
identifiedDomain = identifiedSomething (fail "Expected a valid domain name")
identifiedValue :: ModelParser (OrForward NamedValueID ForwardPossible)
identifiedValue = identifiedSomething (fail "Expected a valid named value name")
identifiedClass :: ModelParser (OrForward DomainClassID ForwardPossible)
identifiedClass = identifiedSomething (fail "Expected a valid class name")
identifiedLabel :: ModelParser (OrForward ELabel ForwardPossible)
identifiedLabel = identifiedSomething (fail "Expected a valid label name")
identifiedUnit :: ModelParser (OrForward UnitID ForwardPossible)
identifiedUnit = identifiedSomething (fail "Expected a valid unit name")

identifiedDomainAndLabel :: ModelParser (OrForward ELabel ForwardPossible, DomainExpression ForwardPossible)
identifiedDomainAndLabel = do
  ss <- mkSrcPoint
  fullPath <- pathSpec
  ss' <- finishSrcSpan ss
  st <- getState
  
  when (null fullPath) $ fail "Expected a path"
  let (startNS, relPath) = if head fullPath == "::"
                             then (toplevelNamespace . psModel $ st, tail fullPath)
                             else (psCurrentNamespace st, fullPath)
  when (null relPath) (fail "Expected a valid domain name")
  return (OFForward startNS ss' True relPath, UseNewDomain (OFForward startNS ss' True relPath))

traceToSource :: Eq b => (Model forward -> M.Map (BS.ByteString, b) b) -> Maybe BS.ByteString -> Model forward -> b -> ModelParser (Maybe BS.ByteString, Model forward, b)
traceToSource f url model fid =
  case url of
    Nothing -> return (Nothing, model, fid)
    Just _ ->
      case lookup fid (map (\(a,b)->(b, a)) (M.toList (f model))) of
        Nothing -> return (url, model, fid)
        Just (url', fid') -> do
          model' <- lookupModel url'
          return (Just url', model', fid')

ensureStructureLocalM :: Data a => Maybe BS.ByteString -> Model forward -> a -> ModelParser a
ensureStructureLocalM foreignURL mForeign s =
  transformBiM (ensureNamespaceLocal foreignURL mForeign) =<<
  transformBiM (ensureDomainLocal foreignURL mForeign) =<<  
  transformBiM (ensureDomainClassLocal foreignURL mForeign) =<<
  transformBiM (ensureNamedValueLocal foreignURL mForeign) =<<
  transformBiM (ensureUnitsLocal foreignURL mForeign) s

ensureSomethingLocal :: (Show a, Eq a, Ord a, Data b) =>
                        (Model forward -> M.Map (BS.ByteString, a) a) -> (Model forward-> M.Map a b) -> (Model forward -> a) ->
                        (Model forward -> Model forward) -> (Model forward -> M.Map a b -> Model forward) ->
                        (Model forward -> M.Map (BS.ByteString, a) a -> Model forward) -> Maybe BS.ByteString -> Model forward -> a ->
                        ModelParser a
ensureSomethingLocal modelForeignSomething allSomething nextID incrementNextID setAllSomething setForeignSomething foreignURL' mForeign' foreignID' = do
  (foreignURL, mForeign, foreignID) <- traceToSource modelForeignSomething foreignURL' mForeign' foreignID'
  st <- getState
  let mLocal = psModel st
  case foreignURL of
    Nothing -> return foreignID
    Just v -> case M.lookup (v, foreignID) (modelForeignSomething mLocal) of
      Just lid -> return lid
      Nothing -> do
        case (M.lookup foreignID (allSomething mForeign)) of
          Nothing -> return foreignID -- This happens for builtins which are
                                      -- implicitly defined everywhere, but not
                                      -- actually explicitly defined.
          Just nsData -> do
            let localID = nextID mLocal
            putState (st { psModel = incrementNextID mLocal })
            nsNewData <- ensureStructureLocalM foreignURL mForeign nsData
            st' <- getState
            putState (st' { psModel = ((psModel st') `setAllSomething` (M.insert localID  nsNewData $ allSomething (psModel st')))
                                      `setForeignSomething` (M.insert (v, foreignID) localID $
                                                             modelForeignSomething (psModel st')) } )
            return localID

ensureNamespaceLocal =
  ensureSomethingLocal modelForeignNamespaces allNamespaces nextNSID
    (\m -> m { nextNSID = (\(NamespaceID n) -> NamespaceID (n+1)) (nextNSID m) })
    (\m v -> m { allNamespaces = v })
    (\m v -> m { modelForeignNamespaces = v })

ensureDomainLocal = 
  ensureSomethingLocal modelForeignDomains allNewDomains nextNewDomain
    (\m -> m { nextNewDomain = (\(NewDomainID n) -> NewDomainID (n+1)) (nextNewDomain m) })
    (\m v -> m { allNewDomains = v })
    (\m v -> m { modelForeignDomains = v })

ensureDomainClassLocal = 
  ensureSomethingLocal modelForeignDomainClasses allDomainClasses nextDomainClass
    (\m -> m { nextDomainClass = (\(DomainClassID n) -> DomainClassID (n+1)) (nextDomainClass m) })
    (\m v -> m { allDomainClasses = v })
    (\m v -> m { modelForeignDomainClasses = v })

ensureNamedValueLocal =
  ensureSomethingLocal modelForeignValues allNamedValues nextNamedValue
    (\m -> m { nextNamedValue = (\(NamedValueID n) -> NamedValueID (n+1)) (nextNamedValue m) })
    (\m v -> m { allNamedValues = v })
    (\m v -> m { modelForeignValues = v })

ensureUnitsLocal =
  ensureSomethingLocal modelForeignUnits allUnits nextUnit
    (\m -> m { nextUnit = (\(UnitID n) -> UnitID (n+1)) (nextUnit m) })
    (\m v -> m { allUnits = v })
    (\m v -> m { modelForeignUnits = v })

importOneNamespace foreignURL mForeign sym foreignID = do
  localID <- ensureNamespaceLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsNamespaces = M.insert sym localID (nsNamespaces ns)
    }

importOneDomain foreignURL mForeign sym foreignDom = do
  localDom <- ensureStructureLocalM foreignURL mForeign foreignDom
  withCurrentNamespace $ \ns -> ns {
      nsDomains = M.insert sym localDom (nsDomains ns)
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
  localID <- ensureNamespaceLocal foreignURL mForeign foreignID
  withCurrentNamespace $ \ns -> ns {
      nsLabels = M.insert sym (ELabel localID val) (nsLabels ns)
    }

importOneUnit foreignURL mForeign sym foreignUnits = do
  localUnits <- ensureStructureLocalM foreignURL mForeign foreignUnits
  withCurrentNamespace $ \ns -> ns {
      nsUnits = M.insert sym localUnits (nsUnits ns)
    }

withCurrentNamespace :: (Namespace forward -> Namespace forward) -> ModelParser ()
withCurrentNamespace f = do
  st <- getState
  let curModel = psModel st
  putState $ st { psModel = curModel { allNamespaces =
    M.insert (psCurrentNamespace st) (f ((allNamespaces curModel)!(psCurrentNamespace st))) (allNamespaces curModel)
                                     } }

withModel :: (Model forward -> Model forward) -> ModelParser ()
withModel f = modifyState $ \st -> st { psModel = f (psModel st) }

getIndent = (head . psIndent) <$> getState

getCurrentNamespace = do
  st <- getState
  let curModel = psModel st
  return $ (allNamespaces curModel)!(psCurrentNamespace st)

getColumn = sourceColumn <$> getPosition

pathSpec = tokenSep *> ((string "::" *> (("::":) <$> relPathSpec)) <|> relPathSpec)
relPathSpec = sepBy justIdentifier (string "::") <*
              lookAhead (((noneOf (':':(identChars ++ identAltChars)) <|>
                           try (char ':' >> noneOf (':':(identChars ++ identAltChars)))) *> pure ()) <|> eof)
commentLine = (char '#' *> many (noneOf "\r\n") *> (eof <|> (oneOf "\r\n" *> pure ()))) <?> "a comment"

blockBegin :: ModelParser ()
blockBegin = do
  oldind <- getIndent
  many ((oneOf "\r\n " *> pure ()) <|> (commentLine <?> "bb"))
  lc <- getColumn
  let l = if (oldind < lc) then lc else (oldind + 1)
  when (oldind >= l) . fail $ "Expected more than " ++ (show oldind) ++ " spaces at start of block."
  p <- getPosition
  modifyState (\s -> s { psIndent = l:(psIndent s) })

tokenSep = try $ do
  many (((commentLine <?> "ts") <|> (oneOf "\r\n " *> pure ())) <?> "whitespace or comments between tokens")
  l <- getColumn
  ind <- getIndent
  when (l < ind) (fail $ "Expected at least " ++ (show l) ++ " spaces within block.")
  
blockEnd = eof <|> (try $ do
  i <- getIndent
  many (try ((oneOf " \r\n" *> pure ()) <|> commentLine))
  c <- getColumn
  when (c >= i) . fail $ "Expected line with indent of less than " ++ (show i) ++ " at end of block."
  p <- getPosition
  modifyState (\s -> s { psIndent = tail (psIndent s) })
  return ())

identStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
identChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'"
identAltChars = "~`!@$%^&*-+=<>?"
endToken = lookAhead ((noneOf identChars *> pure ()) <|> eof)
ftoken n = try (tokenSep *> string n *> endToken)
urlNoHash = tokenSep *> (BS.pack <$> some (noneOf " \t\r\n#"))
justIdentifier = BS.pack <$> (((:) <$> (oneOf identStartChars) <$> (many (oneOf identChars))) <|> some (oneOf identAltChars))
identifier = tokenSep *> justIdentifier

mkSrcPoint = do
  sp <- getPosition
  return $ SrcSpan (sourceName sp) (sourceLine sp) (sourceColumn sp) (sourceLine sp) (sourceColumn sp)

finishSrcSpan (SrcSpan sn sl sc _ _) = do
  sp2 <- getPosition
  return $ SrcSpan sn sl sc (sourceLine sp2) (sourceColumn sp2)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left v) = Left (f v)
mapLeft _ (Right v) = Right v

withVarScope f = do
  st <- getState
  f
  st' <- getState
  -- Restore old scoped variables from backup...
  putState $ st' { psScopedVars = psScopedVars st }

resolveForwardDefinitions :: Model ForwardPossible -> Either String (Model ())
resolveForwardDefinitions m =
  let
    (err, m') = translateForwardDefinitions m
  in
   if null err then Right (unsafeModelOrForwardToNoForward m') else Left err

unsafeModelOrForwardToNoForward :: Model ForwardPossible -> Model ()
unsafeModelOrForwardToNoForward m = m {
    allNamespaces =
       M.map (\ns -> ns {
                 nsNamespaces = M.map unsafeOrForwardToNoForward (nsNamespaces m),
                 nsValues = M.map unsafeOrForwardToNoForward (nsValues m),
                 nsClasses = M.map unsafeOrForwardToNoForward (nsClasses m),
                 nsLabels = M.map unsafeELabelOrForwardToNoForward (nsLabels m),
                 nsUnits = M.map unsafeOrForwardToNoForward (nsUnits m)
                        }) (allNamespaces m),
    allNewDomains =
      M.map (let f (BuiltinDomain ss) = BuiltinDomain ss
                 f (CloneDomain ss ca dt) =
                   CloneDomain ss (fixCloneAnnotation ca)
                               (unsafeDomainTypeOrForwardToNoForward dt)
                 fixCloneAnnotation NormalClone = NormalClone
                 fixCloneAnnotation (SubsetClone expr) = SubsetClone
                   (unsafeExpressionOrForwardToNoForward expr)
                 fixCloneAnnotation (ConnectClone exprs) = ConnectClone
                   (map unsafeExpressionOrForwardToNoForward exprs)
               in f) (allNewDomains m),
    allDomainClasses =
      M.map (\(DomainClass ss ck df cv) ->
              DomainClass ss ck df (M.map (\(i, dt) -> (i, unsafeDomainTypeOrForwardToNoForward dt)) cv)) (allDomainClasses m),
    instancePool =
      M.fromList . map (
        \((dcid, dtl), (Instance ss ip idf ia)) ->
          ((dcid, map unsafeDomainTypeOrForwardToNoForward dtl),
           (Instance ss ip (M.map unsafeDomainTypeOrForwardToNoForward idf)
                     (map unsafeExpressionOrForwardToNoForward ia)))
        ) . M.toList . instancePool $ m,
    modelAssertions =
      map unsafeExpressionOrForwardToNoForward (modelAssertions m)
  }

unsafeExpressionOrForwardToNoForward (Apply ss fex argex) =
  Apply ss (unsafeExpressionOrForwardToNoForward fex)
        (unsafeExpressionOrForwardToNoForward argex)
unsafeExpressionOrForwardToNoForward (NamedValueRef ss nvid) =
  NamedValueRef ss (unsafeOrForwardToNoForward nvid)
unsafeExpressionOrForwardToNoForward (LabelRef ss el) =
  LabelRef ss (unsafeELabelOrForwardToNoForward el)
unsafeExpressionOrForwardToNoForward (LiteralReal ss uexpr r) =
  LiteralReal ss (unsafeUnitExprOrForwardToNoForward uexpr) r
unsafeExpressionOrForwardToNoForward (BoundVar ss sv) =
  BoundVar ss (unsafeOrForwardToNoForward sv)
unsafeExpressionOrForwardToNoForward (MkProduct ss l2e) =
  MkProduct ss (M.fromList . map (\(l, e) -> (unsafeELabelOrForwardToNoForward l,
                                              unsafeExpressionOrForwardToNoForward e)
                                 ) . M.toList $ l2e)
unsafeExpressionOrForwardToNoForward (MkUnion ss l) =
  MkUnion ss (unsafeELabelOrForwardToNoForward l)
unsafeExpressionOrForwardToNoForward (Project ss l) =
  Project ss (unsafeELabelOrForwardToNoForward l)
unsafeExpressionOrForwardToNoForward (Append ss l) =
  Append ss (unsafeELabelOrForwardToNoForward l)
unsafeExpressionOrForwardToNoForward (Lambda ss sv ex) =
  Lambda ss (unsafeOrForwardToNoForward sv) (unsafeExpressionOrForwardToNoForward ex)
unsafeExpressionOrForwardToNoForward (Case ss ex l2f) = 
  Case ss (unsafeExpressionOrForwardToNoForward ex) (M.fromList . map (\(l, ex) -> (unsafeELabelOrForwardToNoForward l, unsafeExpressionOrForwardToNoForward ex)) . M.toList $ l2f)

unsafeELabelOrForwardToNoForward (ELabel ens v) =
  ELabel (unsafeOrForwardToNoForward ens) v

unsafeUnitExprOrForwardToNoForward (UnitDimensionless ss) = UnitDimensionless ss
unsafeUnitExprOrForwardToNoForward (UnitRef ss uid) =
  UnitRef ss (unsafeOrForwardToNoForward uid)
unsafeUnitExprOrForwardToNoForward (UnitTimes ss u1 u2) =
  UnitTimes ss (unsafeUnitExprOrForwardToNoForward u1)
            (unsafeUnitExprOrForwardToNoForward u2)
unsafeUnitExprOrForwardToNoForward (UnitPow ss u v) =
  UnitPow ss (unsafeUnitExprOrForwardToNoForward u) v
unsafeUnitExprOrForwardToNoForward (UnitScalarMup ss scal u) =
  UnitScalarMup ss scal (unsafeUnitExprOrForwardToNoForward u)
unsafeUnitExprOrForwardToNoForward (UnitScopedVar ss sv) =
  UnitScopedVar ss (unsafeOrForwardToNoForward sv)

unsafeDomainTypeOrForwardToNoForward (DomainType ss (DomainHead dcrs) de) =
  DomainType ss
    (DomainHead
     (map 
      (let
          f (DomainClassRelation dcid dts) =
            DomainClassRelation (unsafeOrForwardToNoForward dcid)
              (map unsafeDomainTypeOrForwardToNoForward dts)
          f (DomainClassEqual dt1 dt2) =
              DomainClassEqual (unsafeDomainTypeOrForwardToNoForward dt1)
                (unsafeDomainTypeOrForwardToNoForward dt2)
          f (DomainUnitConstraint ue) =
            DomainUnitConstraint (unsafeOrForwardToNoForward ue)
                           in f) dcrs))
    (unsafeDomainExpressionOrForwardToNoForward de)

unsafeDomainExpressionToOrForwardNoForward (UseNewDomain d) =
  UseNewDomain (unsafeOrForwardToNoForward d)
unsafeDomainExpressionOrForwardToNoForward (UseRealUnits ue) =
  UseRealUnits (unsafeUnitExprOrForwardToNoForward ue)
unsafeDomainExpressionOrForwardToNoForward (ApplyDomain to sv val) =
  ApplyDomain (unsafeDomainExpressionOrForwardToNoForward to)
              (unsafeOrForwardToNoForward sv)
              (unsafeDomainExpressionOrForwardToNoForward val)
unsafeDomainExpressionOrForwardToNoForward (DomainVariable sv) =
  DomainVariable (unsafeOrForwardToNoForward sv)
unsafeDomainExpressionOrForwardToNoForward (ProductDomain l2e) =
  ProductDomain . M.fromList . map (\(l, de) -> (unsafeELabelOrForwardToNoForward l, unsafeDomainExpressionOrForwardToNoForward de)) . M.toList $ l2e
unsafeDomainExpressionOrForwardToNoForward (DisjointUnion l2e) =
  DisjointUnion . M.fromList . map (\(l, de) -> (unsafeELabelOrForwardToNoForward l, unsafeDomainExpressionOrForwardToNoForward de)) . M.toList $ l2e
unsafeDomainExpressionOrForwardToNoForward (FieldSignature d cd) =
  FieldSignature (unsafeDomainExpressionOrForwardToNoForward d)
                 (unsafeDomainExpressionOrForwardToNoForward cd)
unsafeDomainExpressionOrForwardToNoForward (EvaluateDomainFunction dcid fid de) =
  EvaluateDomainFunction (unsafeOrForwardToNoForward dcid) fid
                 (unsafeDomainExpressionOrForwardToNoForward de)

unsafeOrForwardToNoForward :: OrForward t ForwardPossible -> OrForward t ()
unsafeOrForwardToNoForward (OFKnown v) = OFKnown v
unsafeOrForwardToNoForward (OFForward _ _ _) = error "unsafeOrForwardToNoForward encountered an OFForward, but it is only supposed to be used once all possible OFForward entries have been resolved"

translateForwardDefinitions :: Model ForwardPossible -> (String, Model ForwardPossible)
translateForwardDefinitions = undefined

justOrFail failWith Nothing = prettyFail failWith
justOrFail _ (Just x) = return x

prettyFail x = do
  p <- getPosition
  lift . fail $ x ++ "\n  at " ++ show p

mFailUnless msg pred f = do
  r <- f
  if pred r
    then fail msg
    else return r

foldUntil :: (a -> b -> Maybe a) -> a -> [b] -> (a, [b])
foldUntil _ s0 [] = (s0, [])
foldUntil f s0 l
  | Nothing <- r = (s0, l)
  | Just s1 <- r = foldUntil f s0 l
    where
      r = f s0 l
