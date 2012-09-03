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
  newNamespace nsName
  return ()

newNamespace name = do
  ensureIdentifierUnique name
  st <- getState
  sp <- mkSrcPoint
  let oldModel = psModel st
  let oldNSID = psCurrentNamespace st
  let newNSID = nextNSID oldModel
  withCurrentNamespace $ \ns -> ns { nsNamespaces = M.insert name newNSID (nsNamespaces ns) }
  let newModel = oldModel {
          nextNSID = (\NamespaceID n -> NamespaceID (n + 1)) newNSID,
          allNamespaces = M.insert newNSID (blankNamespace sp oldNSID) (allNamespaces oldModel)
        }
  putState $ st { psCurrentNamespace = newNSID, psModel = newModel }
  blockBegin
  namespaceContentsParser
  blockEnd
  return newNSID

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
