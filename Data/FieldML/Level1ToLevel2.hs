{-# LANGUAGE DeriveDataTypeable #-}
module Data.FieldML.Level1ToLevel2 where

import qualified Data.FieldML.Level1Structure as L1
import qualified Data.FieldML.Level2Structure as L2
import Network.Curl
import Data.IORef
import System.FilePath
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Data.FieldML.Parser
import Data.FieldML.InitialModel

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
  maybe (firstJustM t)

loadL2ModelFromURL' :: [String] -> String -> LookupModel L2.L2Model
loadL2ModelFromURL' incl mpath = do
  r <-
    firstJustM $ flip map incl $ \tryIncl -> do
      (r, v) <- liftIO $ curlGetString_ (tryIncl ++ mpath) []
      if r == CurlOK
        then do
          nsc <- LookupModel . lift . ErrorT . return $ return (parseFieldML v)
          finalImpMap <- tryResolveAllImports incl nsc
          liftM Just $
            LookupModel . lift . ErrorT . return . runIdentity . runErrorT . $ translateL1ToL2 mpath nsc finalImpMap
        else
          return Nothing
  case r of
    Just v -> v
    Nothing -> fail $ "Could not find model " ++ mpath ++ " anywhere in the include paths " ++ show mpath

tryResolveAllImports :: [String] -> L1.L1NamespaceContents -> ErrorT String IO (M.Map BS.ByteString L2.L2Model)
tryResolveAllImports incl nsc = do
  forM_ (uniq . sort $ [v | L1.L1NSImport { L1.l1nsImportFrom = Just v} <- universeBi nsc]) $ \toImport -> do
    lm <- loadL2ModelFromURL incl (LBS.unpack toImport)
    modify $ M.insert toImport lm
  get

data L1NSPath = L1NSPathStop | L1NSPathNamespace BS.ByteString L1NSPath | L1NSPathDomain BS.ByteString |
                L1NSPathClass BS.ByteString | L1NSPathValue BS.ByteString
                deriving (Eq, Ord, Show, Data, Typeable)

translateL1ToL2 mpath l1ns impmap =
  flip runReaderT (l1ns, mpath, impmap) $ flip execStateT initialModel $ do
    recursivelyTranslateModel S.empty S.empty [L1NSPathStop]

type ModelTranslation a = StateT L2Model (ReaderT (L1NamespaceContents, String, M.Map BS.ByteString L2.L2Model) (ErrorT String IO)) a

recursivelyTranslateModel :: S.Set L1NSPath -> S.Set L1NSPath -> [L1NSPath] -> ModelTranslation L2.L2Model
recursivelyTranslateModel deferred done queue =
  if null queue
    then return ()
    else
      do
        let firstEntry = head queue
        r <- tryTranslatePartNow firstEntry done
        case r of
          Nothing ->
            recursivelyTranslateModel (S.delete firstEntry deferred)
                                      (S.insert firstEntry deferred)
                                      (tail queue)
          Just deps ->
            forM_ deps $ \dep ->
              when (S.member dep deferred) $
                fail ("The model contains an illegal reference cycle - need to process " ++
                      show dep ++ " to process " ++ show firstEntry ++ ", and vice versa.")
              recursivelyTranslateModel (S.insert firstEntry deferred) done (deps ++ queue)

tryTranslatePartNow :: L1NSPath -> S.Set L1NSPath -> ModelTranslation (Maybe [L1NSPath])
tryTranslatePartNow p done =
  (l1ns, _, _) <- ask
  tryTranslatePartNow' p done nsMain l1ns

tryTranslatePartNow' (L1NSPathNamespace nsName nsPath)
