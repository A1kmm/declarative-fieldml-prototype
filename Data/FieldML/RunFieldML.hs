{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.FieldML.Parser
import Data.FieldML.Level1Structure
import System.Console.CmdArgs.Implicit
import Data.Typeable
import Data.Data
import Control.Monad.Error
import Network.Curl

data RunFieldML = RunFieldML {
  includePaths :: [String],
  modelURL :: String
                             } deriving (Eq, Ord, Show, Data, Typeable)

runFieldML = RunFieldML { includePaths = def &= explicit &= name "I" &= typ "PATH" &= help "Path to search for imports (may be given multiple times)",
                          modelURL = "omitted" &= args &= typ "MODELURL" } &=
               summary "Tests the library by running a FieldML model"

main = do
  flags <- cmdArgs runFieldML
  if modelURL flags == "omitted"
    then putStrLn "You must specify a modelURL"
    else do
      s <- snd `liftM` (curlGetString_ (modelURL flags) [])
      case parseFieldML s of
        Left e -> do
          putStrLn "Compilation failed. Details follow:"
          putStrLn e
        Right m -> do
          putStrLn "Loaded a model:"
          print m
