{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.FieldML.Parser
import Data.FieldML.Level1Structure
import Data.FieldML.Level1ToLevel2
import System.Console.CmdArgs.Implicit
import Data.Typeable
import Data.Data
import Control.Monad.Error

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
      res <- runErrorT
        (loadL2ModelFromURL (includePaths flags) (modelURL flags))
      case res of
        Left e -> do
          putStrLn "Compilation failed. Details follow:"
          putStrLn e
        Right m -> do
          putStrLn "Loaded a model:"
          print m
