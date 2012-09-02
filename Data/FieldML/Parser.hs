module Data.FieldML.Parser where

import Data.FieldML.Structure
import qualified Data.ByteString.Char8 as BS
import Text.Parsec.ByteString

modelParser = 

parseModel :: String -> BS.ByteString -> Model
parseModel = parse modelParser