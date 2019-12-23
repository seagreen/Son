module JQ where

import Data.Aeson
import Data.String (String)
import Protolude
import System.Process (readProcess)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

encodeJQ :: ToJSON a => a -> IO String
encodeJQ a =
  readProcess "jq" ["--compact-output", "--sort-keys", "."] jsonString
  where
    jsonString :: String
    jsonString = T.unpack (decodeUtf8 (LBS.toStrict (encode a)))
