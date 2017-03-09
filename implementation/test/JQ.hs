module JQ where

import           Protolude

import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson
import           Data.String (String)
import qualified Data.Text as T
import           System.Process (readProcess)

encodeJQ :: ToJSON a => a -> IO String
encodeJQ a =
  readProcess "jq" ["--compact-output", "--sort-keys", "."] jsonString
  where
    jsonString :: String
    jsonString = T.unpack (decodeUtf8 (LBS.toStrict (encode a)))
