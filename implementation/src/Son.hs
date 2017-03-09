
module Son where

import           Protolude

import           Data.Aeson
import           Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import           Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Vector as V
import           Test.QuickCheck hiding (generate)

import           Son.Generator (generateSon)
import           Son.Parser (sonValue)

newtype Son
  = Son { _unSon :: Value }
  deriving (Eq, Show, NFData)

-- * Parsing

parse :: Text -> Either Text Son
parse = bimap T.pack Son . parseOnly (sonValue <* endOfInput)

data DecodingError
  = UTF8Error   UnicodeException
  | SyntaxError Text
  deriving (Eq, Show)

decode :: ByteString -> Either DecodingError Son
decode = first SyntaxError . parse
     <=< first UTF8Error . decodeUtf8'

-- * Generation

generate :: Son -> Text
generate = generateSon . _unSon

encode :: Son -> ByteString
encode = encodeUtf8 . generate

-- * Arbitrary

instance Arbitrary Son where
  arbitrary = Son <$> sized f
    where
      f :: Int -> Gen Value
      f n | n <= 1    = oneof nonRecursive
          | otherwise = oneof $
                fmap (Array . V.fromList) (traverse (const (f (n `div` 10)))
                  =<< (arbitrary :: Gen [()]))
              : fmap (Object . HM.fromList) (traverse (const (g (n `div` 10)))
                  =<< (arbitrary :: Gen [()]))
              : nonRecursive

      g :: Int -> Gen (Text, Value)
      g n = (,) <$> arbitraryText <*> f n

      nonRecursive :: [Gen Value]
      nonRecursive =
        [ pure Null
        , Bool <$> arbitrary
        , String <$> arbitraryText
        , Number <$> arbitraryScientific
        ]

      arbitraryText :: Gen Text
      arbitraryText = T.pack <$> arbitrary

      arbitraryScientific :: Gen Scientific
      arbitraryScientific = (fromFloatDigits :: Double -> Scientific)
                        <$> arbitrary
