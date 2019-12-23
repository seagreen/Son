module Son where

import Data.Aeson
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text.Encoding.Error (UnicodeException)
import Protolude
import Son.Generator (generateSon)
import Son.Parser (sonValue)
import Test.QuickCheck hiding (generate)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

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

-- * Random Son value creation (for testing)

instance Arbitrary Son where
  arbitrary = Son <$> sized arbitraryValue
    where
      arbitraryValue :: Int -> Gen Value
      arbitraryValue n
        | n <= 1    = oneof nonRecursive
        | otherwise = oneof $
              (Array . V.fromList <$> arbitraryArray (n `div` 10))
            : (Object . HM.fromList <$> arbitraryObject (n `div` 10))
            : nonRecursive

      arbitraryArray :: Int -> Gen [Value]
      arbitraryArray n = traverse (const (arbitraryValue n))
                     =<< (arbitrary :: Gen [()])

      arbitraryObject :: Int -> Gen [(Text, Value)]
      arbitraryObject n = traverse (const textAndValue)
                      =<< (arbitrary :: Gen [()])
        where
          textAndValue :: Gen (Text, Value)
          textAndValue = (,) <$> arbitraryText <*> arbitraryValue n

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
