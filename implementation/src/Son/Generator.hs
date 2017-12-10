
module Son.Generator where

import           Protolude

import           Data.Aeson
import           Data.Char (intToDigit)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import           Data.Vector (Vector)

generateSon :: Value -> Text
generateSon = TL.toStrict . TB.toLazyText . genValue

genValue :: Value -> Builder
genValue (Object hm)  = genObject hm
genValue (Array xs)   = genArray xs
genValue (String s)   = genString s
genValue (Number n)   = genNumber n
genValue (Bool True)  = "true"
genValue (Bool False) = "false"
genValue Null         = "null"

genObject :: HashMap Text Value -> Builder
genObject hm = "{" <> foldl' addMember mempty sortedMembers <> "}"
  where
    sortedMembers :: [(Text, Value)]
    sortedMembers = sortOn fst (HM.toList hm)

    addMember :: Builder -> (Text, Value) -> Builder
    addMember a (k,v)
      | a == mempty = pair
      | otherwise   = a <> "," <> pair
      where
        pair :: Builder
        pair = genString k <> ":" <> genValue v

genArray :: Vector Value -> Builder
genArray xs = "[" <> foldl' addElement mempty xs <> "]"
  where
    addElement :: Builder -> Value -> Builder
    addElement a v
      | a == mempty = genValue v
      | otherwise   = a <> "," <> genValue v

genString :: Text -> Builder
genString t = TB.singleton '"'
           <> TB.fromText (T.concatMap escape t)
           <> TB.singleton '"'
  where
    escape :: Char -> Text
    escape c =
      case c of
        '"'    -> '\\' `T.cons` T.singleton '"'
        '\\'   -> '\\' `T.cons` T.singleton '\\'
        '\x00' -> "\\u0000"
        '\x01' -> "\\u0001"
        '\x02' -> "\\u0002"
        '\x03' -> "\\u0003"
        '\x04' -> "\\u0004"
        '\x05' -> "\\u0005"
        '\x06' -> "\\u0006"
        '\x07' -> "\\u0007"
        '\x08' -> "\\b" -- backspace
        '\x09' -> "\\t" -- tab
        '\x0a' -> "\\n" -- line feed
        '\x0b' -> "\\u000b"
        '\x0c' -> "\\f" -- form feed
        '\x0d' -> "\\r" -- carriage return
        '\x0e' -> "\\u000e"
        '\x0f' -> "\\u000f"
        '\x10' -> "\\u0010"
        '\x11' -> "\\u0011"
        '\x12' -> "\\u0012"
        '\x13' -> "\\u0013"
        '\x14' -> "\\u0014"
        '\x15' -> "\\u0015"
        '\x16' -> "\\u0016"
        '\x17' -> "\\u0017"
        '\x18' -> "\\u0018"
        '\x19' -> "\\u0019"
        '\x1a' -> "\\u001a"
        '\x1b' -> "\\u001b"
        '\x1c' -> "\\u001c"
        '\x1d' -> "\\u001d"
        '\x1e' -> "\\u001e"
        '\x1f' -> "\\u001f"
        _      -> T.singleton c

genNumber :: Scientific -> Builder
genNumber = TB.fromString . formatScientific

-- | Based on @scientific@ 0.3.4.10.
--
-- Modified to remove scientific notation options
-- and trailing digits in fractions.
formatScientific :: Scientific -> String
formatScientific s
    | Sci.coefficient s < 0 = '-':formatPositiveScientific (-s)
    | otherwise             =     formatPositiveScientific   s
  where
    formatPositiveScientific :: Scientific -> String
    formatPositiveScientific = fmtAsFixed . Sci.toDecimalDigits

-- | Based on @scientific@ 0.3.4.10.
--
-- Modified not to print trailing zeros in fractions.
fmtAsFixed :: ([Int], Int) -> String
fmtAsFixed (is, e)
    | e <= 0    = '0':mkFractional (replicate (-e) '0' <> ds)
    | otherwise = f e "" ds
  where
    mk0 :: String -> String
    mk0 "" = "0"
    mk0 ls = ls

    mkFractional :: String -> String
    mkFractional ""  = ""
    mkFractional "0" = ""
    mkFractional ls  = '.':ls

    ds :: String
    ds = intToDigit <$> is

    f :: Int -> String -> String -> String
    f 0 s rs     = mk0 (reverse s) <> mkFractional rs
    f n s ""     = f (n-1) ('0':s) ""
    f n s (r:rs) = f (n-1) (r:s) rs
