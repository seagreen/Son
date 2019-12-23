
module Son.Parser where

import           Protolude hiding (option, take)

import           Control.Monad.Fail (fail)
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char (isDigit)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (Scientific)
import           Data.String (unlines)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

sonValue :: Parser Value
sonValue = fmap Object       sonObject
       <|> fmap Array        sonArray
       <|> fmap String       sonString
       <|> fmap Number       sonNumber
       <|> fmap Bool         sonBoolean
       <|> fmap (const Null) sonNull

sonObject :: Parser (HashMap Text Value)
sonObject = do
  void (char '{')
  es <- element `sepBy` char ','
  void (char '}')
  checkOrder es
  pure (HM.fromList es)
  where
    element :: Parser (Text, Value)
    element = do
      t <- sonString
      void (char ':')
      x <- sonValue
      pure (t,x)

    checkOrder :: [(Text, Value)] -> Parser ()
    checkOrder = void . foldM f Nothing
      where
        f :: Maybe Text -> (Text, a) -> Parser (Maybe Text)
        f Nothing    (k,_) = pure (Just k)
        f (Just acc) (k,_)
          | k > acc   = pure (Just k)
          | otherwise = fail ("Key out of order: " <> T.unpack k)

sonArray :: Parser (Vector Value)
sonArray = do
  void (char '[')
  xs <- V.fromList <$> sonValue `sepBy` char ','
  void (char ']')
  pure xs

sonString :: Parser Text
sonString = do
  void (char '"')
  ts <- mempty <$ char '"'
    <|> body <* char '"'
  pure (T.concat ts)
    where
      body :: Parser [Text]
      body = many (takeWhile1 (\c -> c /= '"' && c /= '\\') <|> unescape)

      unescape :: Parser Text
      unescape = do
        void (char '\\')
        T.singleton <$> (shortcutEscape <|> unescapeCodePoint)

      shortcutEscape :: Parser Char
      shortcutEscape =
            char '"'
        <|> char '\\'
        -- NOTE: solidus (U+002F) isn't listed here, because even though
        -- JSON allows it to be escaped with @\/@ it doesn't have to be.
        <|> (char 'b' *> pure '\x08') -- backspace
        <|> (char 't' *> pure '\x09') -- tab
        <|> (char 'n' *> pure '\x0a') -- line feed
        <|> (char 'f' *> pure '\x0c') -- form feed
        <|> (char 'r' *> pure '\x0d') -- carriage return

      unescapeCodePoint :: Parser Char
      unescapeCodePoint = do
        void (char 'u')
        void (char '0')
        void (char '0')
        n <- take 2
        case n of
          "00" -> pure '\x00'
          "01" -> pure '\x01'
          "02" -> pure '\x02'
          "03" -> pure '\x03'
          "04" -> pure '\x04'
          "05" -> pure '\x05'
          "06" -> pure '\x06'
          "07" -> pure '\x07'
          -- <first set of shortcut escapes>
          "0b" -> pure '\x0b'
          -- <second set of shortcut escapes>
          "0e" -> pure '\x0e'
          "0f" -> pure '\x0f'
          "10" -> pure '\x10'
          "11" -> pure '\x11'
          "12" -> pure '\x12'
          "13" -> pure '\x13'
          "14" -> pure '\x14'
          "15" -> pure '\x15'
          "16" -> pure '\x16'
          "17" -> pure '\x17'
          "18" -> pure '\x18'
          "19" -> pure '\x19'
          "1a" -> pure '\x1a'
          "1b" -> pure '\x1b'
          "1c" -> pure '\x1c'
          "1d" -> pure '\x1d'
          "1e" -> pure '\x1e'
          "1f" -> pure '\x1f'
          _ -> fail ("\\u escape followed by invalid sequence: " <> T.unpack n)

sonNumber :: Parser Scientific
sonNumber = makeScientific -- TODO: note that Order matters here.
        <|> 0 <$ char '0'
  where
    -- Use @nonZero@ to make sure the number's a valid Son number,
    -- then use @readMaybe@ to turn it into a @Scientific@.
    --
    -- This would be more efficient if @nonZero@ could create the
    -- @Scientific@ itself. To do so we'll need a function going from
    -- @Bool -> [Char] -> [Char] -> Scientific@, where the @Bool@ is
    -- the sign of the number and the @Char@s are digits.
    makeScientific :: Parser Scientific
    makeScientific = do
      (t, ()) <- match nonZero
      case readMaybe (T.unpack t) of
        Nothing -> fail (unlines [ "readMaybe failed (this should"
                                 , " never happen) on input: "
                                 , show t
                                 ])
        Just s  -> pure s

nonZero :: Parser ()
nonZero = do
  option () (void (char '-'))
  startsWithNonZero <|> char '0' *> void sonFraction
  where
    startsWithNonZero :: Parser ()
    startsWithNonZero = do
      void sonPositiveInteger
      option () (void sonFraction)

sonPositiveInteger :: Parser Text
sonPositiveInteger = do
  n <- takeTill (not . isDigit)
  when (T.null n) (fail "integer part is empty")
  when (T.head n == '0') (fail ("integer part stars with zero: " <> show n))
  pure n

sonFraction :: Parser Text
sonFraction = do
  void (char '.')
  n <- takeTill (not . isDigit)
  when (T.null n) (fail "no digits after decimal point")
  when (T.last n == '0') (fail (show ("fractional part ends in zero: " <> n)))
  pure n

sonBoolean :: Parser Bool
sonBoolean = True <$ string "true"
         <|> False <$ string "false"

sonNull :: Parser ()
sonNull = () <$ string "null"
