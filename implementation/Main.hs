
module Main where

import           Protolude

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Options.Applicative

import           Son (Son(Son))
import qualified Son

data Mode
  = JSONToSon
  | Verify
  | VerifyNoNewline
  deriving (Eq, Show)

execInfo :: ParserInfo Mode
execInfo = info (options <**> helper)
  (  fullDesc
  <> progDesc (fold [ "Convert JSON to Son."
                    , " Note that no newline will be appended."
                    , " `stdin JSON -> Either (stderr String) (stdout Son)`"
                    ])
  )
  where
    options :: Parser Mode
    options = fromMaybe JSONToSon
          <$> optional (verifyFlag <|> verifyNoNewlineFlag)

verifyFlag :: Parser Mode
verifyFlag =
  flag' Verify
    (  long "verify"
    <> help (fold [ "Check if Son is valid."
                  , " Warning: any error messages are likely to be terrible."
                  , " `stdin SonWithNewline -> Either (stderr String) ()`"
                  ])
    )

verifyNoNewlineFlag :: Parser Mode
verifyNoNewlineFlag =
  flag' VerifyNoNewline
    (  long "verify-no-newline"
    <> help (fold [ "Check if Son is valid."
                  , " Warning: any error messages are likely to be terrible."
                  , " `stdin Son -> Either (stderr String) ()`"
                  ])
    )

sonFromJSON :: ByteString -> IO ByteString
sonFromJSON bts =
  case eitherDecodeStrict bts of
    Left e  -> panic (T.pack e)
    Right v -> pure (Son.encode (Son v))

isSon :: ByteString -> IO ()
isSon bts =
  case Son.decode bts of
    Left e  -> print bts >> panic (show e)
    Right _ -> pure ()

main :: IO ()
main = do
  mode <- execParser execInfo
  bts <- BS.getContents
  case mode of
    JSONToSon       -> sonFromJSON bts >>= BS.putStr
    Verify          -> stripNewline bts >>= isSon
    VerifyNoNewline -> isSon bts
  where
    stripNewline :: ByteString -> IO ByteString
    stripNewline bts =
      case BS.unsnoc bts of
        Nothing             -> panic "empty input"
        Just (noNewline, c) ->
          if BS.singleton c == "\n"
            then pure noNewline
            else panic ( "instead of ending with a newline, input ends with: "
                       <> show c
                       )
