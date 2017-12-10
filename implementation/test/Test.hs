
module Main where

import           Protolude

import           Data.Aeson
import           Data.List (unlines)
import           Data.String (String)
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           JQ
import           Son (Son(..))
import qualified Son

main :: IO ()
main = defaultMain $ testGroup "Son"
  [ testProperty "is always valid JSON" isJSON
  , testProperty "decodes to the same Aeson Value" isSameJSON
  , testProperty "roundtrips through JSON without changing" roundtrip
  , testCase "properties must be ordered" orderedProperties
  , testGroup "numbers are serialized correctly"
    [ testCase "0" (Son.generate (Son (Number 0)) @?= "0")
    , testCase "1" (Son.generate (Son (Number 1)) @?= "1")
    ]
  -- , testProperty "outputs the same JSON as jq with certain arguments" jqTest
  ]

isJSON :: Son -> Property
isJSON a =
  let b = Son.generate a
      c = eitherDecodeStrict (encodeUtf8 b) :: Either String Value
      s = unlines [ "serialized to: " <> T.unpack b
                  , "parsed to: " <> show c
                  ]
  in counterexample s (isRight c)

-- | This won't hold for every JSON library. For instance if a
-- library parses `1.0` and `1` to different values it won't pass.
-- I don't think Aeson encodes any info in ways that are squashed
-- by Son serializaton though.
isSameJSON :: Son -> Property
isSameJSON a =
  let b = Son.generate a
      c = eitherDecodeStrict (encodeUtf8 b)
      s = unlines [ "serialized to: " <> T.unpack b
                  , "parsed to: " <> show c
                  ]
  in counterexample s (Right (_unSon a) == c)

roundtrip :: Son -> Property
roundtrip a =
  let b = Son.generate a
      c = Son.decode (encodeUtf8 b)
      s = unlines [ "serialized to: " <> T.unpack b
                  , "parsed to: " <> show c
                  ]
  in counterexample s (Right a == c)

jqTest :: Son -> Property
jqTest a = monadicIO $ do
  let b = Son.generate a
  c <- run $ encodeJQ (_unSon a)
  let s = unlines [ "serialized to Son: " <> T.unpack b
                  , "serialized with jq: " <> c
                  ]
  -- The following is useful if QuickCheck produces a huge counterexample:
  --
  -- when (b <> T.singleton '\n' /= T.pack c) $ do
  --   run (writeFile "first" (b <> "\n"))
  --   run (writeFile "second" (T.pack c))
  monitor (counterexample s)
  assert (b <> T.singleton '\n' == T.pack c)

orderedProperties :: Assertion
orderedProperties =  isLeft (Son.parse "{\"b\":null,\"a\":null}") @?= True
