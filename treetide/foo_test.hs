{-# Language OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Hedgehog.Range
import           Test.Tasty
import           Test.Tasty.Hedgehog            ( testProperty )

import TreeTide.Foo (foo)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "foo"
    [ testProperty "parses only if positive" prop_parse_positive
    ]

prop_parse_positive :: Property
prop_parse_positive = property $ do
    n <- forAll (Gen.int (linear (-1000) 1000))
    let parsed = foo . T.pack . show $ n
    parsed === if n > 0 then Just n else Nothing

