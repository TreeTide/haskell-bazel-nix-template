module TreeTide.Foo (foo) where

import Data.Text (Text)

import qualified Data.Attoparsec.Text.Extended as A

foo :: Text -> Maybe Int
foo = A.parseMaybe' A.decimal

