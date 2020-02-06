{-# Language OverloadedStrings #-}
module Data.Attoparsec.Text.Extended
    ( module X
    -- * Easier parsing
    , parseMaybe
    , parseMaybe'
    -- * Convenience
    , charIn
    -- * Delimiter combinators
    , followedByEoiOr
    , spaceDelimited
    , notAlphaNumDelimited
    )
where

import           Control.Monad                  ( guard )
import           Data.Attoparsec.Text          as X
import           Data.Char                      ( isAlphaNum, isSpace )
import           Data.Text                      ( Text )
import qualified Data.Vector.Unboxed           as V

-- | Peeks that the parser is either followed by a character satisfying given
-- condition, or end of input.
followedByEoiOr :: (Char -> Bool) -> Parser r -> Parser r
followedByEoiOr f parser = do
    r   <- parser
    mbC <- peekChar
    case mbC of
        Nothing -> return $! r
        Just c  -> guard (f c) >> (return $! r)

spaceDelimited :: Parser r -> Parser r
spaceDelimited = followedByEoiOr isSpace

-- | Useful to delimit entities ending in alphanum.
notAlphaNumDelimited :: Parser r -> Parser r
notAlphaNumDelimited = followedByEoiOr (not . isAlphaNum)

-- | Feeds given text to the parser and calls the continuation on parsing
-- success.
parseMaybe :: Parser r -> Text -> (Text -> r -> Maybe a) -> Maybe a
parseMaybe parser t k = go (parse parser t)
  where
    go res = case res of
        Fail _ _ _ -> Nothing
        Partial f ->
            go (f {- signal end of input -}
                  "")
        Done s r -> k s r

-- | Like 'parseMaybe' without ability to observe leftovers.
parseMaybe' :: Parser r -> Text -> Maybe r
parseMaybe' parser t = parseMaybe parser t $ \_ r -> Just $! r

-- | Monomorphised 'elem' to avoid ambiguous type signature complaint.
charIn :: [Char] -> Char -> Bool
charIn s = flip V.elem (V.fromList s)
