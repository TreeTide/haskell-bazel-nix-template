{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.Parse.Extended
    ( matchingLocalName
    , ignoreTagTree
    , ignoreTagSequence
    , ignoreTagsExcept
    , ignoreAllRemainingTags
    , parseTag
    , parseTagContent
    , parseTagOnly
    , parseFoldState
    , manyYieldResult
    , many1
    -- * Reexport
    , module X
    )
where
import           Protolude

import           Conduit                        ( ConduitT
                                                , mapOutput
                                                , yield
                                                , MonadThrow
                                                )
import           Control.Monad                  ( join
                                                , void
                                                )
import           Data.Function                  ( on )
import           Data.Text                      ( Text
                                                , toCaseFold
                                                )
import           Data.XML.Types                 ( Event )
import qualified Text.XML                      as XML
                                                ( Name(..) )
import qualified Text.XML.Stream.Parse         as XML
import           Text.XML.Stream.Parse         as X

-- | Similar to 'manyYield' but disallows inner stream parsers to emit
-- anything.
manyYieldResult :: Monad m => ConduitT a Void m (Maybe b) -> ConduitT a b m ()
manyYieldResult consumer = fix $ \loop ->
    mapOutput (const (panic "Inner should not output anything")) consumer
        >>= maybe (return ()) (\x -> yield x >> loop)

-- | Like 'many', but returns Nothing if no value was matching.
many1
    :: Monad m => ConduitT Event o m (Maybe a) -> ConduitT Event o m (Maybe [a])
many1 c = do
    res <- XML.many c
    return $ if null res then Nothing else Just res

-- | Tag name matcher which ignores case and namespace
matchingLocalName :: Text -> XML.NameMatcher XML.Name
matchingLocalName localName = XML.matching isTagWithLocalName
  where
    isTagWithLocalName :: XML.Name -> Bool
    isTagWithLocalName = ((==) `on` toCaseFold) localName . XML.nameLocalName

-- | Skip tag with a given name. No-op if next tag to parse does not match.
ignoreTagTree :: MonadThrow m => Text -> ConduitT Event o m ()
ignoreTagTree tagName =
    void (XML.ignoreTreeContent $ matchingLocalName tagName)

-- | Skip tags in sequence. Only tags in given order are skipped.
ignoreTagSequence :: MonadThrow m => [Text] -> ConduitT Event o m ()
ignoreTagSequence =
    mapM_ $ XML.many_ . XML.ignoreTreeContent . matchingLocalName

-- | Skip tags which are not on the list.
ignoreTagsExcept :: MonadThrow m => [Text] -> ConduitT Event o m ()
ignoreTagsExcept tagsToKeep = XML.many_
    $ XML.ignoreTreeContent (XML.matching $ not . isTagToKeep)
  where
    isTagToKeep :: XML.Name -> Bool
    isTagToKeep t =
        (toCaseFold . XML.nameLocalName) t `elem` map toCaseFold tagsToKeep

-- | Skip any tag until end of stream.
ignoreAllRemainingTags :: MonadThrow m => ConduitT Event o m ()
ignoreAllRemainingTags = XML.many_ XML.ignoreAnyTreeContent

-- | Parse tag's subtree using a content parser.
parseTag
    :: MonadThrow m
    => Text
    -> ConduitT Event o m (Maybe a)
    -> ConduitT Event o m (Maybe a)
parseTag tagName contentParser =
    join <$> XML.tagIgnoreAttrs (matchingLocalName tagName) contentParser

-- | Parse tag's text contents.
parseTagContent :: MonadThrow m => Text -> ConduitT Event o m (Maybe Text)
parseTagContent tagName =
    XML.tagIgnoreAttrs (matchingLocalName tagName) XML.content

-- | Skips all tags until the given one, parses it, then skips all remaining
-- tags. Can be used to pick a specific tag from an already scoped subtree.
parseTagOnly
    :: MonadThrow m
    => Text
    -> ConduitT Event o m (Maybe a)
    -> ConduitT Event o m (Maybe a)
parseTagOnly tagName tagContentParser = do
    ignoreTagsExcept [tagName]
    t <- parseTag tagName tagContentParser
    ignoreAllRemainingTags
    return t

-- | Parses all remaining tags using one of the alternative parsers while
-- maintaining state.
--
-- The parsers are attempted in the given order for each tag.
-- Subtrees not matched by any of the alternative parsers are skipped.
--
-- Note: don't use a parser which itself skips tags, since that would prevent
-- the other alternatives from handling them. Unles that is what you really
-- want.
parseFoldState
    :: MonadThrow m
    => [s -> ConduitT Event o m (Maybe s)]
    -> s
    -> ConduitT Event o m (Maybe s)
parseFoldState parsers0 s0 = Just <$> go s0
  where
    parsers = parsers0 <> [parseFallback]
        where
          -- Swallows an unhandled tag.
          parseFallback s = fmap (fmap (const s)) XML.ignoreAnyTreeContent
    -- | Tries the first subparser that works (falling back to an accept-any
    -- parser), and loops while there's any tag to try.
    go s = do
        res <- XML.choose (map ($ s) parsers)
        case res of
            Nothing -> return s
            Just s1 -> go s1

