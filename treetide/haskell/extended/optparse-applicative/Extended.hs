{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Applicative.Extended
    ( module Options.Applicative
    , parseOptionsIO
    , programHelp
    , defineOption
    )
where

import           Data.ByteString                ( ByteString )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Options.Applicative
import           Options.Applicative.Builder.Internal
                                                ( HasName
                                                , HasValue
                                                )

-- | Shorthand for defining a program help and executing the option parser.
parseOptionsIO :: Text -> Parser a -> IO a
parseOptionsIO desc p = execParser (programHelp desc p)

-- | Shorthand for constructing a commonly used program help, parsing the given
-- options.
programHelp :: Text -> Parser a -> ParserInfo a
programHelp shortDesc args =
    info (helper <*> args) (fullDesc <> progDesc (T.unpack shortDesc))

-- | Types that can be turned into program options (with some restrictions).
class Optionable a where
    -- | Defines an option with default value. The type can be anything that
    -- supports Show and Read, as well as Text and Bool.
    --
    -- Bools have special support, as for a name "myflag" both "--myflag" and
    -- "--nomyflag" are accepted, to be able to turn it on or off, respectively.
    defineOption :: Text  -- ^ Long name.
                 -> a     -- ^ Default value.
                 -> Text  -- ^ Description.
                 -> Parser a

-- | Restricted to numeric types only. We should write instances for more
-- complicated types manually. But preferrably complicated stuff should not be
-- passed as options.
instance (Show a, Read a, Num a) => Optionable a where
    defineOption name defVal desc =
        option auto (defOptionableMods name (defVal + 0 {- to force constraint -}
                                                       ) desc show)

instance {-# OVERLAPPING #-} Optionable Text where
    defineOption name defVal desc =
        fmap T.pack . option str $ defOptionableMods name
                                                     (T.unpack defVal)
                                                     desc
                                                     id

-- | Defined in terms of the Text instance.
instance {-# OVERLAPPING #-} Optionable ByteString where
    defineOption name defVal desc =
        fmap T.encodeUtf8 $ defineOption name (T.decodeUtf8 defVal) desc

instance {-# OVERLAPPING #-} Optionable FilePath where
    defineOption name defVal desc =
        option str $ defOptionableMods name defVal desc id

instance {-# OVERLAPPING #-} Optionable Bool where
    defineOption name defVal desc =
        flag' True mods
            <|> flag' False (long (T.unpack ("no" <> name)) <> internal)
            <|> pure defVal
      where
        mods = long (T.unpack name)
            <> help (T.unpack desc <> " Default: " <> show defVal)

defOptionableMods
    :: (HasValue f, HasName f) => Text -> a -> Text -> (a -> String) -> Mod f a
defOptionableMods name defVal desc shower =
    long (T.unpack name)
        <> help (T.unpack desc)
        <> value defVal
        <> showDefaultWith shower
