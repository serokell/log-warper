{-# LANGUAGE UndecidableInstances #-}

-- | Contains @newtype@ wrapper 'LoggerName' for list of components for logger name.

module Log.Name
       ( LoggerName (..)
       ) where

import Universum

import Data.Aeson.Types (ToJSON, ToJSONKey (..))
import Fmt (build, fmt)
import GHC.Exts (IsList (..))
import Text.Show (Show (show))

import qualified Data.Text as Text

{- | Logger name is a unique label for some 'LogAction'. Loggers in @log-warper@
form tree. Every 'LoggerName' contains some list of components. In textual
representation those commponents separated by dot character. Below you can see
example of correspondence between list of components and string:

@
["foor", "bar", "baz"] <-> "foo.bar.baz"
@
-}
newtype LoggerName = LoggerName
    { unLoggerName :: Vector Text
    } deriving (Eq, Ord, Generic, Semigroup, Monoid, Container)

instance Hashable LoggerName where
    hashWithSalt = foldl' hashWithSalt

-- | This instance is mostly for convenient way to create 'LoggerName' from list.
instance IsList LoggerName where
    type Item LoggerName = Text
    fromList = LoggerName . fromList
    toList   = Universum.toList

{- | Split a dot-separated string. Empty string turns into a 'LoggerName' with
zero components.
-}
instance IsString LoggerName where
    fromString :: String -> LoggerName
    fromString s  = fromList $ case s of
        "" -> []
        s' -> Text.splitOn "." (fromString s')

instance Buildable LoggerName where
    build = mconcat . intersperse "." . Universum.toList . map smartBuild . unLoggerName
      where
        smartBuild "" = "<empty>"
        smartBuild s  = build s

instance Show LoggerName where
    show = fmt . build

instance ToJSON    LoggerName
instance ToJSONKey LoggerName
