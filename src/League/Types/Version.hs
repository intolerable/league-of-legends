module League.Types.Version
  ( Version(..)
  , VersionString(..)
  , version ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text, splitOn, unpack)
import Text.Read (readMaybe)
import qualified Data.Version as Version

data VersionString = VersionString Text
  deriving (Show, Read, Eq)

instance FromJSON VersionString where
  parseJSON j = VersionString <$> parseJSON j

data Version = Version [Int]
  deriving (Show, Read, Eq)

version :: Iso' Version Version.Version
version = iso (\(Version v) -> Version.Version v []) (\(Version.Version v _) -> Version v)

instance FromJSON Version where
  parseJSON (String s) =
    maybe mempty (return . Version) $
      mapM (readMaybe . unpack) (splitOn "." s)
  parseJSON _ = mempty
