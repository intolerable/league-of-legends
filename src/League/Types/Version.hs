module League.Types.Version
  ( Version(..)
  , VersionString(..) ) where

import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text, splitOn, unpack)
import Text.Read (readMaybe)

data VersionString = VersionString Text
  deriving (Show, Read, Eq)

instance FromJSON VersionString where
  parseJSON j = VersionString <$> parseJSON j

data Version = Version [Int]
  deriving (Show, Read, Eq)

instance FromJSON Version where
  parseJSON (String s) =
    maybe mempty (return . Version) $
      mapM (readMaybe . unpack) (splitOn "." s)
  parseJSON _ = mempty
