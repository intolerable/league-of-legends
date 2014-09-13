module League.Types.Error where

import Data.Aeson
import Data.Monoid

data LeagueError = InvalidResponseError
  deriving (Show, Read, Eq)

instance FromJSON LeagueError where
  parseJSON = const mempty
