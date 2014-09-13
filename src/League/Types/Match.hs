module League.Types.Match where

import League.Types.Constants
import League.Types.Region

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Function (on)
import Data.Monoid
import Data.Text (Text)

newtype MatchID = MatchID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON MatchID where
  parseJSON j = MatchID <$> parseJSON j

data Match = Match { _matchMatchID :: MatchID
                   , _matchRegion :: Region
                   , _matchCreationTime :: Integer
                   , _matchDuration :: Integer
                   , _matchMode :: MatchMode
                   , _matchSeason :: Season }
  deriving (Show, Read, Eq)

instance Ord Match where
  compare = compare `on` _matchMatchID

instance FromJSON Match where
  parseJSON (Object o) =
    Match <$> o .: "matchId"
          <*> o .: "region"
          <*> o .: "matchCreation"
          <*> o .: "matchDuration"
          <*> o .: "matchMode"
          <*> o .: "season"
  parseJSON _ = mempty

data Season = PreSeason3
            | Season3
            | PreSeason4
            | Season4
            | UnknownSeason Text
  deriving (Show, Read, Eq)

instance FromJSON Season where
  parseJSON (String s) =
    return $ case s of
      "PRESEASON3" -> PreSeason3
      "SEASON3" -> Season3
      "PRESEASON2014" -> PreSeason4
      "SEASON2014" -> Season4
      x -> UnknownSeason x
  parseJSON _ = mempty

makeFields ''Match
