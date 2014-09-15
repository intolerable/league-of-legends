module League.Types.Match where

import League.Types.Constants
import League.Types.Region
import League.Types.Version

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

data MatchDetail = MatchDetail
  { _match_detailMatchID :: MatchID
  , _match_detailRegion :: Region
  , _match_detailCreationTime :: Integer
  , _match_detailDuration :: Integer
  , _match_detailMode :: MatchMode
  , _match_detailSeason :: Season
  , _match_detailVersion :: Version }
  deriving (Show, Read, Eq)

instance Ord MatchDetail where
  compare = compare `on` _match_detailMatchID

instance FromJSON MatchDetail where
  parseJSON (Object o) =
    MatchDetail <$> o .: "matchId"
                <*> o .: "region"
                <*> o .: "matchCreation"
                <*> o .: "matchDuration"
                <*> o .: "matchMode"
                <*> o .: "season"
                <*> o .: "matchVersion"
  parseJSON _ = mempty

data MatchSummary = MatchSummary
  { _match_summaryMatchID :: MatchID
  , _match_summaryRegion :: Region
  , _match_summaryCreationTime :: Integer
  , _match_summaryDuration :: Integer
  , _match_summaryMode :: MatchMode
  , _match_summarySeason :: Season
  , _match_summaryVersion :: Version }
  deriving (Show, Read, Eq)

instance Ord MatchSummary where
  compare = compare `on` _match_summaryMatchID

instance FromJSON MatchSummary where
  parseJSON (Object o) =
    MatchSummary <$> o .: "matchId"
                 <*> o .: "region"
                 <*> o .: "matchCreation"
                 <*> o .: "matchDuration"
                 <*> o .: "matchMode"
                 <*> o .: "season"
                 <*> o .: "matchVersion"
  parseJSON _ = mempty

data MatchSummaries = MatchSummaries [MatchSummary]
  deriving (Show, Read, Eq)

instance FromJSON MatchSummaries where
  parseJSON (Object o) = MatchSummaries <$> o .: "matches"
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

makeFields ''MatchDetail
makeFields ''MatchSummary
