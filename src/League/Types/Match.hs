module League.Types.Match where

import League.Internal.TH.Shared
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
  { matchDetailMatchID :: MatchID
  , matchDetailRegion :: Region
  , matchDetailCreationTime :: Integer
  , matchDetailDuration :: Integer
  , matchDetailMode :: MatchMode
  , matchDetailSeason :: Season
  , matchDetailVersion :: Version }
  deriving (Show, Read, Eq)

instance Ord MatchDetail where
  compare = compare `on` matchDetailMatchID

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
  { matchSummaryMatchID :: MatchID
  , matchSummaryRegion :: Region
  , matchSummaryCreationTime :: Integer
  , matchSummaryDuration :: Integer
  , matchSummaryMode :: MatchMode
  , matchSummarySeason :: Season
  , matchSummaryVersion :: Version }
  deriving (Show, Read, Eq)

instance Ord MatchSummary where
  compare = compare `on` matchSummaryMatchID

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
