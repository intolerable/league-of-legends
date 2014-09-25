module League.Actions.StaticData
  ( getSummonerSpell
  , getSummonerSpells ) where

import League.Types.League
import League.Types.Region
import League.Types.SummonerSpell

import Control.Applicative
import Data.Aeson
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import qualified Data.Map as Map
import qualified Data.Text as Text

newtype StaticDataList a = StaticDataList [a]
  deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (StaticDataList a) where
  parseJSON (Object o) =
    StaticDataList <$> o .: "data"
  parseJSON _ = mempty

getSummonerSpell :: SummonerSpellID -> League SummonerSpell
getSummonerSpell (SummonerSpellID i) = do
  (_, r) <- leagueState
  LeagueT $ runRoute $
    Route ["api", "lol", "static-data", shortRegion r, "v1.2", "summmoner-spell", Text.pack $ show i]
          [ ]
          "GET"

getSummonerSpells :: League [SummonerSpell]
getSummonerSpells = do
  (_, r) <- leagueState
  ssMap <- LeagueT $ runRoute $
    Route ["api", "lol", "static-data", shortRegion r, "v1.2", "summoner-spell"]
          [ ]
          "GET"
  return $ Map.elems (ssMap :: Map Text SummonerSpell)

