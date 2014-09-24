module League.Actions.StaticData where

import League.Types.League
import League.Types.Region
import League.Types.SummonerSpell

import Network.API.Builder
import qualified Data.Text as Text

getSummonerSpell :: SummonerSpellID -> League SummonerSpell
getSummonerSpell (SummonerSpellID i) = do
  (_, r) <- leagueState
  LeagueT $ runRoute $
    Route ["api", "lol", "static-data", shortRegion r, "v1.2", "summmoner-spell", Text.pack $ show i]
          [ ]
          "GET"
