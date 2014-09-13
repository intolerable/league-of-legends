module League.Actions.Champion where

import League.Types.Region
import League.Types.Champion
import League.Types.League

import Network.API.Builder
import qualified Data.Text as Text

getAllChampions :: League [Champion]
getAllChampions = do
  (_, r) <- leagueState
  ChampionList champions <- LeagueT $
    runRoute $ Route ["api", "lol", shortRegion r, "v1.2", "champion"]
                     [ ]
                     "GET"
  return champions

getChampion :: ChampionID -> League Champion
getChampion (ChampionID c) = do
  (_, r) <- leagueState
  LeagueT $ runRoute $
    Route ["api", "lol", shortRegion r, "v1.2", "champion", Text.pack $ show c ]
          [ ]
          "GET"
