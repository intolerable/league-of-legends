module League.Actions.Champion where

import League.Types.Error
import League.Types.Region
import League.Types.Champion
import League.Types.League

import APIBuilder
import Control.Monad.Trans.State (get)
import qualified Data.Text as Text

getAllChampions :: League [Champion]
getAllChampions = do
  (_, r) <- LeagueT $ liftState get
  ChampionList champions <- LeagueT $
    runRoute $ Route ["api", "lol", shortRegion r, "v1.2", "champion"]
                     [ ]
                     "GET"
  return champions

getChampion :: ChampionID -> League Champion
getChampion (ChampionID c) = do
  (_, r) <- LeagueT $ liftState get
  LeagueT $ runRoute $
    Route ["api", "lol", shortRegion r, "v1.2", "champion", Text.pack $ show c ]
          [ ]
          "GET"
