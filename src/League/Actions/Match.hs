module League.Actions.Match where

import League.Types.Region
import League.Types.Match
import League.Types.League

import Network.API.Builder
import qualified Data.Text as Text

getMatch :: MatchID -> League MatchDetail
getMatch (MatchID m) = do
  (_, r) <- leagueState
  LeagueT $ runRoute $
    Route ["api", "lol", shortRegion r, "v2.2", "match", Text.pack $ show m]
          [ ]
          "GET"
