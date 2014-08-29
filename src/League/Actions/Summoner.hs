module League.Actions.Summoner where

import League.Types.Error
import League.Types.Region
import League.Types.Summoner
import League.Types.League

import APIBuilder
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Either (EitherT(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

getSummonerBySummonerName :: SummonerName -> League Summoner
getSummonerBySummonerName n = do
  ss <- getSummonersBySummonerName [n]
  case ss of
    s : [] -> return s
    _ -> LeagueT $ EitherT $ return $ Left $ APIError InvalidResponseError

getSummonersBySummonerName :: [SummonerName] -> League [Summoner]
getSummonersBySummonerName ns = do
  let names = Text.intercalate "," $ map (\(SummonerName x) -> x) ns
  (_, r) <- LeagueT $ liftState get
  summonerMap <- LeagueT $
    runRoute $ Route ["api", "lol", shortRegion r, "v1.4", "summoner", "by-name", names]
                     [ ]
                     "GET"
  return $ Map.elems (summonerMap :: Map Text Summoner)


