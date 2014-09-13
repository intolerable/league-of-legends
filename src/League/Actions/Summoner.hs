module League.Actions.Summoner where

import League.Types.Error
import League.Types.Region
import League.Types.Summoner
import League.Types.League

import Control.Monad.Trans.Either (EitherT(..))
import Data.Map (Map)
import Data.Text (Text)
import Network.API.Builder
import qualified Data.Map as Map
import qualified Data.Text as Text

getSummonerByName :: SummonerName -> League Summoner
getSummonerByName n = do
  ss <- getSummonersByName [n]
  case ss of
    s : [] -> return s
    _ -> LeagueT $ EitherT $ return $ Left $ APIError InvalidResponseError

getSummonersByName :: [SummonerName] -> League [Summoner]
getSummonersByName ns = do
  let names = Text.intercalate "," $ map (\(SummonerName x) -> x) ns
  (_, r) <- leagueState
  summonerMap <- LeagueT $
    runRoute $ Route ["api", "lol", shortRegion r, "v1.4", "summoner", "by-name", names]
                     [ ]
                     "GET"
  return $ Map.elems (summonerMap :: Map Text Summoner)

getSummonerByID :: SummonerID -> League Summoner
getSummonerByID n = do
  ss <- getSummonersByID [n]
  case ss of
    s : [] -> return s
    _ -> LeagueT $ EitherT $ return $ Left $ APIError InvalidResponseError

getSummonersByID :: [SummonerID] -> League [Summoner]
getSummonersByID is = do
  let ids = Text.intercalate "," $ map (\(SummonerID i) -> Text.pack $ show i) is
  (_, r) <- leagueState
  summonerMap <- LeagueT $
    runRoute $ Route ["api", "lol", shortRegion r, "v1.4", "summoner", ids]
                     [ ]
                     "GET"
  return $ Map.elems (summonerMap :: Map Text Summoner)

