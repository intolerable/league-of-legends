module League.Types.League
  ( APIKey(..)
  , League
  , LeagueT(..)
  , run
  , leagueState ) where

import League.Types.Region
import League.Types.Error

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State (get)
import Data.Text (Text)
import Network.API.Builder

newtype APIKey = APIKey Text
  deriving (Show, Read, Eq)

type League a = LeagueT IO a

newtype LeagueT m a = LeagueT { unLeagueT :: APIT (APIKey, Region) LeagueError m a }
  deriving (Functor, Applicative, Monad, MonadIO)

builder :: Region -> Builder
builder = basicBuilder "League of Legends API" . regionalEndpoint

run :: MonadIO m => APIKey -> Region -> LeagueT m a -> m (Either (APIError LeagueError) a)
run key region (LeagueT act) =
  execAPI (builder region) (key, region) $ do
    customizeRoute $ addAPIKey key
    act

addAPIKey :: APIKey -> Route -> Route
addAPIKey (APIKey key) (Route fs ps m) =
  Route fs ("api_key" =. key : ps) m

leagueState :: League (APIKey, Region)
leagueState = LeagueT $ liftState get
