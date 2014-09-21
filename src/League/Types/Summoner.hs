module League.Types.Summoner where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Function (on)
import Data.Monoid
import Data.Text (Text)

newtype SummonerID = SummonerID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON SummonerID where
  parseJSON j = SummonerID <$> parseJSON j

newtype SummonerName = SummonerName Text
  deriving (Show, Read, Eq)

instance FromJSON SummonerName where
  parseJSON j = SummonerName <$> parseJSON j

data Summoner = Summoner { summonerSummonerID :: SummonerID
                         , summonerName :: SummonerName
                         , summonerProfileIconID :: Integer
                         , summonerLevel :: Integer }
  deriving (Show, Read, Eq)
makeFields ''Summoner

instance Ord Summoner where
  compare = compare `on` summonerSummonerID

instance FromJSON Summoner where
  parseJSON (Object o) =
    Summoner <$> o .: "id"
             <*> o .: "name"
             <*> o .: "profileIconId"
             <*> o .: "summonerLevel"
  parseJSON _ = mempty

