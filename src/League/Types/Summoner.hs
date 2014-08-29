module League.Types.Summoner where

import APIBuilder.Query
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

data Summoner = Summoner { _summonerSummonerID :: SummonerID
                         , _summonerName :: SummonerName
                         , _summonerProfileIconID :: Integer
                         , _summonerLevel :: Integer }
  deriving (Show, Read, Eq)
makeFields ''Summoner

instance Ord Summoner where
  compare = compare `on` _summonerSummonerID

instance FromJSON Summoner where
  parseJSON (Object o) =
    Summoner <$> o .: "id"
             <*> o .: "name"
             <*> o .: "profileIconId"
             <*> o .: "summonerLevel"
  parseJSON _ = mempty

