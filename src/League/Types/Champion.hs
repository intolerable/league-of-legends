module League.Types.Champion where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Monoid

newtype ChampionID = ChampionID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON ChampionID where
  parseJSON j = ChampionID <$> parseJSON j

data Champion = Champion { championChampionID :: ChampionID
                         , championActive :: Bool
                         , championRankedPlayEnabled :: Bool
                         , championBotEnabled :: Bool
                         , championBotMatchMakingEnabled :: Bool
                         , championFreeToPlay :: Bool }
  deriving (Show, Read, Eq)
makeFields ''Champion

instance FromJSON Champion where
  parseJSON (Object o) =
    Champion <$> o .: "id"
             <*> o .: "active"
             <*> o .: "rankedPlayEnabled"
             <*> o .: "botEnabled"
             <*> o .: "botMmEnabled"
             <*> o .: "freeToPlay"
  parseJSON _ = mempty

data ChampionList = ChampionList [Champion]
  deriving (Show, Read, Eq)

instance FromJSON ChampionList where
  parseJSON (Object o) =
    ChampionList <$> o .: "champions"
  parseJSON _ = mempty
