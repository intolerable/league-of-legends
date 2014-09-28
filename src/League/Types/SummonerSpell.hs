module League.Types.SummonerSpell where

import League.Internal.TH.Shared

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

newtype SummonerSpellID = SummonerSpellID Integer
  deriving (Read, Show, Eq, Ord)

instance FromJSON SummonerSpellID where
  parseJSON j = SummonerSpellID <$> parseJSON j

data SummonerSpell = SummonerSpell
  { summonerSpellSummonerSpellID :: SummonerSpellID
  , summonerSpellName :: Text
  , summonerSpellKey :: Text
  , summonerSpellDescription :: Text }
  deriving (Show, Read, Eq)

makeFields ''SummonerSpell

instance FromJSON SummonerSpell where
  parseJSON (Object o) =
    SummonerSpell <$> o .: "id"
                  <*> o .: "name"
                  <*> o .: "key"
                  <*> o .: "description"
  parseJSON _ = mempty
