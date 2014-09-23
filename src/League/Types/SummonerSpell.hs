module League.Types.SummonerSpell where

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

newtype SummonerSpellID = SummonerSpellID Integer
  deriving (Read, Show, Eq, Ord)

instance FromJSON SummonerSpellID where
  parseJSON j = SummonerSpellID <$> parseJSON j

data SummonerSpell = SummonerSpell
  { summonerspellSummonerSpellID :: SummonerSpellID
  , summonerspellName :: Text
  , summonerspellKey :: Text
  , summonerspellDescription :: Text }
  deriving (Show, Read, Eq)

instance FromJSON SummonerSpell where
  parseJSON (Object o) =
    SummonerSpell <$> o .: "id"
                  <*> o .: "name"
                  <*> o .: "key"
                  <*> o .: "description"
  parseJSON _ = mempty
