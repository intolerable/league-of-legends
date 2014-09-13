module League.Types.Constants
  ( Map(..)
  , MatchMode(..) ) where

import Data.Aeson
import Data.Monoid

data Map = SummonersRift
         | SummonersRiftAutumn
         | ProvingGrounds
         | TwistedTreelineClassic
         | CrystalScar
         | TwistedTreeline
         | HowlingAbyss
  deriving (Show, Read, Eq)

instance Enum Map where
  toEnum = toMapID
  fromEnum = fromMapID

toMapID :: Int -> Map
toMapID 1 = SummonersRift
toMapID 2 = SummonersRiftAutumn
toMapID 3 = ProvingGrounds
toMapID 4 = TwistedTreelineClassic
toMapID 8 = CrystalScar
toMapID 10 = TwistedTreeline
toMapID 12 = HowlingAbyss
toMapID _ = undefined

fromMapID :: Map -> Int
fromMapID SummonersRift = 1
fromMapID SummonersRiftAutumn = 2
fromMapID ProvingGrounds = 3
fromMapID TwistedTreelineClassic = 4
fromMapID CrystalScar = 8
fromMapID TwistedTreeline = 10
fromMapID HowlingAbyss = 12

data MatchMode = Classic
               | Odin
               | ARAM
               | Tutorial
               | OneForAll
               | Ascension
               | FirstBlood
  deriving (Show, Read, Eq)

instance FromJSON MatchMode where
  parseJSON (String s) =
    case s of
      "CLASSIC" -> return Classic
      "ODIN" -> return Odin
      "ARAM" -> return ARAM
      "TUTORIAL" -> return Tutorial
      "ONEFORALL" -> return OneForAll
      "ASCENSION" -> return Ascension
      "FIRSTBLOOD" -> return FirstBlood
      _ -> mempty
  parseJSON _ = mempty
