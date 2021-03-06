module League.Types.Region where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

data Region = BR
            | EUNE
            | EUW
            | KR
            | LAS
            | LAN
            | NA
            | OCE
            | TR
            | RU
            | Global
  deriving (Show, Read, Eq)

instance FromJSON Region where
  parseJSON (String s) =
    case s of
      "BR" -> return BR
      "EUNE" -> return EUNE
      "EUW" -> return EUW
      "KR" -> return KR
      "LAS" -> return LAS
      "LAN" -> return LAN
      "NA" -> return NA
      "OCE" -> return OCE
      "TR" -> return TR
      "RU" -> return RU
      "Global" -> return Global
      _ -> mempty
  parseJSON _ = mempty

regionalEndpoint :: Region -> Text
regionalEndpoint BR = "https://br.api.pvp.net"
regionalEndpoint EUNE = "https://eune.api.pvp.net"
regionalEndpoint EUW = "https://euw.api.pvp.net"
regionalEndpoint KR = "https://kr.api.pvp.net"
regionalEndpoint LAS = "https://las.api.pvp.net"
regionalEndpoint LAN = "https://lan.api.pvp.net"
regionalEndpoint NA = "https://na.api.pvp.net"
regionalEndpoint OCE = "https://oce.api.pvp.net"
regionalEndpoint TR = "https://tr.api.pvp.net"
regionalEndpoint RU = "https://ru.api.pvp.net"
regionalEndpoint Global = "https://global.api.pvp.net"

shortRegion :: Region -> Text
shortRegion BR = "br"
shortRegion EUNE = "eune"
shortRegion EUW = "euw"
shortRegion KR = "kr"
shortRegion LAS = "las"
shortRegion LAN = "lan"
shortRegion NA = "na"
shortRegion OCE = "oce"
shortRegion TR = "tr"
shortRegion RU = "ru"
shortRegion Global = "global"
