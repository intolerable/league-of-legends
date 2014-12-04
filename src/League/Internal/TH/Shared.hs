module League.Internal.TH.Shared where

import Control.Lens.TH.SharedFields

generateFields [ "Name"
               , "MatchID"
               , "Region"
               , "CreationTime"
               , "Duration"
               , "Mode"
               , "Season"
               , "Version" ]
