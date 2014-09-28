module League.Internal.TH.Shared where

import Control.Lens

data Shared = Shared
  { sharedName :: ()
  , sharedMatchID :: ()
  , sharedRegion :: ()
  , sharedCreationTime :: ()
  , sharedDuration :: ()
  , sharedMode :: ()
  , sharedSeason :: ()
  , sharedVersion :: () }
  deriving (Show, Read, Eq)

makeFields ''Shared
