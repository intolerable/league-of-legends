module League.Types.SummonerSpec where

import League.Types.Summoner

import Control.Applicative
import Control.Lens
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SummonerID" $ do
    it "has a working FromJSON instance" $ do
      let decode' = decode :: ByteString -> Either (APIError ()) SummonerID
      decode' "5" `shouldBe` Right (SummonerID 5)

  describe "SummonerName" $ do
    it "has a working FromJSON instance" $ do
      let decode' = decode :: ByteString -> Either (APIError ()) SummonerName
      decode' "\"XxXintolerableXx\"" `shouldBe` Right (SummonerName "XxXintolerableXx")

  describe "Summoner" $ do
    getSummonerByNameExample <- runIO $ ByteString.readFile "test/data/getSummonerByName_example.json"

    it "can read the example" $ do
      getSummonerByNameExample `shouldSatisfy` not . ByteString.null

    it "has a working FromJSON instance" $ do
      let decoded = decode getSummonerByNameExample :: Either (APIError ()) (Map Text Summoner)

      case Map.elems <$> decoded of
        Left _ -> expectationFailure "json parse failed"
        Right (summoner:[]) -> do
          summoner ^. summonerID `shouldBe` SummonerID 27016110
        Right _ -> expectationFailure "json parse failed"
