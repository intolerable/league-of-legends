module League.Types.ChampionSpec where

import League.Types.Champion

import Control.Lens
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ChampionID" $ do
    it "has a working FromJSON instance" $ do
      let decode' = decode :: ByteString -> Either (APIError ()) ChampionID
      decode' "5" `shouldBe` Right (ChampionID 5)

  describe "Champion" $ do
    getChampionExample <- runIO $ ByteString.readFile "test/data/getChampion_example.json"

    it "can read the example" $ do
      getChampionExample `shouldSatisfy` not . ByteString.null

    it "has a working FromJSON instance" $ do
      let decoded = decode getChampionExample :: Either (APIError ()) Champion

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right champ -> do
          champ ^. championID `shouldBe` ChampionID 42
          champ ^. active `shouldBe` True
          champ ^. freeToPlay `shouldBe` False
          champ ^. rankedPlayEnabled `shouldBe` True
          champ ^. botEnabled `shouldBe` False
          champ ^. botMatchMakingEnabled `shouldBe` False

  describe "ChampionList" $ do
    getAllChampionsExample <- runIO $ ByteString.readFile "test/data/getAllChampions_example.json"

    it "can read the example" $ do
      getAllChampionsExample `shouldSatisfy` not . ByteString.null

    it "has a working FromJSON instance" $ do
      let decoded = decode getAllChampionsExample :: Either (APIError ()) ChampionList

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right (ChampionList cs) -> do
          length cs `shouldBe` 121
