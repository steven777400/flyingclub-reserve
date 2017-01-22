module Database.Persist.Types.UUIDSpec where

import Data.Aeson
import Data.Text
import Database.Persist.Sql
import System.Random
import Web.PathPieces
import Web.HttpApiData

import Test.Hspec
import Database.Persist.Types.UUID



spec :: Spec
spec = do
    describe "uuid" $ do
        it "write and parse UUID to/from JSON" $ do
            uuid <- (randomIO :: IO UUID)            
            let (Success uuid') = (fromJSON (toJSON uuid)) :: Result UUID
            uuid' `shouldBe` uuid
        it "write and parse UUID to/from PathPiece" $ do
            uuid <- (randomIO :: IO UUID)            
            let (Just uuid') = (fromPathPiece (toPathPiece uuid)) :: Maybe UUID
            uuid' `shouldBe` uuid
        it "write and parse UUID to/from HttpApiData" $ do
            uuid <- (randomIO :: IO UUID)            
            let (Right uuid') = (parseUrlPiece (toUrlPiece uuid)) :: Either Text UUID
            uuid' `shouldBe` uuid
        it "write and parse UUID to/from PersistField" $ do
            uuid <- (randomIO :: IO UUID)            
            let (Right uuid') = (fromPersistValue (toPersistValue uuid)) :: Either Text UUID
            uuid' `shouldBe` uuid

