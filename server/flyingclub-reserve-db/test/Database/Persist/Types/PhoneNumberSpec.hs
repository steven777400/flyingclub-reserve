{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Types.PhoneNumberSpec where


import Data.Aeson
import Data.Text
import Database.Persist.Sql
import System.Random
import Web.PathPieces
import Web.HttpApiData


import           Database.Persist.Types.PhoneNumber
import           Test.Hspec


spec :: Spec
spec = do
    describe "phone number" $ do
        it "write and parse phone number to/from JSON" $ do
            let p = toPhoneNumber "3605551212"
            let (Success p') = (fromJSON (toJSON p)) :: Result PhoneNumber
            p' `shouldBe` p
        it "write and parse phone number to/from PersistField" $ do
            let p = toPhoneNumber "3605551212"
            let (Right p') = (fromPersistValue (toPersistValue p)) :: Either Text PhoneNumber
            p' `shouldBe` p
        it "accepts correct phone numbers" $ do
            let p1 = toPhoneNumber "(360) 555-1212"
            let p2 = toPhoneNumber "360-555-1212"
            let p3 = toPhoneNumber "360 555 1212"
            let p4 = toPhoneNumber "+13605551212"
            let p5 = toPhoneNumber "3605551212"
            p1 `shouldBe` p2
            p2 `shouldBe` p3
            p3 `shouldBe` p4
            p4 `shouldBe` p5
            let p6 = toPhoneNumber "3605541313"
            p6 `shouldNotBe` p5
            let p7 = toPhoneNumber "3615551212"
            p7 `shouldNotBe` p5
        it "rejects incorrect phone numbers" $ do
            (print $ toPhoneNumber "555-1212") `shouldThrow` anyException
            (print $ toPhoneNumber "+2 250-555-1212") `shouldThrow` anyException
            (print $ toPhoneNumber "360-ABC-1212") `shouldThrow` anyException
            
