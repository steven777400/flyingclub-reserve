{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Types.PINSpec where


import Data.Aeson
import Data.Text
import Database.Persist.Sql
import System.Random
import Web.PathPieces
import Web.HttpApiData


import           Database.Persist.Types.PIN
import           Test.Hspec

spec :: Spec
spec = do
    describe "pin" $ do
        it "parse pin from JSON" $ do            
            let (Success p') = (fromJSON (String "12345")) :: Result PIN
            (verifyPIN "12345" p') `shouldBe` True        
            (verifyPIN "1234" p') `shouldBe` False
            
        it "accepts correct pin" $ do
            let p1 = toPIN "12345"            
            let p3 = toPIN "09876"
            let p4 = toPIN "9876"            
            (verifyPIN "12345" p1) `shouldBe` True
            (verifyPIN "1234" p1) `shouldBe` False
            (verifyPIN "123456" p1) `shouldBe` False
            (verifyPIN "9876" p3) `shouldBe` False
            (verifyPIN "09876" p3) `shouldBe` True
            (verifyPIN "9876" p4) `shouldBe` True
            (verifyPIN "09876" p4) `shouldBe` False    
            
        it "rejects incorrect pins" $ do
            (print $ toPersistValue $ toPIN "") `shouldThrow` anyException
            (print $ toPersistValue $ toPIN "12") `shouldThrow` anyException
            (print $ toPersistValue $ toPIN "12a34") `shouldThrow` anyException
            
