{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Types.JSONSpec where

import           Data.Aeson
import           Data.Text
import           Database.Persist.Sql
import           GHC.Generics

import           Database.Persist.Types.JSON ()
import           Test.Hspec

data Person = Person {
      name :: Text
    , age  :: Int
    , nums :: [Int]
    } deriving (Generic, Show, Eq)
instance ToJSON Person where
instance FromJSON Person where


spec :: Spec
spec = do
    describe "json" $ do
        it "write and parse JSON to/from PersistField" $ do
            let p = toJSON $ Person "test name" 35 [1,3,55]
            let (Right p') = (fromPersistValue (toPersistValue p)) :: Either Text Value
            p' `shouldBe` p
