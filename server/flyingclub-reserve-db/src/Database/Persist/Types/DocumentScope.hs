{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric              #-}
module Database.Persist.Types.DocumentScope where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

data DocumentScope = Members | Officers | Individual
    deriving (Show, Read, Eq, Generic) -- must have Show+Read for derivePersistField
    
derivePersistField "DocumentScope"

instance ToJSON DocumentScope where
instance FromJSON DocumentScope where

