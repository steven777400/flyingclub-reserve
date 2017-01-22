{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric              #-}
module Database.Persist.Types.UserType where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

data UserType = NoAccess | Social | Pilot | Officer
    deriving (Show, Read, Eq, Ord, Generic) -- must have Show+Read for derivePersistField
    
derivePersistField "UserType"

instance ToJSON UserType where
instance FromJSON UserType where

