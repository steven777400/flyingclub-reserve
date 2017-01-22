{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Database.Persist.Owner.Class where

import           Data.Aeson
import qualified Database.Persist.Class  as DB
import           Database.Persist.Schema
import           Database.Persist.Sql

class (ToJSON a, DB.PersistEntity a, DB.PersistEntityBackend a ~ SqlBackend) =>
    Owner a where
    owner :: a -> DB.Key User
    ownerField :: DB.EntityField a (DB.Key User)
