{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Database.Persist.Audit.Class where

import           Data.Aeson
import           Data.Time.Clock
import qualified Database.Persist.Class      as DB
import           Database.Persist.Sql
import           Database.Persist.Types.UUID

class (ToJSON a, DB.PersistEntity a, DB.PersistEntityBackend a ~ SqlBackend) =>
    Audit a where
    toKey :: UUID -> DB.Key a
    fromKey :: DB.Key a -> UUID
    deleteField :: DB.EntityField a (Maybe UTCTime)
    deleted :: a -> Maybe UTCTime
