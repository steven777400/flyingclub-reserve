module Database.Persist.Owner.Operations where

import           Database.Persist
import           Database.Persist.Owner.Class
import           Database.Persist.Owner.Instances ()
import           Database.Persist.Schema


ownedBy :: (Owner a) => Key User -> Filter a
ownedBy userId = ownerField ==. userId
