{-# LANGUAGE ViewPatterns #-}
module Database.Persist.Audit.Operations (isDeleted, notDeleted,
  insert, delete, update, getOrNotFound, getNotDeletedOrNotFound, filterE) where

import           Control.Exception.StackError
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe                   (isJust)
import           Data.Time.Clock
import qualified Database.Persist.Audit.Class as A
import qualified Database.Persist.Class       as DB
import           Database.Persist.Schema
import           Database.Persist.Sql         ((=.), (==.))
import           Database.Persist.Types
import           Database.Persist.Types.UUID
import           Prelude                      hiding (error)
import           System.Random

genAuditKey :: SqlM (DB.Key Audit)
genAuditKey = liftIO $ AuditKey <$> (randomIO :: IO UUID)


notDeleted :: (A.Audit a) => Filter a
notDeleted = A.deleteField ==. Nothing


isDeleted :: A.Audit a => a -> Bool
isDeleted = isJust.(A.deleted)

-- TODO replace, and replace must also ensure not deleted
insert :: (A.Audit a) => DB.Key User -> a -> SqlM (DB.Key a)
insert userid val = if isDeleted val then error "can't insert deleted value" else do
    uuid <- liftIO (randomIO :: IO UUID)
    auditkey <- genAuditKey
    now <- liftIO getCurrentTime
    let valkey = A.toKey uuid
    DB.insertKey valkey val
    DB.insertKey auditkey $ Audit uuid userid now Nothing (Just $ toJSON val)
    return valkey

delete :: (A.Audit a) => DB.Key User -> DB.Key a -> SqlM ()
delete userid valkey = do
    auditkey <- genAuditKey
    now <- liftIO getCurrentTime
    current <- DB.get valkey
    case current of
        Just (isDeleted -> False) -> do
            DB.update valkey [A.deleteField =. Just now]
            DB.insertKey auditkey $ Audit (A.fromKey valkey) userid now (Just $ toJSON current) Nothing
        Just _ -> error $ "Key "++show valkey++" has already been deleted"
        Nothing -> error $ "Key "++show valkey++" does not exist"

update :: (A.Audit a) => DB.Key User -> DB.Key a ->
    [Update a] -> SqlM a
update userid valkey updates = do
    auditkey <- genAuditKey
    now <- liftIO getCurrentTime
    current <- DB.get valkey
    val <- DB.updateGet valkey updates
    DB.insertKey auditkey $ Audit (A.fromKey valkey) userid now
        (Just $ toJSON current) (Just $ toJSON val)
    return val

getOrNotFound :: A.Audit a => Key a -> SqlM (Entity a)
getOrNotFound objId = do
    item <- DB.get objId
    case item of
        Just item' -> return $ Entity objId item'
        Nothing    -> error $ "couldn't find " ++ show objId

getNotDeletedOrNotFound :: A.Audit a => Key a -> SqlM (Entity a)
getNotDeletedOrNotFound objId = do
    item <- getOrNotFound objId
    if isDeleted (entityVal item) then error $ "accessed deleted object " ++ show objId
      else return item

filterE :: (a -> Bool) -> [Entity a] -> [Entity a]
filterE f = filter (\(Entity _ v) -> f v)
