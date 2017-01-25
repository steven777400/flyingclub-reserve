{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Persist.Schema where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Text                          as T
import           Data.Time.Clock
import qualified Database.Persist.Audit.Class       as A
import           Database.Persist.Sql
import           Database.Persist.TH
import qualified Database.Persist.Types.JSON        as JSON
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           GHC.Generics


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    Id          UUID
    firstname   String
    lastname    String
    title       String
    permission  UserType
    deleted     UTCTime Maybe
    deriving Show Generic
Address
    Id          UUID
    userId      UserId
    address     String
    city        String
    state       String
    zip         String
    showInDirectory  Bool
    deleted     UTCTime Maybe
    deriving Show Generic
Email
    Id          UUID
    userId      UserId
    description String
    address     String
    receiveNotification  Bool
    showInDirectory  Bool
    deleted     UTCTime Maybe
    deriving Show Generic
Phone
    Id          UUID
    userId      UserId
    description String
    number      PhoneNumber
    receiveSms  Bool
    showInDirectory  Bool
    deleted     UTCTime Maybe
    deriving Show Generic
Authentication
    Id          UUID
    userId      UserId
    AuthUserId  userId
    failedLoginCount  Int
    pin         PIN
Airplane
    Id          UUID
    tail        String
    description String
    deleted     UTCTime Maybe
    deriving Show Generic
Reservation
    Id          UUID
    userId      UserId
    airplaneId  AirplaneId
    start       UTCTime
    end         UTCTime
    deleted     UTCTime Maybe
    maintenance Bool
    comment     String
    deriving Show Generic
Notification
    Id          UUID
    userId      UserId
    posted      UTCTime
    sent        UTCTime Maybe
    content     String
Session
    Id          UUID
    authKey     UUID
    userId      UserId
    created     UTCTime
    used        UTCTime
    expired     UTCTime Maybe
    deriving Show Generic
SecurityAudit
    Id          UUID
    sessionId   SessionId Maybe
    kind        String
    username    String
    originHost  String
    when        UTCTime
    deriving Show Generic
Audit
    Id          UUID
    objectId    UUID    -- generic foreign key to any table
    userId      UserId
    when        UTCTime
    original    JSON.Value Maybe
    new         JSON.Value Maybe
    deriving Show Generic
|]

-- TODO BEGIN MOVE
type SqlM o = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) o

-- Non-int keys don't work with migration and foreign key
-- See https://github.com/yesodweb/persistent/issues/455
getAdjustedMigration :: SqlM [Sql]
getAdjustedMigration = do
    sqls <- getMigration migrateAll
    return $ map
        ((T.replace "INTEGER NOT NULL REFERENCES" "CHAR(36) NOT NULL REFERENCES").
        (T.replace "BIGINT NOT NULL REFERENCES" "CHAR(36) NOT NULL REFERENCES"))
        sqls

runAdjustedMigration :: SqlM ()
runAdjustedMigration = getAdjustedMigration >>= mapM_ (flip rawExecute [])


-- END BEGIN MOVE


instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity key val) = toJSON val

instance ToJSON User where
instance FromJSON User where
instance A.Audit User where
    toKey = UserKey
    fromKey = unUserKey
    deleteField = UserDeleted
    deleted = userDeleted


instance ToJSON Phone where
instance FromJSON Phone where
instance A.Audit Phone where
    toKey = PhoneKey
    fromKey = unPhoneKey
    deleteField = PhoneDeleted
    deleted = phoneDeleted


instance ToJSON Email where
instance FromJSON Email where
instance A.Audit Email where
    toKey = EmailKey
    fromKey = unEmailKey
    deleteField = EmailDeleted
    deleted = emailDeleted

instance ToJSON Address where
instance FromJSON Address where
instance A.Audit Address where
    toKey = AddressKey
    fromKey = unAddressKey
    deleteField = AddressDeleted
    deleted = addressDeleted

instance ToJSON Airplane where
instance FromJSON Airplane where
instance A.Audit Airplane where
    toKey = AirplaneKey
    fromKey = unAirplaneKey
    deleteField = AirplaneDeleted
    deleted = airplaneDeleted

instance ToJSON Reservation where
instance FromJSON Reservation where
instance A.Audit Reservation where
    toKey = ReservationKey
    fromKey = unReservationKey
    deleteField = ReservationDeleted
    deleted = reservationDeleted

instance ToJSON Session where
instance FromJSON Session where
