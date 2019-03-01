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
import           Data.Time.Calendar
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
    firstname   T.Text
    lastname    T.Text
    permission  UserType
    signedRulesOfOp Day
    deleted     UTCTime Maybe
    deriving Show Generic
Address
    Id          UUID
    userId      UserId
    address     T.Text
    city        T.Text
    state       T.Text
    zip         T.Text
    showInDirectory  Bool
    deleted     UTCTime Maybe
    deriving Show Generic
Email
    Id          UUID
    userId      UserId
    description T.Text
    address     T.Text
    receiveNotification  Bool
    showInDirectory  Bool
    deleted     UTCTime Maybe
    deriving Show Generic
Phone
    Id          UUID
    userId      UserId
    description T.Text
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
    tail        T.Text
    description T.Text
    deleted     UTCTime Maybe
    deriving Show Generic
Reservation
    Id          UUID
    userId      UserId
    airplaneId  AirplaneId
    start       UTCTime
    end         UTCTime    
    maintenance Bool
    comment     T.Text
    deleted     UTCTime Maybe
    deriving Show Generic
Currency
    Id          UUID
    userId      UserId
    name        T.Text -- if a user has multiple family members, this could just be a descriptive field
    flightReview    Day Maybe
    medical         Day Maybe
    overrideMedicalLength   Int Maybe
    dateOfBirth     Day    
    comment     T.Text -- for officers only
    deleted     UTCTime Maybe
    deriving Show Generic
Notification
    Id          UUID
    userId      UserId
    posted      UTCTime
    sent        UTCTime Maybe
    content     T.Text
Session
    Id          UUID
    authKey     UUID
    userId      UserId
    created     UTCTime
    used        UTCTime
    expired     UTCTime Maybe
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
type SqlM o = ReaderT SqlBackend (LoggingT (ResourceT IO)) o

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


instance (PersistEntity a, ToJSON a) => ToJSON (Entity a) where
    toJSON = entityIdToJSON

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

instance ToJSON Currency where
instance FromJSON Currency where
instance A.Audit Currency where
    toKey = CurrencyKey
    fromKey = unCurrencyKey
    deleteField = CurrencyDeleted
    deleted = currencyDeleted
        
instance ToJSON Session where
instance FromJSON Session where
