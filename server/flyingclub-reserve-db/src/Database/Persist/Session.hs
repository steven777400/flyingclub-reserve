module Database.Persist.Session (login, getValidSession, logout, logoutAll) where

import           Control.Exception.StackError
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.ByteString                    (ByteString)
import           Data.List                          (nub)
import           Data.Maybe                         (fromJust)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Persist.Audit.Operations  (isDeleted)
import           Database.Persist.Class
import           Database.Persist.Schema
import           Database.Persist.Sql
import           Database.Persist.Types
import qualified Database.Persist.Types.PhoneNumber as PN
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Prelude                            hiding (error)
import           System.Random

lockOnFailedLogins :: Int
lockOnFailedLogins = 10

verifyPassword :: Key User -> ByteString -> SqlM Bool
verifyPassword userId password = do
  -- Note: We assume there must be an authentication record for this user!
  auth <- fromJust <$> getBy (AuthUserId userId)
  let correctpin = verifyPIN password (authenticationPin (entityVal auth))
  -- if not correct pin, increment bad login count
  when (not correctpin) $ update (entityKey auth) [AuthenticationFailedLoginCount +=. 1]
  -- regardless, if not correct or too many bad attempts, abort
  let allow = authenticationFailedLoginCount (entityVal auth) < lockOnFailedLogins && correctpin
  -- if allowed, reset bad login count
  when allow $ update (entityKey auth) [AuthenticationFailedLoginCount =. 0]
  return allow

login :: ByteString -> ByteString -> SqlM (Maybe (Entity Session))
login username password = do
    emails <-
         (map (emailUserId.entityVal)) <$> ((selectList [EmailAddress ==. decodeUtf8 username] []) :: SqlM [Entity Email])
    phones <- case PN.toMaybePhoneNumber username of
        Just pn ->
           (map (phoneUserId.entityVal)) <$> ((selectList [PhoneNumber ==. pn] []) :: SqlM [Entity Phone])
        Nothing -> return []

    case nub (emails++phones) of
        [userId] -> do
            user <- get userId
            let validUser = case user of
                  Nothing -> False
                  Just u  -> not (isDeleted u) && userPermission u /= NoAccess
            -- verify password
            allow <- verifyPassword userId password
            if (not allow) || (not validUser)
              then return Nothing
              else do
                -- CREATE session
                uuid <- liftIO (randomIO :: IO UUID)
                authKey <- liftIO (randomIO :: IO UUID)
                now <- liftIO getCurrentTime
                let valkey = SessionKey uuid
                let session = Session authKey userId now now Nothing
                insertKey valkey session
                return $ Just (Entity valkey session)
        [] -> return Nothing
        _ -> error "multiple users"

getValidSession :: Key Session -> SqlM (Maybe (Entity Session))
getValidSession sessionId = do
  msession <- get sessionId
  case msession of
    Nothing -> return Nothing
    Just session -> if sessionExpired session == Nothing
      then do
        now <- liftIO getCurrentTime
        update sessionId [SessionUsed =. now]
        return $ Just (Entity sessionId session)
      else return Nothing

logout :: Key Session -> SqlM ()
logout sessionId = do
  now <- liftIO getCurrentTime
  update sessionId [SessionExpired =. Just now]

logoutAll :: Key User -> SqlM ()
logoutAll userId = do
    now <- liftIO getCurrentTime
    updateWhere [SessionUserId ==. userId] [SessionExpired =. Just now]
