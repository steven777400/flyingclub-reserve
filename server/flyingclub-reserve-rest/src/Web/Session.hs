{-# LANGUAGE ViewPatterns #-}
module Web.Session (postLoginR) where

import           Control.Monad.Trans
import           Data.ByteString
import           Data.ReserveRoute
import qualified Data.Text.Encoding              as E
import           Database.Persist.Schema
import qualified Database.Persist.Session        as S
import           Database.Persist.Sql
import           Database.Persist.Types.UUID
import           Network.HTTP.Types
import           Network.Wai.Middleware.HttpAuth
import           Wai.Routes

handleLogin :: ByteString -> SqlM (Maybe (Entity Session))
handleLogin (extractBasicAuth -> Just (username, password))
  = S.login username password
handleLogin (\x -> extractBearerAuth x >>= return.keyFromValues.(:[]).(PersistText).(E.decodeUtf8) -> Just (Right token))
  = S.getValidSession token
handleLogin _ = return Nothing

-- TODO: needs tests
postLoginR :: Handler ReserveRoute
postLoginR = runHandlerM $ do
    ReserveRoute sql <- sub
    authHeader <- lookup hAuthorization <$> reqHeaders
    case authHeader of
      Nothing -> status status401
      Just auth -> do
        session <- liftIO $ sql $ handleLogin auth
        case session of
            Just session -> json session -- TODO include user info
            Nothing      -> status status401
