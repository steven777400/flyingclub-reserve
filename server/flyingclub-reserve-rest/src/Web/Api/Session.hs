{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Web.Api.Session (postLoginR) where

import           Control.Monad.Trans
import           Data.ByteString
import           Data.ReserveRoute
import           Database.Persist.Schema
import qualified Database.Persist.Session        as S
import           Database.Persist.Sql
import           Network.HTTP.Types
import           Network.Wai.Middleware.HttpAuth
import           Wai.Routes
import           Web.Authentication

handleLogin :: ByteString -> SqlM (Maybe (Entity Session))
handleLogin (extractBasicAuth -> Just (username, password))
  = S.login username password
handleLogin (extractSessionToken -> Just token)
  = S.getValidSession token
handleLogin _ = return Nothing


postLoginR :: Handler ReserveRoute
postLoginR = runHandlerM $ do
    ReserveRoute{..} <- sub
    authHeader <- lookup hAuthorization <$> reqHeaders
    case authHeader of
      Nothing -> status status401
      Just auth -> do
        session <- liftIO $ sql $ handleLogin auth
        case session of
            Just session -> json session -- TODO include user info
            Nothing      -> status status401
