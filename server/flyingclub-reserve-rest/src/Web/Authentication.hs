{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Authentication where

import           Control.Monad.Trans
import           Data.Aeson                      (ToJSON)
import           Data.ByteString
import           Data.ReserveRoute
import qualified Data.Text.Encoding              as E
import           Database.Persist.Schema
import qualified Database.Persist.Session        as S
import           Database.Persist.Sql
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Network.HTTP.Types
import           Network.Wai.Middleware.HttpAuth
import           Request.Api.AuthorizedAction
import           Wai.Routes

extractSessionToken :: ByteString -> Maybe (Key Session)
extractSessionToken x = case extractBearerAuth x >>= return.keyFromValues.(:[]).(PersistText).(E.decodeUtf8) of
  Just (Right token) -> Just token
  _                  -> Nothing


authorizedSession :: ToJSON a => (ReserveRoute -> ByteString -> Session -> SqlM (Either Status a)) -> Handler ReserveRoute
authorizedSession f = runHandlerM $ do
    rr@ReserveRoute{..} <- sub
    authHeader <- lookup hAuthorization <$> reqHeaders
    authKey <- reqHeader "auth-key"
    let sid = authHeader >>= extractSessionToken
    let sauthkey = authKey >>= fromText
    case (sid, sauthkey) of
      -- ensure that both a session id and key are present
      (Just sid', Just sauthkey') -> do
        session <- liftIO $ sql $ S.getValidSession sid'
        -- check if the actual session auth key matches the submitted session auth key
        let authm = session >>= (\x->return $ (Just (sessionAuthKey (entityVal x)) == sauthkey))
        case authm of
          Just True -> do
            let (Just s) = entityVal <$> session
            body <- rawBody
            resstat <- liftIO $ sql $ f rr body s
            case resstat of
              Left stat    -> status stat
              Right result -> json result
          _ -> status status401
      _ -> status status401

authorizedRoute :: ToJSON a => AuthorizedAction a -> Handler ReserveRoute
authorizedRoute action = authorizedSession $ const $ const $ \session -> do
  let userId = sessionUserId session
  result <- runAuthorizedAction userId action
  return $ Right result


-- for testing
getVerifyAuthR :: Handler ReserveRoute
getVerifyAuthR = authorizedRoute (authorize Officer $ const $ return ("OK" :: String))
