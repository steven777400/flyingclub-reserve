{-# LANGUAGE ViewPatterns      #-}
module Web.Session (postLoginR) where

import           Control.Monad.Trans
import           Data.ReserveRoute
import           Database.Persist.Schema
import qualified Database.Persist.Session        as S
import           Database.Persist.Sql
import           Network.HTTP.Types
import           Network.Wai.Middleware.HttpAuth
import           Wai.Routes
import Data.ByteString
import           Web.PathPieces
import qualified Data.Text.Encoding as E

handleLogin :: ByteString -> SqlM (Maybe (Entity Session))
handleLogin (extractBasicAuth -> Just (username, password))
  = S.login username password
handleLogin (\x -> extractBearerAuth x >>= (fromPathPiece.(E.decodeUtf8)) -> Just token)
  = S.getValidSession token
handleLogin _ = return Nothing

postLoginR :: Handler ReserveRoute
postLoginR = runHandlerM $ do
    ReserveRoute sql <- sub
    authHeader <- lookup hAuthorization <$> reqHeaders
    case authHeader of
      Nothing -> status status401
      Just auth -> do
        session <- liftIO $ sql $ handleLogin auth
        case session of
            Just session -> json session
            Nothing      -> status status401
