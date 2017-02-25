{-# LANGUAGE ScopedTypeVariables #-}
module Web.ErrorHandler (errorHandler) where

import           Control.Exception
import           Control.Exception.Unauthorized
import           Data.ByteString.Lazy.Char8
import           Network.HTTP.Types
import           Network.Wai

handleWithStatus :: Exception e => (Response -> IO ResponseReceived) -> e -> Status -> IO ResponseReceived
handleWithStatus sendResponse ex status  = sendResponse $ responseLBS status [] $ pack $ show ex

errorHandler :: Middleware
errorHandler app req sendResponse = catches  (app req sendResponse)
    [Handler (\ (ex :: UnauthorizedException)   -> handleWithStatus sendResponse ex status403),
     Handler (\ (ex :: SomeException)           -> handleWithStatus sendResponse ex status500)]
