{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.ErrorHandler (errorHandler) where

import           Control.Exception
import           Control.Exception.Conflict
import           Control.Exception.Format
import           Control.Exception.Unauthorized
import           Data.Aeson
import           Network.HTTP.Types
import           Network.Wai

handleWithStatus :: Exception e => (Response -> IO ResponseReceived) -> e -> Status -> IO ResponseReceived
handleWithStatus sendResponse ex status  = putStrLn (show ex) >> (sendResponse $ responseLBS status [] "TODO") -- TODO need to log, need context

errorHandler :: Middleware
errorHandler app req sendResponse = catches  (app req sendResponse)
    [
        Handler (\ (ex :: ConflictException)        -> handleWithStatus sendResponse ex status409),
        Handler (\ (ex :: FormatException)          -> handleWithStatus sendResponse ex status400),        
        Handler (\ (ex :: UnauthorizedException)    -> handleWithStatus sendResponse ex status403),
        Handler (\ (ex :: SomeException)            -> handleWithStatus sendResponse ex status500)]
