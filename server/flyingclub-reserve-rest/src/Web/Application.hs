{-# LANGUAGE OverloadedStrings #-}
module Web.Application (application) where

import           Data.ReserveRoute
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Wai.Routes
import           Web.ErrorHandler
import           Web.Route

appCors :: Middleware
appCors = cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Authorization", "Auth-Key", "Content-Type"] })

application :: ReserveRoute -> Application
application ctx = waiApp $ do
  middleware appCors
  middleware (logger ctx)
  middleware errorHandler
  route ctx
