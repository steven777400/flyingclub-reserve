module Web.Application where

import           Data.ReserveRoute
import           Network.Wai
import           Wai.Routes
import           Web.ErrorHandler
import           Web.Route

application :: ReserveRoute -> Application
application ctx = waiApp $ do
  middleware (logger ctx)
  middleware errorHandler
  --middleware $ checkCreds pool userkey
  route $ ctx
