module Web.Application where

import Data.ReserveRoute
import Web.Route
import Network.Wai
import Wai.Routes

application :: ReserveRoute -> Application
application ctx = do
  --middleware logStdoutDev
  --middleware errorHandler
  --middleware $ checkCreds pool userkey
  waiApp $ route $ ctx
