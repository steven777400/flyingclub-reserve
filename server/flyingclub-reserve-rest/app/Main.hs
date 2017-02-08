module Main where

import Data.ReserveRoute
import Web.Application
import Web.Route
import Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp (run)



main :: IO ()
main = Warp.run 8080 $ application ReserveRoute
