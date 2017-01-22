module Main where

import qualified Database.Persist.MySQL          as MP
--import qualified Network.Wai.Handler.FastCGI    as FCGI (run)
import qualified Network.Wai.Handler.Warp        as Warp (run)
--import           Network.Wai.Middleware.Approot
--import           Web.Application
import           Control.Monad.Logger            (runLoggingT,
                                                  runStderrLoggingT)
import           Control.Monad.Trans
import           Database.Persist.Schema
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           System.Random

devConnStr = MP.defaultConnectInfo {
  MP.connectUser = "test",
  MP.connectPassword = "test",
  MP.connectDatabase = "test"}


devUser :: IO ()
devUser = runStderrLoggingT $ MP.withMySQLPool devConnStr 10 $ \pool -> liftIO $ do
    flip MP.runSqlPersistMPool pool $ do
        MP.getMigration migrateAll -- just to force type
        u1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
        MP.insertKey u1 $ User "Steve" "K" "" Officer Nothing
        e1 <- liftIO $ EmailKey <$> (randomIO :: IO UUID)
        MP.insertKey e1 $ Email u1 "" "steve@kolls.net" True True Nothing



showMigration :: IO ()
showMigration = runStderrLoggingT $ MP.withMySQLPool devConnStr 10 $ \pool -> liftIO $ do
    flip MP.runSqlPersistMPool pool $ do
      MP.printMigration migrateAll

main :: IO ()
main = undefined

{--
dev :: IO ()
dev = do
    migrate devConnStr
    print "http://localhost:8080/"
    prepareApp devConnStr $ Warp.run 8080
--}
